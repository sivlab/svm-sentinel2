#-------------------2 STEPS (MASKING)-------------------#
# Versione del software: 2-steps (masking)
# Questa versione del software effettua la classificazione delle immagini in due step
# Tra lo STEP 1 e STEP 2 viene effettuato il MASKING di alcune classi
# Tali classi sono scelte nel seguente modo:
# Una volta calcolate le metriche relative alla matrice di confusione dello STEP 1, vengono selezionate
# le classi che hanno già un ottimo F1-Score
# Di default si scelgono: (2,9,11,12,13,14)
# Per modificare -> variabile "index_to_remove"


#Si resetta tutto l'ambiente
rm(list = ls())
invisible(gc())
#Pulisce la console
cat("\014")

#Output su file
sink("log_file.txt")

#-----------------VARIABILI DA SETTARE-----------------#
#Settare a TRUE se si vogliono mostrare le istruzioni all'avvio dello script
instructions <- FALSE

#Variabile che se settata a TRUE, mostra in console una serie di informazioni utili per il debug
#Ad es. tempi di completamento delle singole sezioni di codice, messaggi, etc
debug <- FALSE

#Variabile da settare a TRUE solo se si ha la libreria "Rgtsvm" installata
#Variante di SVM in "e1071" che implementa calcolo parallelo su GPU
#Necessita di framework CUDA e BOOST
cuda_installed <- FALSE #TRUE or FALSE

#Variabile che abilita/disabilita il tuning del modello SVM
#Tecnicamente la libreria ha un metodo tune.svm() che permette, dati un RANGE di valori di C e gamma,
#di trovare i valori migliori per creare il modello SVM
#
#Tempo richiesto: elevato
#Una volta che la funzione è stata lanciata 1 volta e quindi sono stati individuati i valori di C e gamma
#è possibile usare in maniera statica quest'ultimi, senza lanciare il tuning di svm
tuning <- FALSE #TRUE or FALSE
range_costo <- 2^(-5:5) #range valori tuning: 2^(-5:5)
range_gamma <- 2^(-5:5) #range valori tuning: 2^(-5:5)
C_std_value_zone_edif <- 16 #Tuning zone edificate: 16 - STEP 1
gamma_std_value_zone_edif <- 0.25 #Tuning zone edificate: 0.25 - STEP 1
C_std_value_veget <- 32 #Tuning vegetazione: 32 - STEP 2
gamma_std_value_veget <- 0.0625 #Tuning vegetazione: 0.0625 - STEP 2


#------------------------------------------------------#


#Carico tutti i file .R
sources <- c("checkDependencies.R","checkDirectory.R","loadInput.R","validateImage.R","createDataset.R","checkTrainingTest.R","splitDataset.R","helperFunctions.R")
for (i in 1:length(sources)) {
  source(sources[i])
}


#------------------CHECK DEPENDENCIES------------------#
#Controllo che tutte le librerie siano installate e caricate
checkDependencies()
#Debug per controllare se i pacchetti sono stati caricati correttamente:
print_debug(.packages())


tic_debug("Total time")
#-------------------MOSTRO ISTRUZIONI------------------#
istruzioni()


#-------------------CHECK DIRECTORY-------------------#
#Vedi file "checkDirectory.R" per dettagli
image_dir <- "images"
training_dir <- "training"
test_dir <- "test"
final_dir <- "final_images"
checkDirectory(image_dir,training_dir,test_dir,final_dir)


#-------------------LOAD TRAINING--------------------#
print("Loading Shapefiles")
flush.console()
tic_debug("Load Shapefiles")
#Vedere file checkTrainingTest.R per ulteriori dettagli
shapes <- checkTrainingTest(training_dir,test_dir)
shapefileNamesTraining = shapes$shapefileNamesTraining #NON contengono "no_data"
shapefileNamesTest = shapes$shapefileNamesTest         #NON contengono "no_data"
corrispondenza_classi = shapes$corrispondenza_classi   #NON contengono "no_data"
no_data_present = shapes$no_data_present
rm(shapes) #free memory
invisible(gc()) #free memory
toc_debug()


#------------------CARICAMENTO INPUT------------------#
print("Loading input images")
flush.console()
tic_debug("Loading input images")
#Vedi file "Training.R" per dettagli
#La funzione "loadingInput" restituisce una lista contenente:
# -Lista di nomi delle immagini in input
# -Lista delle immagini (lista di raster objects)
input <- loadInput(image_dir,training_dir,test_dir)
imageNames <- input$imageNames
imageList <- input$imageList
rm(input) #free memory
invisible(gc()) #free memory
toc_debug()


#----------------VALIDAZIONE IMMAGINI-----------------#
#Vedere file validateImage.R per ulteriori dettagli
dimensions <- validateImage(imageList)
dimX=dimensions$dim_x
dimY=dimensions$dim_y
rm(dimensions) #free memory
invisible(gc()) #free memory


#--------------COSTRUZIONE DATASET STEP 1---------------#
print("Creating Dataset - Step 1")
flush.console()
#Vedere file createDataset.R per ulteriori dettagli
tic_debug('Creating Dataset - Step 1')

#È possibile passare i valori: "vegetazione" e "zona_edificata"
# -STEP 1 -> "zona_edificata"
# -STEP 2 -> "vegetazione"
matrice <- createDataset(dimX,dimY,imageList,"zona_edificata")
toc_debug()


#------------------SPLIT DATASET STEP 1-----------------#
print("Splitting Dataset - Step 1")
flush.console()
#Vedere file splitDataset.R per ulteriori dettagli
tic_debug('Splitting dataset in: (Training, Test, Prediction, NO_DATA)')
dataset <- splitDataset(training_dir,test_dir,imageList,matrice,shapefileNamesTraining,shapefileNamesTest,no_data_present,corrispondenza_classi)
datasetTraining <- dataset$datasetTraining
datasetTest <- dataset$datasetTest
datasetPrediction <- dataset$datasetPrediction
toc_debug()

#Dati NO_DATA semplicemente "eliminati" dal tutto il processo di Machine Learning
#Ri-aggiunti prima del plot quando si "ricostruisce" l'immagine
#ACH: possono essere null, quindi prima di reinserirli vanno controllati
if(no_data_present){
  datasetNoData <-dataset$datasetNoData
}

rm(matrice) #free memory
rm(dataset) #free memory
invisible(gc()) #free memory

#Si mette in relazione il numero di pixel di training rispetto ai pixel totali dell'immagine
percentuale_training <- (nrow(datasetTraining)/(dimX*dimY))*100
cat(sprintf("Percentuale di dati di Training rispetto ai pixel totali: %.2f%% \n",percentuale_training))


#--------------------SVM - STEP 1--------------------#
if(cuda_installed){
  #Struttura dati BigMatrix -> tipo di dato accettato da Rgtsvm
  #Anzichè copiare la matrice ne crea una reference -> svm() e predict() (teoricamente) più veloci
  dati_training <- attach.bigmatrix(data = datasetTraining[,2:(ncol(datasetTraining)-1)])
  dati_target <- datasetTraining[,ncol(datasetTraining)]
}else{
  dati_training <- datasetTraining[,2:(ncol(datasetTraining)-1)]
  dati_target <- datasetTraining[,ncol(datasetTraining)]
}


#-----------CREATING SVM MODEL - STEP 1--------------#
set.seed(1) #Crea risultati SVM "riproducibili" e non casuali. Eliminare set.seed() per risultati casuali

if(tuning){
  print("Tuning SVM Model - Step 1")
  flush.console()
  tic_debug('Tuning SVM Model - Step 1')
  tuned <- tune.svm(x=dati_training,
                    y=factor(dati_target), #usando "factor" SVM switcha direttamente sul metodo "C-Classification" (altrimenti si imposta sul metodo regressione)
                    kernel ="radial",
                    gamma = range_gamma, cost = range_costo, #ACH: range MOLTO grande
                    tunecontrol = tune.control(cross = 10)) #cross validation
  print(tuned)
  #valori ottimali trovati
  C_value_zone_edif <- tuned$best.parameters$cost
  gamma_value_zone_edif <- tuned$best.parameters$gamma
  toc_debug()
}else{
  C_value_zone_edif <- C_std_value_zone_edif
  gamma_value_zone_edif <- gamma_std_value_zone_edif
}


print("Creating SVM Model - Step 1")
flush.console()
tic_debug('Creating SVM Model - Step 1')
svm_model <- svm(x=dati_training,
                 y=factor(dati_target),
                 type = "C-classification",
                 kernel ="radial",
                 gamma = gamma_value_zone_edif,
                 cost = C_value_zone_edif)
toc_debug()


#--------TEST & CONFUSION MATRIX - STEP 1------------#
print("Creating Confusion Matrix and Metrics - Step 1")
flush.console()
tic_debug("Create Confusion Matrix and Metrics - Step 1")
print("MATRICE DI CONFUSIONE - Step 1 (zona edificata):")
if(cuda_installed){
  dati_matrice_confusione <- attach.bigmatrix( data = datasetTest[,2:(ncol(datasetTest)-1)])
}else{
  dati_matrice_confusione <- datasetTest[,2:(ncol(datasetTest)-1)]
}

predizione_test <- predict(svm_model,dati_matrice_confusione)
confusion_matrix <- table(predizione_test,datasetTest[,ncol(datasetTest)])
#RIGHE=PREDIZIONI
#COLONNE=VARIABILI TARGET TEST
print(confusion_matrix)

#Calcolo indici:
print("Metriche del sistema - Step 1 (zona edificata):")
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall <- diag(confusion_matrix) / colSums(confusion_matrix)
f1 <- 2 * precision * recall / (precision + recall)
print(paste("Accuratezza del sistema:",accuracy,sep=" "))
print(data.frame(precision, recall, f1))
toc_debug()


#----------------PREDICTION - STEP 1-----------------#
print("Predicting - Step 1")
flush.console()
tic_debug('Prediction - Step 1')

if(cuda_installed){
  dati_prediction <- attach.bigmatrix( data = datasetPrediction[,2:(ncol(datasetPrediction)-1)])
}else{
  dati_prediction <- datasetPrediction[,2:(ncol(datasetPrediction)-1)]
}
prediction <- predict(svm_model,dati_prediction)
prediction <- as.numeric(as.character(prediction))

toc_debug() #tic_debug('Prediction')


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------#
print("Masking")
flush.console()


#---------------MASKING ZONE EDIFICATE---------------#
#Elimino i pixel relativi alle classi: 2,9,11,12,13,14 (quelle con f1-score migliore dopo lo STEP 1)
#
#Tecnicamente creo un "dataset zone edificate" che si andrà poi a unire in fase di ricostruzione immagine -> MASKING
#
#!!!OSS!!! Si devono:
# -Eliminare i pixel che cadono nelle categorie zone edificate (Altrimenti vengono predetti nuovamente nella fase successiva)
# -Eliminare le categorie appartenenti alle classi di cui si vuole fare masking
#
#Elimino dalla variabile "corrispondenza_classi" le classi di cui fare masking (vedi dopo)
prediction_step1 <- cbind(datasetPrediction[,1],prediction)
datasetZoneEdif <- prediction_step1[prediction_step1[,ncol(prediction_step1)]==2
                                   |prediction_step1[,ncol(prediction_step1)]==9
                                   |prediction_step1[,ncol(prediction_step1)]==11
                                   |prediction_step1[,ncol(prediction_step1)]==12
                                   |prediction_step1[,ncol(prediction_step1)]==13
                                   |prediction_step1[,ncol(prediction_step1)]==14,]


#--------------COSTRUZIONE DATASET STEP 2---------------#
print("Creating Dataset Step - Step 2")
flush.console()
tic_debug('Creating Dataset Step - Step 2')
matrice <- createDataset(dimX,dimY,imageList,"vegetazione") #vegetazione, zona_edificata, all
toc_debug()


#------------------------MASKING------------------------#
#Le operazioni necessarie per il masking:
# -Eliminare dal datasetPrediction gli elementi di "datasetZoneEdif", perchè
#  tali pixel non devono essere ri-classificati -> FATTO IN SEGUITO
#
# -Eliminare dalla variabile "corrispondenza_classi","shapefileNamesTraining" e "shapefileNamesTest"
#  TUTTI i riferimenti alle classi già predette, quindi: 2,9,11,12,13,14
#
# -Salvare "datasetTraining" e "datasetTest" per la fase di ricostruzione immagine
index_to_remove <- c(2,9,11,12,13,14) #elimino riferimenti alle classi già predette
corrispondenza_classi <- corrispondenza_classi[-index_to_remove,]
shapefileNamesTraining <- shapefileNamesTraining[-index_to_remove]
shapefileNamesTest <- shapefileNamesTest[-index_to_remove]
datasetTraining_step1 <- datasetTraining
datasetTest_step1 <- datasetTest


#------------------SPLIT DATASET STEP 2-----------------#
print("Splitting Dataset - Step 2")
flush.console()
tic_debug('Splitting dataset in: (Training, Test, Prediction)')
dataset <- splitDataset(training_dir,test_dir,imageList,matrice,shapefileNamesTraining,shapefileNamesTest,no_data_present,corrispondenza_classi)
datasetTraining <- dataset$datasetTraining
datasetTest <- dataset$datasetTest
datasetPrediction <- dataset$datasetPrediction
toc_debug()

#Dati NO_DATA semplicemente "eliminati" dal tutto il processo di Machine Learning
#Ri-aggiunti prima del plot quando si "ricostruisce" l'immagine
#ACH: possono essere null, quindi prima di reinserirli vanno controllati
if(no_data_present){
  datasetNoData <-dataset$datasetNoData
}

rm(matrice) #free memory
rm(dataset) #free memory
invisible(gc()) #free memory

#!!!IMPORTANTE: Elimino dal "datasetPrediction" i pixel predetti nello STEP 1
datasetPrediction <- datasetPrediction[!(datasetPrediction[,1] %in% datasetZoneEdif[,1]),]

#Nello STEP 2, i dataset di Training e Test sono più piccoli (meno elementi)
#Poichè il dataset di Prediction si crea per DIFFERENZA (vedi spliDataset.R), vuol dire che dei pixel che prima (STEP 1)
#erano di Training o Test, ora che si sono tolte le classi già predette, "cadranno" nel dataset di Prediction
#Perciò, vanno eliminati
datasetPrediction <- datasetPrediction[!(datasetPrediction[,1] %in% datasetTraining_step1[,1]),]
datasetPrediction <- datasetPrediction[!(datasetPrediction[,1] %in% datasetTest_step1[,1]),]


#--------------------SVM - STEP 2--------------------#
if(cuda_installed){
  #Struttura dati BigMatrix -> tipo di dato accettato da Rgtsvm
  #Anzichè copiare la matrice ne crea una reference -> svm() e predict() (teoricamente) più veloci
  dati_training <- attach.bigmatrix(data = datasetTraining[,2:(ncol(datasetTraining)-1)])
  dati_target <- datasetTraining[,ncol(datasetTraining)]
}else{
  dati_training <- datasetTraining[,2:(ncol(datasetTraining)-1)]
  dati_target <- datasetTraining[,ncol(datasetTraining)]
}


#-----------CREATING SVM MODEL - STEP 2--------------#
set.seed(1) #Crea risultati SVM "riproducibili" e non casuali. Eliminare set.seed() per risultati casuali

if(tuning){
  print("Tuning SVM Model - Step 2")
  flush.console()
  tic_debug('Tuning SVM Model - Step 2')
  tuned <- tune.svm(x=dati_training,
                    y=factor(dati_target), #usando "factor" SVM switcha direttamente sul metodo "C-Classification" (altrimenti si imposta sul metodo regressione)
                    kernel ="radial",
                    gamma = range_gamma, cost = range_costo, #ACH: range MOLTO grande
                    tunecontrol = tune.control(cross = 10)) #cross validation
  print(tuned)
  #valori ottimali trovati
  C_value_veget <- tuned$best.parameters$cost
  gamma_value_veget <- tuned$best.parameters$gamma
  toc_debug()
}else{
  C_value_veget <- C_std_value_veget
  gamma_value_veget <- gamma_std_value_veget
}

print("Creating SVM Model - Step 2")
flush.console()
tic_debug('Creating SVM Model - Step 2')
svm_model <- svm(x=dati_training,
                 y=factor(dati_target),
                 type = "C-classification",
                 kernel ="radial",
                 gamma = gamma_value_zone_edif,
                 cost = C_value_zone_edif)
toc_debug()

print("Creating Confusion Matrix and Metrics - Step 2")
flush.console()
tic_debug("Create Confusion Matrix and Metrics - Step 2")
#--------TEST & CONFUSION MATRIX - STEP 2------------#
print("MATRICE DI CONFUSIONE - Step 2 (vegetazione):")
if(cuda_installed){
  dati_matrice_confusione <- attach.bigmatrix( data = datasetTest[,2:(ncol(datasetTest)-1)])
}else{
  dati_matrice_confusione <- datasetTest[,2:(ncol(datasetTest)-1)]
}

predizione_test <- predict(svm_model,dati_matrice_confusione)
confusion_matrix <- table(predizione_test,datasetTest[,ncol(datasetTest)])
#RIGHE=PREDIZIONI
#COLONNE=VARIABILI TARGET TEST
print(confusion_matrix)

#Calcolo indici:
print("Metriche del sistema - Step 2 (vegetazione):")
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall <- diag(confusion_matrix) / colSums(confusion_matrix)
f1 <- 2 * precision * recall / (precision + recall)
print(paste("Accuratezza del sistema:",accuracy,sep=" "))
print(data.frame(precision, recall, f1))
toc_debug()


#-------------PREDICTION ZONE VEGETATIVE-------------#
#Si effettua nuovamente tutto il processo SVM utilizzando SOLO le features della vegetazione
#In fase di predizione, ovviamente, vengono eliminate le classe "zone edificate"
print("Predicting - Step 2")
flush.console()
tic_debug('Prediction - Step 2')

if(cuda_installed){
  dati_prediction <- attach.bigmatrix( data = datasetPrediction[,2:(ncol(datasetPrediction)-1)])
}else{
  dati_prediction <- datasetPrediction[,2:(ncol(datasetPrediction)-1)]
}
prediction <- predict(svm_model,dati_prediction)
prediction <- as.numeric(as.character(prediction))

toc_debug() #tic_debug('Prediction')


#-------------RICOSTRUZIONE IMMAGINE-----------------#
cat("Writing \"file_image_STEP2\" in \"images/\".")
flush.console()
#(x,y) con y = variabile predetta da SVM - STEP 2
image_predicted_svm <- cbind(datasetPrediction[,1],prediction)
#Quando unisco i dataset di Training e di Test nell'immagine finale, devo considerare quelli creati nello STEP 1!!!
#Infatti quelli creati nello STEP 2 non hanno le classi affini alla "zone edificata" -> Masking
#(x,y) con y = variabile target già impostata dai dati di training - NELLO STEP 1
image_predicted_training <- cbind(datasetTraining_step1[,1],datasetTraining_step1[,ncol(datasetTraining_step1)])
#(x,y) con y = variabile target già impostata dai dati di test - NELLO STEP 2
image_predicted_test <- cbind(datasetTest_step1[,1],datasetTest_step1[,ncol(datasetTest_step1)])


#Se ci sono "NO_DATA" vanno reinseriti prima di ricostruire l'immagine
if(no_data_present){
  #(x,y) con y = variabile target già impostata dai dati di no_data
  image_predicted_nodata <- cbind(datasetNoData[,1],datasetNoData[,ncol(datasetNoData)])
  #RICORDA!: Devo inserire ciò che calcolato nello STEP 1 !!! -> datasetZoneEdif
  image_predicted <- rbind(datasetZoneEdif,image_predicted_svm,image_predicted_training,image_predicted_test,image_predicted_nodata)
}else{
  #RICORDA!: Devo inserire ciò che calcolato nello STEP 1 !!! -> datasetZoneEdif
  image_predicted <- rbind(datasetZoneEdif,image_predicted_svm,image_predicted_training,image_predicted_test)
}
#IMPORTANTE: nella variabile "image_predicted" ho una matrice con:
#   -prima colonna: numero di celle relativa all'immagine satellitare (immagine in input)
#   -seconda colonna: valore della variabile target

#Per ricostruire l'immagine in maniera efficiente, si:
# -prende una "singola immagine", che tecnicamente è una sola banda di una sola immagine in input
# -si sostituiscono i valori relativi alle celle, con i valori della variabile target
# -IN CONCLUSIONE: ad ogni cella (=> pixel) sarà associata una "classe"
singleimage <- imageList[[1]]
singleband <- raster(singleimage, layer=1)
#si usa la funzione "order()" altrimenti i valori inseriti nell'immagine non corrispondono ai "giusti" pixel
#ciò è dovuto al fatto che il dataset iniziale è stato splittato, quindi è stato perso l'ordinamento originale dei pixel
image_predicted <- image_predicted[order(image_predicted[,1]),]
final_image<-setValues(singleband, image_predicted[,2])

#Scrittura dell'immagine su disco
#È possibile aprire tale immagine con un qualunque software ed usare una "palette" di colori
file_image <- writeRaster(final_image, filename=paste(final_dir,"file_image_2STEP",sep="/"), format="ENVI", bandorder='BIL', overwrite=TRUE)

toc_debug()#toc_debug finale -> tic_debug("Total time")