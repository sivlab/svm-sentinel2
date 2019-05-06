#--------------------------ALL---------------------------#
# Versione del software: ALL
# Questa versione del software effettua la classificazione delle immagini in un UNICO step
# Vengono considerate tutte le immagini di input
# Per quanto riguarda le bande:
# -12 bande (1/mese) per MSAVI, VARI, NDRE, NDBBBI, NDBSI
# -1 banda per BRIGHTNESS (MAX valore di ogni pixel assunto nelle varie bande)
# -6 bande per PHENO_NDVI


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
C_std_value_all<- 16 #Tuning C_std_value_all: 16
gamma_std_value_all <- 0.0625 #Tuning gamma_std_all: 0.0625


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


#-----------------COSTRUZIONE DATASET-------------------#
print("Creating Dataset")
flush.console()
#Vedere file createDataset.R per ulteriori dettagli
tic_debug('Creating Dataset')

#È possibile passare i valori: "all"
matrice <- createDataset(dimX,dimY,imageList,"all")
toc_debug()


#---------------------SPLIT DATASET---------------------#
print("Splitting Dataset")
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


#--------------------------SVM--------------------------#
if(cuda_installed){
  #Struttura dati BigMatrix -> tipo di dato accettato da Rgtsvm
  #Anzichè copiare la matrice ne crea una reference -> svm() e predict() (teoricamente) più veloci
  dati_training <- attach.bigmatrix(data = datasetTraining[,2:(ncol(datasetTraining)-1)])
  dati_target <- datasetTraining[,ncol(datasetTraining)]
}else{
  dati_training <- datasetTraining[,2:(ncol(datasetTraining)-1)]
  dati_target <- datasetTraining[,ncol(datasetTraining)]
}


#-----------------CREATING SVM MODEL--------------------#
set.seed(1) #Crea risultati SVM "riproducibili" e non casuali. Eliminare set.seed() per risultati casuali

if(tuning){
  print("Tuning SVM Model")
  flush.console()
  tic_debug('Tuning SVM Model')
  tuned <- tune.svm(x=dati_training,
                    y=factor(dati_target), #usando "factor" SVM switcha direttamente sul metodo "C-Classification" (altrimenti si imposta sul metodo regressione)
                    kernel ="radial",
                    gamma = range_gamma, cost = range_costo, #ACH: range MOLTO grande
                    tunecontrol = tune.control(cross = 10)) #cross validation
  print(tuned)
  #valori ottimali trovati
  C_value_all <- tuned$best.parameters$cost
  gamma_value_all <- tuned$best.parameters$gamma
  toc_debug()
}else{
  C_value_all <- C_std_value_all
  gamma_value_all <- gamma_std_value_all
}


print("Creating SVM Model")
flush.console()
tic_debug('Creating SVM Model')
svm_model <- svm(x=dati_training,
                 y=factor(dati_target),
                 type = "C-classification",
                 kernel ="radial",
                 gamma = gamma_value_all,
                 cost = C_value_all)
toc_debug()


#--------------TEST & CONFUSION MATRIX---------------#
print("Creating Confusion Matrix and Metrics")
flush.console()
tic_debug("Create Confusion Matrix and Metrics")
print("MATRICE DI CONFUSIONE:")
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
print("Metriche del sistema:")
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall <- diag(confusion_matrix) / colSums(confusion_matrix)
f1 <- 2 * precision * recall / (precision + recall)
print(paste("Accuratezza del sistema:",accuracy,sep=" "))
print(data.frame(precision, recall, f1))
toc_debug()


#---------------------PREDICTION---------------------#
print("Predicting")
flush.console()
tic_debug('Prediction')

if(cuda_installed){
  dati_prediction <- attach.bigmatrix( data = datasetPrediction[,2:(ncol(datasetPrediction)-1)])
}else{
  dati_prediction <- datasetPrediction[,2:(ncol(datasetPrediction)-1)]
}
prediction <- predict(svm_model,dati_prediction)
prediction <- as.numeric(as.character(prediction))

toc_debug() #tic_debug('Prediction')


#-------------RICOSTRUZIONE IMMAGINE-----------------#
cat("Writing \"file_image_ALL\" in \"images/\".")
flush.console()
#(x,y) con y = variabile predetta da SVM - STEP 2
image_predicted_svm <- cbind(datasetPrediction[,1],prediction)
#(x,y) con y = variabile target già impostata dai dati di training
image_predicted_training <- cbind(datasetTraining[,1],datasetTraining[,ncol(datasetTraining)])
#(x,y) con y = variabile target già impostata dai dati di test
image_predicted_test <- cbind(datasetTest[,1],datasetTest[,ncol(datasetTest)])


#Se ci sono "NO_DATA" vanno reinseriti prima di ricostruire l'immagine
if(no_data_present){
  #(x,y) con y = variabile target già impostata dai dati di no_data
  image_predicted_nodata <- cbind(datasetNoData[,1],datasetNoData[,ncol(datasetNoData)])
  image_predicted <- rbind(image_predicted_svm,image_predicted_training,image_predicted_test,image_predicted_nodata)
}else{
  image_predicted <- rbind(image_predicted_svm,image_predicted_training,image_predicted_test)
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
file_image <- writeRaster(final_image, filename=paste(final_dir,"file_image_ALL",sep="/"), format="ENVI", bandorder='BIL', overwrite=TRUE)

toc_debug()#toc_debug finale -> tic_debug("Total time")