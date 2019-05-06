splitDataset <- function(training_dir,test_dir,imageList,matrice,shapefileNamesTraining,shapefileNamesTest,no_data_present,corrispondenza_classi){

  #--------------------SPLIT DATASET--------------------#
  # Metodo che splitta il dataset in: training, test, prediction, no_data[opzionale]
  
  # Operazioni sugli SHAPEFILE:
  # -Dati gli shapefile (precedentemente caricati)
  # -Si individuano i pixel dell'immagine che tali shapefile vanno a mappare
  # -In corrispondenza di tali pixel si imposta la variabile target relativa
  
  # RICORDA: in "imageList" ci sono tutte le immagini di input
  # Per ogni immagine ho tot. bande (es. 132 bande = 132 giorni)
  # Il compito da svolgere qui è quello di capire quali celle dell'immagine "cadono" all'interno degli shapefile
  # Quindi, DATO CHE è stato effettuato un controllo sull'uguaglianza delle dimensioni delle immagini, posso
  # usare solo la prima immagine di "imageList" e tra l'altro solo la prima banda
  banda <- raster(imageList[[1]], layer=1)
  

  #-----------------------TRAINING----------------------#
   
  #Ricavo indici con cui si va a creare il DATASET TRAINING
  index_training_cell <- matrix(ncol=2) #lista che conterrà solo gli indici dei pixel con variabile target settata

  for(shapefile in shapefileNamesTraining){
    
    #Relativamente allo shapefile selezionato, ricavo la classe dalla tabella "corrispondenza_classi"
    index_class <- which(corrispondenza_classi[,1] == shapefile)
    classe <- strtoi(corrispondenza_classi[index_class,2])
    
    shape_file_training <- shapefile(paste(training_dir,shapefile,sep="/"))
    
    #Il metodo extract, con il parametro "cellnumbers", mi restituisce il numero di cella relativo ai pixel che "cadono" all'interno di uno shapefile
    cells_extract_training <- extract(banda, shape_file_training, cellnumbers=TRUE)
    
    #DEBUG:
    print_debug(paste0("Shapefile TRAINING della classe: ",classe))
    
    # Una volta effettuata l'estrazione delle celle che ricadono nello shapefile, si ottiene una LISTA
    # Infatti PER OGNI POLIGONO appartenente allo shapefile, SI OTTIENE UNA LISTA di celle
    lunghezza_celle_classe <- 0
    for(polygon in cells_extract_training){
      slot <- cbind(polygon[,'cell'], rep(classe,length(polygon[,'cell'])))
      index_training_cell <- rbind(index_training_cell,slot)
      lunghezza_celle_classe <- lunghezza_celle_classe+length(polygon[,'cell'])
    }
    #DEBUG:
    print_debug(paste0("Numero di celle estratte: ",lunghezza_celle_classe))
  }
  

  #-------------------------TEST------------------------#
  
  #Ricavo indici con cui si va a creare il DATASET TEST
  index_test_cell <- matrix(ncol=2) #lista che conterrà solo gli indici dei pixel con variabile target settata

  for(shapefile in shapefileNamesTest){
    
    #Relativamente allo shapefile selezionato, ricavo la classe dalla tabella "corrispondenza_classi"
    index_class <- which(corrispondenza_classi[,1] == shapefile)
    classe <- as.numeric(corrispondenza_classi[index_class,2])
    
    shape_file_test <- shapefile(paste(test_dir,shapefile,sep="/"))
    
    #Il metodo extract, con il parametro "cellnumbers", mi restituisce il numero di cella relativo ai pixel che "cadono" all'interno di uno shapefile
    cells_extract_test <- extract(banda, shape_file_test, cellnumbers=TRUE)
    
    #DEBUG:
    print_debug(paste0("Shapefile TEST della classe: ",classe))

    # Una volta effettuata l'estrazione delle celle che ricadono nello shapefile, si ottiene una LISTA
    # Infatti PER OGNI POLIGONO appartenente allo shapefile, SI OTTIENE UNA LISTA di celle
    lunghezza_celle_classe <- 0
    for(polygon in cells_extract_test){
      slot <- cbind(polygon[,'cell'], rep(classe,length(polygon[,'cell'])))
      index_test_cell <- rbind(index_test_cell,slot)
      lunghezza_celle_classe <- lunghezza_celle_classe+length(polygon[,'cell'])
    }
    #DEBUG:
    print_debug(paste0("Numero di celle estratte: ",lunghezza_celle_classe))
  }
  

  #------------------------NO DATA----------------------#
  
  if(no_data_present){
    #Ricavo indici con cui si va a creare il DATASET NODATA
    index_nodata_cell <- matrix(ncol=2) #lista che conterrà solo gli indici dei pixel con variabile target settata
  
    shapefile = "NO_DATA.shp" #a differenza di training e test non c'è bisogno di ciclare, è solo 1 file
    shape_file_nodata <- shapefile(paste(training_dir,shapefile,sep="/")) #"no_data.shp" da specifiche può esserci solo in "training_dir"
    
    #Il metodo extract, con il parametro "cellnumbers", mi restituisce il numero di cella relativo ai pixel che "cadono" all'interno di uno shapefile
    cells_extract_nodata <- extract(banda, shape_file_nodata, cellnumbers=TRUE)
    
    #DEBUG:
    print_debug("Caricamento shapefile di NO_DATA numero:")
    # Una volta effettuata l'estrazione delle celle che ricadono nello shapefile, si ottiene una LISTA
    # Infatti PER OGNI POLIGONO appartenente allo shapefile, SI OTTIENE UNA LISTA di celle
    lunghezza_celle_classe <- 0
    for(polygon in cells_extract_nodata){
      #IMPORTANTE: classe associata ai "NO_DATA" è "-1"
      slot <- cbind(polygon[,'cell'], rep(-1,length(polygon[,'cell'])))
      index_nodata_cell <- rbind(index_nodata_cell,slot)
      lunghezza_celle_classe <- lunghezza_celle_classe+length(polygon[,'cell'])
    }
    #DEBUG:
    print_debug(paste0("Numero di celle estratte: ",lunghezza_celle_classe))
  }
  
  
  #Elimino prima riga della matrice "index_training_cell" dato che si è creata in maniera fittizia in fase di dichiarazione
  index_training_cell <- index_training_cell[-1,]
  #Elimino prima riga della matrice "index_test_cell" dato che si è creata in maniera fittizia in fase di dichiarazione
  index_test_cell <- index_test_cell[-1,]
  #Elimino prima riga della matrice "index_nodata_cell" dato che si è creata in maniera fittizia in fase di dichiarazione
  if(no_data_present){
    index_nodata_cell <- index_nodata_cell[-1,]
  }

  
  #-----------------CREAZIONE DATASETS-----------------#
  #Avendo gli indici dei dataset (training, test, no_data), vado a creare i relativi dataset
  #Questi dataset vengono creati "semplicemente" prelevando dal grande dataset "matrice" solo le righe pertinenti
  #Ad es. per creare il dataset di training vengono prelevate dalla matrice solo le righe relativi agli "indici di training"
  #
  #ACH: Per differenza, viene creato anche il dataset "prediction" cioè tutte le celle non mappate e di cui il valore
  #deve essere predetto in fase di "prediction"
  
  #CREAZIONE DATASET TRAINING
  datasetTraining <- matrice[index_training_cell[,1],]
  #elimino ultima colonna (che era tutta NA)
  datasetTraining <- datasetTraining[,-ncol(datasetTraining)]
  #come ultima colonna inserisco i valori presi dagli shapefile
  datasetTraining <- cbind(datasetTraining,index_training_cell[,2])
  
  #CREAZIONE DATASET TEST
  datasetTest <- matrice[index_test_cell[,1],]
  #elimino ultima colonna (che era tutta NA)
  datasetTest <- datasetTest[,-ncol(datasetTest)]
  #come ultima colonna inserisco i valori presi dagli shapefile
  datasetTest <- cbind(datasetTest,index_test_cell[,2])
  
  #CREAZIONE DATASET NO_DATA [opzionale]
  if(no_data_present){
    datasetNoData <- matrice[index_nodata_cell[,1],]
    #elimino ultima colonna (che era tutta NA)
    datasetNoData <- datasetNoData[,-ncol(datasetNoData)]
    #come ultima colonna inserisco i valori presi dagli shapefile
    datasetNoData <- cbind(datasetNoData,index_nodata_cell[,2])
  }
  else{
    datasetNoData = NULL
  }
  
  #CREAZIONE DATASET PREDICTION
  #Creo una matrice contenente gli indici delle celle training, test e no_data
  index <- index_training_cell
  index <- rbind(index,index_test_cell)
  if(no_data_present){
    index <- rbind(index,index_nodata_cell)
  }
  #Per differenza si crea il dataset di Prediction
  datasetPrediction <- matrice[-index[,1],]
  
  
  #return
  return(list(datasetTraining=datasetTraining,datasetTest=datasetTest,datasetPrediction=datasetPrediction,datasetNoData=datasetNoData))
}