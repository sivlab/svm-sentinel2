checkTrainingTest <- function(training_dir,test_dir){
  #Si è scelto di effettuare una CROSS-VALIDATION sui dati di training
  #I dati vengono quindi suddivisi logicamente in:
  # -Training set -> Apprendimento del sistema
  # -Test set -> Test del modello
  #
  #In maniera AUTOMATICA, il metodo "svm" permette di eseguire la K-Fold Cross-Validation
  #Il dataset di training, quindi, viene diviso in k-fold e viene effettuata la cross-validation,
  #quindi la libreria sceglie in maniera automatica il miglior modello svm.
  #
  #Tale modello risultante, viene poi testato sul Test dataset.
  
  #Condizioni che si vanno a controllare:
  # -Directory di "training" e "test" non vuote
  # -Shapefile in test "sottoinsieme" di quelli in training
  # -Presenza di "NO_DATA" nei dati di training
  
  
  #Variabile flag che mi segnala la presenza dei NO_DATA
  #Mi serve saperlo perchè se presenti devo agire diversamente, cioè eliminarli dal training del sistema
  no_data_present <- FALSE
  
  #Prelevo i nomi dei file presenti all'interno delle directory
  list_shapefile <- list.files(training_dir, pattern = "\\.shp$", ignore.case=TRUE)
  list_shapefile_test <- list.files(test_dir, pattern = "\\.shp$", ignore.case=TRUE)
  
  #Le directory devono NECESSARIAMENTE essere piene ENTRAMBE
  if(length(list_shapefile)==0 || length(list_shapefile_test)==0){
    stop("Le directory training/ e test/ devono NECESSARIAMENTE contenere degli shapefile.")
  }
  
  #Controllo che il file "NO_DATA.shp" NON sia in "test/" -> Scelta progettuale
  #Lo shapefile dei "NO_DATA" (NO_DATA.shp) deve essere contenuto solo nella cartella di training
  if("NO_DATA.shp" %in% list_shapefile_test){
    stop("Lo shapefile relativo ai NO_DATA deve essere inserito SOLO nella cartella di training/")
  }
  
  #Controllo SE il file "NO_DATA.shp" è presente in "training/" -> Vedi sopra il perchè
  if("NO_DATA.shp" %in% list_shapefile){
    no_data_present <- TRUE
  }
  
  #Controllo che "list_shapefile_test" sia contenuta in "list_shapefile"
  #Questo equivale a controllare che ogni shapefile nella cartella "test/" sia contenuto ANCHE in "training/"
  #In caso contrario nel dataset di Test ci sarebbero "samples" che il sistema non potrebbe predire in quanto su di essi
  #non è stato fatto nessuno training.
  if(!all(list_shapefile_test %in% list_shapefile)){
    stop("Ci sono shapefile nella directory test/ NON presenti ANCHE in training/. Ciò non è semanticamente corretto.")
  }
  
  #Controllo che per ogni shapefile ci siano anche i file "accessori"
  for(shapefile in list_shapefile){
    if(
      (!file.exists(paste(training_dir,"/",file_path_sans_ext(shapefile),".shx", sep = ""))
       &&
       !file.exists(paste(training_dir,"/",file_path_sans_ext(shapefile),".SHX", sep = ""))
      )
      ||
      (
        !file.exists(paste(training_dir,"/",file_path_sans_ext(shapefile),".dbf", sep = ""))
        &&
        !file.exists(paste(training_dir,"/",file_path_sans_ext(shapefile),".DBF", sep = ""))
      )
      ||
      (
        !file.exists(paste(training_dir,"/",file_path_sans_ext(shapefile),".prj", sep = ""))
        &&
        !file.exists(paste(training_dir,"/",file_path_sans_ext(shapefile),".PRJ", sep = ""))
      )
    ){
      stop("Nella directory training/ PER OGNI SHAPEFILE, devono essere forniti i seguenti file: \".shp\",\".shx\",\".dbf\",\".prj\".")
    }
  }
  for(shapefile in list_shapefile_test){
    if(
      (!file.exists(paste(test_dir,"/",file_path_sans_ext(shapefile),".shx", sep = ""))
       &&
       !file.exists(paste(test_dir,"/",file_path_sans_ext(shapefile),".SHX", sep = ""))
      )
      ||
      (
        !file.exists(paste(test_dir,"/",file_path_sans_ext(shapefile),".dbf", sep = ""))
        &&
        !file.exists(paste(test_dir,"/",file_path_sans_ext(shapefile),".DBF", sep = ""))
      )
      ||
      (
        !file.exists(paste(test_dir,"/",file_path_sans_ext(shapefile),".prj", sep = ""))
        &&
        !file.exists(paste(test_dir,"/",file_path_sans_ext(shapefile),".PRJ", sep = ""))
      )
    ){
      stop("Nella directory test/ PER OGNI SHAPEFILE, devono essere forniti i seguenti file: \".shp\",\".shx\",\".dbf\",\".prj\".")
    }
  }
  
  #Creo una tabella che mi tiene traccia dei:
  #(file,classe)
  #Es. allo shapefile "A11_A3A5S5_ARABLELAND_verdeoct-seccodejan_leguminose.shp" associo la classe "k"
  #Dal momento che gli shapefile in "test/" devono essere un sottoinsieme di quelli in "training/"
  #mi basta controllare la lista "più lunga", cioè quella di "training/"
  if(no_data_present){
    #Elimino la classe "no_data" in quanto non è una reale classe
    list_shapefile_wo_nodata <- list_shapefile[list_shapefile != "NO_DATA.shp"]
    corrispondenza_classi <- cbind(list_shapefile_wo_nodata,seq(1:length(list_shapefile_wo_nodata)))
    
    #Così la lista restituita dei file in "training/" NON contiene "NO_DATA.shp"
    #Non è un problema, perchè l'informazione che i NO_DATA ci sono è memorizzata nel flag "no_data_present"
    list_shapefile <- list_shapefile[list_shapefile != "NO_DATA.shp"]
  }
  else{
    corrispondenza_classi <- cbind(list_shapefile,seq(1:length(list_shapefile)))
  }
  
  #Print corrispondenza classi
  cat("Corrispondenza classi creata:\n")
  print(corrispondenza_classi)
  
  #return di ciò che mi sarà utile, cioè la lista dei nomi degli shapefile e il numero di classi
  return(list(shapefileNamesTraining = list_shapefile, shapefileNamesTest = list_shapefile_test, corrispondenza_classi = corrispondenza_classi, no_data_present = no_data_present))
}