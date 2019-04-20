loadInput <- function(image_dir,training_dir,test_dir){
  #------------------CARICAMENTO INPUT------------------#
  #Dalle directory precedentemente discusse si prelevano tutti i file all'interno
  #Inoltre si effettuano dei controlli per verificare che tali file siano compatibili con il sistema
  
  #controllo dati in "images"
  #Controllo1: Si controlla che le immagini in "images" abbiano estensione "envi"
  #Controllo2: Si controlla che le immagini sia tutte COPPIE di (dati+header)
  #Controllo3: Si controlla che le immagini abbiano la stessa definizione
  list_image <- list.files(image_dir, pattern = "\\.envi$", ignore.case=TRUE)
  if(length(list_image)==0){
    stop("Nessun file trovato nella cartella images/")
  }
  for(image in list_image){
    #per ogni immagine controllo che ci sia sia il file "nome.envi" sia il file "nome.hdr"
    #in realtà controllo solo che sia il file "name.hdr" perchè il controllo su "name.envi" è stato già fatto
    if(!file.exists(paste(image_dir,"/",file_path_sans_ext(image),".hdr", sep = ""))
       &&
       !file.exists(paste(image_dir,"/",file_path_sans_ext(image),".HDR", sep = ""))){
      stop("Nella directory images/ non ci sono COPPIE di file \"name.envi\",\"name.hdr\".")
    }
  }
  
  #Una volta effettuati i controlli "carico" tutte le immagini
  #ACH: nella variabile "list_image" sono presenti i NOMI delle immagini
  #Nella variabile sottostante "image_list" saranno caricate le immagini, cioè ogni oggetto sarà di tipo "raster"
  image_list <- list()
  for(image in list_image){
    image_list <- c(image_list, stack(paste(image_dir,image,sep="/")))
  }
  
  return(list(imageNames = list_image, imageList = image_list))
}