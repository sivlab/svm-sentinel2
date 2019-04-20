checkDependencies <- function(){
  #-------------------CHECK DEPENDENCIES-------------------#
  #Lista di pacchetti
  list_of_packages <- c("tools","raster","rgdal","tictoc","tcltk2","e1071","Rgtsvm")

  #Se la variabile "cuda_installed" è TRUE allora carica la libreria "Rgtsvm"
  if(cuda_installed){
    list_of_packages <- list_of_packages[list_of_packages!= "e1071"]
  }else{
    list_of_packages <- list_of_packages[list_of_packages!= "Rgtsvm"]
  }
  
  #Installa i pacchetti se non presenti nel sistema
  new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  
  #Carica pacchetti tutti in una volta a partire dalla lista "list_of_packages"
  #invisible() per non "sporcare l'output da console
  invisible(lapply(list_of_packages, require, character.only = TRUE))
}