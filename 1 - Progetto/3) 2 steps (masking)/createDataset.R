createDataset <- function(dim_x,dim_y,image_list,tipo_superficie){
  #----------------COSTRUZIONE DATASET------------------#
  #In questa versione dello script si costruisce il database prendendo:
  #Per la vegetazione:
  # -12 bande per MSAVI (1,13,22,34,45,55,67,78,90,99,109,120)
  # -12 per VARI (1,13,22,34,45,55,67,78,90,99,109,120)
  # -12 per NDRE (1,13,22,34,45,55,67,78,90,99,109,120)
  # -6 per PHENO_NDVI (1,2,3,4,5,6,7,8)
  #
  #Per le zone edificate:
  # -1 banda (MAX) per BRIGHTNESS 
  # -12 bande per NDBBBI (1,13,22,34,45,55,67,78,90,99,109,120)
  # -12 bande per NDBSI (1,13,22,34,45,55,67,78,90,99,109,120)
  
  #Anzitutto controllo che esistano i file: "MSAVI.envi","VARI.envi","NDRE.envi","PHENO_NDVI.envi","BRIGHTNESS.envi","NDBBBI.envi","NDBSI.envi"
  if(
    !file.exists(paste(image_dir,"MSAVI.envi",sep="/")) ||
    !file.exists(paste(image_dir,"VARI.envi",sep="/")) ||
    !file.exists(paste(image_dir,"NDRE.envi",sep="/")) ||
    !file.exists(paste(image_dir,"PHENO_NDVI.envi",sep="/")) ||
    !file.exists(paste(image_dir,"BRIGHTNESS.envi",sep="/")) ||
    !file.exists(paste(image_dir,"NDBBBI.envi",sep="/")) ||
    !file.exists(paste(image_dir,"NDBSI.envi",sep="/"))
  ){
    stop("Nella directory \"images/\" devono essere presenti i file:
         \"MSAVI.envi\",\"VARI.envi\",\"NDRE.envi\",\"PHENO_NDVI.envi\",\"BRIGHTNESS.envi\",\"NDBBBI.envi\",\"NDBSI.envi\".")
  }
  
    
  #Prima colonna numero tutti i pixel -> Mi serve per conservare ordinamento dal momento che il dataset 
  #"unico" verrà "splittato" (vedi "splitDataset.R)
  matrice <- matrix(seq(1:(dim_y*dim_x)),nrow = dim_y*dim_x)
  
  #Man mano aggiungo tutte le bande come sopra, a seconda che la variabile "tipo_superficie" sia:
  # -"vegetazione"
  # -"zona_edificata"
  
  if(tipo_superficie=="vegetazione"){

    #Prelevo le bande da MSAVI, VARI, NDRE
    image_msavi <- stack(paste(image_dir,"MSAVI.envi",sep="/"))
    image_vari <- stack(paste(image_dir,"VARI.envi",sep="/"))
    image_ndre <- stack(paste(image_dir,"NDRE.envi",sep="/"))
    for(i in c(1,13,22,34,45,55,67,78,90,99,109,120)){
      matrice <- cbind(matrice,getValues(raster(image_msavi, layer=i)))
    }
    for(i in c(1,13,22,34,45,55,67,78,90,99,109,120)){
      matrice <- cbind(matrice,getValues(raster(image_vari, layer=i)))
    }
    for(i in c(1,13,22,34,45,55,67,78,90,99,109,120)){
      matrice <- cbind(matrice,getValues(raster(image_ndre, layer=i)))
    }
    
    #Prelevo le bande da PHENO
    image_pheno <- stack(paste(image_dir,"PHENO_NDVI.envi",sep="/"))
    for(i in c(1,2,3,4,5,6,7,8)){
      matrice <- cbind(matrice,getValues(raster(image_pheno, layer=i)))
    }
    
  }else if(tipo_superficie=="zona_edificata"){
    
    #Prelevo il valore MAX di Brightness: per ogni pixel il massimo valore assunto in una delle bande
    image_brightness <- stack(paste(image_dir,"BRIGHTNESS.envi",sep="/"))
    raster_stackapply <- stackApply(image_brightness, indices=rep(1,nlayers(image_brightness)), fun=max)
    matrice <- cbind(matrice,getValues(raster_stackapply))
    
    #Prelevo le bande da NDBBBI,NDBSI
    image_ndbbbi <- stack(paste(image_dir,"NDBBBI.envi",sep="/"))
    image_ndbsi <- stack(paste(image_dir,"NDBSI.envi",sep="/"))
    for(i in c(1,13,22,34,45,55,67,78,90,99,109,120)){
      matrice <- cbind(matrice,getValues(raster(image_ndbbbi, layer=i)))
    }
    for(i in c(1,13,22,34,45,55,67,78,90,99,109,120)){
      matrice <- cbind(matrice,getValues(raster(image_ndbsi, layer=i)))
    }
    
  }else{
    stop("Per la creazione del dataset, la variabile \"tipo_superficie\" può essere solo: \"vegetazione\",\"zona_edificata\" oppure \"all\".")
  }

  #Aggiungo la colonna della variabile target
  matrice <- cbind(matrice,rep(NA,nrow(matrice)))
  
  return(matrice)
}