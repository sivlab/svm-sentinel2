library(raster)
library(rgdal)
image_all <- stack("file_image_ALL.envi")
image_2step <- stack("file_image_2STEP.envi")
valori_all <- getValues(image_all)
valori_2step <- getValues(image_2step)

#NO_DATA -> Settati ad 0
nodata<-0
for(i in 1:nrow(valori_all)){
  if(valori_all[i]==-1){
    valori_all[i]=0
    nodata<-nodata+1
  }
}

#NO_DATA -> Settati ad 0
nodata<-0
for(i in 1:nrow(valori_2step)){
  if(valori_2step[i]==-1){
    valori_2step[i]=0
    nodata<-nodata+1
  }
}

valori_differenza<-valori_all-valori_2step

#Numero pixel differenti
different<-0
for(i in 1:nrow(valori_differenza)){
  if(valori_differenza[i]<0){
    different<-different+1
  }
}

index_different<-which(valori_differenza<0)

#Setto in una delle due immagini, i pixel differenti come classe "-2"
valori_2step[index_different]=-2

# #DEBUG: Conto classi -2
# counter<-0
# for(i in 1:nrow(valori_2step)){
#   if(valori_2step[i]==-2){
#     counter<-counter+1
#   }
# }

#Ricostruzione immagine
final_image<-setValues(image_2step, valori_2step)
file_image <- writeRaster(final_image, filename="immagine_differenza", format="ENVI", bandorder='BIL', overwrite=TRUE)