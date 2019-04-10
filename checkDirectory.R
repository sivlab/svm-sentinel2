checkDirectory <- function(image_dir,training_dir,test_dir,final_dir){
  #-------------------CHECK DIRECTORY-------------------#
  #Il sistema controlla dell'esistenza di 4 directory:
  #-images
  #  -Devono essere inserite tutte le immagini
  #  -Immagini in formato ENVI
  #  -Tutte le immagini devono essere correlate dal relativo file di header (.hdr)
  #
  #-training
  #  -Devono essere inseriti tutti gli shapefile che servono per il training del sistema
  #
  #-test
  #  -Devono essere inseriti tutti gli shapefile che servono per il test del sistema (valutare la qualità dell'SVM creato)
  #
  #-final_images
  # -Vengono memorizzate le immagini "finali", cioè dove è stato effettuato SVM
  if(!dir.exists(file.path(image_dir))){
    dir.create(file.path(image_dir))
  }
  if(!dir.exists(file.path(training_dir))){
    dir.create(file.path(training_dir))
  }
  if(!dir.exists(file.path(test_dir))){
    dir.create(file.path(test_dir))
  }
  if(!dir.exists(file.path(final_dir))){
    dir.create(file.path(final_dir))
  }
}