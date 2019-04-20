createDataset <- function(dim_x,dim_y,image_list){
  #----------------COSTRUZIONE DATASET------------------#
  # Costruzione di una dataset dove ogni colonna corrisponde ad una immagine
  # Cioè gli elementi della colonna sono i valori di un determinato indice (ad es. MSAVI o BRIGHTNESS) relativi a tutti i pixel di una immagine
  # In maniera equivalente le righe corrispondono, fissato un pixel, a tutti i valori che esso assumme nelle varie immagini
  # Dal momento che le immagini passate sono in realtà stack di immagini (ognuna di esse ha k bande), allora il dataset, se le immagini sono n, avrà:
  #   -n*k colonne
  #   -m righe, dove "m" è la definizione delle immagini (numeri di pixel complessivi)
  #
  #ALTERNATIVA 1: Prendo TUTTE LE BANDE per ogni immagine di input
  #ALTERNATIVA 2: Prendo NUMBER_BANDS per ogni immagine di input
  
  
  if(number_bands=="-1"){
    #-------------------ALTERNATIVA 1---------------------#
    #Nel sottostante si prelevano TUTTE le bande relative a TUTTE le immagini
    #Se si vogliono scegliere solo determinate bande fare uso dell'alternativa 2 (vedi sotto)
    matrice <- matrix(seq(1:(dim_y*dim_x)),nrow = dim_y*dim_x) #ATT: allo spazio occupato
    for(image in image_list){
      matrice <- cbind(matrice,getValues(image))
    }
  }else{
    #-------------------ALTERNATIVA 2---------------------#
    #Nel ciclo for di sopra (alternativa 1) si prelevano TUTTE le bande di una immagine
    #Se si vuole scegliere quale di queste prendere, usare il ciclo for sottostante
    
    #(vedi Training.R)dal momento che devo separare il dataset, perdo l'ordinamento delle celle
    #per cui aggiungo un campo colonna che riporta il numero di cella -> mi serve in fase di plotting
    matrice <- matrix(seq(1:(dim_y*dim_x)),nrow = dim_y*dim_x) #ATT: allo spazio occupato
    for(image in image_list){
      for(i in (1:number_bands)){#for(i in (1:nlayers(image))){
        #prelevo la i-esima banda e la inserisco nella colonna del dataset
        matrice <- cbind(matrice,getValues(raster(image, layer=i)))
      }
    }
  }
  
  #aggiungo la colonna della variabile target
  matrice <- cbind(matrice,rep(NA,nrow(matrice)))
  
  return(matrice)
}