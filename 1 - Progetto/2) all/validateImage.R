validateImage <- function(image_list){
  #----------------VALIDAZIONE IMMAGINI-----------------#
  # Una volta che sono state caricate le immagini bisogna capire se sono "compatibili" tra di loro
  # Nello specifico vengono effettuati i seguenti controlli:
  # Controllo1: le immagini hanno stessa definizione 
  # controllo2: le immagini hanno la stessa risoluzione -> Sentinel2 di prova: 10mx10m
  # ACH: controllo sul numero di bande non effettuato -> "formalmente" è possibile costruire un sistema SVM con immagini che contribuiscono al dataset con "features" diverse
  
  # Prelevo i dati della prima immagine
  # I dati di tutte le immagini in "image_list" devono essere uguali, altrimenti il controllo fallisce
  dim_x <- dim(image_list[[1]])[1]
  dim_y <- dim(image_list[[1]])[2]
  res <- res(image_list[[1]])
  
  for(image in image_list){
    if(
      dim(image)[1] != dim_x ||
      dim(image)[2] != dim_y ||
      res(image) != res(image_list[[1]])
    ){
      # Controllo fallito
      stop("Nella directory images/ ci sono immagini diverse tra loro. Controllare: definizione, risoluzione e numero di bande.")
    }
  }
  
  return(list(dim_x=dim_x,dim_y=dim_y))
}