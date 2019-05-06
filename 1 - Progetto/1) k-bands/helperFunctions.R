tic_debug <- function(message){
  if(debug){
    tic(message)
  }
}


toc_debug <- function(){
  if(debug){
    toc()
  }
}


print_debug <- function(message){
  if(debug){
    print(message)
  }
}


istruzioni <- function(){
  
  if(instructions){
    #Funzione per mostrare all'utente un popup contenenti le istruzioni per il progetto
    message1 <- "Per il corretto funzionamento di questo script sono necessarie alcune librerie.
    Il software tenta di installarle automaticamente, ma se ci dovessero essere problemi, di seguito si elencano le librerie da INSTALLARE e CARICARE:
    -tools
    -raster
    -rgdal
    -tictoc
    -tcltk2
    -e1071
    -Rgtsvm (SE si utilizza CUDA in sostituzione a \"e1071\")"
    
    message2 <- "Il software rispetta alcune convenzioni.
    -Lo script crea automaticamente le directory \"images/\", \"training/\", \"test/\", \"final_images/\".
    
    -Nella directory \"images/\" vanno inserite le immagini di input, che DEVONO:
    -Essere > 1 (directory non vuota)
    -Essere in formato \".envi\"
    -Per ogni immagine \".envi\" deve esserci il relativo file \".hdr\"
    -Essere immagini raster con una o più bande
    -Se ci sono più immagini, devono avere lo stesso numero di pixel (x e y) e la stessa risoluzione spaziale
    
    -Nella directory \"training/\" vanno inseriti gli shapefile di TRAINING, che DEVONO:
    -Essere > 1 (directory non vuota)
    -Essere in formato \".shp\"
    -Per ogni immagine \".envi\" devono esserci anche i relativi file \".shx\",\".dbf\",\".prj\"
    -SE devono essere presenti \"NO DATA\", il file \"NO_DATA.shp\" va inserito in questa directory e NON in quella di test
    
    -Nella directory \"test/\" vanno inseriti gli shapefile di TEST, che DEVONO:
    -Essere > 1 (directory non vuota)
    -Gli shapefile di test devono essere un SOTTOINSIEME di quelli di training
    -Essere in formato \".shp\"
    -Per ogni immagine \".envi\" devono esserci anche i relativi file \".shx\",\".dbf\",\".prj\"
    -NON deve essere presente il file \"NO_DATA.shp\" (va inserito nella directory di training/)
    "
    message3 <- "Assicurarsi di aver riempito adeguatamente (vedi messaggio precedente) le directory."
    
    message4 <- "ALL'INIZIO dello script è presente la sezione \"---VARIABILI DA SETTARE---\".
    In questa sezione sono presenti tutte le variabili che verosimilmente l'utente può voler modificare.
    
    -instructions: se settata a \"TRUE\" mostra le istruzioni all'avvio dello script.
    -debug: se settata a \"TRUE\" sono mostrati una serie di messaggi utili per il debug.
    -number_bands: (IMPORTANTE) numero di bande che saranno prelevate ed inserite come FEATURES da ogni immagine di input.
    -cuda_installed: settare a \"TRUE\" SOLO SE si ha installato il framework CUDA e la libreria RGTSVM.
    -tuning: settare a \"TRUE\" se si vuole effettuare il tuning del modello svm. Altrimenti verranno usati i valori \"C_standard_value\" e \"gamma_standard_value\".
    "
    
    message5 <- "Di seguito le operazioni fondamentali effettuate (versione 2-step (masking)):
    -Check delle directory
    -Caricamento shapefiles (training+test)
    -Caricamento immagini di input
    -Validazione immagini di input
    -Costruzione dataset generale
    -Split dataset in: training,test,prediction,nodata
    -Tuning modello SVM [opzionale]
    -Creazione modello SVM
    -Predizione dataset TEST -> Confusion matrix e metriche
    -Predizione dataset PREDICTION
    -Ricostruzione immagine
    -Salvataggio immagine finale"
    messages <- c(message1,message2,message3,message4,message5)
    
    for(msg in messages){
      win <- tktoplevel()
      #Titolo finestra
      tktitle(win) <- "ISTRUZIONI"
      #Mostro messaggio
      tkgrid(tk2label(win, text = msg))
      #Bottone OK
      ok <- tclVar(0)
      win$env$butOK <- tk2button(win, text = "OK", command = function() tclvalue(ok) <- 1)
      tkgrid(win$env$butOK)
      tkbind(win, "<Destroy>", function() tclvalue(ok) <- 2)
      tkfocus(win)
      tkwait.variable(ok)
      tkdestroy(win)
      
      #Alternativa con scarsa personalizzazione grafica
      #tkmessageBox(title = "ISTRUZIONI", message = msg, icon = "info", type = "ok",rowspan = 2)
    }
  }
}