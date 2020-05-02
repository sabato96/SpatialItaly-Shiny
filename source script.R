#################################################################################
# Progetto Lab. DISES 2020                                                      #
#                                                      SOURCE SCRIPT            #
# Gargiulo Sabato & Manzo Maria                                                 #
#                                                                               #
#################################################################################

#####Librerie  ----- 
  library(car)
  library(plyr)
  library(MASS)
  library(tidyverse)
  library(raster)
  library(ggmap)
  library(devtools)
  library(sp)
  library(sf)
  library(tmap)
  library(leaflet)
  library(RColorBrewer)
  library(shinyjs)
  library(rgdal)
  library(rgeos)
  library(spdep)
  library(dplyr)
  library(spatstat)
  library(maptools)
  library(dismo)
  library(boot)
  library(DCluster)
  library(rsconnect) #shiny app
  library(shiny)
  library(shinydashboard)
  ggmap::register_google(key="AIzaSyCCCEngniuORKmDvGus4ppsJ7oksOjKrWU")
  
######

  
  redditi <- read.csv("Data/Redditi_comuni.csv", header=TRUE, sep=";", quote="", na.strings="")
  redditi <- as_tibble(redditi)
 
  # pulizia doppioni
  #
   a <- which(table(redditi[,4])>1)
   doppi <- names(a)
  
   for (i in doppi){
  
     redditi <- redditi[!(redditi$Denominazione.Comune==i),]
  
   }
  
   # Il loop rimuove le righe contenenti i nomi presenti in doppi
   
   which(table(redditi[,4])>1)
   rm(doppi)
   
  
  ### Dividiamo ammontare e frequenza 
  redditi.compl <- redditi
  redditi<- redditi[,c(4:50)]
  aaa <- redditi[,c(6:47)]
  aaa <- as_tibble(aaa)
  red <- matrix(0,ncol=42,nrow=length(redditi$Denominazione.Comune))
  red <- as_tibble(red)
  
  for (i in seq(from=1, to= 41, by=2)){
    
    
    red[i] <- aaa[i+1]/aaa[i]
    
  }
  
  red <- red[,seq(from=-2, to= -42, by=-2)]
  
  names(red) <- c("Red. da fabbricati","Red. lav. dip", "Red. pens.","Red. lav. aut.","Red. spett impr. ord.",
                  "Red. spett impr. sempl.","Red. da partecip.","Red. imponibile","Imposta netta",
                  "Bonus spett.","Red. imp. addizionale","Addiz. regionale","Addiz. comun.",
                  "Reddito compl. 0 o minore","Reddito da 0 a 10K","Reddito da 10K a 15K",
                  "Reddito da 15K a 26K","Reddito da 26K a 55K","Reddito da 55K a 75K",
                  "Reddito da 75K a 120K","Reddito oltre 120K")
  etichette <- names(red)
  
  red.fin<-cbind(redditi[,c(1:5)], red)
  
  ####
  
  
  #################### DIVIDIAMO LE REGIONI IN NORD, SUD, CENTRO
  red.fin <- as_tibble(red.fin[-7965,])
  redditi <- redditi[-7965,] # Rimuoviamo riga con "non indicato"
  
  red.fin$Regione <- as.factor(red.fin$Regione)
  regioni.nord <- levels(red.fin$Regione)[c(5,6,8,9,13,18,20,21)]
  regioni.centro <- levels(red.fin$Regione)[c(1,7,10,11,17,19)]
  regioni.mezzogiorno <- levels(red.fin$Regione)[c(2,3,4,14,15,16)]
  
  ###########################  CREIAMO UNA NUOVA COLONNA CON LE ETICHETTE NORD SUD CENTRO 
  red.fin$macro <- ifelse(red.fin$Regione%in%regioni.nord, "Nord", 0)
  red.fin$macro <- ifelse(red.fin$Regione%in%regioni.centro, "Centro", red.fin$macro)
  red.fin$macro <- ifelse(red.fin$Regione%in%regioni.mezzogiorno, "Sud", red.fin$macro)
  
 
  rm(aaa,red,i,a)
  

##### CARICHIAMO SHAPEFILE DI PROVINCE REGIONI E COMUNI  
  
  
  
#################### ---- Province ------------
    
  
  OGR.prov <- readOGR(dsn=path.expand("Data/shape file/ProvCM01012016"), layer="ProvCM01012016_WGS84") #Shapefile province
  xy.prov <- coordinates(OGR.prov) #Coordinate dei centroidi
  prov <- as.character(names(table(red.fin$Sigla.Provincia))) # Otteniamo i nomi delle singole province
  prov <- prov[-1]
  
  
  # Adiacenza
  
      # Metodo semplice QUEEN
  
        wr.prov <- poly2nb(OGR.prov, row.names = OGR.prov$SIGLA, queen = TRUE ) #oggetto class "nb", lista con adiacenze
  
      # Metodo distance based
    
        dsts <- unlist(nbdists(wr.prov,xy.prov))
        wr.prov <- dnearneigh(xy.prov, d1 = 0, d2 = 0.93 * max(dsts),  row.names = prov) #class "nb"
  
  # Lista e matrice pesi 
    ww.prov <- nb2listw(wr.prov, style ="B") #Lista pesi, "B" metodo binario
  
    summary(unlist(ww.prov$weights))
    summary(sapply(ww.prov$weights, sum))
  
    wm.prov <- nb2mat(wr.prov, style = "B", zero.policy = TRUE) # Matrice di pesi W(xy)
  
  # Plot province e adiacenze
  
      #   plot(OGR.prov, col="gray", border="blue")
  
      #   plot(wr.prov, xy.prov, col="red", lwd="2", add=TRUE)
  
# Raggruppiamo i dati dei singoli comuni facendo media per provincia
  
  red.fin <- as_tibble(red.fin)
  red.fin$Sigla.Provincia <- as.character(red.fin$Sigla.Provincia)
  names(red.fin)
  
  
  a <- names(red.fin[,6:26])
  a <- red.fin %>% 
    group_by(Sigla.Provincia) %>%
    mutate_at(a, mean, na.rm = TRUE)
  
  fun.mut <- function(x){  
    c <- a[which(a[[2]] == x),c(2,3,6:27)]   # Funzione generica per estrarre in base a provincia 
    
    c <- c[1,]    # Otteniamo una singola riga per ogni provincia
  }
  
  mean.prov = NULL
  
  for(i in prov) {
    
    mean.prov <- rbind(mean.prov,fun.mut(i))
    
  }
  
  # Il ciclo rbinda ogni riga provinciale creando il dataset mean.prov
  head(mean.prov)
  rm(a,i)
  
  
  OGR.prov <- merge(OGR.prov, mean.prov, by.x="SIGLA", by.y="Sigla.Provincia") # Merge shapefile e mean.prov
  
  
  
  #################### ---- Regioni ------------
  
  
  OGR.reg <- readOGR(dsn=path.expand("Data/shape file/Reg01012016"),  
                     layer="Reg01012016_WGS84") #shapefile regioni
  xy.reg <- coordinates(OGR.reg) #coordinate centroidi
  reg <- as.character(names(table(red.fin$Regione))) #estrae etichette regioni
  reg <- reg[-12] # Rimuove "non indicato"
  
  
### Adiacenza
  
    # Metodo semplice QUEEN
  
      wr.reg <- poly2nb(OGR.reg, row.names = OGR.reg$DEN_REG, queen = TRUE ) #oggetto class "nb", lista con adiacenze
  
    # Metodo distance based
  
      
      dsts <- unlist(nbdists(wr.reg,xy.reg))
      wr.reg <- dnearneigh(xy.reg, d1 = 0, d2 = 1.8* max(dsts),  row.names = reg) #class "nb"
  
  
  # Lista e matrice pesi 
    ww.reg <- nb2listw(wr.reg, style ="B", zero.policy = TRUE) #Lista pesi, "B" metodo binario
  
    summary(unlist(ww.reg$weights))
    summary(sapply(ww.reg$weights, sum))
  
    wm.reg <- nb2mat(wr.reg, style = "B", zero.policy = TRUE) # Matrice di pesi W(xy)
  
# Plot province e adiacenze
  
  #plot(OGR.reg, col="gray", border="blue")
  
  #plot(wr.reg, xy.reg, col="red", lwd="2", add=TRUE)
  

## Fare media per regione.
  
  red.fin <- as_tibble(red.fin)
  red.fin$Regione <- as.character(red.fin$Regione)
  names(red.fin)
  
  a <- names(red.fin[,6:26])
  a <- red.fin %>% 
    group_by(Regione) %>%
    mutate_at(a, mean, na.rm = TRUE)

  fun.mut <- function(x){  
    
    
    c <- a[which(a[[3]] == x),c(2,3,6:27)]
    c <- c[1,]
  }
  
 
  
  mean.reg = NULL
  
  for(i in reg) {
    
    mean.reg <- rbind(mean.reg,fun.mut(i))
    
  }
  
  
  
  ## Alcune regioni presentano nomi diversi tra i due dataset MEAN.REG E OGR.REG
  # Per ovviare a ciò individuiamo le righe diverse e fixiamo le differenze
  # Questo poichè altrimenti il comando merge successivo ci dsarà NA dove i nomi sono diversi alche di una sola virgola.
  
  OGR.reg[["DEN_REG"]]  <- as.character(OGR.reg[["DEN_REG"]]) # Trasforma in carattere la colonna "den_reg"
  a <- OGR.reg[["DEN_REG"]] #Etichette regioni dello shapefile
  b <- mean.reg$Regione #Etichette regioni in redditi_comuni
  
  x <- a[which(a%in%b == FALSE)] # Nomi presenti in shapefile ma non in redditi_comuni
  y <- b[which(b%in%a == FALSE)] # Nomi presenti in redditi_com ma non in shapefile
  x <- x[order(x)] #ordina in ordine alfabetico
  y <- y[order(y)]
  
  #seq(...) è una successione di numeri che va da 1 al numero di errori nello shapefile
  
  for (i in seq(1:length(x))){
  
  
  OGR.reg[["DEN_REG"]][which(OGR.reg@data$DEN_REG == x[i])] <- y[i]
  
  # Selezioni gli elementi di "DEN_REG" che sono errati e li sostituisce con i rispettivi elementi di redditi_comuni che sono giusti
  }
  
  
  
  OGR.reg[["DEN_REG"]]  <- as.factor(OGR.reg[["DEN_REG"]]) #ritorna come fattore
  
  
  
  OGR.reg <- merge(OGR.reg, mean.reg, by.x = "DEN_REG", by.y = "Regione") #merge shapefile e mean.reg che grazie alla trasformaz avranno stesse etichette
  
  rm(a,b,x,y,i)
  

  
  
  
  #################### ---- Comuni ------------
  

  OGR.com <- readOGR(dsn=path.expand("Data/shape file/Com01012016"), layer="Com01012016_WGS84")
  xy.com <- coordinates(OGR.com)
  
  
  
  ### Adicenze
  
    #Metodo semplice QUEEN
  
      wr.com <- poly2nb(OGR.com, row.names = OGR.com$PRO_COM, queen = TRUE )
  

  
    #Metodo distance based
      dsts.com <- unlist(nbdists(wr.com,xy.com))
      wr.com <- dnearneigh(xy.com, d1 = 0, d2 = 0.3 * max(dsts.com),  row.names = OGR.com@data$PRO_COM)
  
  
## Plot adiacenze
  
    #plot(OGR.com, col="gray", border="blue") # Disegna shapefile vuoto 
    #plot(wr.com, xy.com, col="red", lwd="0.5", add=TRUE) #Disegna su shapefile precedente sia centroidi che collegamenti
  

  
  #Matrice di pesi spaziali
  
  ww.com <- nb2listw(wr.com, style ="B", zero.policy = TRUE)
  
  summary(unlist(ww.com$weights))
  summary(sapply(ww.com$weights, sum))
  
  
  wm.com <- nb2mat(wr.com, style = "B", zero.policy = TRUE)

  
  # Lo shapefile e red.fin hanno grandezze di caratteri diverse quindi riduciamo con tolower()
  

  OGR.com@data$COMUNE <- as.factor(tolower(OGR.com@data$COMUNE))
  ut <- redditi.compl[-7965,]
  
  ut <- ut[order(ut$Denominazione.Comune),]
  red.fin <- red.fin[order(red.fin$Denominazione.Comune),]
  
  ut <- ut[,(-9:-51)]
  ut <- cbind(ut,red.fin[,6:27])
  
  ut$Denominazione.Comune <- as.factor(tolower(ut$Denominazione.Comune))
  
  
  
  
  
  #names(ut)[1] <- "COMUNE" #Cambiamo nome alla colonna
  
  OGR.com <- sp::merge(OGR.com, ut, by.x="PRO_COM", by.y="Codice.Istat.Comune") #merge shapefile e red.fin
  rm(ut)
  
  
  
  
  
  #Subset dello shapefile basato sulla variabile macro
  ##################################
  
  
  
  # NORD --------------------
  OGR.com.nord <- OGR.com[!is.na(OGR.com$macro) & OGR.com$macro == "Nord",] #Estraggo dallo shapefile comuni solo i comuni che hanno "nord" come variabile macro e non sono NA
  OGR.com.nord@data$new <- seq(1:length(OGR.com.nord)) # Creiamo sequenza ausiliaria come etichetta "row.names" in ploy2nb
  
  
  wr.com.nord <- poly2nb(OGR.com.nord, row.names = OGR.com.nord$new, queen = TRUE )
  
  #plot(OGR.com.nord, col="gray", border="blue")
  xy.com.nord <- coordinates(OGR.com.nord)
  #plot(wr.com.nord, xy.com.nord, col="red", lwd="0.5", add=TRUE)
  
  ww.com.nord <- nb2listw(wr.com.nord, style ="B", zero.policy = TRUE)
  wm.com.nord <- nb2mat(wr.com.nord, style = "B", zero.policy = TRUE)
  
  # Plot interattivo
  #  tm <- tm_shape(OGR.com.nord) + tm_fill(col = "Red. pens.", palette = "Blues", style = "quantile",
  #                                         n = 6, contrast = c(0.28, 0.87), id = "Denominazione.Comune") +
  # 
  #    tm_borders(alpha=.7) + tm_legend(legend.position = c("left", "bottom")) +
  #    tm_layout(title = paste("Red. pens.","medio per comune"),
  #             title.size = 1.1) +
  # 
  #    tm_shape(OGR.reg) + tm_borders(col = "black")
  # 
  # 
  # tmap_mode("view")
  # 
  # tmap_leaflet(tm) 
  #                   
  # 

  
  ## CENTRO ----------------------
  
  
  OGR.com.centro <- OGR.com[!is.na(OGR.com$macro) & OGR.com$macro == "Centro",]
  OGR.com.centro@data$new <- seq(1:length(OGR.com.centro))
  
  wr.com.centro <- poly2nb(OGR.com.centro, row.names = OGR.com.centro$new, queen = TRUE )
  
  #plot(OGR.com.centro, col="gray", border="blue")
  xy.com.centro <- coordinates(OGR.com.centro)
  #plot(wr.com.centro, xy.com.centro, col="red", lwd="0.5", add=TRUE)
  
  ww.com.centro <- nb2listw(wr.com.centro, style ="B", zero.policy = TRUE)
  wm.com.centro <- nb2mat(wr.com.centro, style = "B", zero.policy = TRUE)
  
  # Plot interattivo 
  # tm <- tm_shape(OGR.com.centro) + tm_fill(col = "Red. pens.", palette = "Blues", style = "quantile", 
  #                                        n = 6, contrast = c(0.28, 0.87)) + 
  #   
  #   tm_borders(alpha=.7) + tm_legend(legend.position = c("left", "bottom")) +
  #   tm_layout(title = paste("Red. pens.","medio per comune"),
  #             title.size = 1.1) +
  #   
  #   tm_shape(OGR.reg) + tm_borders(col = "black") 
  # 
  # tmap_mode("view")
  # 
  # tmap_leaflet(tm) 
  # 
  # 
  
  
  ######### SUD --------------

  OGR.com.sud <- OGR.com[!is.na(OGR.com$macro) & OGR.com$macro == "Sud",]
  OGR.com.sud@data$new <- seq(1:length(OGR.com.sud))
  
  wr.com.sud <- poly2nb(OGR.com.sud, row.names = OGR.com.sud$new, queen = TRUE )
  
  #plot(OGR.com.sud, col="gray", border="blue")
  xy.com.sud <- coordinates(OGR.com.sud)
  #plot(wr.com.sud, xy.com.sud, col="red", lwd="0.5", add=TRUE)
  
  ww.com.sud <- nb2listw(wr.com.sud, style ="B", zero.policy = TRUE)
  wm.com.sud <- nb2mat(wr.com.sud, style = "B", zero.policy = TRUE)

  # Plot interattivo
   # tm <- tm_shape(OGR.com.sud) + tm_fill(col = "Red. pens.", palette = "Blues", style = "quantile",
   #                                          n = 6, contrast = c(0.28, 0.87)) +
   # 
   #   tm_borders(alpha=.7) + tm_legend(legend.position = c("left", "bottom")) +
   #   tm_layout(title = paste("Red. pens.","medio per comune"),
   #             title.size = 1.1) +
   # 
   #   tm_shape(OGR.reg) + tm_borders(col = "black")
   # 
   # tmap_mode("view")
   # 
   # tmap_leaflet(tm)
   # 

  
################## FUNZIONE PER CALCOLARE MORAN INDEX
  
  Moran.fun <- function(shape,x,wm){
    
    n <- length(shape)
    y <- shape[[etichette[x]]]
    ybar <- mean(y, na.rm = TRUE)
    
    # Ora ci serve (yi - ybar)(yj - ybar)
    
    dy <- y - ybar # Scarto dalla media (vettore)
    g <- expand.grid(dy, dy) # Combinazione del vettore
    yiyj <- g[,1] * g[,2]
    
    pm <- matrix(yiyj, ncol = n) # Crea matrice da yiyj
    
    pmw <- pm * wm #pm * matrice dei pesi -> (yi - ybar)(yj - ybar)*wij
    spwm <- sum(pmw) #Doppia sommatoria di -> (yi - ybar)(yj-ybar)*wij
    smw <- sum(wm) #Somma dei pesi
    sw <- spwm/smw #Doppia sommatoria "spwm" diviso somma dei pesi "smw"
    vr <- n / sum(dy^2) # Prima parte della formula
    
    # Varianza
    
    S0 <- smw # Somma dei pesi
    
    a1 <- wm.prov + t(wm.prov) 
    a2 <- rowSums(a1)^2
    S2 <- sum(a2)
    
    b1 <- a1^2
    S1 <- (1/2)sum(b1)
    
    A <- n*((n^2-3*n+3)*S1-n*S2+3*S0^2)
    D <- sum(dy^4)/(sum(dy^2))^2
    B <- D*((n^2-n)*S1-2*n*S2+6*S0^2)
    C <- (n-1)*(n-2)*(n-3)*S0^2
    
    m.sec <- (A-B)/C
    
    
    
    EI <- -1 /(n-1) # Valore atteso teorico sotto H0 cioè assenza di correlazione spaziale
    MI <- vr * sw # Calcolo moran index
    VI <- m.sec-EI^2
    
    
    
    #print(paste("Moran I",round(MI, digits = 7)))
    #print(paste("Expected",round(EI, digits = 7)))
    
    
    
    
    invisible(base::list(
      "Moran" = MI,
      "Expected" = EI,
      "Variance" = VI,
      "Shape" = deparse(substitute(shape)),
      "Variable" = etichette[x],
      "Weight" = deparse(substitute(wm))))
    
    #E' come return ma non printa ulteriormente il valore
    
    
  }
  
  
  #Moran.fun(OGR.prov,3,wm.prov)
  
#### FUNZIONE PER MORAN BOOTSTRAP PERMUTATION
  
  Moran.boot <- function(shape,x,wm,n.sim){
    
    
    Morans <- NULL
    
    m.list <- Moran.fun(shape,x,wm) #, print.boot = TRUE)
    
    for (i in 1:n.sim){
      
      w.per <- wm[sample(nrow(wm)),sample(ncol(wm))]
      
      Morans[i] <- Moran.fun(shape,x,w.per)[[1]] # calcola n.sim volte l'I moran sulla matrice permutata
      
    }
    
    MI <- m.list[[1]]
    EI <- m.list[[2]]
    VI <- m.list[[3]]
    ZS <- (MI - EI)/(sqrt(VI))
    p.value <- 2*pnorm(-abs(ZS)) 
    
    
    invisible(base::list(
      "Moran" = MI,
      "Expected" = EI,
      "Variance" = VI,
      "Shape" = deparse(substitute(shape)),
      "Variable" = etichette[x],
      "Weight" = deparse(substitute(wm)),
      "N.sim" = n.sim,
      "Simulated" = as.vector(Morans),
      "z-score" = ZS,
      "p.value" = p.value))
    
  }

  print.moran <- function(moran.list, boot = FALSE, plot = FALSE) {
    
    if(boot == FALSE){
      
      cat("\n")
      cat("                         Moran's I  \n")
      cat("Shape: ", moran.list[[4]], "\n") # deparse(substitute(x)) serve a far leggere il nome dell'oggetto nell'environment
      cat("Data: ", moran.list[[5]], "\n")                 
      cat("Weights: ", moran.list[[6]], "\n") # senza deparse(sub(..)) e con wm solo,  avremmo avuto una stringa di 01011010001 poichè mostrava gli elementi
      cat("\n")
      cat("Moran I statistic: ", moran.list[[1]], "\n") # cat fa un paste() degli argomenti in c("..","..","...")
      cat("Expected value: ", moran.list[[2]], "\n") # \n significa a capo
      cat("Variance: ", moran.list[[3]], "\n") 
      
    } else {
      
      
      cat("\n")
      cat("           Moran I test under randomization      \n")
      cat("Shape: ", moran.list[[4]], "\n") # deparse(substitute(x)) serve a far leggere il nome dell'oggetto nell'environment
      cat("Data: ", moran.list[[5]], "\n")                 
      cat("Weights: ", moran.list[[6]], "\n") # senza deparse(sub(..)) e con wm solo,  avremmo avuto una stringa di 01011010001 poichè mostrava gli elementi
      cat("N.sim: ", moran.list[[7]], "\n")
      cat("\n")
      cat("Moran I statistic: ", moran.list[[1]], "\n") # cat fa un paste() degli argomenti in c("..","..","...")
      cat("Expected value: ", moran.list[[2]], "\n") # \n significa a capo
      cat("Variance: ", moran.list[[3]], "\n")
      cat("Z-score: ", moran.list[[9]], "    p-value: ", moran.list[[10]], "\n")
      
      if(plot == TRUE){
        a <- as.data.frame(moran.list[8])
        
        p <- ggplot(a, aes(x = Simulated)) + 
          geom_histogram(color = "black", fill = "white", binwidth=0.003) +
          geom_vline(aes(xintercept = moran.list[[1]]), color = "red", linetype = "dashed", size = 1)
        
        p
      }
      
    }
    
  }
  
  #Moran.boot(OGR.prov, 1, wm.prov, 5000)
  

save.image("~/Desktop/Shiny Progetto Deployment/.RData")
