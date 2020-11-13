#################################################################################
#                                                                               #
#                                                      SOURCE SCRIPT            #
#                                                                               #
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
  library(magrittr)
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
    
    a1 <- wm + t(wm) 
    a2 <- rowSums(a1)^2
    S2 <- sum(a2)
    
    b1 <- a1^2
    S1 <- (1/2)*sum(b1)
    
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
  
  Moran.perm <- function(shape,x,wm,n.sim){
    
    
    Morans <- NULL
    
    m.list <- Moran.fun(shape,x,wm) #, print.boot = TRUE)
    
    for (i in 1:n.sim){
      
      w.per <- wm[sample(nrow(wm)),sample(ncol(wm))]
      w.per <- (1/2)*(w.per+t(w.per))
      Morans[i] <- Moran.fun(shape,x,w.per)[[1]] # calcola n.sim volte l'I moran sulla matrice permutata
      
    }
    
    MI <- m.list[[1]]
    EI <- m.list[[2]]
    #EI <- mean(Morans)
    VI <- m.list[[3]]
    #VI <- sd(Morans)^2
    ZS <- (MI - EI)/(sqrt(VI))
    #p.value <- pnorm(-abs(ZS))
    p.value <- ecdf(as.vector(Morans)) #2*pnorm(-abs(ZS)) 
    p.value <- 1-p.value(MI) #
    
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

  Moran.boot <- function(shape,x,wm,n.sim,alpha=0.05){
    
    n <- length(shape)
    y <- shape[[etichette[x]]]
    
    ya <- array(NA, dim=c(length(OGR.prov),1,n.sim))
    Morans <- NULL
    
    for (i in 1:n.sim){
      
      ya[,,i] <- y[sample(1:length(y), length(y), replace = TRUE)]
      #y <- as.vector(y[,1, drop=TRUE])
    }
    
    for (i in 1:n.sim){
      
      y <- ya[,,i]
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
      
      Morans[i] <- vr * sw
    }
    
    sorted <- sort(Morans)
    interval <- c(sorted[round(n.sim*alpha/2)],sorted[round(n.sim-n.sim*alpha/2)])
    
    m.list <- Moran.fun(shape,x,wm)
    
    invisible(base::list(
      "Moran" = m.list[[1]],
      "Expected" = mean(Morans),
      "Variance" = sd(Morans)^2,
      "Shape" = deparse(substitute(shape)),
      "Variable" = etichette[x],
      "Weight" = deparse(substitute(wm)),
      "N.sim" = n.sim,
      "Boot sample" = as.vector(Morans),
      "Boot interval" = interval,
      "Significance" = alpha))
    
  }
  
  Moran.boot <- function(shape,x,wm,n.sim,alpha=0.05){
    
    n <- length(shape)
    y <- shape[[etichette[x]]]
    
    ya <- array(NA, dim=c(length(OGR.prov),1,n.sim))
    Morans <- NULL
    # Ogni dimensione [,,i] dell'array contiene un ricampionamento
    for (i in 1:n.sim){
      
      ya[,,i] <- y[sample(1:length(y), length(y), replace = TRUE)]
      #y <- as.vector(y[,1, drop=TRUE])
    }
    #Calcolo moran index per n volte
    for (i in 1:n.sim){
      
      y <- ya[,,i]
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
      
      Morans[i] <- vr * sw
    }
    
    #Creo intervallo di confidenza
    sorted <- sort(Morans)
    interval <- c(sorted[round(n.sim*alpha/2)],sorted[round(n.sim-n.sim*alpha/2)])
    
    m.list <- Moran.fun(shape,x,wm)
    
    #Intervallo basic-boot con differenze
    delta.boot <- sort(Morans-m.list[[1]])
    perc.delta <- c(delta.boot[round(n.sim*alpha/2)],delta.boot[round(n.sim-n.sim*alpha/2)])
    
    basic.interval <- c(m.list[[1]]-perc.delta[2],m.list[[1]]-perc.delta[1])
    
    invisible(base::list(
      "Moran" = m.list[[1]],
      "Expected" = mean(Morans),
      "Variance" = sd(Morans)^2,
      "Shape" = deparse(substitute(shape)),
      "Variable" = etichette[x],
      "Weight" = deparse(substitute(wm)),
      "N.sim" = n.sim,
      "Boot sample" = as.vector(Morans),
      "Boot interval" = interval,
      "Significance" = alpha,
      "Basic boot" = basic.interval))
    
  }
  
  print.boot <- function(boot.list, plot=FALSE){
    
    cat("\n")
    cat("               Moran's I Bootstrap   \n")
    cat("Shape: ", boot.list[[4]], "\n") # deparse(substitute(x)) serve a far leggere il nome dell'oggetto nell'environment
    cat("Data: ", boot.list[[5]], "\n")                 
    cat("Weights: ", boot.list[[6]], "\n") # senza deparse(sub(..)) e con wm solo,  avremmo avuto una stringa di 01011010001 poichè mostrava gli elementi
    cat("N.sim: ", boot.list[[7]], "\n")
    cat("Significance: ", boot.list[[10]], "\n")
    cat("\n")
    cat("Moran I statistic: ", boot.list[[1]], "\n") # cat fa un paste() degli argomenti in c("..","..","...")
    cat("Sample mean: ", boot.list[[2]], "\n") # \n significa a capo
    cat("Sample var: ", boot.list[[3]], "\n") 
    cat("Percentile interval: ", boot.list[[9]], "\n")
    cat("Basic-boot interval: ", boot.list[[11]], "\n")
    
    
    if(plot == TRUE){
      
      a <- as.data.frame(boot.list[8])
      
      p <- ggplot(a, aes(x = Boot.sample)) + 
        geom_histogram(color = "black", fill = "white", binwidth=0.003) +
        geom_vline(aes(xintercept = boot.list[[9]][1]), color = "red", size = 1) +
        geom_vline(aes(xintercept = boot.list[[9]][2]), color = "red", size = 1) 
      
      p
      
    }
    
  }
  
  
  block.boot <- function(x,wm,n.sim,alpha=0.05){
    
    
    
    a <- array(NA,dim = c(10,1,11))
    
    #Creazione blocchi
    #####
    
    # Primo blocco Piemonte+v.daosta+varese
    a[,,1] <- subset(OGR.prov, DEN_PCM == "Aosta" |
                       DEN_PCM == "Torino" | 
                       DEN_PCM == "Asti" |
                       DEN_PCM == "Vercelli" |
                       DEN_PCM == "Biella" | 
                       DEN_PCM == "Verbano-Cusio-Ossola" |
                       DEN_PCM == "Alessandria" |
                       DEN_PCM == "Cuneo" | 
                       DEN_PCM == "Novara" |
                       DEN_PCM == "Varese")[[etichette[x]]]
    
    
    # 2o blocco, lombardia tranne mantova e brescia
    a[,,2] <- subset(OGR.prov, DEN_PCM == "Monza e della Brianza" |
                       DEN_PCM == "Como" | 
                       DEN_PCM == "Lecco" |
                       DEN_PCM == "Sondrio" |
                       DEN_PCM == "Cremona" | 
                       DEN_PCM == "Bergamo" |
                       DEN_PCM == "Milano" |
                       DEN_PCM == "Pavia" | 
                       DEN_PCM == "Lodi" |
                       DEN_PCM == "Brescia")[[etichette[x]]]
    
    # 3o blocco 
    a[,,3] <- subset(OGR.prov, DEN_PCM == "Mantova" | 
                       DEN_PCM == "Venezia" | 
                       DEN_PCM == "Bolzano" |
                       DEN_PCM == "Trento" |
                       DEN_PCM == "Belluno" | 
                       DEN_PCM == "Udine" |
                       DEN_PCM == "Pordenone" |
                       DEN_PCM == "Gorizia" | 
                       DEN_PCM == "Trieste" |
                       DEN_PCM == "Treviso")[[etichette[x]]]
    # 4o blocco
    a[,,4] <- subset(OGR.prov, DEN_PCM == "Imperia" |
                       DEN_PCM == "Vicenza" | 
                       DEN_PCM == "Verona" |
                       DEN_PCM == "Padova" |
                       DEN_PCM == "Rovigo" | 
                       DEN_PCM == "Ferrara" |
                       DEN_PCM == "Ravenna" |
                       DEN_PCM == "Forli'-Cesena" | 
                       DEN_PCM == "Modena" |
                       DEN_PCM == "Bologna")[[etichette[x]]]
    
    #11o blocco
    a[,,11] <- subset(OGR.prov, DEN_PCM == "Livorno" |
                        DEN_PCM == "Savona" | 
                        DEN_PCM == "Genova" |
                        DEN_PCM == "La Spezia" |
                        DEN_PCM == "Piacenza" | 
                        DEN_PCM == "Parma" |
                        DEN_PCM == "Reggio nell'Emilia" |
                        DEN_PCM == "Massa Carrara" | 
                        DEN_PCM == "Lucca" |
                        DEN_PCM == "Pistoia")[[etichette[x]]]
    
    #5o blocco
    a[,,5] <- subset(OGR.prov, DEN_PCM == "Perugia" |
                       DEN_PCM == "Pisa" | 
                       DEN_PCM == "Prato" |
                       DEN_PCM == "Firenze" |
                       DEN_PCM == "Arezzo" | 
                       DEN_PCM == "Siena" |
                       DEN_PCM == "Grosseto" |
                       DEN_PCM == "Rimini" | 
                       DEN_PCM == "Pesaro e Urbino" |
                       DEN_PCM == "Ancona")[[etichette[x]]]
    
    #6o blocco
    a[,,6] <- subset(OGR.prov, DEN_PCM == "Frosinone" |
                       DEN_PCM == "Terni" | 
                       DEN_PCM == "Viterbo" |
                       DEN_PCM == "Macerata" |
                       DEN_PCM == "Fermo" | 
                       DEN_PCM == "Ascoli Piceno" |
                       DEN_PCM == "Rieti" |
                       DEN_PCM == "Pescara" | 
                       DEN_PCM == "Teramo" |
                       DEN_PCM == "L'Aquila")[[etichette[x]]]
    #7o blocco
    a[,,7] <- subset(OGR.prov, DEN_PCM == "Roma" |
                       DEN_PCM == "Latina" | 
                       DEN_PCM =="Campobasso" |
                       DEN_PCM == "Caserta" |
                       DEN_PCM == "Benevento" | 
                       DEN_PCM == "Napoli" |
                       DEN_PCM == "Avellino" |
                       DEN_PCM == "Salerno" | 
                       DEN_PCM == "Chieti" |
                       DEN_PCM == "Isernia")[[etichette[x]]]
    
    #8o blocco
    a[,,8] <- subset(OGR.prov, DEN_PCM == "Crotone" |
                       DEN_PCM == "Foggia" | 
                       DEN_PCM == "Barletta-Andria-Trani" |
                       DEN_PCM == "Bari" |
                       DEN_PCM == "Brindisi" | 
                       DEN_PCM == "Lecce" |
                       DEN_PCM == "Taranto" |
                       DEN_PCM == "Matera" | 
                       DEN_PCM == "Potenza" |
                       DEN_PCM == "Cosenza")[[etichette[x]]]
    
    #9o blocco
    a[,,9] <- subset(OGR.prov, DEN_PCM == "Palermo" |
                       DEN_PCM == "Catanzaro" | 
                       DEN_PCM == "Vibo Valentia" |
                       DEN_PCM == "Reggio di Calabria" |
                       DEN_PCM == "Messina" | 
                       DEN_PCM == "Catania" |
                       DEN_PCM == "Siracusa" |
                       DEN_PCM == "Ragusa" | 
                       DEN_PCM == "Caltanissetta" |
                       DEN_PCM == "Enna")[[etichette[x]]]
    
    #10o blocco
    a[,,10] <- subset(OGR.prov, DEN_PCM == "Medio Campidano"  |
                        DEN_PCM == "Agrigento" | 
                        DEN_PCM == "Trapani" |
                        DEN_PCM == "Olbia-Tempio" |
                        DEN_PCM == "Sassari" | 
                        DEN_PCM == "Nuoro" |
                        DEN_PCM == "Ogliastra" |
                        DEN_PCM == "Cagliari" | 
                        DEN_PCM == "Oristano" |
                        DEN_PCM == "Carbonia-Iglesias")[[etichette[x]]]
    
    
    ######
    
    
    n <- 110
    
    Morans <- NULL
    yx <- array(NA,dim=c(110,1,n.sim))
    
    
    for(i in 1:n.sim){
      
      yx[,,i] <- c(as.vector(a[,,sample(1:11,11,replace=TRUE)]))
      
    }
    
    
    for (i in 1:n.sim){
      
      y <- yx[,,i]
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
      
      Morans[i] <- vr * sw
    }
    
    m.list <- Moran.fun(OGR.prov,x,wm)
    
    
    ### Intervallo con differenze
    delta.boot <- sort(Morans-m.list[[1]])
    perc.delta <- c(delta.boot[round(n.sim*alpha/2)],delta.boot[round(n.sim-n.sim*alpha/2)])
    
    basic.interval <- c(m.list[[1]]-perc.delta[2],m.list[[1]]-perc.delta[1])
    
    
    #Intervallo studentizzato
    
    stud <- sqrt(110)*delta.boot*(1/sd(Morans))
    perc.stud <- c(stud[round(n.sim*alpha/2)],stud[round(n.sim-n.sim*alpha/2)])
    stud.interval <- c(m.list[[1]]+(perc.stud[1]*sd(Morans)),m.list[[1]]+(perc.stud[2]*sd(Morans)))
    
    
    #Intervallo diretto (percentile bootstrap) 
    sorted <- sort(Morans)
    percentile.boot <- c(sorted[round(n.sim*alpha/2)],sorted[round(n.sim-n.sim*alpha/2)])
    
    
    
    invisible(base::list(
      "Moran" = m.list[[1]],
      "Expected" = mean(Morans),
      "Variance" = sd(Morans)^2,
      "Shape" = m.list[[4]],
      "Variable" = etichette[x],
      "Weight" = deparse(substitute(wm)),
      "N.sim" = n.sim,
      "Boot sample" = as.vector(Morans),
      "Boot percentile interval" = percentile.boot,
      "Boot basic interval" = basic.interval,
      "Boot student interval" = stud.interval,
      "Significance" = alpha))
    
  }
  
  block.print <- function(block.list, plot=FALSE){
    
    
    cat("\n")
    cat("               Moran's I block bootstrap   \n")
    cat("Shape: ", block.list[[4]], "\n") # deparse(substitute(x)) serve a far leggere il nome dell'oggetto nell'environment
    cat("Data: ", block.list[[5]], "\n")                 
    cat("Weights: ", block.list[[6]], "\n") # senza deparse(sub(..)) e con wm solo,  avremmo avuto una stringa di 01011010001 poich mostrava gli elementi
    cat("N.sim: ", block.list[[7]], "\n")
    cat("Significance: ", block.list[[12]], "\n")
    cat("\n")
    cat("Moran I statistic: ", block.list[[1]], "\n") # cat fa un paste() degli argomenti in c("..","..","...")
    cat("Sample mean: ", block.list[[2]], "\n") # \n significa a capo
    cat("Sample var: ", block.list[[3]], "\n") 
    cat("Percentile interval: ", block.list[[9]], "\n")
    cat("Basic-boot interval: ", block.list[[10]], "\n")
    cat("Studentized interval: ", block.list[[11]], "\n")
    
    
    if(plot==TRUE){
      
      a <- as.data.frame(block.list[8])
      
      p <- ggplot(a, aes(x = Boot.sample)) + 
        geom_histogram(color = "black", fill = "white", binwidth=0.003) +
        geom_vline(aes(xintercept = block.list[[9]][1]), color = "red", size = 1) +
        geom_vline(aes(xintercept = block.list[[9]][2]), color = "red", size = 1) +
        geom_vline(aes(xintercept = block.list[[1]]), color = "blue", size = 1)
      
      p
      
    }
  }
  
  
  
  
  
  #Moran.boot(OGR.prov, 1, wm.prov, 5000)

  
  
  
  
  # Data Wrangling risultati finali (TABLE GENERATOR.R)
  
  
  library(magrittr)
  
  
  table.generator <- function(OGR.prov,method="Queen",red,dist=1,k=2){
    OGR.prov.sub <- OGR.prov[]
    OGR.prov.sub@data$seq <- seq(1:length(OGR.prov.sub))
    xy.sub <- coordinates(OGR.prov.sub)
    
    ### Adicenze
    
    #Metodo semplice QUEEN
    
    if(method == "Queen"){
      wr.sub <- poly2nb(OGR.prov.sub, row.names = OGR.prov.sub$seq, queen = TRUE )
    }
    
    
    #Metodo distance based
    if (method == "Distance"){
      
      dsts.sub <- unlist(nbdists(wr.sub,xy.sub))
      wr.sub <- dnearneigh(xy.sub, d1 = 0, d2 = dist * max(dsts.com),  
                           row.names = OGR.prov.sub@data$seq)
      
    }
    
    
    
    if(method == "Nearest"){
      wr.sub <- knn2nb(knearneigh(xy.sub,k=k, RANN=FALSE),row.names = OGR.prov.sub$seq)
    }
    
    
    # Scelta dei pesi: binaria o pesata con distanze
    # B = Binary
    # W = Row standardized
    # C= Globally standardized
    # S= Variance stabilizing scheme (Tiefelsdorf et al. 1999, p. 167-168)
    
    
    #ty <- list(Binary="B",
    #Row.standardized="W",
    #Global.standardized="C",
    # Variance.reduction="S")
    
    
    
    
    
    wm.prov <- nb2mat(wr.sub, style = "B", zero.policy = TRUE)
    
    n <- length(OGR.prov)
    z <- red
    y <- OGR.prov@data[[z]]
    ybar <- mean(y)
    
    Moran.I <- Moran.fun(OGR.prov, which(etichette == z), wm.prov)
    return(Moran.I)
  }
  
  
  result.QB <- list()
  for(i in 1:length(etichette)){
    
    result.QB[[etichette[i]]] <- table.generator(OGR.prov,method="Queen",etichette[i])
  }
  
  # result.QC <- list()
  # for(i in 1:length(etichette)){
  #   
  #   result.QC[[etichette[i]]] <- table.generator(OGR.prov,method="Queen",wei.m="C",etichette[i])
  # }
  # result.QS <- list()
  # for(i in 1:length(etichette)){
  #   
  #   result.QS[[etichette[i]]] <- table.generator(OGR.prov,method="Queen",wei.m="S",etichette[i])
  # }
  
  
  #DISTANCE
  
  # 2
  
  result.D2B <- list()
  
  for(i in 1:length(etichette)){
    
    result.D2B[[etichette[i]]] <- table.generator(OGR.prov,method="Distance",etichette[i],dist=2)
  }
  
  # result.D2C <- list()
  # 
  # for(i in 1:length(etichette)){
  #   
  #   result.D2C[[etichette[i]]] <- table.generator(OGR.prov,method="Distance",wei.m="C",etichette[i],dist=2)
  # }
  # result.D2S <- list()
  # 
  # for(i in 1:length(etichette)){
  #   
  #   result.D2S[[etichette[i]]] <- table.generator(OGR.prov,method="Distance",wei.m="S",etichette[i],dist=2)
  # }
  
  # 3 
  
  result.D3B <- list()
  for(i in 1:length(etichette)){
    
    result.D3B[[etichette[i]]] <- table.generator(OGR.prov,method="Distance",etichette[i],dist=3)
  }
  # 
  # result.D3C <- list()
  # for(i in 1:length(etichette)){
  #   
  #   result.D3C[[etichette[i]]] <- table.generator(OGR.prov,method="Distance",wei.m="C",etichette[i],dist=3)
  # }
  # 
  # result.D3S <- list()
  # for(i in 1:length(etichette)){
  #   
  #   result.D3S[[etichette[i]]] <- table.generator(OGR.prov,method="Distance",wei.m="S",etichette[i],dist=3)
  # }
  
  # 4
  
  
  result.D4B <- list()
  for(i in 1:length(etichette)){
    
    result.D4B[[etichette[i]]] <- table.generator(OGR.prov,method="Distance",etichette[i],dist=4)
  }
  
  # 
  # result.D4C <- list()
  # for(i in 1:length(etichette)){
  #   
  #   result.D4C[[etichette[i]]] <- table.generator(OGR.prov,method="Distance",wei.m="C",etichette[i],dist=4)
  # }
  # 
  # result.D4S <- list()
  # for(i in 1:length(etichette)){
  #   
  #   result.D4S[[etichette[i]]] <- table.generator(OGR.prov,method="Distance",wei.m="S",etichette[i],dist=4)
  # }
  
  # 5
  
  
  result.D5B <- list()
  for(i in 1:length(etichette)){
    
    result.D5B[[etichette[i]]] <- table.generator(OGR.prov,method="Distance",etichette[i],dist=5)
  }
  
  # result.D5C <- list()
  # for(i in 1:length(etichette)){
  #   
  #   result.D5C[[etichette[i]]] <- table.generator(OGR.prov,method="Distance",wei.m="C",etichette[i],dist=5)
  # }
  # 
  # result.D5S <- list()
  # for(i in 1:length(etichette)){
  #   
  #   result.D5S[[etichette[i]]] <- table.generator(OGR.prov,method="Distance",wei.m="S",etichette[i],dist=5)
  # }
  
  
  
  # Nearest neighbour
  
  #2
  result.N2B <- list()
  for(i in 1:length(etichette)){
    
    result.N2B[[etichette[i]]] <- table.generator(OGR.prov,method="Nearest",etichette[i],k=2)
  }
  
  
  #3
  result.N3B <- list()
  for(i in 1:length(etichette)){
    
    result.N3B[[etichette[i]]] <- table.generator(OGR.prov,method="Nearest",etichette[i],k=3)
  }
  
  
  #4
  result.N4B <- list()
  for(i in 1:length(etichette)){
    
    result.N4B[[etichette[i]]] <- table.generator(OGR.prov,method="Nearest",etichette[i],k=4)
  }
  
  
  #5
  result.N5B <- list()
  for(i in 1:length(etichette)){
    
    result.N5B[[etichette[i]]] <- table.generator(OGR.prov,method="Nearest",etichette[i],k=5)
  }
  
  
  #6
  result.N6B <- list()
  for(i in 1:length(etichette)){
    
    result.N6B[[etichette[i]]] <- table.generator(OGR.prov,method="Nearest",etichette[i],k=6)
  }
  
  
  
  
  
  
  
  
  
  
  
  res.all <- list(
    Queen = result.QB,
    
    Distance = list(
      "2" = result.D2B,
      "3" = result.D3B,
      "4" = result.D4B,
      "5" = result.D5B
    ),
    
    Nearest = list(
      "2" = result.N2B,
      "3" = result.N3B,
      "4" = result.N4B,
      "5" = result.N5B,
      "6" = result.N6B)
    
  )
  
  
  
  
  rm(result.QB,
     result.D2B,
     result.D3B,
     result.D4B,
     result.D5B,
     result.N2B,
     result.N3B,
     result.N4B,
     result.N5B,
     result.N6B
  )
  
  
  #xx <- res.all %>% extract2(c(1,1,1,1))
  
  # Estrazione dati moran index queen
  
  mat <- vector("numeric",length=length(etichette))
  
  for(i in 1:length(etichette)){
    
    
    mat[i] <- res.all %>% extract2(c(1,i,1))
    
  }
  
  
  queen.table <- data.frame(
    reddito = etichette,
    binary = mat
  )
  names(queen.table) <- c("Etichette","Queen")
  
  rm(mat)
  
  
  
  
  # Estrazione ARRAY Distance
  
  dist.table <- matrix(0, ncol=4, nrow=length(etichette))
  
  
  for (d in 1:4){
    for(i in 1:length(etichette)){
      
      
      dist.table[i,d] <- res.all %>% extract2(c(2,d,i,1))
      
    }}
  
  dist.table <- data.frame(
    reddito = etichette,
    distance = dist.table
  )
  names(dist.table) <- c("Etichette","D2","D3","D4","D5")
  
  
  ### ESTRAZIONE NEAREST
  
  near.table <- matrix(0, ncol=5, nrow=length(etichette))
  
  
  for (d in 1:5){
    for(i in 1:length(etichette)){
      
      
      near.table[i,d] <- res.all %>% extract2(c(3,d,i,1))
      
    }}
  
  near.table <- data.frame(
    reddito = etichette,
    nearest = near.table
  )
  names(near.table) <- c("Etichette","K2","K3","K4","K5","K6")
  
  all.table <- cbind(queen.table,dist.table[,-1],near.table[,-1])
  
  
  
## Estrazione intervallo di confidenza

  
boot.generator <- function(OGR.prov,method="Queen",red,dist=1,k=2){
    OGR.prov.sub <- OGR.prov[]
    OGR.prov.sub@data$seq <- seq(1:length(OGR.prov.sub))
    xy.sub <- coordinates(OGR.prov.sub)
    
    ### Adicenze
    
    #Metodo semplice QUEEN
    
    if(method == "Queen"){
      wr.sub <- poly2nb(OGR.prov.sub, row.names = OGR.prov.sub$seq, queen = TRUE )
    }
    
    
    #Metodo distance based
    if (method == "Distance"){
      
      dsts.sub <- unlist(nbdists(wr.sub,xy.sub))
      wr.sub <- dnearneigh(xy.sub, d1 = 0, d2 = dist * max(dsts.com),  
                           row.names = OGR.prov.sub@data$seq)
      
    }
    
    
    
    if(method == "Nearest"){
      wr.sub <- knn2nb(knearneigh(xy.sub,k=k, RANN=FALSE),row.names = OGR.prov.sub$seq)
    }
    
    
    wm.prov <- nb2mat(wr.sub, style = "B", zero.policy = TRUE)
    
    n <- length(OGR.prov)
    z <- red
    y <- OGR.prov@data[[z]]
    ybar <- mean(y)
    
    Moran.I <- block.boot(which(etichette == z), wm.prov,9000)
    return(Moran.I)
  } 

#Queen

quant.interval.q <- array(NA,dim=c(1,2,length(etichette)))  
  
for (i in 1:length(etichette)){

quant.interval.q[,,i] <- boot.generator(OGR.prov,"Queen",etichette[i])[[9]]

  
}

quant.interval.q <- matrix(quant.interval.q, ncol=2, byrow = TRUE)

#Distance 2

quant.interval.d2 <- array(NA,dim=c(1,2,length(etichette)))  

for (i in 1:length(etichette)){
  
  quant.interval.d2[,,i] <- boot.generator(OGR.prov,"Distance",etichette[i],dist=2)[[9]]
  
  
}

quant.interval.d2 <- matrix(quant.interval.d2, ncol=2, byrow = TRUE)

#Distance 3

quant.interval.d3 <- array(NA,dim=c(1,2,length(etichette)))  

for (i in 1:length(etichette)){
  
  quant.interval.d3[,,i] <- boot.generator(OGR.prov,"Distance",etichette[i],dist=3)[[9]]
  
  
}

quant.interval.d3 <- matrix(quant.interval.d3, ncol=2, byrow = TRUE)

#Distance 4

quant.interval.d4 <- array(NA,dim=c(1,2,length(etichette)))  

for (i in 1:length(etichette)){
  
  quant.interval.d4[,,i] <- boot.generator(OGR.prov,"Distance",etichette[i],dist=4)[[9]]
  
  
}

quant.interval.d4 <- matrix(quant.interval.d4, ncol=2, byrow = TRUE)
  
#Distance 5

quant.interval.d5 <- array(NA,dim=c(1,2,length(etichette)))  

for (i in 1:length(etichette)){
  
  quant.interval.d5[,,i] <- boot.generator(OGR.prov,"Distance",etichette[i],dist=5)[[9]]
  
  
}
  
quant.interval.d5 <- matrix(quant.interval.d5, ncol=2, byrow = TRUE)


#Neigb 2

quant.interval.n2 <- array(NA,dim=c(1,2,length(etichette)))  

for (i in 1:length(etichette)){
  
  quant.interval.n2[,,i] <- boot.generator(OGR.prov,"Nearest",etichette[i],k=2)[[9]]
  
  
}

quant.interval.n2 <- matrix(quant.interval.n2, ncol=2, byrow = TRUE)


#Neigb 3

quant.interval.n3 <- array(NA,dim=c(1,2,length(etichette)))  

for (i in 1:length(etichette)){
  
  quant.interval.n3[,,i] <- boot.generator(OGR.prov,"Nearest",etichette[i],k=3)[[9]]
  
  
}

quant.interval.n3 <- matrix(quant.interval.n3, ncol=2, byrow = TRUE)
  
#Neigb 4

quant.interval.n4 <- array(NA,dim=c(1,2,length(etichette)))  

for (i in 1:length(etichette)){
  
  quant.interval.n4[,,i] <- boot.generator(OGR.prov,"Nearest",etichette[i],k=4)[[9]]
  
  
} 

quant.interval.n4 <- matrix(quant.interval.n4, ncol=2, byrow = TRUE)
  

#Neigb 5

quant.interval.n5 <- array(NA,dim=c(1,2,length(etichette)))  

for (i in 1:length(etichette)){
  
  quant.interval.n5[,,i] <- boot.generator(OGR.prov,"Nearest",etichette[i],k=5)[[9]]
  
  
}  

quant.interval.n5 <- matrix(quant.interval.n5, ncol=2, byrow = TRUE)
  
#Neigb 6

quant.interval.n6 <- array(NA,dim=c(1,2,length(etichette)))  

for (i in 1:length(etichette)){
  
  quant.interval.n6[,,i] <- boot.generator(OGR.prov,"Nearest",etichette[i],k=6)[[9]]
  
  
} 

quant.interval.n6 <- matrix(quant.interval.n6, ncol=2, byrow = TRUE)


all.interval <- as.data.frame(cbind(quant.interval.q,
                                    quant.interval.d2,
                                    quant.interval.d3,
                                    quant.interval.d4,
                                    quant.interval.d5,
                                    quant.interval.n2,
                                    quant.interval.n3,
                                    quant.interval.n4,
                                    quant.interval.n5,
                                    quant.interval.n6))

row.names(all.interval) <- etichette
colnames(all.interval) <- c("q.l","q.u","d2.l","d2.u","d3.l","d3.u",
                            "d4.l","d4.u","d5.l","d5.u",
                            "k2.l","k2.u","k3.l","k3.u",
                            "k4.l","k4.u","k5.l","k5.u","k6.l","k6.u")
  
all.interval <- round(all.interval, digits = 4)
all.interval <- all.interval %>%
                 unite("q",c("q.l","q.u"),sep =" _ ") %>%
                 unite("d2",c("d2.l","d2.u"),sep =" _ ") %>%
                 unite("d3",c("d3.l","d3.u"),sep =" _ ") %>%
                 unite("d4",c("d4.l","d4.u"),sep =" _ ") %>%
                 unite("d5",c("d5.l","d5.u"),sep =" _ ") %>%
                 unite("k2",c("k2.l","k2.u"),sep =" _ ") %>%
                 unite("k3",c("k3.l","k3.u"),sep =" _ ") %>%
                 unite("k4",c("k4.l","k4.u"),sep =" _ ") %>%
                 unite("k5",c("k5.l","k5.u"),sep =" _ ") %>%
                 unite("k6",c("k6.l","k6.u"),sep =" _ ") 




rm(quant.interval.q,
   quant.interval.d2,
   quant.interval.d3,
   quant.interval.d4,
   quant.interval.d5,
   quant.interval.n2,
   quant.interval.n3,
   quant.interval.n4,
   quant.interval.n5,
   quant.interval.n6)
  
  
  


save.image("~/Desktop/Shiny Progetto Deployment/.RData")
