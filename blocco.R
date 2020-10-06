

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
