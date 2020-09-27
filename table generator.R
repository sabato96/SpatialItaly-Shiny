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
library(DT)
library(ggplot2)
library(plotly)
library(GGally)
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


