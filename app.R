#################################################################################
# Progetto Lab. DISES 2020            Studio spaziale sul dataset redditi_com   #
#                                                                               #
# Gargiulo Sabato & Manzo Maria                                                 #
#                                                                               #
#################################################################################



# Get the data

#source("source script.R")
#load("C:/Users/Maria/Desktop/Progetto Lab")
load(".RData")

# Librerie
#####
library(plyr)
library(tidyverse)
library(ggmap)
library(sp)
library(sf)
library(tmap)
library(tmaptools)
library(leaflet)
library(RColorBrewer)
library(shinyjs)
library(spdep)
library(dplyr)
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(GGally)
library(magrittr)
# Shiny App
########


# UI serve a definire l'interfaccia grafica
  
ui <- dashboardPage(
  dashboardHeader(title = "Shiny Project"),
  
  dashboardSidebar( #menu laterale con selezioni
    sidebarMenu(
      
      menuItem("Introduction", tabName = "intro", icon = icon("dashboard")),
      
      menuItem("Explorative", tabName = "explorative", icon = icon("certificate", lib = "glyphicon"),
               menuSubItem("Data Table", tabName = "table", icon = icon("dashboard")), #7,8
               menuSubItem("Circle Plot", tabName = "cirplot", icon = icon("dashboard")), #2
               menuSubItem("Violin Plot", tabName = "viol", icon = icon("dashboard")),#3
               menuSubItem("Correlogramm", tabName="Corr", icon=icon("dashboard"))#10,11,12,13
               ),
      
      menuItem("Leaflet map", tabName = "leaflethome", icon = icon("globe", lib = "glyphicon"),
               menuSubItem("Leaflet global", tabName = "leafletglobal", icon = icon("blackboard", lib = "glyphicon")), #4
               menuSubItem("Leaflet local", tabName = "leafletlocal", icon = icon("blackboard", lib = "glyphicon")) #9
               ), 

      menuItem("Moran", tabName = "moran", icon = icon("th")), #5
      menuItem("Bootstrap", tabName = "bootstrhome", icon = icon("tasks",lib = "glyphicon"),
               menuSubItem("Classic Bootstrap", tabName = "bootstr", icon = icon("tasks", lib = "glyphicon")), #19
               menuSubItem("Block Bootstrap", tabName = "blockbootstrap", icon = icon("tasks", lib = "glyphicon")) #21
               ), 
      menuItem("Permutation Test", tabName = "perm", icon = icon("flash", lib = "glyphicon")) #6
    )
  ),

  
  # Dashboard body dice a shiny cosa far vedere in base al menu scelto dall'utente
  
  # Il codice di cui sotto serve a creare gli elementi che andranno a comporre ogni singola
  # scheda presente alla sinistra dell'app e i relativi elementi da visualizzare
  

  dashboardBody(

    tabItems(
      
      
      tabItem(tabName = "table",
#####              

              fluidPage(
              
                #sidebar panel serve a inserire le scelte dell'utente
                
                DTOutput("table", width = "100%"),
                
                  fluidRow(
                    column(6,
                    
                   selectInput(inputId = "red7",
                               label = "Choose a variable to display",
                               choices = etichette,
                               selected = "Red. pens.") ),
                   
                   column(6,
                   selectInput(inputId = "red8",
                               label = "Choose a variable to display",
                               choices = etichette,
                               selected = "")),
                   

              
                mainPanel ( column(6, 
                  plotlyOutput("scatter",width = "100%")),
                  column(5,
                
                plotlyOutput("barplot", width = "100%"),offset = 1), width= 12)
               
                 
                  
                  
                
                  
                  
                  
                ))),
#####      
      
      tabItem(tabName = "cirplot",
#####

              sidebarLayout(
                
                sidebarPanel(
                  
                  
                  selectInput(inputId = "red2",
                              label = "Choose a variable to display",
                              choices = etichette[1:7],
                              selected = ""),
                  
                  

                  
                  #checkboxInput("check", label = "Interactive plot", value = FALSE),
                  
                  actionButton("Run2", "Run code")
                  
                  
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                  
                  plotOutput("cirplot", width = "100%", height = 800)
                  
                  
                  
                ))),  
      
#####
      
      tabItem(tabName = "viol",
#####              
              sidebarLayout(
               
                sidebarPanel(
                  
                  
                  selectInput(inputId = "red3",
                              label = "Choose a variable to display",
                              choices = etichette,
                              selected = ""),
                  

                  
                  selectInput(inputId = "palette3",
                              label = "Choose color palette",
                              choices = list("Blues","BuGn",
                                              "Greens","Greys",
                                             "PuBuGn","Dark2"),
                              selected = "Greys"),
                  
                  #checkboxInput("check", label = "Interactive plot", value = FALSE),
                  
                  actionButton("Run3", "Run code")
                  
                  
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                  
                  plotlyOutput("violinplot", width = "100%", height = 800)
                  
                  
                  
                ))),
      


#####
      tabItem(tabName = "Corr",
        #####
        
            sidebarLayout(
          
              sidebarPanel(
            
            
                selectInput(inputId = "red10",
                            label = "Choose a variable to display",
                            choices = etichette,
                            selected = ""),
            
                selectInput(inputId = "red11",
                            label = "Choose a variable to display",
                            choices = etichette,
                            selected = ""),
            
                selectInput(inputId = "red12",
                            label = "Choose a variable to display",
                            choices = etichette,
                            selected = ""),
            
                selectInput(inputId = "red13",
                            label = "Choose a variable to display",
                            choices = etichette,
                            selected = ""),
            
            
            
            #checkboxInput("check", label = "Interactive plot", value = FALSE),
            
                actionButton("Run10", "Run code")
            
            
          ),
          
          # Show a plot of the generated distribution
          mainPanel(
            
            plotOutput("corr", width = "100%"),
            
          ))), 
#####
      
      tabItem(tabName = "leafletglobal",
          
#####           

fluidPage(
  
  #sidebar panel serve a inserire le scelte dell'utente
  
  
  fluidRow(
    column(3,
           
           selectInput(inputId = "red4",
                       label = "Choose a variable to display",
                       choices = etichette,
                       selected = "")),
    
    column(3,
           numericInput(inputId = "g.col1",
                        label = ("Choose n colors"),
                        value = "5")),
    column(3,
           
           selectInput(inputId = "palette4",
                       label = "Choose color palette",
                       choices = list("Blues","BuGn",
                                      "Greens","Greys",
                                      "PuBuGn","Dark2"),
                       selected = "Greys")),
    
    column(3,actionButton("Run4", "Run code")),
    
    
    leafletOutput("qtm", width = "100%", height = 800),
  
    
  ))),
#####  
    

      tabItem(tabName = "leafletlocal",
#####           
fluidPage(
  
  #sidebar panel serve a inserire le scelte dell'utente
  
  
  fluidRow(
    column(2,
           
           selectInput(inputId = "shape1",
                       label = "Choose a macro to display",
                       choices = list("Nord",
                                      "Centro",
                                      "Sud"),
                       selected = "Nord")),
    
    column(2,
           selectInput(inputId = "red9",
                       label = "Choose a variable to display",
                       choices = etichette,
                       selected = "")),
    column(2,
           
           numericInput(inputId = "g.col2",
                        label = ("Choose n colors"),
                        value = "5")),
    
    column(2,
           selectInput(inputId = "palette9",
                       label = "Choose color palette",
                       choices = list("Blues","BuGn",
                                      "Greens","Greys",
                                      "PuBuGn","Dark2"),
                       selected = "Greys")),
    
    column(2,
           checkboxInput("all1","All")),
    
    column(2,
           actionButton("Run9", "Run code")),
           
    
    
    
    leafletOutput("qtm.local", width = "100%", height = 800),
    
    
    
    
    
    
    
    
  ))),
#####  


    tabItem(tabName = "moran",
            sidebarLayout(
#####             
               sidebarPanel(
                 
                 selectInput(inputId = "method1",
                             label = "Choose adiacency method",
                             choices = list("Queen","Distance","Nearest"),
                             selected = "Queen"),
                 
                 
                 numericInput(inputId = "k1",
                              label = "N. of neighbour",
                              value = 2,
                              min=2,
                              max=6),
                 
                 
                 
                 
                 sliderInput(inputId = "distance",
                             label = "Choose distance",
                             value = 1,
                             min = 1.5,
                             max = 5,
                             step = 0.5),
                 
             
                 selectInput(inputId = "red5",
                          label = "Choose a variable to display",
                          choices = etichette,
                          selected = ""),
                 
                 actionButton("Run5", "Run code")
            ),
            
            
            mainPanel(plotlyOutput("moran", width ="100%", height = 600),
                      verbatimTextOutput("moran.text"))
            
            
            )),
#####    
   
    
     tabItem(tabName = "perm",
            sidebarLayout(
#####              
              sidebarPanel(
                
                selectInput(inputId = "method3",
                            label = "Choose adiacency method",
                            choices = list("Queen","Distance","Nearest"),
                            selected = "Queen"),
              
                numericInput(inputId = "k2",
                             label = "N. of neighbour",
                             value = 2,
                             min=2,
                             max=6),
                
                sliderInput(inputId = "distance1",
                            label = "Choose distance",
                            value = 1,
                            min = 1.5,
                            max = 5,
                            step = 0.5),
                
                selectInput(inputId = "red6",
                            label = "Choose a variable to display",
                            choices = etichette,
                            selected = ""),
              
              
              numericInput(inputId = "n.sim",
                           label = ("Choose n iteration"),
                           value = "20"),
              
              actionButton("Run6", "Run code")
                  
                  
                ),
              
              mainPanel(plotlyOutput("perm", width = "100%", height = 600),
                        verbatimTextOutput("perm.text"))
              
#####              
              
              )),


tabItem(tabName = "bootstr",
        sidebarLayout(
          #####              
          sidebarPanel(
            
            selectInput(inputId = "method19",
                        label = "Choose adiacency method",
                        choices = list("Queen","Distance","Nearest"),
                        selected = "Queen"),
            
            numericInput(inputId = "k19",
                         label = "N. of neighbour",
                         value = 2,
                         min=2,
                         max=6),
            
            sliderInput(inputId = "distance19",
                        label = "Choose distance",
                        value = 1,
                        min = 1.5,
                        max = 5,
                        step = 0.5),
            
            selectInput(inputId = "red19",
                        label = "Choose a variable to display",
                        choices = etichette,
                        selected = ""),
            
            numericInput(inputId = "sign",
                         label = "Significance level",
                         value = 0.05,
                         min = 0.005,
                         max = 0.1,
                         step = 0.005),
            
            
            numericInput(inputId = "n.sim19",
                         label = ("Choose n iteration"),
                         value = "20"),
            
            actionButton("Run19", "Run code")
            
            
          ),
          
          mainPanel(plotlyOutput("bootstr", width = "100%", height = 600),
                    verbatimTextOutput("bootstr.text"))
          
          #####              
          
        )),

tabItem(tabName = "blockbootstrap",
        sidebarLayout(
          #####              
          sidebarPanel(
            
            selectInput(inputId = "method20",
                        label = "Choose adiacency method",
                        choices = list("Queen","Distance","Nearest"),
                        selected = "Queen"),
            
            numericInput(inputId = "k20",
                         label = "N. of neighbour",
                         value = 2,
                         min=2,
                         max=6),
            
            sliderInput(inputId = "distance20",
                        label = "Choose distance",
                        value = 1,
                        min = 1.5,
                        max = 5,
                        step = 0.5),
            
            selectInput(inputId = "red20",
                        label = "Choose a variable to display",
                        choices = etichette,
                        selected = ""),
            
            numericInput(inputId = "sign20",
                         label = "Significance level",
                         value = 0.05,
                         min = 0.005,
                         max = 0.1,
                         step = 0.005),
            
            
            numericInput(inputId = "n.sim20",
                         label = ("Choose n iteration"),
                         value = "20"),
            
            actionButton("Run20", "Run code")
            
            
          ),
          
          mainPanel(plotlyOutput("blockplot", width = "100%", height = 600),
                    verbatimTextOutput("blocktext"))
          
          #####              
          
        ))

            )
    )
  )
    
    
    


server <- function(input, output) {

 
  output$qtm <- renderLeaflet({
    
    if(input$Run4 == 0)
      return()
    
    isolate({
      
      x <- input$g.col1
      y <- input$red4
      tm <- tm_shape(OGR.prov) + tm_fill(input$red4, palette = input$palette4, style = "quantile", 
                                         n = x , contrast = c(0.28, 0.87),
                                         id = "DEN_PCM") + 
        
            tm_borders(alpha=.7) + tm_legend(legend.position = c("left", "bottom")) +
            tm_layout(title = paste(y,"medio per provincia"),
                  title.size = 1.1) +
        
            tm_shape(OGR.reg) + tm_borders(col = "black") 
      
      tmap_mode("view")
      
      tmap_leaflet(tm) 
      
    }) }) #Grafico interattivo per province
  
  
  output$qtm.local <- renderLeaflet({
    
    if(input$Run9 == 0)
      return()
    
    isolate({
      
      x <- input$g.col2
      y <- input$red9
      
      if(input$shape1 == "Nord" && input$all1==FALSE){
        
        tm <- tm_shape(OGR.com.nord) + tm_fill(input$red9, palette = input$palette9, style = "quantile", 
                                               n = input$g.col2, contrast = c(0.28, 0.87),
                                               id = "COMUNE") + 
          
          tm_borders(alpha=.7) + tm_legend(legend.position = c("left", "bottom")) +
          tm_layout(title = paste(y,"medio per comune"),
                    title.size = 1.1) +
          
          tm_shape(OGR.reg) + tm_borders(col = "black") 
      }
      
      if(input$shape1 == "Centro" && input$all1==FALSE){
        
        tm <- tm_shape(OGR.com.centro) + tm_fill(input$red9, palette = input$palette9, style = "quantile", 
                                                 n = input$g.col2, contrast = c(0.28, 0.87),
                                                 id = "COMUNE") + 
          
          tm_borders(alpha=.7) + tm_legend(legend.position = c("left", "bottom")) +
          tm_layout(title = paste(y,"medio per comune"),
                    title.size = 1.1) +
          
          tm_shape(OGR.reg) + tm_borders(col = "black") 
      }
      
      if(input$shape1 == "Sud" && input$all1==FALSE){
        
        tm <- tm_shape(OGR.com.sud) + tm_fill(input$red9, palette = input$palette9, style = "quantile", 
                                              n = input$g.col2, contrast = c(0.28, 0.87),
                                              id = "COMUNE") + 
          
          tm_borders(alpha=.7) + tm_legend(legend.position = c("left", "bottom")) +
          tm_layout(title = paste(y,"medio per comune"),
                    title.size = 1.1) +
          
          tm_shape(OGR.reg) + tm_borders(col = "black") 
      }
      
      if(input$all1==TRUE){
        tm <- tm_shape(OGR.com) + tm_fill(input$red9, palette = input$palette9, style = "quantile", 
                                              n = input$g.col2, contrast = c(0.28, 0.87),
                                              id = "COMUNE") + 
          
          tm_borders(alpha=.7) + tm_legend(legend.position = c("left", "bottom")) +
          tm_layout(title = paste(y,"medio per comune"),
                    title.size = 1.1) +
          
          tm_shape(OGR.reg) + tm_borders(col = "black") 
        
      }
      
      
      tmap_mode("view")
      
      tmap_leaflet(tm) 
      
    }) }) #Grafico iterattivo per comuni
  
  
  # output$qtm.plot <- renderPlot({
  #   
  #   if(input$Run1 == 0)
  #     return()
  #   isolate({
  #     
  #     x <- input$g.col
  #     y <- input$red4
  #     tm <- tm_shape(OGR.prov) + tm_fill(y, palette = input$palette, style = "quantile", 
  #                                        n = input$g.col, contrast = c(0.28, 0.87)) + 
  #       
  #       tm_borders(alpha=.7) + tm_legend(legend.position = c("left", "bottom")) +
  #       tm_layout(title = paste(y,"medio per provincia"),
  #                 title.size = 1.1) +
  #       
  #       tm_shape(OGR.reg) + tm_borders(col = "black") 
  #    
  #     plot(tm) 
  #   }) })


  
  #################################################
  # Moran Scatterplot 
  output$moran <- renderPlotly({
    
      if(input$Run5 == 0)
        return()
      isolate({
      
        #Scelta criterio adiacenze
        
        OGR.prov.sub <- OGR.prov[]
        OGR.prov.sub@data$seq <- seq(1:length(OGR.prov.sub))
        xy.sub <- coordinates(OGR.prov.sub)
        
        ### Adicenze
        
        #Metodo semplice QUEEN
        
        
        if(input$method1 == "Queen"){
          wr.sub <- poly2nb(OGR.prov.sub, row.names = OGR.prov.sub$seq, queen = TRUE )
          wm.prov <- nb2mat(wr.sub, style = "B", zero.policy = TRUE)
        }
        

        
        if(input$method1 == "Nearest"){
          wr.sub <- knn2nb(knearneigh(xy.sub,k=input$k1, RANN=FALSE),row.names = OGR.prov.sub$seq)
          wm.prov <- nb2mat(wr.sub, style = "B", zero.policy = TRUE)
          wm.prov <- (1/2)*(wm.prov+t(wm.prov))
        }
      
        
        #Metodo distance based
        if (input$method1 == "Distance"){
          
          
          wr.sub <- dnearneigh(xy.sub, d1 = 0, d2 = input$distance * max(dsts.com),  
                               row.names = OGR.prov.sub@data$seq)
          dsts.sub <- unlist(nbdists(wr.sub,xy.sub))
          wm.prov <- nb2mat(wr.sub, style = "B", zero.policy = TRUE)
          wm.prov <- (1/2)*(wm.prov+t(wm.prov))
        }
        
        # Scelta dei pesi: binaria o pesata con distanze
        # B = Binary
        # W = Row standardized
        # C= Globally standardized
        # S= Variance stabilizing scheme (Tiefelsdorf et al. 1999, p. 167-168)
        

                
             n <- length(OGR.prov)
             z <- input$red5
             y <- OGR.prov@data[[z]]
             ybar <- mean(y)
    

             ms <- cbind(id = rep(1:n, each = n), y = rep(y, each = n),
                         value = as.vector(wm.prov * y))
             ms <- ms[ms[,3] > 0,]
             ams <- aggregate(ms[,2:3], list(ms[,1]), FUN = mean)
             ams <- ams[,-1]
             head(ams)
             
             p <- ggplot(data = ams, aes(x=y, y= value)) + 
               xlim(c(min(y)-mean(y),max(y)+mean(y))) +
               geom_point() + geom_smooth(method = "lm", color = "black") +
               geom_hline(yintercept=mean(ams[,2]), linetype="dashed", color = "red") +
               geom_vline(xintercept = ybar, linetype="dashed", color = "red") +
               ylab("Reddito medio locale") +
               xlab("Reddito")
               
             
           
             #p <- plotly(p)
             ggplotly(p)
             
    
        }) }) #Plot
    
  output$moran.text <- renderPrint({
      
      if(input$Run5 == 0)
        return()
      isolate({
        OGR.prov.sub <- OGR.prov[]
        OGR.prov.sub@data$seq <- seq(1:length(OGR.prov.sub))
        xy.sub <- coordinates(OGR.prov.sub)
        
        ### Adicenze
        
        #Metodo semplice QUEEN
        
        
        if(input$method1 == "Queen"){
          wr.sub <- poly2nb(OGR.prov.sub, row.names = OGR.prov.sub$seq, queen = TRUE )
          wm.prov <- nb2mat(wr.sub, style = "B", zero.policy = TRUE)
        }
        
        
        
        if(input$method1 == "Nearest"){
          wr.sub <- knn2nb(knearneigh(xy.sub,k=input$k1, RANN=FALSE),row.names = OGR.prov.sub$seq)
          wm.prov <- nb2mat(wr.sub, style = "B", zero.policy = TRUE)
          wm.prov <- (1/2)*(wm.prov+t(wm.prov))
        }
        
        
        #Metodo distance based
        if (input$method1 == "Distance"){
          
          
          wr.sub <- dnearneigh(xy.sub, d1 = 0, d2 = input$distance * max(dsts.com),  
                               row.names = OGR.prov.sub@data$seq)
          dsts.sub <- unlist(nbdists(wr.sub,xy.sub))
          wm.prov <- nb2mat(wr.sub, style = "B", zero.policy = TRUE)
          wm.prov <- (1/2)*(wm.prov+t(wm.prov))
        }
        
        
        
         
      n <- length(OGR.prov)
      z <- input$red5
      y <- OGR.prov@data[[z]]
      ybar <- mean(y)
      
      Moran.I <- Moran.fun(OGR.prov, which(etichette == z), wm.prov)
      
      print.moran(Moran.I)

    }) }) #Testo
  #################################################
  
  
  
  #################################################
  # Permutation test
  
  output$perm <- renderPlotly({
      
      
      if(input$Run6 == 0)
        return()
      isolate({
        
        
        #Scelta criterio adiacenze
        
        OGR.prov.sub <- OGR.prov[]
        OGR.prov.sub@data$seq <- seq(1:length(OGR.prov.sub))
        xy.sub <- coordinates(OGR.prov.sub)
        
        ### Adicenze
        
        #Metodo semplice QUEEN
        
        
        if(input$method3 == "Queen"){
          wr.sub <- poly2nb(OGR.prov.sub, row.names = OGR.prov.sub$seq, queen = TRUE )
          wm.prov <- nb2mat(wr.sub, style = "B", zero.policy = TRUE)
          
        }
        
        
        
        if(input$method3 == "Nearest"){
          wr.sub <- knn2nb(knearneigh(xy.sub,k=input$k2, RANN=FALSE),row.names = OGR.prov.sub$seq)
          wm.prov <- nb2mat(wr.sub, style = "B", zero.policy = TRUE)
          wm.prov <- (1/2)*(wm.prov+t(wm.prov))
        }
        
        
        #Metodo distance based
        if (input$method3 == "Distance"){
          
          
          wr.sub <- dnearneigh(xy.sub, d1 = 0, d2 = input$distance1 * max(dsts.com),  
                               row.names = OGR.prov.sub@data$seq)
          dsts.sub <- unlist(nbdists(wr.sub,xy.sub))
          wm.prov <- nb2mat(wr.sub, style = "B", zero.policy = TRUE)
          wm.prov <- (1/2)*(wm.prov+t(wm.prov))
        }
        
        # Scelta dei pesi: binaria o pesata con distanze
        # B = Binary
        # W = Row standardized
        # C= Globally standardized
        # S= Variance stabilizing scheme (Tiefelsdorf et al. 1999, p. 167-168)
        

             z <- input$red6
             zz <- input$n.sim
             
             
          
             M.perm <- Moran.perm(OGR.prov, which(etichette == z), wm.prov, zz)
             
             print.moran(M.perm, boot = TRUE, plot = TRUE)
            
             
             # a <- cbind(OGR.prov[[z]],rep(mean(OGR.prov[[z]]), n= length(OGR.prov)))
             # a <- as.data.frame(a)
             # names(a) <- c("Observed","Expected")
             # ww <- nb2listw(wr.prov, zero.policy = TRUE)
             # niter <- zz
             # 
             # moran.mboot<-boot(a, statistic=moranI.pboot, sim="parametric",
             #                   ran.gen=poisson.sim,
             #                   R=niter, listw=ww.prov,
             #                   n=length(wr.prov),
             #                  S0=Szero(ww.prov) )
             # plot(moran.mboot)
    
    }) }) #Plot

  output$perm.text <- renderPrint({
      
      
      if(input$Run6 == 0)
        return()
      isolate({
        
        #Scelta criterio adiacenze
        
        OGR.prov.sub <- OGR.prov[]
        OGR.prov.sub@data$seq <- seq(1:length(OGR.prov.sub))
        xy.sub <- coordinates(OGR.prov.sub)
        
        ### Adicenze
        
        #Metodo semplice QUEEN
        
        
        if(input$method3 == "Queen"){
          wr.sub <- poly2nb(OGR.prov.sub, row.names = OGR.prov.sub$seq, queen = TRUE )
          wm.prov <- nb2mat(wr.sub, style = "B", zero.policy = TRUE)
          
        }
        
        
        
        if(input$method3 == "Nearest"){
          wr.sub <- knn2nb(knearneigh(xy.sub,k=input$k2, RANN=FALSE),row.names = OGR.prov.sub$seq)
          wm.prov <- nb2mat(wr.sub, style = "B", zero.policy = TRUE)
          wm.prov <- (1/2)*(wm.prov+t(wm.prov))
        }
        
        
        #Metodo distance based
        if (input$method3 == "Distance"){
          
          
          wr.sub <- dnearneigh(xy.sub, d1 = 0, d2 = input$distance1 * max(dsts.com),  
                               row.names = OGR.prov.sub@data$seq)
          dsts.sub <- unlist(nbdists(wr.sub,xy.sub))
          wm.prov <- nb2mat(wr.sub, style = "B", zero.policy = TRUE)
          wm.prov <- (1/2)*(wm.prov+t(wm.prov))
        }
        
        # Scelta dei pesi: binaria o pesata con distanze
        # B = Binary
        # W = Row standardized
        # C= Globally standardized
        # S= Variance stabilizing scheme (Tiefelsdorf et al. 1999, p. 167-168)

        z <- input$red6
        zz <- input$n.sim
        
        
        
        M.perm <- Moran.perm(OGR.prov, which(etichette == z), wm.prov, zz)
        
        print.moran(M.perm, boot = TRUE)
        
        
        
        
        # a <- cbind(OGR.prov[[z]],rep(mean(OGR.prov[[z]]), n= length(OGR.prov)))
        # a <- as.data.frame(a)
        # names(a) <- c("Observed","Expected")
        # ww <- nb2listw(wr.prov, zero.policy = TRUE)
        # niter <- zz
        # 
        # moran.mboot<-boot(a, statistic=moranI.pboot, sim="parametric",
        #                   ran.gen=poisson.sim,  R=niter, listw=ww.prov,
        #                   n=length(wr.prov),
        #                   S0=Szero(ww.prov) )
        # print(moran.mboot)
        
      }) }) #Testo
  #################################################
  
  
  
  #################################################
  #Bootstrap classico
  
  boot.data <- reactive({
    
    
    if(input$Run19 == 0)
      return()
    isolate({
      
      
      
      OGR.prov.sub <- OGR.prov[]
      OGR.prov.sub@data$seq <- seq(1:length(OGR.prov.sub))
      xy.sub <- coordinates(OGR.prov.sub)
      
      ### Adicenze
      
      #Metodo semplice QUEEN
      
      
      if(input$method19 == "Queen"){
        wr.sub <- poly2nb(OGR.prov.sub, row.names = OGR.prov.sub$seq, queen = TRUE )
        wm.prov <- nb2mat(wr.sub, style = "B", zero.policy = TRUE)
        
      }
      
      
      
      if(input$method19 == "Nearest"){
        wr.sub <- knn2nb(knearneigh(xy.sub,k=input$k19, RANN=FALSE),row.names = OGR.prov.sub$seq)
        wm.prov <- nb2mat(wr.sub, style = "B", zero.policy = TRUE)
        wm.prov <- (1/2)*(wm.prov+t(wm.prov))
      }
      
      
      #Metodo distance based
      if (input$method19 == "Distance"){
        
        
        wr.sub <- dnearneigh(xy.sub, d1 = 0, d2 = input$distance19 * max(dsts.com),  
                             row.names = OGR.prov.sub@data$seq)
        dsts.sub <- unlist(nbdists(wr.sub,xy.sub))
        wm.prov <- nb2mat(wr.sub, style = "B", zero.policy = TRUE)
        wm.prov <- (1/2)*(wm.prov+t(wm.prov))
      }
      
      
      
      z <- input$red19
      zz <- input$n.sim19
      
      
      M.boot <- Moran.boot(OGR.prov, which(etichette == z), wm.prov, zz, alpha = input$sign)
    
      
      return(M.boot)
      
  }) }) #Reactive content per creare lista con dati sulla simulazione
  
  
  output$bootstr <- renderPlotly({
    
    if(input$Run19 == 0)
      return()
    isolate({

      a <- boot.data()
      
      print.boot(a,plot=TRUE)
      
    
  }) }) #Plot
  
  output$bootstr.text <- renderPrint({
    
    
    if(input$Run19 == 0)
      return()
    isolate({
      
      
      b <- boot.data()
      
      print.boot(b)
    
  }) }) #Testo
  #################################################
  
  
  #################################################
  # Bootstrap a blocchi
  
  block.data <- reactive({
    
    
    if(input$Run20 == 0)
      return()
    isolate({
      
      
      
      OGR.prov.sub <- OGR.prov[]
      OGR.prov.sub@data$seq <- seq(1:length(OGR.prov.sub))
      xy.sub <- coordinates(OGR.prov.sub)
      
      ### Adicenze
      
      #Metodo semplice QUEEN
      
      
      if(input$method20 == "Queen"){
        wr.sub <- poly2nb(OGR.prov.sub, row.names = OGR.prov.sub$seq, queen = TRUE )
        wm.prov <- nb2mat(wr.sub, style = "B", zero.policy = TRUE)
        
      }
      
      
      
      if(input$method20 == "Nearest"){
        wr.sub <- knn2nb(knearneigh(xy.sub,k=input$k20, RANN=FALSE),row.names = OGR.prov.sub$seq)
        wm.prov <- nb2mat(wr.sub, style = "B", zero.policy = TRUE)
        wm.prov <- (1/2)*(wm.prov+t(wm.prov))
      }
      
      
      #Metodo distance based
      if (input$method20 == "Distance"){
        
        
        wr.sub <- dnearneigh(xy.sub, d1 = 0, d2 = input$distance20 * max(dsts.com),  
                             row.names = OGR.prov.sub@data$seq)
        dsts.sub <- unlist(nbdists(wr.sub,xy.sub))
        wm.prov <- nb2mat(wr.sub, style = "B", zero.policy = TRUE)
        wm.prov <- (1/2)*(wm.prov+t(wm.prov))
      }
      
      
      
      z <- input$red20
      zz <- input$n.sim20
      
      
      M.boot <- block.boot(which(etichette == z), wm.prov, zz, alpha = input$sign20)
      
      return(M.boot)
    
    
    
    
    
  }) }) #Reactive content per creare lista con dati sulla simulazione
  
  output$blockplot <- renderPlotly({
    
    
    if(input$Run20 == 0)
      return()
    isolate({

      a <- block.data()
      
      block.print(a,plot = TRUE)

    }) }) #Plot
  
  output$blocktext <- renderPrint({
    
    
    if(input$Run20 == 0)
      return()
    isolate({
      
      b <- block.data()
      
      block.print(b)

      
    }) }) #Testo
  #################################################
  
  

  output$table <- DT::renderDataTable({ 
      
    
      a <- red.fin %>% mutate_if(is.numeric, ~round(.,3))
      
      datatable(a[,-4], filter = "top", options = list(scrollX = TRUE, target = "column") ) 
      
      
      }) #Render della tabella con tutti i dati
    
  output$scatter <- renderPlotly({
    
    
    a <- red.fin %>% mutate_if(is.numeric, ~round(.,3))
    
    s1 = input$table_rows_current
    s2 = input$table_rows_all
 
    
    xx <- c("Denominazione.Comune",input$red7,input$red8)
    aa <- as.data.frame(a[s2,xx])
    
    
   # par(mar = c(4, 4, 1, .1))
    
    b <- ggplot(aa, aes_string(x = aa[,2] ,y=aa[,3], comune = aa[,1] )) +
    geom_point()+labs(x=input$red7,y=input$red8) 
    
    ggplotly(b, tooltip = c("x","y","comune"))
   
    
  })  #Render scatterplot
  
  output$barplot <- renderPlotly({
    
    
    a <- red.fin %>% mutate_if(is.numeric, ~round(.,3))
    
    s1 = input$table_rows_current
    s2 = input$table_rows_all
    
    ##
    ##
    
    x <- c("Denominazione.Comune",input$red8,"macro")
    a <- as.data.frame(a[s2,x])
    
    
    
    #par(mar = c(4, 4, 1, .1))
    
    
    c<- ggplot(a, aes_string(x=a[,input$red8], fill = "macro" )) + 
      geom_histogram(binwidth = 190)+ ylab("count") +
      xlab(input$red8) + xlim(c(min(a[,2], na.rm = TRUE),quantile(a[,2],na.rm=TRUE)[4]))
    
    ggplotly(c, tooltip = c("x","count","fill"))
    
    
   
    #ggplotly(c, tooltip = c(x,"macro"))
    
    
  })  #Render barplot

  output$corr <- renderPlot({
    
    if(input$Run10 == 0)
      return()
    
    isolate({
      
      yy <- red.fin[etichette]
      
      yy$macro<- red.fin$macro
      
      
      x <- c(input$red10, input$red11, input$red12, input$red13,"macro")
      xx <- yy[x]
      
      y <- as.data.frame(xx)
      
      
      d<- ggpairs(y,columns= 1:(ncol(y)-1),
                  ggplot2::aes(colour=macro, alpha=.3, width=.2 )) 
      d 
      
      
    }) 
    }) #Render correlogramma
  
  output$violinplot <- renderPlotly({
      
      
    if(input$Run3 == 0)
      return()

    isolate({
 
    
      xx <- input$red3
      mean <- mean(red.fin[[xx]], na.rm = TRUE)
      sd <- sd(red.fin[[xx]], na.rm = TRUE)

      yy <- red.fin[etichette]
      
      library(data.table)
      outlierReplace = function(dataframe, cols, rows, newValue = NA) {
        if (any(rows)) {
          set(dataframe, rows, cols, newValue)
        }
      }
      
      outlierReplace(yy, xx, which(yy[[xx]] > max(yy[[xx]], na.rm = TRUE)-3000))
      

      yy$macro <- red.fin$macro
      yy <- yy[-1,]
      
      #yy[sapply(yy, is.null)] <- NA
      
      y <- cbind(yy$macro,yy[xx])
      names(y)[1] <- "macro"
      
      p <- ggplot(y, aes(y=y[[xx]], x=y$macro, fill = y$macro)) + 
        geom_violin() + 
        theme(legend.title=element_text(face="bold")) + ylab(xx) +
        xlab("Aree geografiche") + theme(legend.text = element_text( face = "bold")) +
        scale_fill_brewer(palette=input$palette3)
      
      p 

   # p <- ggplot(red.fin, aes(y=red.fin[[xx]], x=red.fin$macro, fill=red.fin$macro)) + 
   #      geom_violin(trim = TRUE) + 
   #      theme(legend.title=element_text(face="bold")) + xlab("Aree geografiche") +
   #      labs(fill="Aree geografiche") + theme(legend.text = element_text( face = "bold")) +
   #      scale_fill_brewer(palette=input$palette3)
    
   p #+ scale_y_continuous(name = xx, limits = c(mean-3*sd,mean+3*sd))
   
     
      })
      
      
    }) #Render violinplot
    
  output$cirplot <- renderPlot({
    
    
    if(input$Run2 == 0)
      return()
    isolate({
      
      data <- mean.reg[,c(2,24,3:9)]
      
      et <- input$red2
      
      # Matrice che raccoglie i dati in base al reddito scelto dall'utente
      dat. <- data.frame(
        individual = data$Regione,
        group = data$macro,
        value = data[et]
      )
      
      names(dat.) <- c("individual","group","value")
      
      dat. = dat. %>% arrange(group,value)
      
      
      
      # empty bar e' una barra vuota che aggiunge spazio tra ogni gruppo
      #Creo dataframe vuoto con stessi nomi colonne di .dat
      empty_bar <- 7
      to_add <- data.frame( matrix(NA, empty_bar*nlevels(dat.$group), ncol(dat.)) )
      colnames(to_add) <- colnames(dat.)
      #Assegno valori a colonna
      to_add$group <- rep(levels(dat.$group), each=empty_bar)
      
      dat. <- rbind(dat., to_add)
      dat. <- dat. %>% arrange(group)
      
      dat.$id <- seq(1, nrow(dat.)) # Aggiungo altra colonna a .dat
      
      # Ottieni nome e la posizione y di ogni etichetta
      label_data <- dat.
      number_of_bar <- nrow(label_data)
      angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
      label_data$hjust <- ifelse(angle < -90, 1, 0)
      label_data$angle <- ifelse(angle < -90, angle+180, angle)
      
      # prepare a data frame for base lines
      base_data <- dat. %>% 
        group_by(group) %>% 
        summarize(start=min(id), end=max(id) - empty_bar) %>% 
        rowwise() %>% 
        mutate(title=mean(c(start, end)))
      
      # prepare a data frame for grid (scales)
      grid_data <- base_data
      grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
      grid_data$start <- grid_data$start -1
      grid_data <- grid_data[-1,]
      
      
      mea <- mean(dat.$value, na.rm = TRUE)
      sd <- sd(dat.$value, na.rm = TRUE)
      
      lev <- c(mea-3*sd, mea-2.3*sd, mea-1.5*sd, mea+0.5*sd)
      
      
      # Make the plot
      p <- ggplot(dat., aes(x=as.factor(id), y=value, fill=group )) +  # Note that id is a factor. If x is numeric, there is some space between the first bar
        geom_bar(aes(x = as.factor(id), y = value, fill = group), stat = "identity", alpha = 0.7) +
        
        geom_segment(data=grid_data, aes(x = end, y = lev[4], xend = start, yend = lev[4]), colour = "black", alpha=1, size=0.6 , inherit.aes = FALSE ) +
        geom_segment(data=grid_data, aes(x = end, y = lev[3], xend = start, yend = lev[3]), colour = "black", alpha=1, size=0.6 , inherit.aes = FALSE ) +
        geom_segment(data=grid_data, aes(x = end, y = lev[2], xend = start, yend = lev[2]), colour = "black", alpha=1, size=0.6 , inherit.aes = FALSE ) +
        geom_segment(data=grid_data, aes(x = end, y = lev[1], xend = start, yend = lev[1]), colour = "black", alpha=1, size=0.6 , inherit.aes = FALSE ) +
        
        annotate("text", x = rep(max(dat.$id),4), y = signif(lev,digits=6), label = as.character(signif(lev),digits=6) , color="black", size=4 , angle=0, fontface="bold", hjust=1) +
        #geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
        
        #ylim(-0.3*lev[1],lev[4]) +
        
        theme_minimal() +
        theme(
          legend.position = "none",
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(rep(-1.5,4), "cm") 
        ) +
        coord_polar() + 
        geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), 
                  color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE ) + 
        
        geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=1.3 , inherit.aes = FALSE )  
      #geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,0,0), colour = "black", alpha=0.8, size=1.9, fontface="bold", inherit.aes = FALSE)
      
      p
      
    })
    
    
    
  }) #Render circle-plot
  

    
}
      


shinyApp(ui, server)