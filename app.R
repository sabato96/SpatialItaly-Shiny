#################################################################################
# Progetto Lab. DISES 2020            Studio spaziale sul dataset redditi_com   #
#                                                                               #
# Gargiulo Sabato & Manzo Maria                                                 #
#                                                                               #
#################################################################################



# Get the data

#source("source script.R")
load("C:/Users/gargi/Desktop/Progetto Lab/.RData")


# Librerie
#####
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

# Shiny App
########



  
ui <- dashboardPage(
  dashboardHeader(title = "Shiny Project"),
  
  dashboardSidebar( #Menù laterale con selezioni
    sidebarMenu(
      
      menuItem("Introduction", tabName = "intro", icon = icon("dashboard")),
      
      menuItem("Explorative", tabName = "explorative", icon = icon("dashboard"),
               menuSubItem("Data Table", tabName = "table", icon = icon("dashboard")), #7,8
               menuSubItem("Circle Plot", tabName = "cirplot", icon = icon("dashboard")), #2
               menuSubItem("Violin Plot", tabName = "viol", icon = icon("dashboard")) #3
               ),
      
      menuItem("Leaflet map", tabName = "leaflethome", icon = icon("dashboard"),
               menuSubItem("Leaflet global", tabName = "leafletglobal", icon = icon("dashboard")), #4
               menuSubItem("Leaflet local", tabName = "leafletlocal", icon = icon("dashboard")) #9
               ), 

      menuItem("Moran", tabName = "moran", icon = icon("th")), #5
      menuItem("Bootstrap", tabName = "bootstrap", icon = icon("cog", lib = "glyphicon")) #6
    )
  ),

  
  # Dashboard body dice a shiny cosa far vedere in base al menu scelto dall'utente
  dashboardBody(

    tabItems(
      
      
      tabItem(tabName = "table",
#####              
              sidebarLayout(
                #sidebar panel serve a inserire le scelte dell'utente
                sidebarPanel(
                  
                   
                   selectInput(inputId = "red7",
                               label = "Choose a variable to display",
                               choices = etichette,
                               selected = ""),
                   
                   selectInput(inputId = "red8",
                               label = "Choose a variable to display",
                               choices = etichette,
                               selected = "")

                ),
                
               
                mainPanel(
                  
                  DTOutput("table", width = "100%", height = 800),
                  plotOutput("scatter",height = 500)
                  
                  
                  
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
      
      tabItem(tabName = "leafletglobal",
          sidebarLayout(
#####           
            sidebarPanel(
              
    
              selectInput(inputId = "red4",
                          label = "Choose a variable to display",
                          choices = etichette,
                          selected = ""),
              
              
              numericInput(inputId = "g.col1",
                           label = ("Choose n colors"),
                           value = "5"),
              
              selectInput(inputId = "palette4",
                          label = "Choose color palette",
                          choices = list("Blues","BuGn",
                                         "Greens","Greys",
                                         "PuBuGn","Dark2"),
                          selected = "Greys"),
              
              actionButton("Run4", "Run code")
              
              
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
              
              leafletOutput("qtm", width = "100%", height = 800)
              
              
              
          ))),
#####  
    

      tabItem(tabName = "leafletlocal",
          sidebarLayout(
#####           
          sidebarPanel(
            
            selectInput(inputId = "shape1",
                        label = "Choose a macro to display",
                        choices = list("Nord",
                                       "Centro",
                                       "Sud"),
                        selected = "Nord"),
            
            selectInput(inputId = "red9",
                        label = "Choose a variable to display",
                        choices = etichette,
                        selected = ""),
            
            
            numericInput(inputId = "g.col2",
                         label = ("Choose n colors"),
                         value = "5"),
            
            selectInput(inputId = "palette9",
                        label = "Choose color palette",
                        choices = list("Blues","BuGn",
                                       "Greens","Greys",
                                       "PuBuGn","Dark2"),
                        selected = "Greys"),
            
            actionButton("Run9", "Run code")
            
            
          ),
          
          # Show a plot of the generated distribution
          mainPanel(
            
            leafletOutput("qtm.local", width = "100%", height = 800)
            
            
            
          ))),
#####  


    tabItem(tabName = "moran",
            sidebarLayout(
#####             
               sidebarPanel(
             
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
   
    
     tabItem(tabName = "bootstrap",
            sidebarLayout(
#####              
              sidebarPanel(
                
                selectInput(inputId = "red6",
                            label = "Choose a variable to display",
                            choices = etichette,
                            selected = ""),
              
              
              numericInput(inputId = "n.sim",
                           label = ("Choose n iteration"),
                           value = "20"),
              
              actionButton("Run6", "Run code")
                  
                  
                ),
              
              mainPanel(plotlyOutput("bootstrap", width = "100%", height = 600),
                        verbatimTextOutput("bootstrap.text"))
              
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
      
    }) })
  
  
  output$qtm.local <- renderLeaflet({
    
    if(input$Run9 == 0)
      return()
    
    isolate({
      
      x <- input$g.col2
      y <- input$red9
      
      if(input$shape1 == "Nord"){
        
        tm <- tm_shape(OGR.com.nord) + tm_fill(input$red9, palette = input$palette9, style = "quantile", 
                                               n = input$g.col2, contrast = c(0.28, 0.87),
                                               id = "COMUNE") + 
          
          tm_borders(alpha=.7) + tm_legend(legend.position = c("left", "bottom")) +
          tm_layout(title = paste(y,"medio per provincia"),
                    title.size = 1.1) +
          
          tm_shape(OGR.reg) + tm_borders(col = "black") 
      }
      
      if(input$shape1 == "Centro"){
        
        tm <- tm_shape(OGR.com.centro) + tm_fill(input$red9, palette = input$palette9, style = "quantile", 
                                                 n = input$g.col2, contrast = c(0.28, 0.87),
                                                 id = "COMUNE") + 
          
          tm_borders(alpha=.7) + tm_legend(legend.position = c("left", "bottom")) +
          tm_layout(title = paste(y,"medio per provincia"),
                    title.size = 1.1) +
          
          tm_shape(OGR.reg) + tm_borders(col = "black") 
      }
      
      if(input$shape1 == "Sud"){
        
        tm <- tm_shape(OGR.com.sud) + tm_fill(input$red9, palette = input$palette9, style = "quantile", 
                                              n = input$g.col2, contrast = c(0.28, 0.87),
                                              id = "COMUNE") + 
          
          tm_borders(alpha=.7) + tm_legend(legend.position = c("left", "bottom")) +
          tm_layout(title = paste(y,"medio per provincia"),
                    title.size = 1.1) +
          
          tm_shape(OGR.reg) + tm_borders(col = "black") 
      }
      
      
      
      tmap_mode("view")
      
      tmap_leaflet(tm) 
      
    }) })
  
  
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


  output$moran <- renderPlotly({
    
      if(input$Run5 == 0)
        return()
      isolate({
      
    
            
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
               geom_point() + geom_smooth(method = "lm", color = "black") +
               geom_hline(yintercept=mean(ams[,2]), linetype="dashed", color = "red") +
               geom_vline(xintercept = ybar, linetype="dashed", color = "red")
             
             #p <- plotly(p)
             ggplotly(p)
             
    
        }) })
    
  output$moran.text <- renderPrint({
      
      if(input$Run5 == 0)
        return()
      isolate({
      
      n <- length(OGR.prov)
      z <- input$red5
      y <- OGR.prov@data[[z]]
      ybar <- mean(y)
      
      Moran.I <- Moran.fun(OGR.prov, which(etichette == z), wm.prov)
      
      print.moran(Moran.I)

    }) })
    

 # output$vario <- renderPlot({
    
    # if(input$Run5 == 0)
    #   return()
    # isolate({
    #   
    # 
    #   
    #   z <- input$red5
    #   y <- OGR.prov@data[[z]]
    #   xx <- as.data.frame(cbind(xy.prov,y))
    #   names(xx) <- c("x","y",z)
    #   coordinates(xx) = ~x+y
    #   
    #   var <- variogram(log(y)~1, xx, cloud = TRUE)
    #   plot(var)
    #   
    #   
    # }) #})
  

  
  output$bootstrap <- renderPlotly({
      
      
      if(input$Run6 == 0)
        return()
      isolate({
        
 
             z <- input$red6
             zz <- input$n.sim
             
             
          
             M.boot <- Moran.boot(OGR.prov, which(etichette == z), wm.prov, zz)
             
             print.moran(M.boot, boot = TRUE, plot = TRUE)
            
             
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
    
    }) })

      
  output$bootstrap.text <- renderPrint({
      
      
      if(input$Run6 == 0)
        return()
      isolate({
        
        
        z <- input$red6
        zz <- input$n.sim
        
        
        
        M.boot <- Moran.boot(OGR.prov, which(etichette == z), wm.prov, zz)
        
        print.moran(M.boot, boot = TRUE)
        
        
        
        
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
        
      }) })
  
  
  
    
  
    
  output$table <- DT::renderDataTable({ 
      
    
      a <- red.fin %>% mutate_if(is.numeric, ~round(.,3))
      
      datatable(a[,-4], filter = "top", options = list(scrollX = TRUE, target = "column") ) 
      
      
      }) 
    
  output$scatter <- renderPlot({
    
    
    a <- red.fin %>% mutate_if(is.numeric, ~round(.,3))
    
    s1 = input$table_rows_current
    s2 = input$table_rows_all
 
    
    x <- c(input$red7,input$red8)
    a <- a[s2,x]
 
    
    par(mar = c(4, 4, 1, .1))
    
    plot(a, pch = 21)
   
    


    
  })  
  
  

  
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
        theme(legend.title=element_text(face="bold")) + xlab("Aree geografiche") +
        labs(fill="Aree geografiche") + theme(legend.text = element_text( face = "bold")) +
        scale_fill_brewer(palette=input$palette3)
      
      p 

   # p <- ggplot(red.fin, aes(y=red.fin[[xx]], x=red.fin$macro, fill=red.fin$macro)) + 
   #      geom_violin(trim = TRUE) + 
   #      theme(legend.title=element_text(face="bold")) + xlab("Aree geografiche") +
   #      labs(fill="Aree geografiche") + theme(legend.text = element_text( face = "bold")) +
   #      scale_fill_brewer(palette=input$palette3)
    
   p #+ scale_y_continuous(name = xx, limits = c(mean-3*sd,mean+3*sd))
   
     
      })
      
      
    })
    

  output$cirplot <- renderPlot({
    
    
    if(input$Run2 == 0)
      return()
    isolate({
    
    data <- mean.reg[,c(2,24,3:9)]
    
    et <- input$red2
    
    dat. <- data.frame(
      individual = data$Regione,
      group = data$macro,
      value = data[et]
    )
    
    names(dat.) <- c("individual","group","value")
    dat. = dat. %>% arrange(group,value)
    
    
    
    # empty bar è una barra vuota che aggiunge spazio tra ogni gruppo
    empty_bar <- 4
    to_add <- data.frame( matrix(NA, empty_bar*nlevels(dat.$group), ncol(dat.)) )
    colnames(to_add) <- colnames(dat.)
    to_add$group <- rep(levels(dat.$group), each=empty_bar)
    dat. <- rbind(dat., to_add)
    dat. <- dat. %>% arrange(group)
    dat.$id <- seq(1, nrow(dat.))
    
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
    grid_data$start <- grid_data$start - 1
    grid_data <- grid_data[-1,]
    
    
    mea <- mean(dat.$value, na.rm = TRUE)
    sd <- sd(dat.$value, na.rm = TRUE)
    
    lev <- c(mea-3*sd, mea-2.3*sd, mea-1.5*sd, mea+0.5*sd)
    
    
    # Make the plot
    p <- ggplot(dat., aes(x=id, y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
      geom_bar(aes(x = as.factor(id), y = value, fill = group), stat = "identity", alpha = 0.7) +
      
      geom_segment(data=grid_data, aes(x = end, y = lev[4], xend = start, yend = lev[4]), colour = "black", alpha=1, size=0.8 , inherit.aes = FALSE ) +
      geom_segment(data=grid_data, aes(x = end, y = lev[3], xend = start, yend = lev[3]), colour = "black", alpha=1, size=0.6 , inherit.aes = FALSE ) +
      geom_segment(data=grid_data, aes(x = end, y = lev[2], xend = start, yend = lev[2]), colour = "black", alpha=1, size=0.3 , inherit.aes = FALSE ) +
      geom_segment(data=grid_data, aes(x = end, y = lev[1], xend = start, yend = lev[1]), colour = "black", alpha=1, size=0.3 , inherit.aes = FALSE ) +
      
      annotate("text", x = rep(max(dat.$id),4), y = signif(lev,digits=6), label = as.character(signif(lev),digits=6) , color="grey", size=5 , angle=0, fontface="bold", hjust=1) +
      
      geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.6) +
      
      
      ylim(-1500,max(data[et])+2200) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,9), "cm") 
      ) +
      coord_polar() + 
      geom_text(data=label_data, aes(x=id, y=value+60, label=individual, hjust=hjust), 
                color="black", fontface="bold",alpha=0.9, size=5, angle= label_data$angle, inherit.aes = FALSE ) + 
      
      geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  
    
    p
    
    })
    
    
    
  })
  

    
}
      



shinyApp(ui, server)
