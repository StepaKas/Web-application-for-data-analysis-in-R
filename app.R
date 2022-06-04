.libPaths("C:\\ProgramData\\Microsoft\\Windows\\Start Menu\\Programs\\RStudio\\Knihovna")  # to include directory with packages
#included packages

if (!require("rsconnect"))
  install.packages("rsconnect")
library(rsconnect)

if (!require("shiny"))
  install.packages("shiny")
library(shiny)

if (!require("shinythemes"))
  install.packages("shinythemes")
library(shinythemes)

if (!require("stats"))
  install.packages("stats")
library(stats)

if (!require("dplyr"))
  install.packages("dplyr")
library(dplyr)

if (!require("ggplot2"))
  install.packages("ggplot2")
library(ggplot2)

if (!require("ggfortify"))
  install.packages("ggfortify")
library(ggfortify)

if (!require("igraph"))
  install.packages("igraph")
library(igraph)

if (!require("rgexf"))
  install.packages("rgexf")
library(rgexf)

if (!require("plyr"))
  install.packages("plyr")
library(plyr)

if (!require("tidyverse"))
  install.packages("tidyverse")
library(tidyverse)

if (!require("DT"))
  install.packages("DT")
library(DT)

if (!require("gtools")) 
  install.packages("gtools")
library(gtools)


# website's user interface
ui <- fluidPage(theme = shinytheme("superhero"),
                
                shinythemes::themeSelector(),
                navbarPage(

                  "Network data analysis aplication",

                  tabPanel("Data load", icon =icon("table"),
                      sidebarPanel(
                      fileInput("file", "Upload file"),
                      #input data button color and progress bar color
                      tags$style("
                             .btn-file {  
                             background-color:red; 
                             border-color: red; 
                             }
                
                             .progress-bar {
                             background-color: green;
                             }"
                      ),
                 
                      h4("Does your dataset have header?"),
                      checkboxInput("header", label = "Header"),
                      h5("If the checkbox is checked then the first row od dataset is converted into names of columns"),
                      ),
    
                      wellPanel(
                         dataTableOutput("input_file")
                      ),
                     
                      ),
         
                  tabPanel("Basic information", icon = icon("info"),
                           plotOutput("plot_datainput"),
                           
                           sliderInput(inputId = "decimal_rounding",
                                       label = "Choose number of digits you want to round values below:",
                                       min = 1,
                                       max = 10,
                                       value = 2),
                           
                           verbatimTextOutput("number_of_vertices"),
                           actionButton("show_distance", "Calculate mean distance & diameter"),
                           verbatimTextOutput("plot_avg_distance"),
                           actionButton("show_closeness", "Calculate closeness centrality"),
                           verbatimTextOutput("closeness"),
                           actionButton("show_betweenness", "Calculate betweenness centrality"),
                           verbatimTextOutput("betweenness"),

                           hr(),
                           downloadButton(outputId = "store_degree_dist_to_csv",label = "Save degree distribution to .csv"),
                           downloadButton(outputId = "store_modularity_gml",label = "Save dataset to .gml"),
                           downloadButton(outputId = "store_modularity_gexf",label = "Save dataset to .gexf"),
                           ),
                  
                 tabPanel("Components",icon = icon("atom"),
                          verbatimTextOutput("is_connected"),
                          plotOutput("component_plot", width = "100%"),
                          sliderInput(inputId = "component_graph_bins",
                                      label = "Number of bins:",
                                      min = 10,
                                      max = 200,
                                      value = 30),
                          plotOutput("component_distribution", width = "100%"),
                          radioButtons(inputId = "formatDownPlotComponents", 
                                       label = "Export format", choices = list("pdf", "png")),
                          downloadButton(outputId = "store_largest_component_distribution",label = "Save"),
                          hr(),
                          verbatimTextOutput("number_of_vertexes_of_the_largest_component"),
                          h2("Largest component"),
                          plotOutput("colored_component", width = "100%"),
                          downloadButton(outputId = "store_largest_component",label = "Save to .csv"),
                 ),
                 
                 tabPanel("Layout", icon = icon("bar-chart-o"),
                          radioButtons(inputId = "layout_alg", label = "Choose your layout algorithm", 
                                       choices = list("Random", "Fruchterman Reingold" , "Circle","Sphere", "DRL")),
                          plotOutput("plot_datainput2"),
                 ),
                 
                 tabPanel("Distribution", icon = icon("signal"),
                          radioButtons(inputId = "distribution_func", label = "Choose what you want to look into",
                                       choices = list("Degree", "Closeness", "Betweenness","PageRank", "Clustering coeficient")),
                          h4("Distribution"),
                          sliderInput(inputId = "graph_bins",
                                                   label = "Number of bins:",
                                                   min = 10,
                                                   max = 200,
                                                   value = 30),
                          plotOutput("degree_distribution",  width = "100%"),
                          radioButtons(inputId = "formatDownPlot", 
                          label = "Export format", choices = list("pdf", "png")),
                          downloadButton(outputId = "DownLoadPlot", label = "Save")
                          ),
                 
              tabPanel("Communities",icon = icon("list-alt"),
                       plotOutput("louvain_modularity"),
                       verbatimTextOutput("modularity_value"),

                       
                    
                          mainPanel(
                             sidebarPanel(
                             
                               radioButtons(inputId = "modularity_algorithm", 
                                            label = "Choose your algorith to determine communities", choices = list("Louvain", "Fast Greedy","Walktrap","Leiden","Edge betweenness","Spinglass"),
                             ),hr(),
                             checkboxInput(inputId = "color_modularity",label = "Do you want to make vertexes with default color?"),
                              
                             ),
                             sidebarPanel(
                               radioButtons(inputId = "show_labels", 
                                            label = "Vertex Labels", choices = list("Show", "Hide"),
                             )),
                             sidebarPanel(
                             radioButtons(inputId = "vertex_size", 
                                          label = "Vertex Size", choices = list("Degree", "10", "15", "20"),
                             ))),
                        
                             sidebarPanel(  
                               h4("Save communities to "),
                               downloadButton(outputId = "store_modularity_csv",label = ".csv"),

                             ),
                       
                       sliderInput(inputId = "com_size_dist_bins",
                                   label = "Number of bins:",
                                   min = 10,
                                   max = 200,
                                   value = 30),
                       h4("Following bar plot represents view of comunity sizes"),
                       plotOutput("com_size_dist"),
                       radioButtons(inputId = "formatDownPlotCommunities", 
                                    label = "Export format", choices = list("pdf", "png")),
                       downloadButton(outputId = "DownLoadPlotCommuSize", label = "Save")
                       ),

                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output, session) {
  
  output$is_connected <- renderText({
    result = "nic"
    if (is.null(graph_my())) {return ("No data")}
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    n = 2
    progress$set(message = "Calculating", value = 0)
    progress$inc(1/n, detail = paste("Components"))
    connectivity = components(graph_my())
    progress$inc(1/n, detail = paste("Connectivity"))
    if(is.connected(graph_my())){
      result =  "Graph is connected"
    }
    else {
      result =  "Graph is not connected"
    }
    result = paste(result, "\nNumber of components",connectivity$no , sep = " ")
    return(result)
  })


  
  output$component_plot <- renderPlot({
    if (is.null(graph_my())) return ()
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())

    progress$set(message = "Working", value = 0)
    
    g = graph_my()
    progress$inc(1/3, detail = paste("Calculating membership"))
    memebar = components(g)$membership
    progress$inc(1/3, detail = paste("Creating colored plot"))
    res = plot(g, vertex.label = "", vertex.color = memebar, layout=coords_my())
    progress$inc(1/3, detail = paste("Done"))
    return (res)
  })  
  
  graph_my <- reactive({
      if (is.null(input_dataset())) return ()

      g <- graph.data.frame(data(), directed = FALSE)
#      if(!is_simple(g))
 #       g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
      return (g)
  })

  observeEvent(input$show_closeness, {
    if (is.null(graph_my())) return ()
    
    g <- graph_my()
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    n = 2
    progress$set(message = "Centrality:", value = 0)
    progress$inc(1/n, detail = paste("Calculating closeness"))
    my_closeness = closeness(g)
    distance_my = ""

    distance_my =paste ("Min closeness:", round(min(my_closeness ),input$decimal_rounding))
    distance_my =paste (distance_my, "\nMean closeness:", round(mean(my_closeness ), input$decimal_rounding))
    distance_my =paste (distance_my, "\nMax closeness:", round(max(my_closeness ), input$decimal_rounding))
    progress$inc(1/n, detail = paste("Retrieving data"))
    output$closeness <- renderText(distance_my)
  })
  
  observeEvent(input$show_betweenness, {
    if (is.null(graph_my())) return ()
    
    g <- graph_my()
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    n = 2
    progress$set(message = "Centrality:", value = 0)
    progress$inc(1/n, detail = paste("Calculating betweenness"))
    my_betweenness = betweenness(g)
    distance_my = ""
    distance_my =paste ( "Min betweenness:", round(min(my_betweenness ),input$decimal_rounding))
    distance_my =paste (distance_my, "\nMean betweenness:", round(mean(my_betweenness ),input$decimal_rounding))
    distance_my =paste (distance_my, "\nMax betweenness:", round(max(my_betweenness ),input$decimal_rounding))
    progress$inc(1/n, detail = paste("Retrieving data"))
    output$betweenness <- renderText(distance_my)
  })
  

  
  
  observeEvent(input$show_distance, {
    if (is.null(graph_my())) return ()
    
    g <- graph_my()
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    n = 5
    progress$set(message = "Centrality:", value = 0)
    progress$inc(1/n, detail = paste("Calculating mean distance"))
    distance_my = paste ("Mean distance is", round(mean_distance(g),input$decimal_rounding))
    
    progress$inc(1/n, detail = paste("Calculating diameter"))
    diaMe = diameter(g)
    distance_my =paste ( distance_my ,"\nData set diameter is:", round((diaMe ),input$decimal_rounding))
    
    progress$inc(1/n, detail = paste("Retrieving data"))
    output$plot_avg_distance <- renderText(distance_my)
    progress$inc(1/n, detail = paste("Finished"))

  })

  coords_my <- reactive({
    g = (graph_my()) 
    coords = NULL
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Layout", value = 0)
    progress$inc(1/2, detail = paste("Calculating"))
    if (input$layout_alg == "Fruchterman Reingold")
    {
      coords = layout.fruchterman.reingold
    }
    else if (input$layout_alg == "Random"){
      coords = layout.random
    }
    else if (input$layout_alg == "Circle"){
      coords = layout.circle
    }
    else if (input$layout_alg == "Sphere"){
      coords = layout.sphere
    }
    else if (input$layout_alg == "DRL"){
      coords = layout.drl
    }
    progress$inc(1/2, detail = paste("Finished calculating"))
    return (coords)
  })
  # plot komunit
  
  community_cols <- reactive({
    df <- input_dataset()
    if (is.null(df)) return ()
    g <- graph_my()
    
    my_ver_col = "RED"
    

 
    ####################################

    ####################################
    if (input$color_modularity== TRUE){
      my_ver_col = "GREEN"
      return (my_ver_col)
    }
    
    else if (input$modularity_algorithm == "Louvain"){
      my_ver_col = (cluster_louvain(g))
    }
    
    else if (input$modularity_algorithm == "Fast Greedy"){
      my_ver_col = (cluster_fast_greedy(g))
    }
    else if (input$modularity_algorithm == "Walktrap"){
      my_ver_col = (cluster_walktrap(g))
    }
    else if (input$modularity_algorithm == "Edge betweenness"){
      my_ver_col = (cluster_edge_betweenness(g))
    }
    else if (input$modularity_algorithm == "Spinglass"){
      my_ver_col = (cluster_spinglass(g))
    }
    else if (input$modularity_algorithm == "Leiden"){
      my_ver_col = (cluster_leiden(g))
    }
    return ( my_ver_col)
  })
  mod_plot <- reactive({
    if (is.null(graph_my())) return()
    g = graph_my()
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Making plot", value = 0)
    
    my_ver_size = 0
    my_label = "nic"
    n = 5
    progress$inc(1/n, detail = paste("Labels"))
    if (input$show_labels == "Show"){
      my_label = V(g)
    }
    else if (input$show_labels == "Hide"){
      my_label = ""
    }
    ####################################
    progress$inc(1/n, detail = paste("Size"))
    if (input$vertex_size == "Degree"){
      my_ver_size = degree(g)
    }
    else if (input$vertex_size == "10"){
      my_ver_size = 10
      
    }
    else if (input$vertex_size == "15"){
      my_ver_size = 15
    }
    else if (input$vertex_size == "20"){
      my_ver_size = 20
    }
    if (input$color_modularity== TRUE){    
      progress$inc(2/n, detail = paste("Default color plot"))
      my_plot = plot(g, vertex.color="GREEN", layout=coords_my(), vertex.label = my_label, vertex.size = my_ver_size)
      
   
      return (my_plot)
    }
    progress$inc(1/n, detail = paste("Calculating memberiships"))
    mambah = membership(community_cols())
    progress$inc(1/n, detail = paste("Calculating memberiships"))
    my_plot = plot(g, vertex.color= mambah, layout=coords_my(), vertex.label = my_label, vertex.size = my_ver_size)
    progress$inc(1/n, detail = paste("Returning"))
    my_plot
    
  })
  
  output$modularity_value <- renderText({
    df <- input_dataset()
    if (is.null(df)) return()
    if (input$modularity_algorithm == "Leiden"){
      return ("Cannot calculate modularity for this algoritm")
    }
    if (input$color_modularity== TRUE){
      
      return ("No community algorithm is currently applied")
    }
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Calculating", value = 0)
    n = 2
    progress$inc(1/n, detail = paste("Modularity"))
    result = paste("Modularity of communities is", round(modularity(community_cols()),3))
    progress$inc(1/n, detail = paste("Done"))
    return (result)
  })
  
  commHist <- reactive({
    if (input$color_modularity== TRUE){ return ()}
    if (is.null(graph_my())){ return ()}
    g = graph_my()
    my_ver_col = community_cols()$membership
    com <- as.data.frame(cbind(V(g),my_ver_col))
    ggplot(data = com  , aes(x=my_ver_col) )  + geom_histogram(fill="green", alpha=0.5, position="identity", bins = 50) + scale_x_log10(name = "Comunity label") 
  })
  output$com_size_dist <- renderPlot  ({
    if (is.null(commHist())) return ()
    commHist()
  })
  output$louvain_modularity <- renderPlot({
    if (is.null(mod_plot())) return ()
    mod_plot()
  })
  
  
  output$store_degree_dist_to_csv <- downloadHandler(
    filename = paste (nameOfDataset() , "_degree_distribution.csv", sep = ""),
    {  },
    content =  function(file){
      g <- graph_my()

      write.csv2(degree(g), file = file)
    }
  )
  
  largestComponent <- reactive({
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "The largest component", value = 0)
    
    g <- graph_my()
    n = 5
    progress$inc(1/n, detail = paste("Calculating components"))
    co <- components(g, mode = "weak")
    progress$inc(1/n, detail = paste("Getting membership"))
    come = co$membership
    progress$inc(1/n, detail = paste("Component size"))
    cosize = co$csize
    progress$inc(1/n, detail = paste("Calculating largest component"))
    new_g<- induced.subgraph(g, which(come == which.max(co$csize)))
    
    return (new_g)
  })
  
  output$number_of_vertexes_of_the_largest_component <- renderText({
    if (is.null(graph_my())) {return ()}
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Calculating", value = 0)
    progress$inc(1/3, detail = paste("Vertexes"))
    result = paste("Number of vertexes in the largest component is", length(  V(largestComponent())))
    progress$inc(2/3, detail = paste("Edges"))
    result = paste (result ,"\nNumber of edges in the largest component is", length (E(largestComponent())))
    progress$inc(3/3, detail = paste("Returning Vertexes and Edges"))
    return (result)
  })
  
  
  output$store_largest_component <- downloadHandler(
    filename = paste (nameOfDataset() , "_largest_component.csv", sep = ""),
    {  },
    content =  function(file){

      
      new_g = largestComponent()
      values  = strtoi(c(get.edgelist(new_g)))
      a = values[0:(length(values)/2)]
      b =values[(length(values)/2)+1:(length(values)/2)]
      new_df = data.frame(a,b)
      write.table(new_df,  file = file, sep = ";", dec = ",", row.names = FALSE)
    }
  )
  output$store_modularity_csv <- downloadHandler(
    filename = paste (nameOfDataset() , "_community.csv" , sep = ""),
  
    content =  function(file){
        g <- graph_my()
        my_ver_col = "nic"
        if (input$color_modularity == TRUE){
          my_ver_col = "GREEN"
        }
        else if (input$modularity_algorithm == "Louvain"){
            my_ver_col = cluster_louvain(g)$membership
        }
        
        else if (input$modularity_algorithm == "Fast Greedy"){
          my_ver_col = cluster_fast_greedy(g)$membership
        }
        else if (input$modularity_algorithm == "Walktrap"){
          my_ver_col = cluster_walktrap(g)$membership
        }
        else if (input$modularity_algorithm == "Edge betweenness"){
          my_ver_col = cluster_edge_betweenness(g)$membership
        }
        else if (input$modularity_algorithm == "Spinglass"){
          my_ver_col = cluster_spinglass(g)$membership
        }
        else if (input$modularity_algorithm == "Leiden"){
          my_ver_col = cluster_leiden(g)$membership
        }
        # community <- my_ver_col
        # 
        # vertexi <- strtoi(as_ids(V(g)))
        # 
        # vcom <- data.frame(vertexi,community)

        # write.table((vcom),  file =  file, sep = ";", dec = ",", row.names = FALSE)

        com <- as.data.frame(cbind(V(g),my_ver_col))
        write.table(com,  file = file, sep = ";", dec = ",", row.names = FALSE)
    }
  )
  
  output$store_modularity_gml <- downloadHandler(
    
    filename = paste (nameOfDataset() ,"_data.gml", sep = ""),
    content = function(file) {
      g <- graph_my()
    #  print(g)

       write.graph(graph = g, file, format = "gml")
    }
  )
  
  output$store_modularity_gexf <- downloadHandler(
    filename = paste (nameOfDataset() , "_data.gexf" , sep = ""),
    content = function(file) {
      g <- graph_my()
      g2.gexf <- igraph.to.gexf(g)
      writeLines(g2.gexf$graph, con = file)
    }
  )
  
  #store_largest_component_distribution
  
  output$store_largest_component_distribution <- downloadHandler(
    filename = function(){
      paste(nameOfDataset() , "_distribution_component.", input$formatDownPlotComponents, sep="")
      
    },
    
    content =  function(file){
      if(input$formatDownPlotComponents == "png"){

        ggsave(file, device = "png")
      }
      else{

        ggsave(file, device = "pdf")
      }
      

      
      
      #df$WEIGHT<- NULL, pokud byste meli i sloupec s vahami hran
      # prevod na objekt "typu" igraph
      g <- graph_my()
      if (is.null(g)) return()

      my_plot = comp_plot()
      my_plot
      
    }
    
  )
  output$DownLoadPlotCommuSize <- downloadHandler(
    filename = function(){
      paste(nameOfDataset() , "_distribution.", input$formatDownPlotCommunities, sep="")
      
    },
    
    content =  function(file){
      if(input$formatDownPlotCommunities == "png"){
        #png(file)
        ggsave(file, device = "png")
      }
      else{
        #pdf(file)
        ggsave(file, device = "pdf")
      }
      
      df <- input_dataset()
      
      
      #df$WEIGHT<- NULL, pokud byste meli i sloupec s vahami hran
      # prevod na objekt "typu" igraph
      g <- graph_my()
      if (is.null(g)) return()
      commHist()
      
      
    }
    
  )
  
      
  output$DownLoadPlot <- downloadHandler(
    filename = function(){
      paste(nameOfDataset() , "_distribution.", input$formatDownPlot, sep="")

    },
    
    content =  function(file){
      if(input$formatDownPlot == "png"){
        #png(file)
        ggsave(file, device = "png")
      }
      else{
        #pdf(file)
        ggsave(file, device = "pdf")
      }
      
      df <- input_dataset()
      
      
      #df$WEIGHT<- NULL, pokud byste meli i sloupec s vahami hran
      # prevod na objekt "typu" igraph
      g <- graph_my()
      if (is.null(g)) return()
      bins <- seq( length.out = input$graph_bins )
      #"Degree", "Closeness", "Betweenness","Page Rank" Clustering coeficient
      Targer_var = 0
      X_axis_label = input$distribution_func
      if (input$distribution_func == "Degree"){
        Targer_var = degree(g)
      }
      else if (input$distribution_func == "Closeness"){
        Targer_var = closeness(g)
      }
      else if (input$distribution_func == "Betweenness"){
        Targer_var = betweenness(g)
      }
      else if (input$distribution_func == "PageRank"){
        Targer_var = page.rank(g)$vector
      }
      else if (input$distribution_func == "Clustering coeficient"){
        Targer_var = transitivity(g, type = "local")
      }
      #hist(degree(g), breaks = bins, main = paste("Histogram of input data set degree distribution"))
      
      data2 <- data.frame(Targer_var)

      my_plot <- ggplot(data = data2, aes(x=Targer_var)) +geom_histogram(fill="green", alpha=0.5, position="identity", bins = input$graph_bins) +labs(x = X_axis_label)
      my_plot


    }

    )

  output$colored_component <- renderPlot({
    if (is.null(graph_my())) {return ()}

    new_g<- largestComponent()
    plot(new_g, vertex.label = "", layout=coords_my())
  })
  
  comp_plot <-reactive({
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Making plot", value = 0)
    progress$inc(1/2, detail = paste("calculating components"))
    SizeOfComponent = components(graph_my())$csize
    progress$inc(2/2, detail = paste("creating distributionplot"))
    result = ggplot(data = data.frame(SizeOfComponent), aes(x=SizeOfComponent)) + geom_histogram(fill="green", position="identity", bins = input$component_graph_bins) + scale_x_log10() 
    return (result)
  })
  output$component_distribution <- renderPlot({
    if (is.null(graph_my()) || components(graph_my())$no == 1) {return ()}
    return (comp_plot())
  })
  
  nameOfDataset <- reactive({
    if (is.null(filename()$name)) return("")
    jmeno <- gsub(pattern = "\\.csv$", "", basename(filename()$name))

    return (jmeno)
  })
  
  filename <- reactive({
    file1 <- input[["file"]]
    if (is.null(file1)) {return ()}
    file1
  })
  
  input_dataset <- reactive({
    if(is.null(filename())){
      return()
    }
    f <- filename()$datapath
    df = read.csv2(f, header =input$header)

  })
  
  
  dist_plot <- reactive({
    df <- input_dataset()
    if (is.null(df)) return ()
    g <- graph_my()
    bins <- seq( length.out = input$graph_bins )
    #"Degree", "Closeness", "Betweenness","Page Rank"
    Targer_var = 0
    X_axis_label = input$distribution_func
    if (input$distribution_func == "Degree"){
      Targer_var = degree(g)
    }
    else if (input$distribution_func == "Closeness"){
      Targer_var = closeness(g)
    }
    else if (input$distribution_func == "Betweenness"){
      Targer_var = betweenness(g)
    }
    else if (input$distribution_func == "PageRank"){
      Targer_var = page.rank(g)$vector
    }
    else if (input$distribution_func == "Clustering coeficient"){
      Targer_var = transitivity(g, type = "local")
    }
    data2 <- data.frame(Targer_var)
    my_plot <- ggplot(data = data2, aes(x=Targer_var)) +geom_histogram(fill="green", alpha=0.5, position="identity", bins = input$graph_bins) +labs(x = X_axis_label) + scale_x_log10()
    my_plot
    })
  
  #shows basic informations about dataset, returns it as a text


  output$number_of_vertices <- renderText({
    df <- input_dataset()
    if (is.null(df)) return()
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Calculating", value = 0)
    n <- 9
    
    progress$inc(1/n, detail = paste("Calculating Max/Min degree"))
    
    
    g <- graph_my()
    max_deg = V(g)$name[degree(g)==max(degree(g))]
    min_deg = V(g)$name[degree(g)==min(degree(g))]
    max_string = ""
    separa = ""
    counter = 0
    for (num_neco in max_deg) {
     
     max_string = paste(max_string, num_neco, sep = separa)
     separa = ","
     counter =  counter + 1
     if (counter >= 10){
       break()
     }
     
    }
    separa = ""
    min_string = ""
    counter = 0
    for (num_neco in min_deg) {
     min_string = paste(min_string, num_neco, sep = separa)
     separa = ","
     counter =  counter + 1
     if (counter >= 10){
       break()
     }
    }
    simpleVar = ""
    progress$inc(1/n, detail = paste("Checking if graph is simple"))
    if (is.simple(g)){
      simpleVar = "\nInput dataset is simple"
    }
    else {
      simpleVar = "\nInput dataset is not simple"
    }
    progress$inc(1/n, detail = paste("Getting number of vertexes"))
    vertexNum = gorder(g)
    progress$inc(1/n, detail = paste("Getting number of edge"))
    edgeNum = gsize(g)
    progress$inc(1/n, detail = paste("Graph density"))
    edgeDens = edge_density(g, loops=FALSE)
    progress$inc(1/n, detail = paste("Getting page rank"))
    pgRank = page.rank(g)$vector

    progress$inc(1/n, detail = paste("Getting clustering coeficient"))
    prum_t<-transitivity(g, type = "local");
    prum_t <- replace(prum_t, is.na(prum_t), 0);
    prum_t <- mean(prum_t)
    progress$inc(1/n, detail = paste("Returning text"))
    paste( "Number of vertexes:", vertexNum, 
          "\nNumber of edges:", edgeNum, 
          "\nNetwork density:", round (edgeDens, input$decimal_rounding),
          "\nMean clustering coeficient:", round (prum_t, input$decimal_rounding),
          simpleVar,
          "\n",
          "\nMin degree:", min(degree(g)),
          "\nLabel of vertexes with min degree (max 10 vertexes displayed):", min_string,
          "\nMean degree:", round(mean(degree(g)),input$decimal_rounding), 
          "\nMax degree:", max(degree(g)), 
          "\nLabel of vertexes with max degree (max 10 vertexes displayed):", max_string,

          "\n",
          "\nMin page rank:", round(min(pgRank ),input$decimal_rounding),
          "\nMean page rank:", round(mean(pgRank ),input$decimal_rounding),
          "\nMax page rank:", round(max(pgRank ),input$decimal_rounding),

          
          

          sep = " " )
   })
  
  output$degree_distribution <- renderPlot({
    dist_plot()
  })

  just_my_plot <- reactive({
    if (is.null(input_dataset())) return ()
    df <- input_dataset()
    g <- graph_my()
    my_plot = plot(g, layout=coords_my(), vertex.label=NA, vertex.size=10)
    return(my_plot)
  })
  
  output$plot_datainput <- renderPlot({
    if (is.null(just_my_plot())) return ()

    just_my_plot()
  })
  
  output$plot_datainput2 <- renderPlot({
    df <- input_dataset()
    if (is.null(df)) return ()

    
    g <- graph_my()
    n = 3
    progress <- shiny::Progress$new()
    
    on.exit(progress$close())

    progress$set(message = "Working", value = 0)
    progress$inc(1/3, detail = paste("Calculating new layout"))
    c = coords_my()
    progress$inc(1/3, detail = paste("Creating plot"))
    my_plot = plot(g, layout=c, vertex.label=NA, vertex.size=10)
    progress$inc(1/3, detail = paste("Done"))
    return(my_plot)
  })
  

  data <-reactive({
    f <- filename()$datapath
    if (is.null(f)) {return ()}
    df = read.csv2(f, header =input$header)
    return (df)
  })
  
  #after loading csv to input an interactive table shows with search options
  output$input_file <- renderDataTable({
      datatable(data(), filter="top", selection="multiple", escape=FALSE, class = 'cell-border stripe',rownames = FALSE,
               
                options = list(sDom  = '<"top">lrt<"bottom">ip'),
      )
  })
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)