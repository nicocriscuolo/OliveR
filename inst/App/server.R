server <- function(input, output, session) {

# Roots for read all the directories of the computer with the ShinyDirChoose function
roots = getVolumes()

shinyDirChoose(input, id = "shape_folder", roots = roots, session = session) # first directory for shapefiles

# Shapefile Directory text
output$shape_folder_datapath <- renderText({

  parseDirPath(roots, input$shape_folder)

})





################################ WORLDCLOUD ####
output$wordcloud <- renderPlot({

  wordcloud(words = c("Data", "Analysis", "Statistic", "Multivariate", "OliveR", "Geographic", "PCA",
                      "ANOVA", "Cluster", "Shapefile", "Dendrogram", "Work", "Mantel test",
                      "Environmental Science", "Genetic", "SSR", "Biology", "Allele", "Boxplot", "Scatterplot",
                      "Shiny", "Plotly", "K-Means", "PAM", "Gap Statistic", "Biochemical", "Morphometric",
                      "Interactive", "User interface", "P-gen", "R", "Ecology", "Widget", "Input", "Output",
                      "Server", "Plot", "Coordinates", "Genome", "Panels", "Label", "Geoplot", "Silhouette",
                      "Post Hoc Test", "Heatmap", "WSS", "Polygon", "Genetic distance", "Similarity", "UTM",
                      "Sample", "Research", "Science", "Matrix", "Spatial", "Variability", "GIS", "Loadings",
                      "Components", "Summary", "GPS", "Waypoints", "3D", "Download", "Subset", "Merge", "RStudio"),
            freq = seq(from = 1, to = 10000, len = 67), scale = c(0.5, 3), min.freq = 1, max.words = 10000,
            random.order = TRUE, random.color = TRUE, rot.per = 0.30, colors = c("darkgreen", "darkred", "darkblue", "darkorange"))

})





################################ CURRENT DATE AND TIME ####
output$current_time <- renderText({

  invalidateLater(millis = 1000, session)
  paste("Current time at the University of Salerno:", as.POSIXlt(x = Sys.time(), tz = "UTC-1"))

})





################################ REACTIVE OBJECTS ####
#### DATASET - REAL DATA ####
Data <- reactive({

  req(input$Dataframe) # Per verificare che il file è disponibile

  File <- input$Dataframe

  df <- read.csv(File$datapath, header = TRUE)

  updateSelectInput(session, inputId = "Subset_Variable", choices = names(df), selected = NULL)

  updateSelectInput(session, inputId = "sample_name", choices = paste0(df[, 1], ".jpg"))

  updateSelectInput(session, inputId = "Variable_1", choices = names(df), selected = names(df)[5])
  updateSelectInput(session, inputId = "Variable_2", choices = names(df), selected = names(df)[6])
  updateSelectInput(session, inputId = "Variable_3", choices = c("-----", names(df)[-c(1, 2, 3, 4)]), selected = "-----")

  updateSelectInput(session, inputId = "PCs", choices = seq(1:ncol(df[-c(1,2,3,4)])), selected = 1) # scheda PCA
  updateSelectInput(session, inputId = "PCs_B", choices = seq(1:ncol(df[-c(1,2,3,4)])), selected = 1) # scheda PCA (PCS_B)

  updateSelectInput(session, inputId = "Variable_Y", choices = names(df)[-c(1, 2)], selected = names(df)[5])

  updateSelectInput(session, inputId = "PCs_2", choices = seq(1:ncol(df[-c(1,2,3,4)])), selected = 1) #scheda Cluster Analysis


  observe({

    if(input$Variable_1 == "UTM_Est" && input$Variable_2 == "UTM_Nord") {

      updateSelectInput(session, inputId = "Variable_3", selected = "-----")

    }

  })

  return(df)

})

#### DATASET TO MERGE ####
Data_M <- reactive({

  req(input$Dataframe_M) # Per verificare che il file è disponibile

  File_M <- input$Dataframe_M

  df_M <- read.csv(File_M$datapath, header = TRUE)

  return(df_M)

})

#### DATASET - GENETIC DATA ####
Data_G <- reactive({

  req(input$Dataframe_G)

  File_G <- input$Dataframe_G

  df_G <- read.csv(File_G$datapath, header = TRUE)

  return(df_G)

})

#### AMOUNT ####
Amount <- reactive({

    req(Data())

    Amount <- Data()[, input$Variable_3]

    return(Amount)

})

#### SHAPEFILES ####
Shapefile_1 <- reactive({

    Shapefile_1 <- readShapeSpatial(paste0(parseDirPath(roots,
                                                        input$shape_folder),
                                           "/",
                                           input$shapefile_name_1,
                                           ".shp"))

    return(Shapefile_1)

})

Shapefile_2 <- reactive({

  Shapefile_2 <- readShapeSpatial(paste0(parseDirPath(roots,
                                                      input$shape_folder),
                                         "/",
                                         input$shapefile_name_2,
                                         ".shp"))

  return(Shapefile_2)

})

Shapefile_3 <- reactive({

  Shapefile_3 <- readShapeSpatial(paste0(parseDirPath(roots,
                                                      input$shape_folder),
                                         "/",
                                         input$shapefile_name_3,
                                         ".shp"))

  return(Shapefile_3)

})

Shapefile_1_df <- reactive({

    Shapefile_1_df <- fortify(Shapefile_1())
    colnames(Shapefile_1_df)[1:2] = c("UTM_Est", "UTM_Nord")

    return(Shapefile_1_df)

})

Shapefile_2_df <- reactive({

  Shapefile_2_df <- fortify(Shapefile_2())
  colnames(Shapefile_2_df)[1:2] = c("UTM_Est", "UTM_Nord")

  return(Shapefile_2_df)

})

Shapefile_3_df <- reactive({

  Shapefile_3_df <- fortify(Shapefile_3())
  colnames(Shapefile_3_df)[1:2] = c("UTM_Est", "UTM_Nord")

  return(Shapefile_3_df)

})

Name_Centroids_Shapefile_1 <- reactive({

    Name_Shapefile_1 <- Shapefile_1()@data$NOME
    Centroids_Shapefile_1 <- as.data.frame(coordinates(Shapefile_1()))
    names(Centroids_Shapefile_1) = c("UTM_Est", "UTM_Nord")
    Name_Centroids_Shapefile_1 <- data.frame(id = Name_Shapefile_1, Centroids_Shapefile_1)
    rownames(Name_Centroids_Shapefile_1) = as.numeric(rownames(Name_Centroids_Shapefile_1)) + 1

    return(Name_Centroids_Shapefile_1)
})

Name_Centroids_Shapefile_2 <- reactive({

  Name_Shapefile_2 <- Shapefile_2()@data$NOME
  Centroids_Shapefile_2 <- as.data.frame(coordinates(Shapefile_2()))
  names(Centroids_Shapefile_2) = c("UTM_Est", "UTM_Nord")
  Name_Centroids_Shapefile_2 <- data.frame(id = Name_Shapefile_2, Centroids_Shapefile_2)
  rownames(Name_Centroids_Shapefile_2) = as.numeric(rownames(Name_Centroids_Shapefile_2)) + 1

  return(Name_Centroids_Shapefile_2)
})

Name_Centroids_Shapefile_3 <- reactive({

  Name_Shapefile_3 <- Shapefile_3()@data$NOME
  Centroids_Shapefile_3 <- as.data.frame(coordinates(Shapefile_3()))
  names(Centroids_Shapefile_3) = c("UTM_Est", "UTM_Nord")
  Name_Centroids_Shapefile_3 <- data.frame(id = Name_Shapefile_3, Centroids_Shapefile_3)
  rownames(Name_Centroids_Shapefile_3) = as.numeric(rownames(Name_Centroids_Shapefile_3)) + 1

  return(Name_Centroids_Shapefile_3)
})

#### PCA FOR PCA TABLE ####
PCA <- reactive({

  req(Data())

  if (input$cov_cor == "Correlation") {

    PCA <- princomp(Data()[, -c(1, 2, 3, 4)], cor = TRUE)

  } else if (input$cov_cor == "Covariance") {

    PCA <- princomp(Data()[, -c(1, 2, 3, 4)], cor = FALSE)

  }

  return(PCA)

})

#### PCA FOR CLUSTER ANALYSIS TABLE ####
PCA_2 <- reactive({

  req(Data())

  if (input$cov_cor_2 == "Correlation") {

    PCA <- princomp(Data()[, -c(1, 2, 3, 4)], cor = TRUE)

  } else if (input$cov_cor_2 == "Covariance") {

    PCA <- princomp(Data()[, -c(1, 2, 3, 4)], cor = FALSE)

  }

  return(PCA)

})
#### SCORES FOR PCA TABLE ####

#### SCORES FOR CLUSTER ANALYSIS TABLE ####
SCORES <- reactive({

  req(c(Data(), PCA_2()))

  scores <- data.frame(Data()[, c(1, 2, 3, 4)], PCA_2()$scores[, c(1:input$PCs_2)])

  return(scores)

})

#### NUMBER OF CLUSTERS ####
k <- reactive({

  req(Data())

  k <- input$Cluster_Count

  return(k)

})

#### SSR MATRIX DISTANCE ####
DIST <- reactive({

  req(Data_G())

  SSR <- data.frame(Data_G())
  rownames(SSR) = SSR$Sample_ID
  SSR <- SSR[, -1]
  SSR_AD <- df2genind(X = SSR[, -c(1, 2, 3)], ploidy = 2, sep = "/")
  SSR_AD_tab <- tab(SSR_AD, NA.method = input$na.method)

  if (input$distance == "binary") {

    Dist <- dist.binary(SSR_AD_tab, method = input$dist_binary)

  }

  else if (input$distance == "geometric") {

    Dist <- dist(SSR_AD_tab, method = input$dist_geometric)

  }

  return(Dist)

})






#### Commands for conditional Panel - Display Subset options
output$fileUploaded_Normal <- reactive({

  return(!is.null(Data()))

})
outputOptions(output,
              name = "fileUploaded_Normal",
              suspendWhenHidden = FALSE)





### Commands for conditional Panel - Display Merge button
output$fileUploaded_Merge <- reactive({

  return(!is.null(Data_M()))

})
outputOptions(output,
              name = "fileUploaded_Merge",
              suspendWhenHidden = FALSE)





################################ TABLE: DATASET NORMAL ####
#### DISPLAY DATA TABLE ####
output$filetable_A <- renderDataTable({

  Data()

  }, options = list(pageLength = 10)

)

#### DATA SUMMARY ####
output$summary_data <- renderPrint({

  summary(Data())

})

#### BASIC STATS ####
output$stats <- renderDataTable({

  Stats  <- as.data.frame(stat.desc(Data()[, -c(1, 2)]))

  Stats_Data <- data.frame(Stats = rownames(Stats), round(Stats, 3))

  Stats_Data

  }, options = list(pageLength = 25)

)

#### SHOW IMAGES ####
output$image_1 <- renderImage({

  shinyDirChoose(input, id = "image_1_folder", roots = roots, session = session)

  return(list(
    src = paste0(parseDirPath(roots, input$image_1_folder), "/", input$sample_name),
    width = 350,
    height = 450,
    alt = ""
  ))
}, deleteFile = FALSE)

output$image_2 <- renderImage({

  shinyDirChoose(input, id = "image_2_folder", roots = roots, session = session)

  return(list(
    src = paste0(parseDirPath(roots, input$image_2_folder), "/", input$sample_name),
    width = 350,
    height = 450,
    alt = ""
  ))
}, deleteFile = FALSE)

output$image_3 <- renderImage({

  shinyDirChoose(input, id = "image_3_folder", roots = roots, session = session)

  return(list(
    src = paste0(parseDirPath(roots, input$image_3_folder), "/", input$sample_name),
    width = 350,
    height = 450,
    alt = ""
  ))
}, deleteFile = FALSE)

#### SUBSET AND MERGE ####
output$download <- downloadHandler(

  filename = function() {
    paste0("Subset_", input$Dataframe)
  },

  content = function(file) {
    write.csv(subset(Data(), select = c(input$Subset_Variable)), file, row.names = FALSE, quote = FALSE)
  }
)

output$download_M <- downloadHandler(

  filename = function() {
    paste0("Merge_", substr(input$Dataframe, start = 1, stop = nchar(input$Dataframe)-4), "_", input$Dataframe_M)
  },

  content = function(file) {
    write.csv(merge(Data(), Data_M()), file, row.names = FALSE, quote = FALSE)

  }
)






################################ TABLE: PLOT ####
#### PLOT ####
output$plot <- renderPlotly({

  x = Data()[, input$Variable_1]
  y = Data()[, input$Variable_2]


  if (input$Variable_1 == "Label" && input$Variable_2 != "Sample_ID") {

    Boxplot <- ggplot(Data(), aes(x, y)) +
      geom_boxplot(aes(fill = Label)) +
      geom_jitter(alpha = 0.2, width = 0.3) +
          labs(x = input$Variable_1, y = input$Variable_2)

    ggplotly(Boxplot)

  }

  else if (input$Variable_1 != "Sample_ID" && input$Variable_2 == "Label") {

    Boxplot <- ggplot(Data(), aes(y, x)) +
      geom_boxplot(aes(fill = Label)) +
      geom_jitter(alpha = 0.2, width = 0.3) +
          labs(x = input$Variable_2, y = input$Variable_1) +
            coord_flip()

    ggplotly(Boxplot)

  }

  else if (input$Variable_1 == "Label" && input$Variable_2 == "Sample_ID") {

    Barplot <- ggplot(Data(), aes(x)) +
      geom_bar(aes(fill = Label), colour = "black", width = 0.7) +
        labs(x = input$Variable_1, y = "Number of units")

    ggplotly(Barplot)

  }

  else if (input$Variable_1 == "Sample_ID" && input$Variable_2 == "Label") {

    Barplot <- ggplot(Data(), aes(y)) +
      geom_bar(aes(fill = Label), colour = "black", width = 2) +
      labs(x = input$Variable_2, y = "Number of units") +
        coord_flip()

    ggplotly(Barplot)

  }

  else if (input$Variable_1 == "Sample_ID" && input$Variable_2 != "Label") {

    Colplot <- ggplot(Data(), aes(x, y)) +
      geom_col(aes(name = Sample_ID, fill = Label)) +
      labs(x = input$Variable_1, y = input$Variable_2) +
      theme(
        axis.text.x = element_text(size = 5, angle = 90, hjust = 1)
      )

    ggplotly(Colplot)

  }

  else if (input$Variable_1 != "Label" && input$Variable_2 == "Sample_ID") {

    Colplot <- ggplot(Data(), aes(y, x)) +
      geom_col(aes(name = Sample_ID, fill = Label)) +
      labs(x = input$Variable_2, y = input$Variable_1) +
      theme(
        axis.text.y = element_text(size = 5)
      ) + coord_flip()

    ggplotly(Colplot)

  }

  else if (input$Variable_3 == "-----") {

    Scatterplot <- ggplot(Data(),aes(x, y)) +
      geom_point(aes(name = Sample_ID, colour = Label)) +
        labs(x = input$Variable_1, y = input$Variable_2)

    ggplotly(Scatterplot)

  }

  else if (input$Variable_3 != "-----") {

    z = Data()[, input$Variable_3]

    plot_ly(data = Data(), x = ~x, y = ~y, z = ~z,
            color = ~Label, marker = list(size = 6),
            text = ~Sample_ID, showlegend = TRUE) %>%
      layout(legend = list(font = list(size = 12)),
        scene = list(
          xaxis = list(title = input$Variable_1),
          yaxis = list(title = input$Variable_2),
          zaxis = list(title = input$Variable_3)
        )
      )

  }

})

#### PEARSON'S COEFFICIENT ####
output$pearson <- renderPrint({

  COR <- round(cor(Data()[, input$Variable_1], Data()[, input$Variable_2]), 3)

  COR.TEST <- cor.test(Data()[, input$Variable_1], Data()[, input$Variable_2])

  COR

})

#### GEOPLOT ####
output$geoplot_1 <- renderPlotly({

  if (input$shapefiles == 1 && input$Variable_3 == "-----") {

    Geoplot_1 <- ggplot(data = Data(), aes(UTM_Est, UTM_Nord))+
      geom_polygon(aes(group = group), Shapefile_1_df(),
                   fill = input$Colours_1, colour = "grey50") +
      geom_point(aes(name = Sample_ID, colour = Label)) +
        labs(x = "UTM Est [m]", y = "UTM Nord [m]")

    if (input$polygon_name_1 == FALSE) {

      ggplotly(Geoplot_1)

    } else {

      Geoplot_1_1 = Geoplot_1 + geom_text(data = Name_Centroids_Shapefile_1(),
                                          aes(label = id), size = 2.5)

      ggplotly(Geoplot_1_1)

    }

  }

  else if (input$shapefiles == 1 && input$Variable_3 != "-----") {

    Geoplot_1 <- ggplot(data = Data(), aes(UTM_Est, UTM_Nord)) +
      geom_polygon(aes(group = group), Shapefile_1_df(), fill = input$Colours_1, colour = "grey50") +
      geom_point(aes(name = Sample_ID, colour = Label, size = Amount())) +
        scale_size(range = c(0.5, 3)) +
          labs(x = "UTM Est [m]", y = "UTM Nord [m]") +
            guides(colour = guide_legend(title = "Label"), size = guide_legend(title = NULL))

    if (input$polygon_name_1 == FALSE) {

      ggplotly(Geoplot_1)

    } else {

      Geoplot_1_1 = Geoplot_1 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5)

      ggplotly(Geoplot_1_1)

    }

  }

  else if (input$shapefiles == 2 && input$Variable_3 == "-----") {

    Geoplot_2 <- ggplot(data = Data(), aes(UTM_Est, UTM_Nord))+
      geom_polygon(aes(group = group), Shapefile_1_df(), fill = input$Colours_1, colour = "grey50") +
      geom_polygon(aes(group = group), Shapefile_2_df(), fill = input$Colours_2, colour = "grey50") +
      geom_point(aes(name = Sample_ID, colour = Label)) +
          labs(x = "UTM Est [m]", y = "UTM Nord [m]")

    if (input$polygon_name_1 == FALSE && input$polygon_name_2 == FALSE) {

      ggplotly(Geoplot_2)

    } else if (input$polygon_name_1 == TRUE && input$polygon_name_2 == FALSE) {

      Geoplot_2_1 <- Geoplot_2 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5)

      ggplotly(Geoplot_2_1)

    } else if (input$polygon_name_1 == FALSE && input$polygon_name_2 == TRUE) {

      Geoplot_2_1 <- Geoplot_2 + geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5)

      ggplotly(Geoplot_2_1)

    } else if (input$polygon_name_1 == TRUE && input$polygon_name_2 == TRUE) {

      Geoplot_2_1 <- Geoplot_2 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5) +
                                geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5)

      ggplotly(Geoplot_2_1)

    }

  }

  else if (input$shapefiles == 2 && input$Variable_3 != "-----") {

    Geoplot_2 <- ggplot(data = Data(), aes(UTM_Est, UTM_Nord))+
      geom_polygon(aes(group = group), Shapefile_1_df(), fill = input$Colours_1, colour = "grey50") +
      geom_polygon(aes(group = group), Shapefile_2_df(), fill = input$Colours_2, colour = "grey50") +
      geom_point(aes(name = Sample_ID, colour = Label, size = Amount())) +
        scale_size(range = c(0.5, 3)) +
          labs(x = "UTM Est [m]", y = "UTM Nord [m]") +
            guides(colour = guide_legend(title = "Label"), size = guide_legend(title = NULL))

    if (input$polygon_name_1 == FALSE && input$polygon_name_2 == FALSE) {

      ggplotly(Geoplot_2)

    } else if (input$polygon_name_1 == TRUE && input$polygon_name_2 == FALSE) {

      Geoplot_2_1 <- Geoplot_2 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5)

      ggplotly(Geoplot_2_1)

    } else if (input$polygon_name_1 == FALSE && input$polygon_name_2 == TRUE) {

      Geoplot_2_1 <- Geoplot_2 + geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5)

      ggplotly(Geoplot_2_1)

    } else if (input$polygon_name_1 == TRUE && input$polygon_name_2 == TRUE) {

      Geoplot_2_1 <- Geoplot_2 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5) +
        geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5)

      ggplotly(Geoplot_2_1)

    }

  }

  else if (input$shapefiles == 3 && input$Variable_3 == "-----") {

    Geoplot_3 <- ggplot(data = Data(), aes(UTM_Est, UTM_Nord))+
      geom_polygon(aes(group = group), Shapefile_1_df(), fill = input$Colours_1, colour = "grey50") +
      geom_polygon(aes(group = group), Shapefile_2_df(), fill = input$Colours_2, colour = "grey50") +
      geom_polygon(aes(group = group), Shapefile_3_df(), fill = input$Colours_3, colour = "grey50") +
      geom_point(aes(name = Sample_ID, colour = Label)) +
          labs(x = "UTM Est [m]", y = "UTM Nord [m]")

    if (input$polygon_name_1 == FALSE && input$polygon_name_2 == FALSE && input$polygon_name_3 == FALSE) {

      ggplotly(Geoplot_3)

    } else if (input$polygon_name_1 == TRUE && input$polygon_name_2 == FALSE && input$polygon_name_3 == FALSE) {

      Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5)

      ggplotly(Geoplot_3_1)

    } else if (input$polygon_name_1 == FALSE && input$polygon_name_2 == TRUE && input$polygon_name_3 == FALSE) {

      Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5)

      ggplotly(Geoplot_3_1)

    } else if (input$polygon_name_1 == FALSE && input$polygon_name_2 == FALSE && input$polygon_name_3 == TRUE) {

      Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_3(), aes(label = id), size = 2.5)

      ggplotly(Geoplot_3_1)

    } else if (input$polygon_name_1 == TRUE && input$polygon_name_2 == TRUE && input$polygon_name_3 == FALSE) {

      Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5) +
                                geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5)

      ggplotly(Geoplot_3_1)

    } else if (input$polygon_name_1 == TRUE && input$polygon_name_2 == FALSE && input$polygon_name_3 == TRUE) {

      Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5) +
                                geom_text(data = Name_Centroids_Shapefile_3(), aes(label = id), size = 2.5)

      ggplotly(Geoplot_3_1)

    } else if (input$polygon_name_1 == FALSE && input$polygon_name_2 == TRUE && input$polygon_name_3 == TRUE) {

      Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5) +
                                geom_text(data = Name_Centroids_Shapefile_3(), aes(label = id), size = 2.5)

      ggplotly(Geoplot_3_1)

    } else if (input$polygon_name_1 == TRUE && input$polygon_name_2 == TRUE && input$polygon_name_3 == TRUE) {

      Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5) +
                                geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5) +
                              geom_text(data = Name_Centroids_Shapefile_3(), aes(label = id), size = 2.5)

      ggplotly(Geoplot_3_1)

    }

  }

  else if (input$shapefiles == 3 && input$Variable_3 != "-----") {

    Geoplot_3 <- ggplot(data = Data(), aes(UTM_Est, UTM_Nord))+
      geom_polygon(aes(group = group), Shapefile_1_df(), fill = input$Colours_1, colour = "grey50") +
      geom_polygon(aes(group = group), Shapefile_2_df(), fill = input$Colours_2, colour = "grey50") +
      geom_polygon(aes(group = group), Shapefile_3_df(), fill = input$Colours_3, colour = "grey50") +
      geom_point(aes(name = Sample_ID, colour = Label, size = Amount())) +
        scale_size(range = c(0.5, 3)) +
          labs(x = "UTM Est [m]", y = "UTM Nord [m]") +
            guides(colour = guide_legend(title = "Label"), size = guide_legend(title = NULL))

    if (input$polygon_name_1 == FALSE && input$polygon_name_2 == FALSE && input$polygon_name_3 == FALSE) {

      ggplotly(Geoplot_3)

    } else if (input$polygon_name_1 == TRUE && input$polygon_name_2 == FALSE && input$polygon_name_3 == FALSE) {

      Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5)

      ggplotly(Geoplot_3_1)

    } else if (input$polygon_name_1 == FALSE && input$polygon_name_2 == TRUE && input$polygon_name_3 == FALSE) {

      Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5)

      ggplotly(Geoplot_3_1)

    } else if (input$polygon_name_1 == FALSE && input$polygon_name_2 == FALSE && input$polygon_name_3 == TRUE) {

      Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_3(), aes(label = id), size = 2.5)

      ggplotly(Geoplot_3_1)

    } else if (input$polygon_name_1 == TRUE && input$polygon_name_2 == TRUE && input$polygon_name_3 == FALSE) {

      Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5) +
        geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5)

      ggplotly(Geoplot_3_1)

    } else if (input$polygon_name_1 == TRUE && input$polygon_name_2 == FALSE && input$polygon_name_3 == TRUE) {

      Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5) +
        geom_text(data = Name_Centroids_Shapefile_3(), aes(label = id), size = 2.5)

      ggplotly(Geoplot_3_1)

    } else if (input$polygon_name_1 == FALSE && input$polygon_name_2 == TRUE && input$polygon_name_3 == TRUE) {

      Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5) +
        geom_text(data = Name_Centroids_Shapefile_3(), aes(label = id), size = 2.5)

      ggplotly(Geoplot_3_1)

    } else if (input$polygon_name_1 == TRUE && input$polygon_name_2 == TRUE && input$polygon_name_3 == TRUE) {

      Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5) +
        geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5) +
        geom_text(data = Name_Centroids_Shapefile_3(), aes(label = id), size = 2.5)

      ggplotly(Geoplot_3_1)

    }

  }

})

#### GOOGLE MAPS ####
output$google_map_1 <- renderGoogle_map({

  key_1 <- ""

  Geo_Coord_UTM_1 = Data()[, c(3, 4)]

  utmcoor_1 <- SpatialPoints(Geo_Coord_UTM_1, proj4string=CRS("+proj=utm +zone=33 +datum=WGS84"))

  longlatcoor_1 <- spTransform(utmcoor_1, CRS("+proj=longlat + datum=WGS84"))

  Geo_Coord_LongLat_1 = longlatcoor_1@coords



  # la funzione SpatialPoints vuole prima UTM_Est e poi UTM_Nord (quindi prima lat e poi lon) e restituisce quindi prima la longitudine e poi la latitudine, quindi rinominare correttamente le colonne
  colnames(Geo_Coord_LongLat_1)[c(1, 2)] = c("lon", "lat")

  Label.Sample_ID_1 = data.frame(Label.Sample_ID_1 = paste(Data()$Label, " ", "-", " ", Data()$Sample_ID))

  Geo_Coord_DD_1 = data.frame(Geo_Coord_LongLat_1, Label.Sample_ID_1)



  google_map(key = key_1, search_box = T) %>% googleway::add_markers(data = Geo_Coord_DD_1, lon = "lon", lat = "lat", info_window = "Label.Sample_ID_1")

})





################################ TABLE: LINEAR MODELS ####
#### LEVENE'S TEST ####
output$levene <- renderPrint({

  LEVENE_TEST = leveneTest(Data()[, input$Variable_Y], Data()$Label, center = median)

  LEVENE_TEST

})

#### ANOVA TEST ####
output$anova <- renderPrint({

  ANOVA = oneway.test(Data()[, input$Variable_Y] ~ Data()$Label)

  ANOVA

})

#### ANOVA PLOT ####
output$anova_plot <- renderPlotly({

  Mean = Data()[, input$Variable_Y]

  Anovaplot = ggplot(Data(), aes(Label, Mean)) +
    stat_summary(fun.y = mean, geom = "point") +
    stat_summary(fun.y = mean, geom = "line",
                 aes(group = 1), colour = "blue", linetype = "dashed") +
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
      labs(x = "Label", y = input$Variable_Y) +
        theme(
          text = element_text(colour = "black"),
          axis.title = element_text(size = 15, face = "bold"),
          axis.text.x = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 12, face = "bold")
        )

  ggplotly(Anovaplot)

})

#### POST HOC BONFERRONI ####
output$bonferroni <- renderPrint({

  POST_HOC = pairwise.t.test(Data()[, input$Variable_Y],
                             Data()$Label,
                             p.adj = "bonferroni")

  POST_HOC

})





################################ TABLE: PCA ####
#### SUMMARY PCA ####
output$summary_PCA <- renderPrint({

    options(digits = 5)
    summary(PCA())

})

#### SCREEPLOT PCA ####
output$screeplot <- renderPlotly({

  Var_Exp.PCA <- data.frame(cumsum((PCA()$sdev^2)/sum(PCA()$sdev^2)))
  Var_Exp.PCA <- cbind(rownames(Var_Exp.PCA), Var_Exp.PCA)
  rownames(Var_Exp.PCA) <- NULL
  colnames(Var_Exp.PCA) <- c("Principal_Component","Variance_Explained")
  Var_Exp.PCA$Principal_Component <- factor(Var_Exp.PCA$Principal_Component,
                                            levels = Var_Exp.PCA$Principal_Component[order(Var_Exp.PCA$Variance_Explained)])


  if (input$choose_variance == FALSE) {

  Var_Exp.PCA.1 = subset(Var_Exp.PCA[1:input$PCs, ])

  Screeplot <- ggplot(Var_Exp.PCA,aes(Principal_Component,Variance_Explained)) +
    geom_point(size = 0.001) +
    geom_line(data = Var_Exp.PCA.1,
              aes(colour = "red", group = 1), linetype = "dotted") +
    geom_point(data = Var_Exp.PCA.1, size = 4, colour = "red") +
    labs(x = "Principal Components", y = "Variance explained [%]") +
    scale_y_continuous(limits = c(0, 1), breaks = c(seq(0, 1, by = 0.1))) +
    theme(
      legend.position = "none"
    )

  ggplotly(Screeplot)

  }

  else if (input$choose_variance == TRUE) {

    if (is.null(input$Box_Var)) {

    Var_Exp.PCA.1 = subset(Var_Exp.PCA[1:input$PCs, ])

    Screeplot <- ggplot(Var_Exp.PCA,aes(Principal_Component,Variance_Explained)) +
      geom_point(size = 0.001) +
      geom_line(data = Var_Exp.PCA.1, aes(colour = "red", group = 1), linetype = "dotted") +
      geom_point(data = Var_Exp.PCA.1, size = 4, colour = "red") +
      labs(x = "Principal Components", y = "Variance explained [%]") +
      scale_y_continuous(limits = c(0, 1), breaks = c(seq(0, 1, by = 0.1))) +
      theme(
        legend.position = "none"
      )

    ggplotly(Screeplot)

    }

    else if (input$Box_Var == 1) {

    Var_Exp.PCA.1 = subset(Var_Exp.PCA, Variance_Explained < 0.75)

    Screeplot <- ggplot(Var_Exp.PCA, aes(Principal_Component, Variance_Explained))+
      geom_point(size = 0.001) + geom_line(data = Var_Exp.PCA.1, aes(colour = "red", group = 1), linetype = "dotted") +
      geom_point(data = Var_Exp.PCA.1, size = 4, colour = "red") +
      labs(x = "Principal Components", y = "Variance explained [%]") +
      scale_y_continuous(limits = c(0, 1), breaks = c(seq(0, 1, by = 0.1))) +
      theme(
        legend.position = "none"
      )

    Screeplot2 = Screeplot + geom_hline(yintercept = 0.7, colour = "darkred", linetype = "dotted")

    ggplotly(Screeplot2)

  }

  else if (input$Box_Var == 2) {

    Var_Exp.PCA.1 = subset(Var_Exp.PCA, Variance_Explained < 0.8)

    Screeplot <- ggplot(Var_Exp.PCA, aes(Principal_Component, Variance_Explained))+
      geom_point(size = 0.001) + geom_line(data = Var_Exp.PCA.1, aes(colour = "red", group = 1), linetype = "dotted") +
      geom_point(data = Var_Exp.PCA.1, size = 4, colour = "red") +
      labs(x = "Principal Components", y = "Variance explained [%]") +
      scale_y_continuous(limits = c(0, 1), breaks = c(seq(0, 1, by = 0.1))) +
      theme(
        legend.position = "none"
      )

    Screeplot2 = Screeplot + geom_hline(yintercept = 0.75, colour = "darkred", linetype = "dotted")

    ggplotly(Screeplot2)

  }

  else if (input$Box_Var == 3) {

    Var_Exp.PCA.1 = subset(Var_Exp.PCA, Variance_Explained < 0.85)

    Screeplot <- ggplot(Var_Exp.PCA, aes(Principal_Component, Variance_Explained))+
      geom_point(size = 0.001) + geom_line(data = Var_Exp.PCA.1, aes(colour = "red", group = 1), linetype = "dotted") +
      geom_point(data = Var_Exp.PCA.1, size = 4, colour = "red") +
      labs(x = "Principal Components", y = "Variance explained [%]") +
      scale_y_continuous(limits = c(0, 1), breaks = c(seq(0, 1, by = 0.1))) +
      theme(
        legend.position = "none"
      )

    Screeplot2 = Screeplot + geom_hline(yintercept = 0.8, colour = "darkred", linetype = "dotted")

    ggplotly(Screeplot2)

  }

  else if (input$Box_Var == 4) {

    Var_Exp.PCA.1 = subset(Var_Exp.PCA, Variance_Explained < 0.9)

    Screeplot <- ggplot(Var_Exp.PCA, aes(Principal_Component, Variance_Explained))+
      geom_point(size = 0.001) + geom_line(data = Var_Exp.PCA.1, aes(colour = "red", group = 1), linetype = "dotted") +
      geom_point(data = Var_Exp.PCA.1, size = 4, colour = "red") +
      labs(x = "Principal Components", y = "Variance explained [%]") +
      scale_y_continuous(limits = c(0, 1), breaks = c(seq(0, 1, by = 0.1))) +
      theme(
        legend.position = "none"
      )

    Screeplot2 = Screeplot + geom_hline(yintercept = 0.85, colour = "darkred", linetype = "dotted")

    ggplotly(Screeplot2)

  }

  else if (input$Box_Var == 5) {

    Var_Exp.PCA.1 = subset(Var_Exp.PCA, Variance_Explained < 0.95)

    Screeplot <- ggplot(Var_Exp.PCA, aes(Principal_Component, Variance_Explained))+
      geom_point(size = 0.001) + geom_line(data = Var_Exp.PCA.1, aes(colour = "red", group = 1), linetype = "dotted") +
      geom_point(data = Var_Exp.PCA.1, size = 4, colour = "red") +
      labs(x = "Principal Components", y = "Variance explained [%]") +
      scale_y_continuous(limits = c(0, 1), breaks = c(seq(0, 1, by = 0.1))) +
      theme(
        legend.position = "none"
      )

    Screeplot2 = Screeplot + geom_hline(yintercept = 0.9, colour = "darkred", linetype = "dotted")

    ggplotly(Screeplot2)

  }

  else if (input$Box_Var == 6) {

    Var_Exp.PCA.1 = subset(Var_Exp.PCA, Variance_Explained < 1)

    Screeplot <- ggplot(Var_Exp.PCA, aes(Principal_Component, Variance_Explained))+
      geom_point(size = 0.001) + geom_line(data = Var_Exp.PCA.1, aes(colour = "red", group = 1), linetype = "dotted") +
      geom_point(data = Var_Exp.PCA.1, size = 4, colour = "red") +
      labs(x = "Principal Components", y = "Variance explained [%]") +
      scale_y_continuous(limits = c(0, 1), breaks = c(seq(0, 1, by = 0.1))) +
      theme(
        legend.position = "none"
      )

    Screeplot2 = Screeplot + geom_hline(yintercept = 0.95, colour = "darkred", linetype = "dotted")

    ggplotly(Screeplot2)

  }

  }

})

#### LOADINGS PCA ####
output$loadings <- renderPlotly ({

    PCA_Sdev <- as.data.frame(round(PCA()$sdev^2/sum(PCA()$sdev^2)*100, 2))

    Load.PCA <- as.data.frame(PCA()$loadings[, as.numeric(input$PCs_B)])
    Load.PCA <- cbind(rownames(Load.PCA), Load.PCA)
    rownames(Load.PCA) <- NULL
    colnames(Load.PCA) <- c("Variables", paste0("PC", input$PCs_B))

    Value <- Load.PCA[, 2]

    Loadplot <- ggplot(Load.PCA) + geom_col(aes(x = Variables, y = Value), fill = "darkgray")+
      labs (y = paste0 ("PC", input$PCs_B, " ", "(", PCA_Sdev[as.numeric(input$PCs_B), ], "%", ")"))

      ggplotly(Loadplot)

})

#### PLOT PCA ####
output$PCs_plot <- renderPlotly({

  PCA_Sdev <- as.data.frame(round(PCA()$sdev^2/sum(PCA()$sdev^2)*100, 2))
  SCORES <- data.frame(Data()[, c(1, 2, 3, 4)], PCA()$scores[, c(1:input$PCs_B)])

    if (input$PCs_B == 1) {

      colnames(SCORES)[5] = "Comp.1"

     Plot <- ggplot(SCORES, aes(x = Comp.1, y = 0)) + geom_point(aes(name = Sample_ID, colour = Label))+
        labs(x = paste0("PC1", " ", "(", PCA_Sdev[1,], "%", ")"), y = NULL)

     ggplotly(Plot)

    }

    else if (input$PCs_B == 2 && input$cov_cor == "Correlation") {

      Biplot <- autoplot(princomp(Data()[,-c(1, 2, 3, 4)], cor = TRUE), data = Data(), loadings = TRUE,
          loadings.colour = "black", loadings.label = TRUE, loadings.label.size = 4) +
            geom_point(aes(name = Sample_ID, colour = Label)) +
              labs(x = paste0("PC1"," ","(",PCA_Sdev[1,],"%",")"), y = paste0("PC2"," ","(",PCA_Sdev[2,],"%",")"))

      ggplotly(Biplot)

    }

    else if (input$PCs_B == 2 && input$cov_cor == "Covariance") {

      Biplot <- autoplot(princomp(Data()[,-c(1,2,3,4)], cor = FALSE), data = Data(), loadings = TRUE,
        loadings.colour = "black", loadings.label = TRUE, loadings.label.size = 4) +
          geom_point(aes(name = Sample_ID, colour = Label)) +
            labs(x = paste0("PC1"," ","(",PCA_Sdev[1,],"%",")"), y = paste0("PC2"," ","(",PCA_Sdev[2,],"%",")"))

      ggplotly(Biplot)

    }

    else if (input$PCs_B == 3) {

      plot_ly(data = SCORES, x = ~SCORES[,5], y = ~SCORES[,6], z = ~SCORES[,7],
              color = ~Label, marker = list(size = 6), # colors = Palette
              text = ~Sample_ID, showlegend = TRUE) %>%
        layout(legend = list(font = list(size = 12)),
               scene = list(xaxis = list(title = paste0("PC1"," ","(",PCA_Sdev[1,],"%",")")),
                            yaxis = list(title = paste0("PC2"," ","(",PCA_Sdev[2,],"%",")")),
                            zaxis = list(title = paste0("PC3"," ","(",PCA_Sdev[3,],"%",")"))))

    }

})





################################ TABLE: CLUSTER ANALYSIS ####
#### WSS PLOT ####
output$wss <- renderPlotly({

  if (input$Perform_PCA == FALSE) {

    if (input$Cluster_Method == "kmeans") {

    BSS = 0
    for (i in 2:10) {
      BSS[i] = eclust(Data()[, -c(1, 2, 3, 4)], "kmeans", i, graph = FALSE)$betweenss
    }
    BSS_Data = data.frame(cluster = c(seq(1:10)), cbind(BSS))


    TWSS  = NA
    for (i in 1:10) {
      TWSS[i] = eclust(Data()[, -c(1, 2, 3, 4)], "kmeans", i, graph = FALSE)$tot.withinss
    }
    TWSS_Data = data.frame(cluster = c(seq(1:10)), cbind(TWSS))

    BSS_TWSS_Data = merge(BSS_Data, TWSS_Data)

    ggplot(BSS_TWSS_Data, aes(x = cluster)) +
      geom_point(aes(y = TWSS), size = 4, colour = "steelblue") +
      geom_point(aes(y = BSS), size = 4, colour = "indianred2") +
      geom_line(aes(y = TWSS), colour = "steelblue") +
      geom_line(aes(y = BSS), colour = "indianred2") +
        labs(x = paste0("Number of clusters"," ", "-", " ", input$Cluster_Method), y = "TWSS / BSS", title = NULL) +
          scale_x_continuous(breaks = c(seq(from = 1, to = 10, by = 1)))

    }

    else if (input$Cluster_Method == "pam") {

      WSSPLOT = fviz_nbclust(Data()[, -c(1, 2, 3, 4)], cluster::pam, method = "wss", k.max = 10) +
        #geom_vline(xintercept = 3, linetype = 2) +
        geom_point(size = 4, col = "steelblue") + theme_gray() +
        labs(x = paste0("Number of clusters"," ", "-", " ", input$Cluster_Method), y = "TWSS", title = NULL)

      WSSPLOT

    }

    else if (input$Cluster_Method == "clara") {

      WSSPLOT = fviz_nbclust(Data()[, -c(1, 2, 3, 4)], cluster::clara, method = "wss", k.max = 10) +
        #geom_vline(xintercept = 3, linetype = 2) +
        geom_point(size = 4, col = "steelblue") + theme_gray() +
        labs(x = paste0("Number of clusters"," ", "-", " ", input$Cluster_Method), y = "TWSS", title = NULL)

      WSSPLOT

    }

    else if (input$Cluster_Method == "fanny") {

      WSSPLOT = fviz_nbclust(Data()[, -c(1, 2, 3, 4)], cluster::fanny, method = "wss", k.max = 10) +
        #geom_vline(xintercept = 3, linetype = 2) +
        geom_point(size = 4, col = "steelblue") + theme_gray() +
        labs(x = paste0("Number of clusters"," ", "-", " ", input$Cluster_Method), y = "TWSS", title = NULL)

      WSSPLOT

    }

  }

  else if (input$Perform_PCA == TRUE) {

    if (input$Cluster_Method == "kmeans") {

  BSS = 0
  for (i in 2:10) {
    BSS[i] = eclust(SCORES()[, -c(1, 2, 3, 4)], "kmeans", i, graph = FALSE)$betweenss
  }
  BSS_Data = data.frame(cluster = c(seq(1:10)), cbind(BSS))


  TWSS  = NA
  for (i in 1:10) {
    TWSS[i] = eclust(SCORES()[, -c(1, 2, 3, 4)], "kmeans", i, graph = FALSE)$tot.withinss
  }
  TWSS_Data = data.frame(cluster = c(seq(1:10)), cbind(TWSS))


  BSS_TWSS_Data <- merge(BSS_Data, TWSS_Data)


  ggplot(BSS_TWSS_Data, aes(x = cluster)) +
    geom_point(aes(y = TWSS), size = 4, colour = "steelblue") +
    geom_point(aes(y = BSS), size = 4, colour = "indianred2") +
    geom_line(aes(y = TWSS), colour = "steelblue") +
    geom_line(aes(y = BSS), colour = "indianred2") +
      labs(x = paste0("Number of clusters"," ", "-", " ", input$Cluster_Method), y = "TWSS / BSS", title = NULL) +
      scale_x_continuous(breaks = c(seq(from = 1, to = 10, by = 1)))

    }

    else if (input$Cluster_Method == "pam") {

      WSSPLOT = fviz_nbclust(SCORES()[, -c(1, 2, 3, 4)], cluster::pam, method = "wss", k.max = 10) +
        #geom_vline(xintercept = 3, linetype = 2) +
        geom_point(size = 4, col = "steelblue") + theme_gray() +
        labs(x = paste0("Number of clusters"," ", "-", " ", input$Cluster_Method), y = "TWSS", title = NULL)

      WSSPLOT

    }

    else if (input$Cluster_Method == "clara") {

      WSSPLOT = fviz_nbclust(SCORES()[, -c(1, 2, 3, 4)], cluster::clara, method = "wss", k.max = 10) +
        #geom_vline(xintercept = 3, linetype = 2) +
        geom_point(size = 4, col = "steelblue") + theme_gray() +
        labs(x = paste0("Number of clusters"," ", "-", " ", input$Cluster_Method), y = "TWSS", title = NULL)

      WSSPLOT

    }

    else if (input$Cluster_Method == "fanny") {

      WSSPLOT = fviz_nbclust(SCORES()[, -c(1, 2, 3, 4)], cluster::fanny, method = "wss", k.max = 10) +
        #geom_vline(xintercept = 3, linetype = 2) +
        geom_point(size = 4, col = "steelblue") + theme_gray() +
        labs(x = paste0("Number of clusters"," ", "-", " ", input$Cluster_Method), y = "TWSS", title = NULL)

      WSSPLOT

    }

  }

})

#### ERROR GAP STATISTIC ####
output$error_gap_statistics <- renderText({

  print("The function of the gap statistics needs to work on a dataframe of at least 2 columns. Please select at least 2 Principal Components.")

})

#### GAP STATISTIC ####
output$gap_statistics <- renderPlotly({

  if (input$Perform_PCA == TRUE) {

    if (input$Cluster_Method == "kmeans") {

      gap_stat <- clusGap(SCORES()[, -c(1, 2, 3, 4)],
                          FUN = kmeans,
                          nstart = 25,
                          K.max = 10,
                          B = 50)

      fviz_gap_stat(gap_stat) +
        geom_point(size = 4, colour = "dodgerblue") +
        geom_line(colour = "dodgerblue") +
        labs(x = paste0("Number of clusters"," ", "-", " ",
                        input$Cluster_Method),
             y = "Gap statistic", title = NULL) +
          theme_gray()

    } else if (input$Cluster_Method == "pam") {

      gap_stat <- clusGap(SCORES()[, -c(1, 2, 3, 4)],
                          FUN = pam,
                          K.max = 10,
                          B = 50)

      fviz_gap_stat(gap_stat) +
        geom_point(size = 4, colour = "dodgerblue") +
        geom_line(colour = "dodgerblue") +
        labs(x = paste0("Number of clusters"," ", "-", " ",
                        input$Cluster_Method),
             y = "Gap statistic", title = NULL) +
        theme_gray()

    } else if (input$Cluster_Method == "clara") {

      gap_stat <- clusGap(SCORES()[, -c(1, 2, 3, 4)],
                          FUN = clara,
                          K.max = 10,
                          B = 50)

      fviz_gap_stat(gap_stat) +
        geom_point(size = 4, colour = "dodgerblue") +
        geom_line(colour = "dodgerblue") +
        labs(x = paste0("Number of clusters"," ", "-", " ",
                        input$Cluster_Method),
             y = "Gap statistic", title = NULL) +
        theme_gray()

    } else if (input$Cluster_Method == "fanny") {

      gap_stat <- clusGap(SCORES()[, -c(1, 2, 3, 4)],
                          FUN = fanny,
                          K.max = 10,
                          B = 50)

      fviz_gap_stat(gap_stat) +
        geom_point(size = 4, colour = "dodgerblue") +
        geom_line(colour = "dodgerblue") +
        labs(x = paste0("Number of clusters"," ", "-", " ",
                        input$Cluster_Method),
             y = "Gap statistic", title = NULL) +
        theme_gray()

    }

  }

  else if (input$Perform_PCA == FALSE) {

    if (input$Cluster_Method == "kmeans") {

      gap_stat = clusGap(Data()[, -c(1, 2, 3, 4)], FUN = kmeans, nstart = 25, K.max = 10, B = 50)

      fviz_gap_stat(gap_stat) + geom_point(size = 4, colour = "dodgerblue") + geom_line(colour = "dodgerblue") +
        labs(x = paste0("Number of clusters"," ", "-", " ", input$Cluster_Method), y = "Gap statistic", title = NULL) +
        theme_gray()

    } else if (input$Cluster_Method == "pam") {

      gap_stat = clusGap(Data()[, -c(1, 2, 3, 4)], FUN = pam, K.max = 10, B = 50)

      fviz_gap_stat(gap_stat) + geom_point(size = 4, colour = "dodgerblue") + geom_line(colour = "dodgerblue") +
        labs(x = paste0("Number of clusters"," ", "-", " ", input$Cluster_Method), y = "Gap statistic", title = NULL) +
        theme_gray()

    } else if (input$Cluster_Method == "clara") {

      gap_stat = clusGap(Data()[, -c(1, 2, 3, 4)], FUN = clara, K.max = 10, B = 50)

      fviz_gap_stat(gap_stat) + geom_point(size = 4, colour = "dodgerblue") + geom_line(colour = "dodgerblue") +
        labs(x = paste0("Number of clusters"," ", "-", " ", input$Cluster_Method), y = "Gap statistic", title = NULL) +
        theme_gray()

    } else if (input$Cluster_Method == "fanny") {

      gap_stat = clusGap(Data()[, -c(1, 2, 3, 4)], FUN = fanny, K.max = 10, B = 50)

      fviz_gap_stat(gap_stat) + geom_point(size = 4, colour = "dodgerblue") + geom_line(colour = "dodgerblue") +
        labs(x = paste0("Number of clusters"," ", "-", " ", input$Cluster_Method), y = "Gap statistic", title = NULL) +
        theme_gray()

    }

  }

})

#### TABLEPLOT ####
output$tabplot <- renderPlotly({

  if (input$Perform_PCA == TRUE) {

    METHOD <- eclust(SCORES()[, -c(1, 2, 3, 4)], FUNcluster = input$Cluster_Method, k(), graph = FALSE)

    if (input$Cluster_Method == "kmeans") {

    Data_CLUSTER <- data.frame(SCORES(), METHOD$cluster)
    colnames(Data_CLUSTER)[colnames(Data_CLUSTER) == "METHOD.cluster"] = "Cluster"

    } else if (input$Cluster_Method == "pam" | input$Cluster_Method == "clara" | input$Cluster_Method == "fanny") {

      Data_CLUSTER <- data.frame(SCORES(), METHOD$clustering)
      colnames(Data_CLUSTER)[colnames(Data_CLUSTER) == "METHOD.clustering"] = "Cluster"

    }

    Tabplot <- ggplot(Data_CLUSTER, aes(Cluster)) +
      geom_bar(aes(fill = factor(Cluster))) +
      facet_grid(~Label) +
      labs(y = "Number of units") +
      scale_x_continuous(breaks = c(seq(0, 9, by = 1))) +
      theme(
        strip.text.x = element_text(size = 13, face = "bold")
      ) + guides(fill = guide_legend(title = "Cluster"))

    ggplotly(Tabplot)

  } else if (input$Perform_PCA == FALSE) {

    METHOD <- eclust(Data()[, -c(1, 2, 3, 4)], FUNcluster = input$Cluster_Method, k(), graph = FALSE)

      if (input$Cluster_Method == "kmeans") {

        Data_CLUSTER <- data.frame(Data(), METHOD$cluster)
        colnames(Data_CLUSTER)[colnames(Data_CLUSTER) == "METHOD.cluster"] = "Cluster"

      } else if (input$Cluster_Method == "pam" | input$Cluster_Method == "clara" | input$Cluster_Method == "fanny") {

        Data_CLUSTER <- data.frame(Data(), METHOD$clustering)
        colnames(Data_CLUSTER)[colnames(Data_CLUSTER) == "METHOD.clustering"] = "Cluster"

      }

    Tabplot <- ggplot(Data_CLUSTER, aes(Cluster)) +
      geom_bar(aes(fill = factor(Cluster))) +
        facet_grid(~Label) +
          labs(y = "Number of units") +
          scale_x_continuous(breaks = c(seq(0, 9, by = 1))) +
            theme(
              strip.text.x = element_text(size = 13, face = "bold")
            ) + guides(fill = guide_legend(title = "Cluster"))

    ggplotly(Tabplot)

  }

})

#### SILHOUETTE PLOT ####
output$silplot <- renderPlotly({

  if (input$Perform_PCA == TRUE) {

    METHOD <- eclust(SCORES()[, -c(1,2,3,4)], FUNcluster = input$Cluster_Method, k(), graph = FALSE)

    Silplot <- fviz_silhouette(METHOD) +
      scale_y_continuous(breaks = c(seq(from = 0, to = 1 , by = 0.1)))

    ggplotly(Silplot)

  }

  else if (input$Perform_PCA == FALSE) {

    METHOD = eclust(Data()[, -c(1,2,3,4)], FUNcluster = input$Cluster_Method, k(), graph = FALSE)

    Silplot <- fviz_silhouette(METHOD) +
      scale_y_continuous(breaks=c(seq(from = 0, to = 1, by = 0.1)))

    ggplotly(Silplot)

  }

})

#### GEOPLOT ####
output$geoplot_2 <- renderPlotly({

  if (input$Perform_PCA == TRUE) {

  METHOD <- eclust(SCORES()[, -c(1,2,3,4)], FUNcluster = input$Cluster_Method, k(), graph = FALSE)

    if (input$Cluster_Method == "kmeans") {

    Data_CLUSTER = data.frame(SCORES(), METHOD$cluster)
    colnames(Data_CLUSTER)[colnames(Data_CLUSTER) == "METHOD.cluster"] = "cluster"

    }

    else if (input$Cluster_Method == "pam" | input$Cluster_Method == "clara" | input$Cluster_Method == "fanny") {

    Data_CLUSTER = data.frame(SCORES(), METHOD$clustering)
    colnames(Data_CLUSTER)[colnames(Data_CLUSTER) == "METHOD.clustering"] = "cluster"

    }

    Cluster <- factor(Data_CLUSTER[, "cluster"])

      if (input$shapefiles == 1) {

        Geoplot_1 <- ggplot(data = Data_CLUSTER, aes(UTM_Est, UTM_Nord))+
          geom_polygon(aes(group = group), Shapefile_1_df(), fill = input$Colours_2_1, colour = "grey50") +
          geom_point(aes(name = Sample_ID, group = Label, colour = Cluster)) +
            labs(x = "UTM Est [m]", y = "UTM Nord [m]")

        if (input$polygon_name_2_1 == FALSE) {

          ggplotly(Geoplot_1)

        } else {

          Geoplot_1_1 <- Geoplot_1 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5)

          ggplotly(Geoplot_1_1)

        }

      } else if (input$shapefiles == 2) {

        Geoplot_2 <- ggplot(data = Data_CLUSTER, aes(UTM_Est, UTM_Nord))+
          geom_polygon(aes(group = group), Shapefile_1_df(), fill = input$Colours_2_1, colour = "grey50") +
          geom_polygon(aes(group = group), Shapefile_2_df(), fill = input$Colours_2_2, colour = "grey50") +
          geom_point(aes(name = Sample_ID, group = Label, colour = Cluster)) +
            labs(x = "UTM Est [m]", y = "UTM Nord [m]")

          if (input$polygon_name_2_1 == FALSE && input$polygon_name_2_2 == FALSE) {

            ggplotly(Geoplot_2)

          } else if (input$polygon_name_2_1 == TRUE && input$polygon_name_2_2 == FALSE) {

            Geoplot_2_1 <- Geoplot_2 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5)

            ggplotly(Geoplot_2_1)

          } else if (input$polygon_name_2_1 == FALSE && input$polygon_name_2_2 == TRUE) {

            Geoplot_2_1 <- Geoplot_2 + geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5)

            ggplotly(Geoplot_2_1)

          } else if (input$polygon_name_2_1 == TRUE && input$polygon_name_2_2 == TRUE) {

            Geoplot_2_1 <- Geoplot_2 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5) +
              geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5)

            ggplotly(Geoplot_2_1)

          }

      } else if (input$shapefiles == 3) {

        Geoplot_3 <- ggplot(data = Data_CLUSTER, aes(UTM_Est, UTM_Nord)) +
          geom_polygon(aes(group = group), Shapefile_1_df(), fill = input$Colours_2_1, colour = "grey50") +
          geom_polygon(aes(group = group), Shapefile_2_df(), fill = input$Colours_2_2, colour = "grey50") +
          geom_polygon(aes(group = group), Shapefile_3_df(), fill = input$Colours_2_3, colour = "grey50") +
          geom_point(aes(name = Sample_ID, group = Label, colour = Cluster)) +
            labs(x = "UTM Est [m]", y = "UTM Nord [m]")


        if (input$polygon_name_2_1 == FALSE && input$polygon_name_2_2 == FALSE && input$polygon_name_2_3 == FALSE) {

          ggplotly(Geoplot_3)

        } else if (input$polygon_name_2_1 == TRUE && input$polygon_name_2_2 == FALSE && input$polygon_name_2_3 == FALSE) {

          Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5)

          ggplotly(Geoplot_3_1)

        } else if (input$polygon_name_2_1 == FALSE && input$polygon_name_2_2 == TRUE && input$polygon_name_2_3 == FALSE) {

          Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5)

          ggplotly(Geoplot_3_1)

        } else if (input$polygon_name_2_1 == FALSE && input$polygon_name_2_2 == FALSE && input$polygon_name_2_3 == TRUE) {

          Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_3(), aes(label = id), size = 2.5)

          ggplotly(Geoplot_3_1)

        } else if (input$polygon_name_2_1 == TRUE && input$polygon_name_2_2 == TRUE && input$polygon_name_2_3 == FALSE) {

          Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5) +
            geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5)

          ggplotly(Geoplot_3_1)

        } else if (input$polygon_name_2_1 == TRUE && input$polygon_name_2_2 == FALSE && input$polygon_name_2_3 == TRUE) {

          Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5) +
            geom_text(data = Name_Centroids_Shapefile_3(), aes(label = id), size = 2.5)

          ggplotly(Geoplot_3_1)

        } else if (input$polygon_name_2_1 == FALSE && input$polygon_name_2_2 == TRUE && input$polygon_name_2_3 == TRUE) {

          Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5) +
            geom_text(data = Name_Centroids_Shapefile_3(), aes(label = id), size = 2.5)

          ggplotly(Geoplot_3_1)

        } else if (input$polygon_name_2_1 == TRUE && input$polygon_name_2_2 == TRUE && input$polygon_name_2_3 == TRUE) {

          Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5) +
            geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5) +
            geom_text(data = Name_Centroids_Shapefile_3(), aes(label = id), size = 2.5)

          ggplotly(Geoplot_3_1)

        }

      }

  } else if (input$Perform_PCA == FALSE) {

    METHOD <- eclust(Data()[, -c(1,2,3,4)], FUNcluster = input$Cluster_Method, k(), graph = FALSE)

      if (input$Cluster_Method == "kmeans") {

      Data_CLUSTER <- data.frame(Data(), METHOD$cluster)
      colnames(Data_CLUSTER)[colnames(Data_CLUSTER) == "METHOD.cluster"] = "cluster"

      }

      else if (input$Cluster_Method == "pam" | input$Cluster_Method == "clara"  | input$Cluster_Method == "fanny") {

      Data_CLUSTER <- data.frame(Data(), METHOD$clustering)
      colnames(Data_CLUSTER)[colnames(Data_CLUSTER) == "METHOD.clustering"] = "cluster"

      }


    Cluster <- factor(Data_CLUSTER[, "cluster"])

    if (input$shapefiles == 1) {

      Geoplot_1 <- ggplot(data = Data_CLUSTER, aes(UTM_Est, UTM_Nord))+
        geom_polygon(aes(group = group), Shapefile_1_df(), fill = input$Colours_2_1, colour = "grey50") +
        geom_point(aes(name = Sample_ID, group = Label, colour = Cluster)) +
          labs(x = "UTM Est [m]", y = "UTM Nord [m]")

      if (input$polygon_name_2_1 == FALSE) {

        ggplotly(Geoplot_1)

      } else {

        Geoplot_1_1 <- Geoplot_1 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5)

        ggplotly(Geoplot_1_1)

      }

    } else if (input$shapefiles == 2) {

      Geoplot_2 <- ggplot(data = Data_CLUSTER, aes(UTM_Est, UTM_Nord))+
        geom_polygon(aes(group = group), Shapefile_1_df(), fill = input$Colours_2_1, colour = "grey50") +
        geom_polygon(aes(group = group), Shapefile_2_df(), fill = input$Colours_2_2, colour = "grey50") +
        geom_point(aes(name = Sample_ID, group = Label, colour = Cluster)) +
        labs(x = "UTM Est [m]", y = "UTM Nord [m]")

      if (input$polygon_name_2_1 == FALSE && input$polygon_name_2_2 == FALSE) {

        ggplotly(Geoplot_2)

      } else if (input$polygon_name_2_1 == TRUE && input$polygon_name_2_2 == FALSE) {

        Geoplot_2_1 <- Geoplot_2 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5)

        ggplotly(Geoplot_2_1)

      } else if (input$polygon_name_2_1 == FALSE && input$polygon_name_2_2 == TRUE) {

        Geoplot_2_1 <- Geoplot_2 + geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5)

        ggplotly(Geoplot_2_1)

      } else if (input$polygon_name_2_1 == TRUE && input$polygon_name_2_2 == TRUE) {

        Geoplot_2_1 <- Geoplot_2 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5) +
          geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5)

        ggplotly(Geoplot_2_1)

      }

    } else if (input$shapefiles == 3) {

      Geoplot_3 <- ggplot(data = Data_CLUSTER, aes(UTM_Est, UTM_Nord)) +
        geom_polygon(aes(group = group), Shapefile_1_df(), fill = input$Colours_2_1, colour = "grey50") +
        geom_polygon(aes(group = group), Shapefile_2_df(), fill = input$Colours_2_2, colour = "grey50") +
        geom_polygon(aes(group = group), Shapefile_3_df(), fill = input$Colours_2_3, colour = "grey50") +
        geom_point(aes(name = Sample_ID, group = Label, colour = Cluster)) +
        labs(x = "UTM Est [m]", y = "UTM Nord [m]")


      if (input$polygon_name_2_1 == FALSE && input$polygon_name_2_2 == FALSE && input$polygon_name_2_3 == FALSE) {

        ggplotly(Geoplot_3)

      } else if (input$polygon_name_2_1 == TRUE && input$polygon_name_2_2 == FALSE && input$polygon_name_2_3 == FALSE) {

        Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5)

        ggplotly(Geoplot_3_1)

      } else if (input$polygon_name_2_1 == FALSE && input$polygon_name_2_2 == TRUE && input$polygon_name_2_3 == FALSE) {

        Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5)

        ggplotly(Geoplot_3_1)

      } else if (input$polygon_name_2_1 == FALSE && input$polygon_name_2_2 == FALSE && input$polygon_name_2_3 == TRUE) {

        Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_3(), aes(label = id), size = 2.5)

        ggplotly(Geoplot_3_1)

      } else if (input$polygon_name_2_1 == TRUE && input$polygon_name_2_2 == TRUE && input$polygon_name_2_3 == FALSE) {

        Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5) +
          geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5)

        ggplotly(Geoplot_3_1)

      } else if (input$polygon_name_2_1 == TRUE && input$polygon_name_2_2 == FALSE && input$polygon_name_2_3 == TRUE) {

        Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5) +
          geom_text(data = Name_Centroids_Shapefile_3(), aes(label = id), size = 2.5)

        ggplotly(Geoplot_3_1)

      } else if (input$polygon_name_2_1 == FALSE && input$polygon_name_2_2 == TRUE && input$polygon_name_2_3 == TRUE) {

        Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5) +
          geom_text(data = Name_Centroids_Shapefile_3(), aes(label = id), size = 2.5)

        ggplotly(Geoplot_3_1)

      } else if (input$polygon_name_2_1 == TRUE && input$polygon_name_2_2 == TRUE && input$polygon_name_2_3 == TRUE) {

        Geoplot_3_1 <- Geoplot_3 + geom_text(data = Name_Centroids_Shapefile_1(), aes(label = id), size = 2.5) +
          geom_text(data = Name_Centroids_Shapefile_2(), aes(label = id), size = 2.5) +
          geom_text(data = Name_Centroids_Shapefile_3(), aes(label = id), size = 2.5)

        ggplotly(Geoplot_3_1)

      }

    }

  }

})

#### ARI ####
output$adjusted_rand_index <- renderPrint({

  if (input$Perform_PCA == TRUE) {

    METHOD <- eclust(SCORES()[, -c(1,2,3,4)], FUNcluster = input$Cluster_Method, k(), graph = FALSE)

      if (input$Cluster_Method == "kmeans") {

        Data_CLUSTER <- data.frame(SCORES(), METHOD$cluster)
        colnames(Data_CLUSTER)[colnames(Data_CLUSTER) == "METHOD.cluster"] = "cluster"

      }

      else if (input$Cluster_Method == "pam" | input$Cluster_Method == "clara" | input$Cluster_Method == "fanny") {

        Data_CLUSTER <- data.frame(SCORES(), METHOD$clustering)
        colnames(Data_CLUSTER)[colnames(Data_CLUSTER) == "METHOD.clustering"] = "cluster"

      }

    Cluster <- factor(Data_CLUSTER[, "cluster"])

    MEMBERS <- METHOD$cluster
    RAND_INDEX <- round(x = arandi(MEMBERS, SCORES()$Label, adjust = TRUE), digits = 3)

    RAND_INDEX

  } else if (input$Perform_PCA == FALSE) {

    METHOD <- eclust(Data()[, -c(1,2,3,4)], FUNcluster = input$Cluster_Method, k(), graph = FALSE)

      if (input$Cluster_Method == "kmeans") {

        Data_CLUSTER <- data.frame(Data(), METHOD$cluster)
        colnames(Data_CLUSTER)[colnames(Data_CLUSTER) == "METHOD.cluster"] = "cluster"

      }

      else if (input$Cluster_Method == "pam" | input$Cluster_Method == "clara" | input$Cluster_Method == "fanny") {

        Data_CLUSTER <- data.frame(Data(), METHOD$clustering)
        colnames(Data_CLUSTER)[colnames(Data_CLUSTER) == "METHOD.clustering"] = "cluster"

      }

    Cluster <- factor(Data_CLUSTER[, "cluster"])

    MEMBERS <- METHOD$cluster
    RAND_INDEX <- round(x = arandi(MEMBERS, Data_CLUSTER$Label, adjust = TRUE), digits = 3)

    RAND_INDEX

  }

})

#### GOOGLE MAPS ####
output$google_map_2 <- renderGoogle_map({

  key_2 = ""


  Geo_Coord_UTM = Data()[, c(3, 4)]

  utmcoor <- SpatialPoints(Geo_Coord_UTM, proj4string=CRS("+proj=utm +zone=33 +datum=WGS84"))

  longlatcoor <- spTransform(utmcoor, CRS("+proj=longlat + datum=WGS84"))

  Geo_Coord_LongLat = longlatcoor@coords



  #la funzione SpatialPoints vuole prima UTM_Est e poi UTM_Nord (quindi prima lat e poi lon) e restituisce quindi prima la longitudine e poi la latitudine, quindi rinominare correttamente le colonne
  colnames(Geo_Coord_LongLat)[c(1, 2)] = c("lon", "lat")



  Label.Sample_ID = data.frame(Label.Sample_ID = paste(Data()$Label, " ", "-", " ", Data()$Sample_ID))

  Geo_Coord_DD = data.frame(Geo_Coord_LongLat, Label.Sample_ID)

  google_map(key = key_2, search_box = T) %>% googleway::add_markers(data = Geo_Coord_DD, lon = "lon", lat = "lat", info_window = "Label.Sample_ID")

})

#### TABLE OF CLUSTERS ####
output$download_Cluster <- downloadHandler(

  filename = function() {

    if (input$Perform_PCA == TRUE) {

      paste0("Table_Cluster_", "PC_", input$PCs_2, "_", input$cov_cor_2, "_", input$Cluster_Method, "_", input$Cluster_Count, "_", input$Dataframe)

    }

    else if (input$Perform_PCA == FALSE) {

      paste0("Table_Cluster_", input$Cluster_Method, "_", input$Cluster_Count, "_", input$Dataframe )

    }

  },

  content = function(file) {

    if (input$Perform_PCA == TRUE) {

      SCORES <- data.frame(Data()[, c(1, 2, 3, 4)], PCA()$scores[, c(1:input$PCs_2)])

      METHOD <- eclust(SCORES[, -c(1,2,3,4)], FUNcluster = input$Cluster_Method, k(), graph = FALSE)

      if (input$Cluster_Method == "kmeans") {

        Data_CLUSTER <- data.frame(SCORES, METHOD$cluster)
        colnames(Data_CLUSTER)[colnames(Data_CLUSTER) == "METHOD.cluster"] = "Cluster"

      }

      else if (input$Cluster_Method == "pam") {

        Data_CLUSTER <- data.frame(SCORES, METHOD$clustering)
        colnames(Data_CLUSTER)[colnames(Data_CLUSTER) == "METHOD.clustering"] = "Cluster"

      }

      else if (input$Cluster_Method == "clara") {

        Data_CLUSTER <- data.frame(SCORES, METHOD$clustering)
        colnames(Data_CLUSTER)[colnames(Data_CLUSTER) == "METHOD.clustering"] = "Cluster"

      }

      else if (input$Cluster_Method == "fanny") {

        Data_CLUSTER <- data.frame(SCORES, METHOD$clustering)
        colnames(Data_CLUSTER)[colnames(Data_CLUSTER) == "METHOD.clustering"] = "Cluster"

      }

      Data_CLUSTER_ordered <- Data_CLUSTER[order(Data_CLUSTER$Label), ]

      write.csv(subset(Data_CLUSTER_ordered, select = c("Sample_ID", "Label", "UTM_Est", "UTM_Nord", "Cluster")), file, row.names = FALSE, quote = FALSE)

    }

    else if (input$Perform_PCA == FALSE) {

    METHOD <- eclust(Data()[, -c(1,2,3,4)], FUNcluster = input$Cluster_Method, k(), graph = FALSE)

    if (input$Cluster_Method == "kmeans") {

      Data_CLUSTER = data.frame(Data(), METHOD$cluster)
      colnames(Data_CLUSTER)[colnames(Data_CLUSTER) == "METHOD.cluster"] = "Cluster"

    }

    else if (input$Cluster_Method == "pam") {

      Data_CLUSTER = data.frame(Data(), METHOD$clustering)
      colnames(Data_CLUSTER)[colnames(Data_CLUSTER) == "METHOD.clustering"] = "Cluster"

    }

    else if (input$Cluster_Method == "clara") {

      Data_CLUSTER = data.frame(Data(), METHOD$clustering)
      colnames(Data_CLUSTER)[colnames(Data_CLUSTER) == "METHOD.clustering"] = "Cluster"

    }

    else if (input$Cluster_Method == "fanny") {

      Data_CLUSTER = data.frame(Data(), METHOD$clustering)
      colnames(Data_CLUSTER)[colnames(Data_CLUSTER) == "METHOD.clustering"] = "Cluster"

    }


    Data_CLUSTER_ordered = Data_CLUSTER[order(Data_CLUSTER$Label), ]

    write.csv(subset(Data_CLUSTER_ordered, select = c("Sample_ID", "Label", "UTM_Est", "UTM_Nord", "Cluster")),
              file,
              row.names = FALSE,
              quote = FALSE)


    }

  }

)


} # final bracket to close SERVER
