
OliveR_UI <- fluidPage(
            fluidRow(
              column(width = 12,
  p(h3(strong(em("OliveR")), strong("1.5"), "- A statistical software for multivariate data analysis and geographic display of results", align = "center"))
              )
            ),
br(),
    fluidRow(
      column(width = 2,
        actionButton(inputId = "import_data", label = h4(icon(name = "upload"), "Import the data"), width = "100%"),
br(),
br(),
conditionalPanel(condition = "input.import_data > 0",
        radioButtons(inputId = "analysis_type", label = h5(icon(name = "book"), "Type of data"), choices = list("Real data" = 1, "Genetic data" = 2), selected = 1),
  conditionalPanel(condition = "input.analysis_type == 1",
        fileInput(inputId = "Dataframe", label = h5(icon(name = "file"), "Input file (.csv)"), accept = c(".csv"))
  ),
  conditionalPanel(condition = "input.analysis_type == 2",
          mainPanel(h5("Work in progress.."))
  )
),
conditionalPanel(condition = "output.fileUploaded_Normal | output.fileUploaded_Genetic",
          h5(icon(name = "folder-open"), "Geographic files directory"),
          shinyDirButton(id = "shape_folder", label = "Select..", title = "Choose a directory"),
          verbatimTextOutput(outputId = "shape_folder_datapath"),
  conditionalPanel(condition = "output.shape_folder_datapath != 0",
          selectInput(inputId = "shapefiles", label = h5(icon(name = "list"), "Number of shapefiles"), choices = list("-----" = 0, "1" = 1, "2" = 2, "3" = 3), selected = 0)
  ),
conditionalPanel(condition = "input.shapefiles != 0",
          textInput(inputId = "shapefile_name_1", label = h5(icon(name = "file-text"), "Name of shapefile n° 1"), value =  ""),
  conditionalPanel(condition = "input.shapefiles == 2 | input.shapefiles == 3",
          textInput(inputId = "shapefile_name_2", label = h5(icon(name = "file-text"), "Name of shapefile n° 2"), value = "")
  ),
  conditionalPanel(condition = "input.shapefiles == 3",
            textInput(inputId = "shapefile_name_3", label = h5(icon(name = "file-text"), "Name of shapefile n° 3"), value = "")
  )
),
br(),
  conditionalPanel(condition = "input.shapefile_name_1 || input.shapefile_name_2 || input.shapefile_name_3",
            actionButton(inputId = "Start_analysis", label = h4(icon(name = "check"), "Start the analysis"), width = "100%")
  )
)
#helpText(strong("Note:"), "The file to be imported must be in .csv format and the first 4 columns must contain default variables. The first, whose name must be 'Sample_ID', must contain the sample names, while the second one, to be renamed 'Label', is a categorical variable and contains the membership groups of each statistical unit. Finally, the last two must contain UTM geographic coordinates and the headers must be 'UTM_Est' and 'UTM_Nord' respectively. The next columns of the file must contain the genetic loci with the two alleles separated by '/'; indicates missing values with one single 'NA'.")

), # parentesi di chiusura pannello input comandi (colonna larghezza 2)

  conditionalPanel(condition = "input.Start_analysis == 0",
      column(width = 10,
        plotOutput(outputId = "wordcloud", height = "600px", width = "1000px")
      )
  ),

  conditionalPanel(condition = "input.Start_analysis > 0",
      column(width = 10,
  conditionalPanel(condition = "input.analysis_type == 1",
        tabsetPanel(id = "panels_principal_analysis", type = "pills",

          tabPanel(title = h4(icon(name = "table"), "Dataset"),
br(),
        tabsetPanel(type = "pills",
          tabPanel(title = "Table",
br(),
            fluidRow(
              column(width = 12,
                dataTableOutput(outputId = "filetable_A")
              )
            )
          ),
          tabPanel(title = "Summary",
                   br(),
                   fluidRow(
                     column(width = 12,
                            verbatimTextOutput (outputId = "summary_data")
                     )
                   )
          ),
          tabPanel(title = "Stats",
br(),
            fluidRow(
              column(width = 12,
                dataTableOutput(outputId = "stats")
              )
            )
          ),
          tabPanel(title = "Images",
br(),
            fluidRow(
              column(width = 3, selectInput(inputId = "sample_name", label = h5("Name of the sample"), choices = "")
              )
            ),
br(),
            fluidRow(
              column(width = 4,
                shinyDirButton(id = "image_1_folder", label = "Select the image folder n° 1", title = "Choose a directory"),
br(),
br(),
                imageOutput(outputId = "image_1")
              ),
              column(width = 4,
                shinyDirButton(id = "image_2_folder", label = "Select the image folder n° 2", title = "Choose a directory"),
br(),
br(),
                imageOutput(outputId = "image_2")
              ),
              column(width = 4,
                shinyDirButton(id = "image_3_folder", label = "Select the image folder n° 3", title = "Choose a directory"),
br(),
br(),
                imageOutput(outputId = "image_3")
              )
            ),
br(),
br()
          ),
          tabPanel(title = "Subset and merge",
br(),
            h4(strong("Subset of the data")),
            h5("Select the variable of the principal dataset"),
            fluidRow(
              column(width = 4, selectInput(inputId = "Subset_Variable", label = NULL, choices = "", multiple = TRUE)
              ),
              column(width = 2, downloadButton(outputId = "download", label = "Download Subset")
              )
            ),
br(),
            h4(strong("Merge the data")),
            h5("Choose the dataset to merge"),
            fluidRow(
              column(width = 4,
                     fileInput(inputId = "Dataframe_M", label = NULL, accept = c(".csv"))
              ),
              column(width = 2,
                     conditionalPanel(condition = "output.fileUploaded_Merge",
                                      downloadButton(outputId = "download_M", label = "Download Merge")
                     )
              )
            )
        )
            )
          ),
          tabPanel(title = h4(icon(name = "bar-chart"), "Plot"),
br(),
            fluidRow(
              column(width = 2, selectInput(inputId = "Variable_1", label = h5("X Variable"), choices = "")
              ),
              column(width = 2, selectInput(inputId = "Variable_2", label = h5("Y Variable"), choices = "")
              ),
  conditionalPanel(condition = "input.Variable_1 != 'Sample_ID' && input.Variable_1 != 'Label' && input.Variable_2 != 'Sample_ID' && input.Variable_2 != 'Label'",
              column(width = 2, selectInput(inputId = "Variable_3", label = h5("Z Variable"), choices = "")
              )
  ),
  conditionalPanel(condition = "input.Variable_1 != 'Sample_ID' && input.Variable_2 != 'Sample_ID' && input.Variable_1 != 'Label' && input.Variable_2 != 'Label' && input.Variable_3 == '-----'",
              column(width = 2, h5("Pearson coefficient"), verbatimTextOutput(outputId = "pearson")
              )
  )#,
              #column(width = 6, helpText(strong("Tip:"), "To define a scatter plot, select two real variables from the drop-down menus. If you want to know the number of static units in each group use the numeric variable 'Sample_ID' and the categorical variable. Both the numeric and categorical variables can be combined with a real variable to obtain, respectively, barplot, boxplot or geoplot.")
              #)
            ),
  conditionalPanel(condition = "input.Variable_1 == 'UTM_Est' && input.Variable_2 == 'UTM_Nord'",
            fluidRow(
              column(width = 2,
                colourInput(inputId = "Colours_1", label = h5("Shapefile n° 1"), value = "#D6FFD6", allowTransparent = TRUE),
                checkboxInput(inputId = "polygon_name_1", label = "Polygon name"),
br(),
  conditionalPanel(condition = "input.shapefiles == 2 | input.shapefiles == 3",
                colourInput(inputId = "Colours_2", label = h5("Shapefile n° 2"), value = "#E0F7FF", allowTransparent = TRUE),
                checkboxInput(inputId = "polygon_name_2", label = "Polygon name")
  ),
br(),
  conditionalPanel(condition = "input.shapefiles == 3",
                colourInput(inputId = "Colours_3", label = h5("Shapefile n° 3"), value = "#FFFFF0", allowTransparent = TRUE),
                checkboxInput(inputId = "polygon_name_3", label = "Polygon name")
  ),
br(),
br(),
                checkboxInput(inputId = "google_1", label = h5(icon(name = "map"), "Google Maps"), width = "100%")
              ),
              column(width = 10,
    conditionalPanel(condition = "input.google_1 == true",
                google_mapOutput(outputId = "google_map_1", height = "650px")
    ),
    conditionalPanel(condition = "input.google_1 == false",
                 plotlyOutput(outputId = "geoplot_1", width = "930px", height = "820px"), align = "right"
    )
              )
            )
  ),
  conditionalPanel(condition = "input.Variable_1 != 'UTM_Est' || input.Variable_2 != 'UTM_Nord'",
            plotlyOutput(outputId = "plot", height = "600px")
  )
          ),
          tabPanel(title = h4(icon(name = "line-chart"), "Linear Models"),
br(),
            tabsetPanel(type = "pills",
              tabPanel(title = "ANOVA",
br(),
            fluidRow(
               column(width = 2, selectInput(inputId = "Variable_Y", label = h5("Y Variable"), choices = "")
               )
             ),
             fluidRow(
               column(width = 7, h4(strong("Levene's Test")),
                      verbatimTextOutput(outputId = "levene")
               ),
               column(width = 5, h4(strong("ANOVA Test")),
                      verbatimTextOutput(outputId = "anova")
               )
             ),
             fluidRow(
               column(width = 12, plotlyOutput(outputId = "anova_plot", height = "600px")
               )
             ),
br(),
             fluidRow(
               column(width = 12, h4(strong("Post Hoc Test")),
                      verbatimTextOutput(outputId = "bonferroni")
              )
            )
          )
        )
          ),
          tabPanel(title = h4(icon(name = "arrows"), "PCA"),
br(),
            fluidRow(
              column(width = 1, radioButtons(inputId = "cov_cor", label = h5("Matrix"), choices = c("Correlation", "Covariance"), selected = "Correlation")
              ),
  conditionalPanel(condition = "input.panels_PCA == 'panel_Screeplot'",
              column(width = 1, checkboxInput(inputId = "choose_variance", label = "Variance explained", value = FALSE)
              )
  ),
  conditionalPanel(condition = "input.panels_PCA == 'panel_Screeplot' && input.choose_variance == false",
              column(width = 2, selectInput(inputId = "PCs", label = h5("Number of PCs"), choices = "")
              )
  ),
  conditionalPanel(condition = "input.panels_PCA == 'panel_Loadings' || input.panels_PCA == 'panel_PCs_Plot'",
                   column(width = 2, selectInput(inputId = "PCs_B", label = h5("Number of PCs"), choices = "")
                   )
  )

            ),
        tabsetPanel(id = "panels_PCA", type = "pills",
          tabPanel(title = "Summary", value = "panel_Summary",
br(),
            fluidRow(
              column(width = 12,
                     verbatimTextOutput (outputId = "summary_PCA")
              )
            )
          ),
          tabPanel(title = "Screeplot", value = "panel_Screeplot",
br(),
              fluidRow(
                column(width = 10, plotlyOutput(outputId = "screeplot", height = "600px")
                ),
  conditionalPanel(condition = "input.choose_variance == true",
                column(width = 2, checkboxGroupInput(inputId = "Box_Var", label = h5("Variance explained"), choices = list("70%" = 1, "75%" = 2, "80%" = 3, "85%" = 4, "90%" = 5, "95%" = 6), selected = NULL)
                )
  )
              )
          ),
          tabPanel(title = "Loadings", value = "panel_Loadings",
br(),
              fluidRow(
                column(width = 12, plotlyOutput (outputId = "loadings", height = "600px")
                )
              )
          ),
          tabPanel(title = "PCs Plot", value = "panel_PCs_Plot",
br(),
              fluidRow(
                column(width = 12, plotlyOutput(outputId = "PCs_plot", width = "100%", height = "600px")
                )
              )
          )
        )
          ),
          tabPanel(title = h4(icon(name = "th-large"), "Cluster Analysis"),
br(),
            fluidRow(
              column(width = 1, checkboxInput(inputId = "Perform_PCA", label = "PCA", value = FALSE)
              ),
  conditionalPanel(condition = "input.Perform_PCA == true",
              column(width = 1, radioButtons(inputId = "cov_cor_2", label = h5("Matrix"), choices = c("Correlation", "Covariance"), selected = "Correlation")
              ),
              column(width = 2, selectInput(inputId = "PCs_2", label = h5("Number of PCs"), choices = "")
              )
  ),
              column(width = 2, selectInput(inputId = "Cluster_Method", label = h5("Partitioning methods"), choices = list("K-Means" = "kmeans", "PAM" = "pam", "Clara" = "clara", "Fanny" = "fanny"), selected = "K-Means")
              ),
  conditionalPanel(condition = "input.panels_cluster_analysis == 'panel_Table' || input.panels_cluster_analysis == 'panel_Silhouette' || input.panels_cluster_analysis == 'panel_Geoplot'",
              column(width = 2, numericInput(inputId = "Cluster_Count", label = h5("Cluster count"), value = 3, min = 2, max = 9)
              )
  ),
  conditionalPanel(condition = "input.panels_cluster_analysis == 'panel_Table' || input.panels_cluster_analysis == 'panel_Silhouette' || input.panels_cluster_analysis == 'panel_Geoplot'",
              column(width = 2, h5("Table of cluster"), downloadButton(outputId = "download_Cluster", label = "Download")
              )
  )
            ),
        tabsetPanel(id = "panels_cluster_analysis", type = "pills",
          tabPanel(title = "WSS Plot",
br(),
            fluidRow(
              column(width = 12, plotlyOutput(outputId = "wss", height = "600px")
              )
            )
          ),
          tabPanel(title = "Gap Statistic",
br(),
            fluidRow(
  conditionalPanel(condition = "input.Perform_PCA == true && input.PCs_2 == '1'",
              column(width = 12, textOutput(outputId = "error_gap_statistics"),
                                 tags$head(tags$style("#error_gap_statistics{color: red;}"))
              )
  ),
  conditionalPanel(condition = "input.Perform_PCA == false || input.Perform_PCA == true && input.PCs_2 != '1'",
              column(width = 12, plotlyOutput(outputId = "gap_statistics", height = "600px")
              )
  )
            )
          ),
          tabPanel(title = "Table Plot", value = "panel_Table",
br(),
            fluidRow(
              column(width = 12, plotlyOutput(outputId = "tabplot", height = "550px")

              )
            )
          ),
          tabPanel(title = "Silhouette Plot", value = "panel_Silhouette",
br(),
            fluidRow(
              column(width = 12, plotlyOutput(outputId = "silplot", height = "550px")
              )
            )
          ),
          tabPanel(title = "Geoplot", value = "panel_Geoplot",
br(),
            fluidRow(
              column(width = 2,
                colourInput(inputId = "Colours_2_1", label = h5("Shapefile n° 1"), value = "#D6FFD6", allowTransparent = TRUE),
                checkboxInput(inputId = "polygon_name_2_1", label = "Polygon name"),
br(),
  conditionalPanel(condition = "input.shapefiles == 2 | input.shapefiles == 3",
                colourInput(inputId = "Colours_2_2", label = h5("Shapefile n° 2"), value = "#E0F7FF", allowTransparent = TRUE),
                checkboxInput(inputId = "polygon_name_2_2", label = "Polygon name")
  ),
br(),
  conditionalPanel(condition = "input.shapefiles == 3",
                colourInput(inputId = "Colours_2_3", label = h5("Shapefile n° 3"), value = "#FFFFF0", allowTransparent = TRUE),
                checkboxInput(inputId = "polygon_name_2_3", label = "Polygon name")
  ),
br(),
br(),
                h5("Adjusted Rand Index"),
                verbatimTextOutput(outputId = "adjusted_rand_index"),
br(),
br(),
                checkboxInput(inputId = "google_2",
                              label = h5(icon(name = "map"), "Google Maps"),
                              width = "100%")
              ),
              column(width = 10,
  conditionalPanel(condition = "input.google_2 == true",
                  google_mapOutput(outputId = "google_map_2", height = "650px")
  ),
  conditionalPanel(condition = "input.google_2 == false",
                  plotlyOutput(outputId = "geoplot_2", width = "930px", height = "820px"), align = "right"
  )
              )
            )
          )
        )
          )
      )
  ), # parentesi chiusura CONDITIONAL PANEL con input.analysis_type = 1

############################ GENETIC DATA
  conditionalPanel(condition = "input.analysis_type == 2",
        mainPanel(h5("Work in progress.."))

  )
      ) # parentesi di chiusura pannello output principali (colonna larghezza 10)
  )
    ), # parentesi chiusura fluidRow contenente pannello input comandi e pannello output

br(),

  fluidRow(
    column(width = 4, offset = 8,
      textOutput("current_time")
    )
  )
)
