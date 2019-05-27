plotView <- function(){
  
  ######## PANEL PLOTS  ######## 
  conditionalPanel(
    
    # if sidebar id is equal with "plot" show page
    condition= "input.sidebar == 'plot'",
    
    uiOutput("selectPhenoDataAgain"),
    
    # Create a panel of tab in body to show plots
    tabsetPanel(type="tabs",
                
                tabPanel("Quality Control",
                         fluidRow(
                           box(title = "Statistic Representation of Sample Intensities",
                               status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                               column(6,plotOutput("plot.raw1", height = 650)),
                               column(6,plotOutput("plot.rma1", height = 650))
                               )
                           ),
                         fluidRow(
                           br(), # HTML new line
                           box(title = "Samples Aggregations", status = "primary",
                               solidHeader = TRUE, collapsible = TRUE, width = 12,
                               column(width=8, offset=2, align="center",
                                      selectInput("select.dendro", "Choose method for dendrogram: ",
                                                  choices = c("euclidean", "maximum", "manhattan", 
                                                              "canberra", "binary", "minkowski"))
                               ),
                               column(6, plotOutput("dendro.raw", height = 650)),
                               column(6, plotOutput("dendro.rma", height = 650))
                           )
                         ),
                         fluidRow(
                           
                           br(),
                           box(title = "Samples Aggregations", status = "primary",
                               solidHeader = TRUE, collapsible = TRUE, width = 12,
                           column(width=8, offset=2, align="center",
                                  plotOutput("plot.MA", height = 650))
                           )
                         )
                ),
                # Create tab and add a title
                tabPanel("Gene Expression",
                         fluidRow(
                           uiOutput("settings_toptable")
                         ),
                         fluidRow(
                           box(title = "HEATMAP", status = "primary", solidHeader = TRUE, 
                               collapsible = TRUE, width = 12,
                               column(width = 9,
                                      d3heatmapOutput("plot.heatMap", height = "80vh")),
                               column(width = 3,
                                      br(),
                                      sliderInput("sli_heatmap", label = "Please select the 
                                              amount of genes to display in heatmap", min = 15,
                                                  max = 300, value = 30))
                               )
                           ),
                         # reactive function which show a volcano plot
                         fluidRow(
                           box(title = "VOLCANO", status = "primary", solidHeader = TRUE,
                               collapsible = TRUE, width = 12,
                           br(),
                           br(),
                           plotOutput("plot.volcano", height = 800)
                           )
                         ),
                         # reactive function which show a plot of genes
                         fluidRow(
                           br(),
                           box(title = "Gene Expression // Search by Gene Symbol", status = "primary", solidHeader = TRUE,
                               collapsible = TRUE, width = 12,
                               column(9, plotlyOutput("plot.gene1",  height = "80vh")),
                               column(3, 
                                      searchInput("searchGene", label="Search gene to evaluate",
                                                     btnReset = icon("remove"), btnSearch = icon("search")),
                                      radioButtons("radioGene", "Select Raw or Normalized data:",
                                               choices=c("Raw", "Normalized")))
                               )
                           )
                         )
                )
    )
}