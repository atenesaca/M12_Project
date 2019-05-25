plotView <- function(){
  ######## PANEL PLOTS  ###### 
  conditionalPanel(
    
    
    # if sidebar id is equal with "plot" show page
    condition= "input.sidebar == 'plot'",
    
    # Create a panel of tab in body to show plots
    tabsetPanel(type="tabs",
                tabPanel("Quality Control",
                         fluidRow(
                           h3("Statistic Representation of Sample Intensities", align="center"),
                           column(6,plotOutput("plot.raw1", height = 650)),
                           column(6,plotOutput("plot.rma1", height = 650))
                         ),
                         fluidRow(
                           br(),
                           column(width=8, offset=2, align="center",
                                  h3("Samples Aggregations"),
                                  selectInput("select.dendro", "Choose method for dendrogram: ",
                                              choices = c("euclidean", "maximum", "manhattan",
                                                          "canberra", "binary", "minkowski"))),
                           column(6, plotOutput("dendro.raw", height = 650)),
                           column(6, plotOutput("dendro.rma", height = 650))
                         ),
                         fluidRow(
                           
                           br(),
                           # reactive function which show MA plot
                           column(width=8, offset=2, align="center",
                                  h3("Differences Between Sample Measurements"),
                                  plotOutput("plot.MA", height = 650)
                           )
                         )
                ),
                # Create tab and add a title
                tabPanel("Gene Expression",
                         # reactive function which show a heat map plot
                         fluidRow(
                           h3("HeatMap", align="center"),
                           column(width = 9,
                                  d3heatmapOutput("plot.heatMap", height = "80vh")
                           ),
                           column(width=3,
                                  sliderInput("sli_heatmap", label = "Please select the 
                                              amount of genes to display in heatmap", min = 15,
                                              max = 300, value = 30)
                           )
                           ),
                         # reactive function which show a plot of genes
                         fluidRow(
                           br(),
                           h3("Gene Expression // Search by Gene Symbol", align="center"),
                           column(9, plotlyOutput("plot.gene1",  height = "80vh")),
                           column(3, searchInput("searchGene", label="Search gene to evaluate",
                                                 btnReset = icon("remove"), btnSearch = icon("search")),
                                  radioButtons("radioGene", "Select Raw or Normalized data:",
                                               choices=c("Raw", "Normalized")))
                           
                         ),
                         # reactive function which show a volcano plot
                         fluidRow(
                           br(),
                           br(),
                           plotOutput("plot.volcano", height = 800)
                           )
                         )
                )
  )
}