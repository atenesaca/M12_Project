tableView <- function(){
  conditionalPanel(
    condition= "input.sidebar == 'data'",
    fluidRow(
      box(title = "Gene Expression Tables", status = "primary", solidHeader = TRUE,
          collapsible = TRUE, width = 12,
          tabBox(id = "data_table",
                 width = 12,
                 tabPanel("Raw Data",
                          DT::dataTableOutput("rawGds")
                          ),
                 tabPanel("Normalized Data",
                          DT::dataTableOutput("rmaGds"))
                 )
          )
      ),
    fluidRow(
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
}
