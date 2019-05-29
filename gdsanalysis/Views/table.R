tableView <- function(){
  conditionalPanel(
    condition= "input.sidebar == 'data'",
    fluidRow(
      box(title = "GO Table", status = "primary", solidHeader = TRUE,
          collapsible = TRUE, width = 12,
          withLoader(DT::dataTableOutput("go_table"), loader="dnaspin")
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
