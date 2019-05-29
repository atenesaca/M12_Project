inputView <- function(){
  ###### PANEL QUERIES  ######
  conditionalPanel(
    condition = "input.sidebar == 'queries'",
    fluidRow(
      box(
        width = 6,
        textInput("queryId", "Input GEO ID", ""),
        actionButton("search", "Search ID")
      ),
      box(
        width = 6,
        tableOutput("gds_db")
      )
    ),
    textOutput("queryError"),
    fluidRow(
      withLoader(uiOutput("ExpDesc"), loader = "dnaspin", proxy.height = "200px")
    )
  )
}