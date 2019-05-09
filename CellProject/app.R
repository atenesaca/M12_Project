## app.R ##
library(shinydashboard)

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(
    title = "Cell Files"
  ),
  dashboardSidebar(
    #collapsed = T,
    #disable = T
    sidebarMenu(id="sidebar",
                menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("Plot", tabName = "plot", icon = icon("chart-bar"))
    ),
    conditionalPanel(
      condition = "input.sidebar == 'plot'"
    )
  ),
  dashboardBody(
    conditionalPanel(
      condition = "input.sidebar == 'home'",
      
      fluidRow(
        box(
          title ="", solidHeader = T, background = "black", collapsible = T,
          textInput("queryId", "Input query ID", ""),
          actionButton("search", "Search ID")
        ),
        box(
          title ="", solidHeader = T, background = "black", collapsible = T,
          fileInput("celFile", "Upload file", accept = ".zip"),
          actionButton("upload", "Upload file")
        )
      )
    ),
    conditionalPanel(
      condition= "input.sidebar == 'plot'",
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
}

shinyApp(ui, server)
