ui <- dashboardPage(
  
  dashboardHeader(
    title = "GDS Analysis"
  ),
  dashboardSidebar(
    sidebarView()
  ),
  dashboardBody(
    tags$script(HTML("$('body').addClass('fixed');")),
    mainView(),
    inputView(),
    plotView(),
    tableView(),
    settingsView(),
    uiChangeThemeOutput()
  )
)