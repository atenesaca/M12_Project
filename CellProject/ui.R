ui <- dashboardPage(
  
  ## HEADER
  dashboardHeader(
    title = "GDS Analysis" # put a title in header
  ),
  
  ## SIDEBAR
  # Create element in a left sidebar
  dashboardSidebar(
    
    ### Custom theme
    uiChangeThemeOutput(),
    
    # call to function which show sidebar
    sidebarView()
  ),
  
  ## BODY
  # create elements in body app
  dashboardBody(
    
    # call to function which show the main view of the page
    mainView(),
    
    # call to function which show inputs to search GDS experiments
    inputView(),
    
    # call to function which show plots of the data
    plotView(),
    
    # call to function which show data in a table
    tableView()
  )
)