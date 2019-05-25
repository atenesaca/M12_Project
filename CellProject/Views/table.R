tableView <- function(){
  ######## PANEL PLOTS  ########
  
  # Conditional panel which show gds data in a table
  conditionalPanel(
    
    ## DATA PANEL
    # if sidebar id is equal with "data" show page
    condition= "input.sidebar == 'data'",
    
    # Create a tab panel in body
    tabsetPanel(type="tabs",
                # Create tab and set title
                tabPanel(
                  "RAW gds data",
                  DT::dataTableOutput("rawGds") # create a output function to print the table
                ),
                tabPanel(
                  "RMA gds data",
                  DT::dataTableOutput("rmaGds") # create a output function to print the table
                )
    )
  )
}