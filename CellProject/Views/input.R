inputView <- function(){
  ###### PANEL QUERIES  ######
  conditionalPanel(
    
    # show input box when id sidebar is equal with id "queries"
    condition = "input.sidebar == 'queries'",
    
    ####
    # Create a fuild row where we can add boxes or columns
    fluidRow(
      ## BOX QUERY
      # Create a box that contains text input where user can enter a Geo Id
      box(
        width = 4,
        # add a text input
        textInput("queryId", "Input GEO ID", ""),
        # add a action button
        actionButton("search", "Search ID")
      ),
      
      ## BOX GEO FROM DDBB
      # Create a box which contains a reactive function which
      # can be rendered to others inputs or outputs objects
      box(
        width = 4,
        uiOutput("gds_db")
      ),
      ## BOX FILE
      # create a box which contain an input file and button
      box(
        width = 4,
        fileInput("cellFile", "Select GDS file", accept = ".gz"),
        actionButton("upload", "Upload file")
      )
    ),
    
    # render function where print error of queries
    textOutput("queryError"),
    
    ## BOX PHENO CHOICE
    # create a reactive box
    fluidRow(
      
      # shows a loader while app download GDS experiment
      withLoader(uiOutput("ExpDesc"), loader = "loader6"),
      
      # output ui to select phenodata columns
      uiOutput("selectPhenoData")
    )
  )
}