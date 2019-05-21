## app.R ##

#Upload max file size to 20M
options(shiny.maxRequestSize=20*1024^2)
#Show app in port 2020
options(shiny.port = 2020)

#Libraries
library(shinydashboard)
library(Biobase)
library(GEOquery)
library("reshape2")
library(ggplot2)
library(dplyr)
library(limma)
library(annotate)
# library(shinycustomloader)
# library(shinyjs)
library(plotly)
library(d3heatmap)
library(DBI)
library(digest)
# library(EnhancedVolcano)

############################## UI INTERFACE #############################

## DASHBOARD
#Create a shiny dasboard page
ui <- dashboardPage(
  skin = "red", #set header color to red
  
  ## HEADER
  dashboardHeader(
    title = "Gene Chips Analysis" #put a title in header
  ),
  
  ## SIDEBAR
  # Create element in a left sidebar
  dashboardSidebar(
    # Create a menu where we can add multiples menu items
    sidebarMenu(id="sidebar", #set an id to sidebar menu
                #Create an item where we can add an id and an icon
                menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("Plot", tabName = "plot", icon = icon("chart-bar"))
    )
  ),
  
  ## BODY
  #create elements in body app
  dashboardBody(
    
    # Create a conditional panel which olny show his content
    # when sidebar menu id is equal whith menu item id
    conditionalPanel(
      ## PANEL HOME
      # show input box when id sidebar is equal with id "home"
      condition = "input.sidebar == 'home'",
      
      # Create a fuild row where we can add boxes or columns
      fluidRow(
        ## BOX QUERY
        # Create a box that contains text input where user can enter a Geo Id
        box(
          # set title of box to blank space, add header and color to black
          title ="", solidHeader = T, background = "black",
          # make collapsible and add width of 4
          collapsible = T, width = 4,
          # add a text input
          textInput("queryId", "Input query ID", ""),
          # add a action button
          actionButton("search", "Search ID")
        ),
        ## BOX QUERY FROM DDBB
        # Create a box which contains a reactive function which
        # can be rendered to others inputs or outputs objects
        box(
          title ="", solidHeader = T, background = "black",
          collapsible = T, width = 4,
          uiOutput("gds_db")
        ),
        ## BOX FILE
        # create a box which contain an input file and button
        box(
          title ="", solidHeader = T, background = "black",
          collapsible = T, width = 4,
          fileInput("cellFile", "Upload file", accept = ".gz"),
          actionButton("upload", "Upload file")
        )
      ),
      ## BOX PHENO CHOICE
      # create a reactive box
      fluidRow(
        uiOutput("selectPhenoData"),
        uiOutput("ExpDesc")
      )
    ),
    
    ## PANEL PLOTS
    # Conditional panel which show plots when sidebar id is equal with "plot"
    conditionalPanel(
      condition= "input.sidebar == 'plot'",
      # Create a panel of tab in body to show plots
      tabsetPanel(type="tabs",
                  # Create tab and add a title
                  tabPanel("Quality Control",
                           # Create a fuild row with columns
                           fluidRow(
                             # reactive function which show a box plot of raw data
                             column(6,plotOutput("plot.raw1")
                             ),
                             # reactive function which show a box plot of normalized data
                             column(6,plotOutput("plot.rma1")
                             )
                           ),
                           # Create a fuild row with columns
                           fluidRow(
                             # add a HTML line break
                             br(),
                             # reactive function which show MA plot
                             column(width=8, offset=2, align="center",
                                    plotOutput("plot.MA")
                             )
                           )
                           
                  ),
                  # Create tab and add a title
                  tabPanel("Gene Expression",
                           # reactive function which show a heat map plot
                           fluidRow(
                             d3heatmapOutput("plot.heatMap")
                           ),
                           # reactive function which show a plot of genes
                           fluidRow(
                             br(),
                             plotlyOutput("plot.gene1")
                           ),
                           # reactive function which show a volcano plot
                           fluidRow(
                             plotOutput("plot.volcano")
                           )
                  )
      )
    )
  )
)

## SERVER
# server function where we can controlle the differents options
# inputed by the user
server <- function(input, output, session) {
  
  ##################################### DATABASE #####################################
  
  # try connection to database cellFiles through user root
  getConnection = tryCatch({
    dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "cellfiles",
      host = "localhost",
      username = "root",
      password = "root")
  },
  error = function(e){ #if connection is invalid, error
    "Error connecting to database"
  })
  
  ##################################### UI THINGS #####################################
  
  # Switch to plot tab when click in Go to Plot button
  observeEvent(input$goToPlot, {
    newtab <- switch(input$sidebar, "home" = "plot","plot" = "home")
    updateTabItems(session, "sidebar", newtab)
  })
  
  # reactive function which render the ui through and id
  # add a box with a select of the columns of the pheno data
  output$selectPhenoData <- renderUI({
    tagList(
      box(title = "Phenotype columns", status = "primary", solidHeader = TRUE,
          collapsible = TRUE, width = 12,
          selectInput("pData", "Choose first pheno: ",
                      choices = colnames(pData(eset.rma()))[2:3]),
          actionButton("goToPlot", "Go to Plot") 
      )
    )
  })
  
  # render an ui which show the description of the gds object
  output$ExpDesc <- renderUI({
    req(gdsObj())
    data <- gdsObj()
    box(title = "Quick Description", status = "primary", solidHeader = TRUE,
        collapsible = TRUE, width = 12,
        p(Meta(data)$description[1])
    )
  })
  
  # render ui which show a select input with default values of GDS id
  output$gds_db <- renderUI({
    if(class(getConnection) == "MySQLConnection"){ #enter is connection is valid
      
      #Close connection when user close app
      on.exit(dbDisconnect(getConnection))
      
      #Save in result of the query in a data frame
      filesDT = dbGetQuery(getConnection, "SELECT dbid FROM files
                           WHERE source = 'Geo Datasets'")
      tagList(
        # create a select with the list of files table from database
        selectInput("selectIdDb","Select GEO Id",filesDT$dbid),
        actionButton("search_db", "Search ID")
      )
    }
    else { #if connection is invalid
      # create a validate panel
      validate(
        # where only show error if getConnection is different of class MySQLConnection
        need(class(getConnection) == "MySQLConnection",
             getConnection)
      )
    }
  })
  
  ##################################### DATA #####################################
  
  ## gdsObj: Reactive event
  # Download GDS object from NCBI Geo and saves into a Geo Object
  gdsObjInput <- eventReactive(input$search, {
    #if query id is empty, show error
    validate(
      need(input$queryId != "", "Please enter a query ID")
    )
    # saves in data the result of getGEO function
    data <- try(getGEO(input$queryId))
    
    # if data class is different of GDS show error
    validate(
      need(class(data) == "GDS", "Please insert a correct GDS ID")
    )
    
    return(data)
  })
  
  ## gdsObj: Reactive event
  # Download GDS obj through the id of the data base
  gdsObjDB <- eventReactive(input$search_db, {
    data <- getGEO(input$selectIdDb)
    return(data)
  })
  
  # if(input$queryId == ""){
  #   output$queryError <- renderText({
  #     "Please enter a query ID"
  #   })
  #   stop()
  # }
  # 
  # if(isClass(data)){
  #   output$queryError <- renderText({
  #     "Please insert a correct GDS ID"
  #   })
  #   stop()
  # }
  
  ## gdsObjFile: Reactive event
  # create gds obj with the gds file uploaded by the user
  gdsObjFile <- eventReactive(input$upload, {
    
    #if uploaded file is empty show error to user
    validate(
      need(length(input$cellFile) != 0 , "Upload a GDS.gz file")
    )
    
    #Unzip .gz file in temp directory and rename it to the name of the uploaded file
    #without the .gz extention
    dest_dir = paste(tempdir(), "/", tools::file_path_sans_ext(input$cellFile$name),
                     sep = "")
    # unzip .gz file in dest_dir and overwrite it
    gunzip(input$cellFile$datapath, dest_dir, overwrite=TRUE)

    #Convert .soft file to GDS object
    data <- try(getGEO(filename = dest_dir))
    
    # if data class is different of GDS show error
    validate(
      need(class(data) == "GDS", "Please insert a correct GDS ID")
    )

    return(data)
  })
  
  ## gdsObj: r function
  # Function which manages to return the same value but differents ways
  gdsObj <- function(){
    # return gds obj only is their class is equal to GDS class
    
    if(class(gdsObjInput()) == "GDS"){
      return(gdsObjInput())
    }
    if(class(gdsObjDB()) == "GDS"){
      return(gdsObjDB())
    }
    if(class(gdsObjFile()) == "GDS"){
      return(gdsObjFile())
    }
  }
  
  ## eset.raw
  # transform a GDS obj to Expression set obj
  eset.raw <- reactive({
    # need the function which return the gds obj
    req(gdsObj)
    GDS2eSet(gdsObj(), do.log2=FALSE)
  })
  
  ## eset.rma
  # transform a GDS obj to Expression set obj and normalized it
  eset.rma <- reactive({
    # need the function which return the gds obj
    req(gdsObj)
    GDS2eSet(gdsObj(), do.log2=TRUE)
  })
  
  ## facLevel
  # Extract factor levels of the pheno data from the second column of expSet
  facLevel <- reactive({
    # need the user input a select value
    req(input$pData)
    # create the pheno data of normalized values an return the values
    # of the column selected by user
    pData(eset.rma())[,input$pData]
  }) 
  
  ## genes.raw
  # create a data frame with the raw data
  genes.raw <- reactive({
    # need functions where create war dara and factor levels
    req(eset.raw(), facLevel())
    
    # saves the sample name of the raw data
    sample <- sampleNames(eset.raw())
    
    #Extract expression and error measurements from raw data
    y <- exprs(eset.raw())
    
    # call function which create factor levels of pheno data
    facLevel<- facLevel()
    
    # convert objects to molten data frame
    g<- melt(y, varnames = c( "probe", "sample"))
    
    # create a new column in molten dataframe where add factor level
    # to each column if the value of sample matchs with value of molten sample
    g$facLevel <- facLevel[match(g$sample, sample)]
    
    return(g)
  })
  
  ## genes.rma
  # create a data frame with the raw data
  genes.rma <- reactive({
    # need functions where create war dara and factor levels
    req(eset.rma(), facLevel())
    
    # saves the sample name of the norm data
    sample <- sampleNames(eset.rma())
    
    #Extract expression and error measurements from norm data
    y <- exprs(eset.rma())
    
    # call function which create factor levels of pheno data
    facLevel<- facLevel()
    
    # convert objects to molten data frame
    g<- melt(y, varnames = c( "probe", "sample"))
    
    # create a new column in molten dataframe where add factor level
    # to each column if the value of sample matchs with value of molten sample
    g$facLevel <- facLevel[match(g$sample, sample)]
    
    return(g)
  })
  
  ## ebayes
  # data for the heatmap (normalized)
  ebayes <- reactive({
    #need the factor levels function and normalized data 
    req(facLevel(), eset.rma())
    
    #saves the value of expression data 
    y <- exprs(eset.rma())
    
    #factor levels of the column selected by user
    g <- facLevel()
    
    # create a matrix which create columns where each factor level
    # has positive value in his position
    design <- model.matrix(~factor(g))
    
    # create a linear model where add the value of each feature
    # to the fators levels of design
    fit <- lmFit(y, design)
    
    return(eBayes(fit))
  })
  
  ranked <- reactive({
    # require ebayes function()
    req(ebayes())
    e <- ebayes()
    
    # Extract the top ranked genes values from the linear model
    # @adjust="fdr" control the false discovery rate
    return(topTable(e, coef=2, adjust="fdr", n=150))
  })
  
  ## labColNames
  # return factor levels as a character vector
  labColNames<- reactive({
    #require this function
    req(facLevel())
    g <- facLevel()
    g <- as.character(g)
    return(g)
  }) 
  
  ##################################### PLOTS  ##################################### 
  
  ########### QUALITY PLOTS ##############
  
  ### Plots raw
  # reactive function that render the plot to create a bar plot
  # of the raw data of the GDS obj
  output$plot.raw1 <- renderPlot({
    # require this function
    req(genes.raw())
    
    rawData <- genes.raw()
    
    # Create a plot with the raw genes rawData
    # @x is the value of the column sample from rawData
    # @y is the value of the column value from rawData
    # @fill assign color based on the factor levels
    # @geom_boxplor type of plot is box
    # @theme rotate the text 90 degrees
    ggplot(rawData, aes(x=sample, y=value, fill=facLevel)) + geom_boxplot() +
      theme(axis.text.x = element_text(angle = 90))
  })
  
  #### Plots normalized
  # reactive function that render the plot to create a bar plot
  # of the normalized data of the GDS obj
  output$plot.rma1 <- renderPlot({
    # requiere this function
    req(genes.rma())
    
    normData <- genes.rma()
    
    # Create a plot with the norm genes normData
    # @x is the value of the column sample from normData
    # @y is the value of the column value from normData
    # @fill assign color based on the factor levels
    # @geom_boxplor type of plot is box
    # @theme rotate the text 90 degrees
    ggplot(normData, aes(x=sample, y=value, fill=facLevel)) + geom_boxplot() +
      theme(axis.text.x = element_text(angle = 90))
  })
  
  ### plot ma
  # reactive function that render the plot to create a MA plot
  # of the normalized data of the GDS obj
  output$plot.MA <- renderPlot({
    # require this function
    req(eset.rma())
    g <- facLevel() # factors levels
    Index <- as.numeric(g) # numeric factor levels
    y <- exprs(eset.rma()) # expression data of norm gds obj
    
    # calculate mean of row where column
    # and substract the same column where 
    #### TO DO ####
    d <- rowMeans(y[,Index==2]) - rowMeans(y[, Index==1])
    
    # calculate mean of the row values
    a <- rowMeans(y)
    
    # create a smoothed color density representation of a scatterplot
    # @Main = main title of the plot
    # @xlab = label name of the axis
    # @ylab = label name of the axis
    # @abline h  create horitzontal line at values 1 and -1
    smoothScatter(a, d, main="MA plot", xlab="A", ylab="M")
    abline(h=c(-1,1), col="red")
  })
  
  
  ######## GENE EXPRESSION ##########
  ### Heat map plot
  # render an interactive heat map plot
  output$plot.heatMap <- renderD3heatmap({
    # requiere this functions
    req(eset.rma())
    
    #saves the expression genes of the normalized data
    expSet<- exprs(eset.rma())
    
    # saves the 150 genes of the linear model
    ranked <- ranked()
    labColNames<- labColNames() # name of the columns
    # select the values of the expression genes where the values is equal
    # with the 150 genes from linear model
    expDataName = expSet[row.names(ranked),]
    
    # create an interactive heatmap
    # @expDataName[1:20,] = select the first 20 values to create the heat map
    # @labCol = names of the columns to use
    # @cexRow = positive numbers
    # @dendogram = number of coluns to draw
    # @k_row = number of groups to color the branches in the row
    # @k_col = number of groups to color the branches in the row
    d3heatmap(expDataName[1:20,], labCol = labColNames, cexRow=0.5, dendrogram="both",
              k_row=3, k_col=3)
  })
  
  ### Plot individual gene expression
  output$plot.gene1 <- renderPlotly({
    req(eset.rma(), facLevel())
    y<- exprs(eset.rma())
    g<- y[23,]
    group <- facLevel()
    dt <- data.frame("Index"=group, "Expr"=g)
    ggplot(dt, aes(x=group, y=Expr)) + geom_jitter(aes(colour=group)) +
      theme(legend.position = "none")
  })
  
  ## Venn diagram
  # reactive function which render 
  output$plot.volcano <- renderPlot({
    eb <- ebayes()
    volcanoplot(eb, coef=2, highlight=5)
  })
  
}

shinyApp(ui, server)

# ##################################### LOG IN #####################################
# 
# output$userPanel <- renderUI({
#   tagList(
#     actionLink("login", "Log in", icon("sign-in-alt"))
#   )
# })
# 
# observeEvent(input$login, {
#   showModal(modalDialog(
#     title = "Log in", footer = modalButton("Close"),
#     textInput("username","", "", "100%", placeholder = "User name"),
#     passwordInput("password","","","100%", placeholder = "Password"),
#     actionButton("logUser","Log in")
#   ))
# })
# 
# observeEvent(input$logUser,{
#   query = sprintf("SELECT * FROM users where name = ('%s') and password = ('%s')",
#                   input$username, input$password)
#   user = dbGetQuery(conn, query)
#   
#   output$userPanel <- renderUI({
#     removeModal()
#     tagList(
#       sidebarUserPanel(
#         name = user$name,
#         subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
#       )
#     )
#   })
# })