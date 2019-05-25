## app.R ##

# Upload max file size to 20M
#options(shiny.maxRequestSize=20*1024^2)

# Libraries
source("Library.R")

# themes
source("./moduleChangeTheme.R")

# views
source("Views/sidebar.R")
source("Views/main.R")
source("Views/input.R")
source("Views/plot.R")
source("Views/table.R")

############################## UI INTERFACE #############################

## DASHBOARD
# Create a shiny dasboard page
ui <- dashboardPage(
  
  ## HEADER
  dashboardHeader(
    title = "Gene Chips Analysis" # put a title in header
  ),
  
  ## SIDEBAR
  # Create element in a left sidebar
  dashboardSidebar(
    
    ### Custom theme
    uiChangeThemeOutput(),
    
    sidebarView()
  ),
  
  ## BODY
  # create elements in body app
  dashboardBody(
    
    mainView(),
    
    inputView(),
    
    plotView(),
    
    tableView()
  )
)

## SERVER
# server function where we can controlle the differents options
# inputed by the user
server <- function(input, output, session) {
  
  ##################################### THEME #####################################
  
  callModule(module = serverChangeTheme, id = "moduleChangeTheme")
  
  ##################################### DATABASE #####################################
  
  ## getConnection
  # try connection to database cellFiles through user root
  getConnection = tryCatch({
    dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "cellfiles",
      host = "localhost",
      username = "root",
      password = "root")
  },
  error = function(e){ # if connection is invalid, error
    "Error connecting to database"
  })
  
  ##################################### UI THINGS #####################################
  
  ## Observe event
  # Switch to plot page when click in Go to plot button
  observeEvent(input$goToPlot, {
    newtab <- switch(input$sidebar, "queries" = "plot", "plot" = "queries")
    updateTabItems(session, "sidebar", newtab)
  })

  
  # Switch to data page when click in Go to data button
  observeEvent(input$goToData, {
    newtab <- switch(input$sidebar, "queries" = "data", "data" = "queries")
    updateTabItems(session, "sidebar", newtab)
  })
  
  ## selectPhenoData
  # reactive function which render the ui through and id
  # add a box with a select of the columns of the pheno data
  output$selectPhenoData <- renderUI({
    tagList(
      box(title = "Phenotype columns", status = "primary", solidHeader = TRUE,
          collapsible = TRUE, width = 12,
          selectInput("pData", "Choose first pheno: ",
                      choices = colnames(pData(eSetRma()))[2:3]),
          actionButton("goToPlot", "Go to plot"),
          actionButton("goToData", "Go to data")
      )
    )
  })
  ## ExpDesc
  # render an ui which show the description of the gds object
  output$ExpDesc <- renderUI({
    req(gdsObj())
    
    clearError() # Clear message error
    
    box(title = "Quick Description", status = "primary", solidHeader = TRUE,
        collapsible = TRUE, width = 12,
        p(Meta(gdsObj())$description[1])
    )
  })
  
  ## gds_db
  # render ui which show a select input with default values of GDS id
  output$gds_db <- renderUI({
    if(class(getConnection) == "MySQLConnection"){ # enter is connection is valid
      
      # Close connection when user close app
      on.exit(dbDisconnect(getConnection))
      
      # Save in result of the query in a data frame
      filesDT = dbGetQuery(getConnection, "SELECT dbid FROM files
                           WHERE source = 'Geo Datasets'")
      tagList(
        # create a select with the list of files table from database
        selectInput("selectIdDb","Select GEO Id",filesDT$dbid),
        actionButton("search_db", "Search ID")
      )
    }
    else { # if connection is invalid
      # create a validate panel
      validate(
        # where only show error if getConnection is different of class MySQLConnection
        need(class(getConnection) == "MySQLConnection",
             getConnection)
      )
    }
  })
  
  ## rawGds
  # render raw data frame to table with DT package
  output$rawGds <- DT::renderDataTable({
    req(genes.raw()) # required function
    DT::datatable(genes.raw(), class = "cell-border stripe")
  })
  
  ## rmaGds
  # render normalized data frame to table with DT package
  output$rmaGds <- DT::renderDataTable({
    req(genes.rma()) # required function
    DT::datatable(genes.rma() , class = "cell-border stripe")
  })
  
  ## clearError
  # set output errorto blank
  clearError <-  function(){
    output$queryError <- renderText({
      ""
    })
  }
  
  ## idError
  # show message error from incorrect geo id
  idError <- function(){
    # if data class is different of GDS show error
    if(class(data) != "GDS"){
      output$queryError <- renderText({
        "Please insert a correct GDS ID"
      })
      req(data)
    }
  }
  
  ## idEmpty
  # show error when id input is empty
  idEmpty <- function(){
    # if query id is empty, show error
    if(input$queryId == ""){
      output$queryError <- renderText({
        "Please enter a query ID"
      })
      req(input$queryId)
    }
  }
  
  ## emptyFile
  # show message error when input file is empty
  emptyFile <- function(){
    # if uploaded file is empty show error to user
    if(length(input$cellFile) == 0){
      output$queryError <- renderText({
        "Upload a GDS.gz file"
      })
      req(input$cellFile)
    }
  }
  
  ##################################### DATA #####################################
  
  ## gdsObjInput: Reactive event
  # Download GDS object from NCBI Geo and saves into a Geo Object
  gdsObjInput <- eventReactive(input$search, {
    idEmpty() # show error if id is empty
    data <- try(getGEO(input$queryId)) # saves downloaded gds object
    idError() # show error if id in invalid
    return(data)
  })
  
  ## gdsObj: Reactive event
  # Download GDS obj through the id of the data base
  gdsObjDB <- eventReactive(input$search_db, {
    data <- getGEO(input$selectIdDb) # saves downloaded gds object
    return(data)
  })
  
  ## gdsObjFile: Reactive event
  # create gds obj with the gds file uploaded by the user
  # Unzip .gz file in temp directory and rename it to the name of the uploaded file
  # without the .gz extention
  # unzip .gz file in dest_dir and overwrite it
  # Convert .soft file to GDS object
  gdsObjFile <- eventReactive(input$upload, {
    emptyFile() # show message error
    dest_dir = paste(tempdir(), "/", tools::file_path_sans_ext(input$cellFile$name),
                     sep = "")
    gunzip(input$cellFile$datapath, dest_dir, overwrite=TRUE)
    data <- try(getGEO(filename = dest_dir))
    idError() # show error if id in invalid
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
  
  ## eSetRaw
  # transform a GDS obj to Expression set obj
  # need the function which return the gds obj
  eSetRaw <- reactive({
    req(gdsObj)
    GDS2eSet(gdsObj(), do.log2=FALSE)
  })
  
  ## eSetRma
  # transform a GDS obj to Expression set obj and normalized it
  # need the function which return the gds obj
  eSetRma <- reactive({
    req(gdsObj)
    GDS2eSet(gdsObj(), do.log2=TRUE)
  })
  
  ## facLevel
  # Extract factor levels of the pheno data from the second column of expSet
  # need the user input a select value
  # create the pheno data of normalized values an return the values
  # of the column selected by user
  facLevel <- reactive({
    req(input$pData)
    pData(eSetRma())[,input$pData]
  })
  
  
  ## genes.raw
  # create a data frame with the raw data
  # need functions where create war dara and factor levels
  # saves the sample name of the raw data
  # Extract expression genes from raw data
  # call function which create factor levels of pheno data
  # convert objects to molten data frame
  # create a new column in molten dataframe where add factor level
  # to each column if the value of sample matchs with value of molten sample
  genes.raw <- reactive({
    req(eSetRaw(), facLevel())
    matriz <- eSetRaw()
    facLevel<- facLevel()
    sample <- sampleNames(matriz)
    y <- exprs(matriz)
    gen <- fData(matriz)[,3]
    probenames<-rownames(matriz)
    g<- melt(y, varnames = c( "probe", "sample"))
    g$group <- facLevel[match(g$sample, sample)]
    g$gen <- gen[match(g$probe, probenames)]
    g
  })
  
  ## genes.rma
  # create a data frame with the norm data
  # need functions where create war dara and factor levels
  # saves the sample name of the norm data
  # Extract expression genes from norm data
  # call function which create factor levels of pheno data
  # convert objects to molten data frame
  # create a new column in molten dataframe where add factor level
  # to each column if the value of sample matchs with value of molten sample
  genes.rma <- reactive({
    req(eSetRma(), facLevel())
    matriz <- eSetRma()
    facLevel<- facLevel()
    sample <- sampleNames(matriz)
    y <- exprs(matriz)
    gen <- fData(matriz)[,3]
    probenames<-rownames(matriz)
    g<- melt(y, varnames = c( "probe", "sample"))
    g$group <- facLevel[match(g$sample, sample)]
    g$gen <- gen[match(g$probe, probenames)]
    g
  })
  
  ## ebayes
  # data for the heatmap (normalized)
  # need the factor levels function and normalized data 
  # saves the value of expression data 
  # factor levels of the column selected by user
  # create a matrix which create columns where each factor level
  # has positive value in his position
  # create a linear model where add the value of each feature
  # to the factors levels of design
  ebayes <- reactive({
    req(facLevel(), eSetRma())
    y <- exprs(eSetRma())
    g <- facLevel()
    design <- model.matrix(~ g)
    fit <- lmFit(y, design)
    return(eBayes(fit))
  })
  
  ## ranked
  # reactive function which return the top tanked 150 genes
  ranked <- reactive({
    req(ebayes())
    e <- ebayes()
    t <- topTable(e, coef = 2, adjust="fdr", n=1000)
    t
  })
  
  ## labColNames
  # return factor levels as a character vector
  # require this function
  labColNames<- reactive({
    req(facLevel())
    return(as.character(facLevel()))
  })

  
  ##################################### PLOTS  ##################################### 
  
  ########### QUALITY PLOTS ##############
  
  ### Plots raw
  # reactive function that render the plot to create a bar plot
  # of the raw data of the GDS obj
  # require this function
  # Create a plot with the raw genes rawData
  # @x is the value of the column sample from rawData
  # @y is the value of the column value from rawData
  # @fill assign color based on the factor levels
  # @geom_boxplor type of plot is box
  # @theme rotate the text 90 degrees
  output$plot.raw1 <- renderPlot({
    req(genes.raw())
    rawData <- genes.raw()
    ggplot(rawData, aes(x=sample, y=value, fill=group)) + geom_boxplot() +
      theme(axis.text.x = element_text(angle = 90))
  })
  
  #### Plots normalized
  # reactive function that render the plot to create a bar plot
  # of the normalized data of the GDS obj
  # requiere this function
  # Create a plot with the norm genes normData
  # @x is the value of the column sample from normData
  # @y is the value of the column value from normData
  # @fill assign color based on the factor levels
  # @geom_boxplor type of plot is box
  # @theme rotate the text 90 degrees
  output$plot.rma1 <- renderPlot({
    req(genes.rma())
    normData <- genes.rma()
    ggplot(normData, aes(x=sample, y=value, fill=group)) + geom_boxplot() +
      theme(axis.text.x = element_text(angle = 90))
  })
  
  ##### Dendrogram ####
  
  output$dendro.raw <- renderPlot({
    req(eSetRaw())
    me<-input$select.dendro
    y <- eSetRaw()
    clust.euclid.average <- hclust(dist(t(exprs(y)), method=me))
    ggdendrogram(clust.euclid.average, rotate = TRUE, theme_dendro = FALSE) +
      labs(title = "BEFORE Normalization") +
      theme(plot.title = element_text(face="bold"), axis.title= element_blank()) 
  })
  
  output$dendro.rma <- renderPlot({
    req(eSetRma())
    me<-input$select.dendro
    y <- eSetRma()
    clust.euclid.average <- hclust(dist(t(exprs(y)), method=me))
    ggdendrogram(clust.euclid.average, rotate = TRUE, theme_dendro = FALSE) +
      labs(title = "AFTER Normalization") +
      theme(plot.title = element_text(face="bold"), axis.title= element_blank()) 
  })
  
  ### plot ma
  # reactive function that render the plot to create a MA plot
  # of the normalized data of the GDS obj
  # require this function
  # calculate mean of row where column
  # and substract the same column where 
  #### TO DO ####
  # calculate mean of the row values
  # create a smoothed color density representation of a scatterplot
  # @Main = main title of the plot
  # @xlab = label name of the axis
  # @ylab = label name of the axis
  # @abline h  create horitzontal line at values 1 and -1
  output$plot.MA <- renderPlot({
    req(eSetRma())
    g <- facLevel() # factors levels
    Index <- as.numeric(g) # numeric factor levels
    y <- exprs(eSetRma()) # expression data of norm gds obj
    d <- rowMeans(y[,Index==2]) - rowMeans(y[, Index==1])
    a <- rowMeans(y)
    smoothScatter(a, d, main="MA plot", xlab="Mean of gene expressions", ylab="Means diff of pheno groups")
    abline(h=c(-1,1), col="red")
  })
  
  
  ######## GENE EXPRESSION ##########
  
  ##### Heat map plot  ##### 
  # render an interactive heat map plot
  # create an interactive heatmap
  # @expDataName[1:20,] = select the first 20 values to create the heat map
  # @labCol = names of the columns to use
  # @cexRow = positive numbers
  # @dendogram = number of coluns to draw
  # @k_row = number of groups to color the branches in the row
  # @k_col = number of groups to color the branches in the column
  output$plot.heatMap <- renderD3heatmap({
    req(eSetRma())
    expSet<- exprs(eSetRma())
    ranked <- ranked()
    labColNames<- labColNames() # name of the columns
    expDataName = expSet[row.names(ranked),]
    d3heatmap(expDataName[1:input$sli_heatmap,], labCol = labColNames, cexRow=0.5,
              k_row=3, k_col=3)
  })
  
  ### Plot individual gene expression ####
  output$plot.gene1 <- renderPlotly({
    req(facLevel())
    gene <- input$searchGene
    matriz <- switch(input$radioGene,
                     "Raw"= eSetRaw(),
                     "Normalized"= eSetRma(),
                     eSetRma())
    gmap <- switch(input$radioGene,
                   "Raw"= genes.raw(),
                   "Normalized"= genes.rma(),
                   genes.raw())
    y<- exprs(matriz)
    group <- facLevel()
    if(gene=="" | !gene %in% gmap$gen){
      dt<- gmap[1,]
    } else {
      dt<-gmap %>% 
        filter(gen==gene)
    }
    ggplot(dt, aes(x=group, y=value)) + geom_jitter(aes(colour=group)) +
      theme(legend.position = "none")
  })
  
  #### Volcano plot ####
  # reactive function which render a volcano plot
  output$plot.volcano <- renderPlot({
    req(ranked())
    t<-ranked()
    EnhancedVolcano(t,
                    lab = rownames(t),
                    x = 'logFC',
                    y = 'P.Value')
  })
  
}

# launch shiny app
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