## app.R ##

# Upload max file size to 20M
#options(shiny.maxRequestSize=20*1024^2)
# Show app in port 2020
options(shiny.port = 2020)

# Libraries
library(shinydashboard)
library(shinyWidgets)
library(Biobase)
library(GEOquery)
library("reshape2")
library(ggplot2)
library(dplyr)
library(limma)
library(annotate)
#library(shinycustomloader)
# library(shinyjs)
library(plotly)
library(d3heatmap)
library(DBI)
# library(digest)
library(EnhancedVolcano)
library(ggdendro)

############################## UI INTERFACE #############################

## DASHBOARD
# Create a shiny dasboard page
ui <- dashboardPage(
  skin = "red", # set header color to red
  
  ## HEADER
  dashboardHeader(
    title = "Gene Chips Analysis" # put a title in header
  ),
  
  ## SIDEBAR
  # Create element in a left sidebar
  dashboardSidebar(
    # Create a menu where we can add multiples menu items
    sidebarMenu(id="sidebar", # set an id to sidebar menu
                # Create an item where we can add an id and an icon
                menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("Queries", tabName = "queries", icon = icon("keyboard")),
                menuItem("Plot", tabName = "plot", icon = icon("chart-bar")),
                menuItem("Data", tabName = "data", icon = icon("table"))
    )
  ),
  
  ## BODY
  # create elements in body app
  dashboardBody(
    
    # Create a conditional panel which olny show his content
    # when sidebar menu id is equal whith menu item id
    conditionalPanel(
      
      ## PANEL HOME
      # show information about NVBI Geo objects
      condition = "input.sidebar == 'home'",
      
      # Create a body tab panel
      tabsetPanel(
        type = "tabs",
        # Create tab and add a title
        tabPanel("NCBI Geo",
                 fluidRow(
                   box(width = 12,
                       h1("Overview of GEO"),
                       p("The NCBI Gene Expression Omnibus (GEO) serves as a public repository
                         for a wide range of high-throughput experimental data. These data include
                         single and dual channel microarray-based experiments measuring mRNA,
                         genomic DNA, and protein abundance, as well as non-array techniques
                         such as serial analysis of gene expression (SAGE), mass spectrometry
                         proteomic data, and high-throughput sequencing data.
                         
                         At the most basic level of organization of GEO, there are four basic entity types.
                         The first three (Sample, Platform, and Series) are supplied by users; the fourth,
                         the dataset, is compiled and curated by GEO staff from the user-submitted data.
                         See the",
                         a("GEO home page", href="https://www.ncbi.nlm.nih.gov/geo/"),
                         "for more information.")
                   ),
                   box(
                     width = 12,
                     h2("Platforms"),
                     p("A Platform record describes the list of elements on the array 
                       (e.g., cDNAs, oligonucleotide probesets, ORFs, antibodies) 
                       or the list of elements that may be detected and quantified in that experiment 
                       (e.g., SAGE tags, peptides). Each Platform record is assigned a unique and stable 
                       GEO accession number (GPLxxx). A Platform may reference many Samples that have been 
                       submitted by multiple submitters.")
                   ),
                   box(
                     width = 12,
                     h2("Samples"),
                     p("A Sample record describes the conditions under which an individual Sample was
                       handled, the manipulations it underwent, and the abundance measurement of each
                       element derived from it. Each Sample record is assigned a unique and stable GEO
                       accession number (GSMxxx). A Sample entity must reference only one Platform and
                       may be included in multiple Series.")
                   ),
                   box(
                     width = 12,
                     h2("Series"),
                     p("A Series record defines a set of related Samples considered to be part of a group,
                       how the Samples are related, and if and how they are ordered. A Series provides a
                       focal point and description of the experiment as a whole. Series records may also
                       contain tables describing extracted data, summary conclusions, or analyses.
                       Each Series record is assigned a unique and stable GEO accession number (GSExxx).
                       Series records are available in a couple of formats which are handled by GEOquery
                       independently. The smaller and new GSEMatrix files are quite fast to parse; a simple
                       flag is used by GEOquery to choose to use GSEMatrix files (see below).")
                   ),
                   box(
                     width = 12,
                     h2("Datasets"),
                     p("GEO DataSets (GDSxxx) are curated sets of GEO Sample data. A GDS record represents 
                       a collection of biologically and statistically comparable GEO Samples and forms the 
                       basis of GEO's suite of data display and analysis tools. Samples within a GDS refer 
                       to the same Platform, that is, they share a common set of probe elements. Value 
                       measurements for each Sample within a GDS are assumed to be calculated in an 
                       equivalent manner, that is, considerations such as background processing and 
                       normalization are consistent across the dataset. Information reflecting experimental 
                       design is provided through GDS subsets.")
                   )
                 )
        ),
        tabPanel(
          "Micro arrays",
          fluidRow(
            box(
              width = 12,
              h1("To Do"),
              a("Micro arrays en espanyol",
                href="https://www.cabimer.es/web3/unidades-apoyo/genomica/microarrays-de-affymetrix/")
            )
          )
        )
      )
    ),
    
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
      
      textOutput("queryError"),
      
      ## BOX PHENO CHOICE
      # create a reactive box
      fluidRow(
        uiOutput("selectPhenoData"),
        uiOutput("ExpDesc")
      )
    ),
    
    ######## PANEL PLOTS  ###### 
    conditionalPanel(
      
     
      # if sidebar id is equal with "plot" show page
      condition= "input.sidebar == 'plot'",
      
      # Create a panel of tab in body to show plots
      tabsetPanel(type="tabs",
                  tabPanel("Quality Control",
                           fluidRow(
                             column(6,plotOutput("plot.raw1")
                             ),
                             column(6,plotOutput("plot.rma1")
                             )
                           ),
                           fluidRow(
                             br(),
                             column(width=8, offset=2, align="center",
                                    selectInput("select.dendro", "Choose method for dendrogram: ",
                                         choices = c("euclidean", "maximum", "manhattan",
                                                     "canberra", "binary", "minkowski"))),
                             column(6, plotOutput("dendro.raw")),
                             column(6, plotOutput("dendro.rma"))
                           ),
                           fluidRow(
                             
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
                             column(width = 9,
                                    d3heatmapOutput("plot.heatMap", height = "80vh")
                                    ),
                             column(width=3,
                                    sliderInput("sli_heatmap", label = "Please select the 
                                    amount of genes to display in heatmap", min = 15,
                                                max = 300, value = 30)
                             )
                           ),
                           # reactive function which show a plot of genes
                           fluidRow(
                             br(),
                             column(9, plotlyOutput("plot.gene1",  height = "80vh")),
                             column(3, searchInput("searchGene", label="Search gene to evaluate",
                                                   btnReset = icon("remove"), btnSearch = icon("search")),
                                    radioButtons("radioGene", "Select Raw or Normalized data:",
                                                 choices=c("Raw", "Normalized")))
                             
                           ),
                           # reactive function which show a volcano plot
                           fluidRow(
                             br(),
                             br(),
                             plotOutput("plot.volcano", height = 800)
                           )
                  )
      )
    ),
    
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
  )
)

## SERVER
# server function where we can controlle the differents options
# inputed by the user
server <- function(input, output, session) {
  
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