server <- function(input, output, session) {
  
  ##################################### THEME #####################################
  
  # lets the user change shiny theme
  callModule(module = serverChangeTheme, id = "moduleChangeTheme")
  
  ##################################### DATABASE #####################################
  
  ## getConnection
  # try connection to a database. Db name, user and password pass as parameters
  getConnection <-  tryCatch({
    dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "cellfiles",
      host = "localhost",
      username = "provenusr",
      password = "Provenpass1.")
  },
  error = function(e){ # if connection is invalid, gives an error
    "Error connecting to database"
  })
  
  ##################################### UI Configuration #####################################
  
  #### Read Database
  # It will send a query to our database to retrieve all default gds id
  # thus user has a couple of examples to play with
  # then, creates a table with this information
  # in case there is any error trying to connect to db,
  # prints a message error
  
  output$gds_db <- renderTable({
    if(class(getConnection) == "MySQLConnection"){ 
      on.exit(dbDisconnect(getConnection)) # Close connection when user close app
      filesDT <-  dbGetQuery(getConnection, "SELECT dbid,filename FROM files")
      filesDT
    }
    else { 
      validate(
        need(class(getConnection) == "MySQLConnection",
             getConnection))
      }
  })
  
  ##### Quick description
  # renders a box with a quick description from the gds= Meta(gdsObj())$description[1]
  # then renders two buttons: one to go to plots and the other to go to tables
  output$ExpDesc <- renderUI({
    req(gdsObj())
    
    clearError() # Clear message error
    
    box(title = "Quick Description", status = "primary", solidHeader = TRUE,
        collapsible = TRUE, width = 12,
        p(Meta(gdsObj())$description[1]),
        actionButton("goToPlot", "Go to plot"),
        actionButton("goToData", "Go to data")
    )
  })
  
  # Switch to plot page when user clicks "Go to plot" button
  observeEvent(input$goToPlot, {
    newtab <- switch(input$sidebar, "queries" = "plot", "plot" = "queries")
    updateTabItems(session, "sidebar", newtab)
  })
  
  # Switch to data page when user clicks in "Go to data" button
  observeEvent(input$goToData, {
    newtab <- switch(input$sidebar, "queries" = "data", "data" = "queries")
    updateTabItems(session, "sidebar", newtab)
  })
  
  ######### selectPhenoData #########
  # Reactive function to print box in ui with phenodata groups from the GDS file
  output$selectPhenoData <- renderUI({
    req(eSetRma())
    tagList(
      box(title = "Phenotype columns", status = "primary", solidHeader = TRUE,
          collapsible = TRUE, width = 12,
          selectInput("pData", "Choose first pheno: ",
                      choices = colnames(pData(eSetRma()))[2:3])
      )
    )
  })
  
  ###### select ma plot ########
  # creates selectInput, fills its choices with sampleNames from ExpressionSet file
  output$ma_selector <- renderUI({
    req(eSetRma())
    samplenames <- sampleNames(eSetRma())
    selectInput("ma_selector1", "Select your sample to compare", choices=samplenames)
  })
  
  ######### toptable settings #########
  # this will print three inputs required to entangle with Heatmap and Volcanoplot
  # 1. first, it creates a slider for amount of genes
  # 2. second, creates a select with Adjust method options
  # 3. third, creates a select with names of contrasts groups coefficients
  # given by the ebayes() results
  output$settings_toptable <- renderUI({
    req(eSetRma())
    tagList(
      box(title="Phenotypical GE Configuration", status = "primary", solidHeader = TRUE,
          collapsible = TRUE, width = 12,
          sliderInput("max_toptable", label = "Please select the max amount of genes to
                                       be considered into the TopTable design:", min = 50,
                      max = length(rownames(eSetRma())), value = 1000),
          fluidRow(
            column(6,
                   selectInput("top_adjust", "Choose Adjust Method: ",
                               choices=c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
                                         "fdr", "none"))),
            column(6,
                   selectInput("top_coef", "Choose Coeficient from Contrast Design: ",
                               choices=colnames(ebayes()$coefficients)))
            )
          )
      )
    })
  
  ######### Error Functions ###########
  
  ## clearError
  # set output error to blank
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
  gdsObj <- eventReactive(input$search, {
    idEmpty() # show error if id is empty
    data <- try(getGEO(input$queryId)) # saves downloaded gds object
    idError() # show error if id in invalid
    return(data)
  })

  
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
  # Extract factor levels of expSet
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
    probenames<-rownames(matriz)
    y <- exprs(matriz)
    gen <- fData(matriz)[,3]
    g<- melt(y, varnames = c( "probe", "sample"))
    g$group <- facLevel[match(g$sample, sample)]
    g$gen <- gen[match(g$probe, probenames)]
    g
  })
  
  ##### function contrast #####
  contrast <- function(x){
    x <- make.names(x)
    num <- 1
    gro <- as.character(x)
    vec <- unique(gro)
    cont <- length(vec)
    h <- ""
    v <- vector()
    m <- vector()
    
    for (i in 1:cont){
      v[i] <- vec[i]
    }
    
    for (i in 1:cont){
      for (j in cont:i){
        if(v[i] != v[j]){
          h <- paste(v[i],v[j], sep = "-")
          m[num] <- h
          num <- num+1
        }
      }
    }
    m
  }
  
  ## Contrast Groups ###
  contrast.gru <- reactive({
    g <- facLevel()
    g <- contrast(g)
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
    groups <- facLevel()
    groups<- make.names(groups)
    vect <- unique(groups)
    fac <- factor(groups,levels=vect)
    design <- model.matrix(~ 0 + fac)
    colnames(design) <- vect
    gru <- contrast.gru()
    df <- lmFit(y,design)
    
    contrast <- makeContrasts(contrasts = gru,levels=design)
    datafitcon <-  contrasts.fit(df,contrast)
    ebayes <- eBayes(datafitcon)
    ebayes
  })
  
  ##### ranked #####
  # reactive function which return the top tanked genes
  ranked <- reactive({
    req(ebayes(), input$top_coef, input$top_adjust, input$max_toptable)
    e <- ebayes()
    t <- topTable(e, coef = input$top_coef, adjust=input$top_adjust, n=input$max_toptable)
    t
  })
  
  ## labColNames
  # return factor levels as a character vector
  # require this function
  labColNames<- reactive({
    req(facLevel())
    return(as.character(facLevel()))
  })
  
  go_data <- reactive({
    req(eSetRma())
    data <- fData(eSetRma()) %>% 
      select("ID", "Gene symbol", "Nucleotide Title", "GO:Function")
    data
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
    g <- input$pData
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.1)
                   }
                 })
    ggplot(rawData, aes(x=sample, y=value, fill=group)) + geom_boxplot() + 
      labs(title= "BEFORE normalization", x = "Samples", y="Expressions", fill=g) +
      theme(plot.title = element_text(face="bold"), axis.text.x = element_text(angle = 90))
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
    g <- input$pData
    ggplot(normData, aes(x=sample, y=value, fill=group)) + geom_boxplot() +
      labs(title= "AFTER normalization", x = "Samples", y="Expressions", fill=g) +
      theme(plot.title = element_text(face="bold"), axis.text.x = element_text(angle = 90))
  })
  
  ##### Dendrogram raw #####
  
  output$dendro.raw <- renderPlot({
    req(eSetRaw())
    me<-input$select.dendro
    y <- eSetRaw()
    clust.euclid.average <- hclust(dist(t(exprs(y)), method=me))
    ggdendrogram(clust.euclid.average, rotate = TRUE, theme_dendro = FALSE) +
      labs(title = "BEFORE Normalization") +
      theme(plot.title = element_text(face="bold"), axis.title= element_blank()) 
  })
  
  ##### Dendrogram rma #####
  
  output$dendro.rma <- renderPlot({
    req(eSetRma())
    me<-input$select.dendro
    y <- eSetRma()
    clust.euclid.average <- hclust(dist(t(exprs(y)), method=me))
    ggdendrogram(clust.euclid.average, rotate = TRUE, theme_dendro = FALSE) +
      labs(title = "AFTER Normalization") +
      theme(plot.title = element_text(face="bold"), axis.title= element_blank()) 
  })
  
  ######## ma ########
  # reactive function that render the plot to create a MA plot
  # of the normalized data of the GDS obj
  # require this function
  # calculate mean of row where column
  # and substract the same column where 
  # calculate mean of the row values
  # create a smoothed color density representation of a scatterplot
  # @Main = main title of the plot
  # @xlab = label name of the axis
  # @ylab = label name of the axis
  # @abline h  create horitzontal line at values 1 and -1
  
  output$plot.MA <- renderPlot({
    req(eSetRma())
    samplen <- sampleNames(eSetRma())
    matriz <- eSetRma()
    index <- as.integer(which(samplen==input$ma_selector1))
    limma::plotMA(matriz, index)
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
  
  #### Volcano plot ####
  # reactive function whihch create a volcano plot
  output$plot.volcano <- renderPlot({
    req(ranked(), input$top_coef)
    t<-ranked()
    EnhancedVolcano(t,
                    title = paste0("Fold changes for this contrast group: ", input$top_coef),
                    lab = rownames(t),
                    x = 'logFC',
                    y = 'P.Value',
                    xlab = "Fold Change",
                    ylab = "Significance",
                    legendPosition = 'right',
                    transcriptLabSize = 4)
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
  
  ########### TABLES ########
  ## rawGds
  # renders raw data frame with DT package, including gene symbols annot
  output$go_table <- DT::renderDataTable({
    req(go_data())
    DT::datatable(go_data(), class= "table-stripe", style = "bootstrap", rownames = FALSE, extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
    # req(genes.raw())
    # DT::datatable(genes.raw(), class = "cell-border stripe")
  })
  
  ## rmaGds
  # render normalized data with DT package, including gene symbols annot
  output$rmaGds <- DT::renderDataTable({
    req(genes.rma())
    DT::datatable(genes.rma() , class = "cell-border stripe")
  })
  
  
}