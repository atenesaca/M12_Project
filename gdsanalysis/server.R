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
      dbname = "dawbi1904",
      host = "127.0.0.1",
      username = "dawbi1904",
      password = "Ax4jeas7!")
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
  
  ###### Select ma plot ########
  # creates selectInput, fills its choices with sampleNames from ExpressionSet file
  output$ma_selector <- renderUI({
    req(eSetRma())
    samplenames <- sampleNames(eSetRma())
    selectInput("ma_selector1", "Select your sample to compare", choices=samplenames)
  })
  
  ######### Toptable settings #########
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
  
  ## ClearError
  # set output error to blank
  clearError <-  function(){
    output$queryError <- renderText({
      ""
    })
  }
  
  ## IdError
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
  # Download GDS package into a Geo Object from NCBI Geo using the input id 
  
  gdsObj <- eventReactive(input$search, {
    idEmpty() # shows error if id is empty
    data <- try(getGEO(input$queryId)) # saves downloaded gds object
    idError() # shows error if id in invalid
    return(data)
  })

  
  ## eSetRaw
  # transforms a GDS obj to Expression Set Feature
  
  eSetRaw <- reactive({
    req(gdsObj)
    GDS2eSet(gdsObj(), do.log2=FALSE)
  })
  
  ## eSetRma
  # transform a GDS obj to Expression Set Feature with normalized data
  eSetRma <- reactive({
    req(gdsObj)
    GDS2eSet(gdsObj(), do.log2=TRUE)
  })
  
  ## facLevel
  # Extracts factor levels of Ex Set Feature, using input from user,
  # and returns the values of the selected column
  
  facLevel <- reactive({
    req(input$pData)
    pData(eSetRma())[,input$pData]
  })
  
  
  ## genes.raw
  # extracts raw gene expression matrix, list fo sample names, recovers
  # selected phenodata group by user and the gene names from the featuredata
  # in order to melt them and create a new dataframe for further analysis
  
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
  # same as genes.raw but with normalized gene expression matrix
  
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
  
  ##### Contrast() #####
  # this function will take all the different factos in
  # phenodata groups and create a vector of contrast groups between them
  # eg: phenodata column of "genotype" has two factors: "control" and
  # "infected". This function will return a vector with "control-infected"
  # its result will be used to make the contrast design in order to do
  # further Ebayes statistic analysis
  
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
  # applies the contrast function to chosen phenodata column
  
  contrast.gru <- reactive({
    g <- facLevel()
    g <- contrast(g)
    g
  })
  
  
  ## Ebayes
  # Empirical Bayes methods are procedures for statistical inference
  # in which the prior distribution is estimated from the data
  # In order to do this inference, we eed to prepare the data:
  # we create a design matrix with factors from chosen phenodata column
  # then create a constrast design with contrast groups, apply
  # Fit linear model to first design then to contrast design and then apply the
  # ebayes function
  
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
  # reactive function which returns the top tanked genes
  # it will use all its parameters as input values from user
  ranked <- reactive({
    req(ebayes(), input$top_coef, input$top_adjust, input$max_toptable)
    e <- ebayes()
    t <- topTable(e, coef = input$top_coef, adjust=input$top_adjust, n=input$max_toptable)
    t
  })
  
  ## labColNames
  # return factor levels as a character vector
  labColNames<- reactive({
    req(facLevel())
    return(as.character(facLevel()))
  })
  
  #### go_data #####
  # it will storage 4 columns of interest in order to print in a datatable
  # genes lyst with names and GO functions from genes in selected microarray
  
  go_data <- reactive({
    req(eSetRma())
    data <- fData(eSetRma()) %>% 
      select("ID", "Gene symbol", "Nucleotide Title", "GO:Function")
    data
  })
  
  
  ##################################### PLOTS  ##################################### 
  
  ########### QUALITY PLOTS ##############
  
  ### Plots raw
  # renders box plot of the raw gene expression matrix
  # for the sake of usability and user-friendly interface, it incorporates
  # a withProgress bar, in order to give some visual inputs to user while
  # app is proccessing all the info
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
  # renders box plot of the normalized gene expression matrix
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
  # plots the Hierarchical clustering, also known as hierarchical cluster analysis,
  # that groups similar samples by gene expressions. Uses input from user to choose the algorithm
  
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
  # Same as dendro.raw but with normalized gene expression values
  
  output$dendro.rma <- renderPlot({
    req(eSetRma())
    me<-input$select.dendro
    y <- eSetRma()
    clust.euclid.average <- hclust(dist(t(exprs(y)), method=me))
    ggdendrogram(clust.euclid.average, rotate = TRUE, theme_dendro = FALSE) +
      labs(title = "AFTER Normalization") +
      theme(plot.title = element_text(face="bold"), axis.title= element_blank()) 
  })
  
  ######## MA plot ########
  # plot that compares gene expressions between one sample and the rest
  # of them, comparing means of log expression; used to compare
  # how much different is one sample expression against all the experiment
  # @matriz assayData
  # @index name of sample translated in integer
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
  # renders an interactive heat map plot
  # create an interactive heatmap
  # @expDataName[1:input$sli_heatmap,]map
  # @labCol = phenodata factors, input from user
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
  # plots contrast design to see how a gene expression differs between
  # phenodata groups and samples, comparing fold changes to p.value
  
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
  # using a jitter and geom_point we can easily see
  # how a unique gene is expressed across samples and phenodata
  # groups
  
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
  
  ########### GO_TABLE ########
  # renders a Dt datatable with gene data, reporting back
  # go functions and gene names
  output$go_table <- DT::renderDataTable({
    req(go_data())
    DT::datatable(go_data(), class= "table-stripe", style = "bootstrap", rownames = FALSE, extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))

  })
  
}