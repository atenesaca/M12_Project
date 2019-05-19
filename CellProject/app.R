## app.R ##
library(shinydashboard)
library(Biobase)
library(GEOquery)
library("reshape2")
library(ggplot2)
library(dplyr)
library(limma)
library(annotate)
library(shinycustomloader)
library(shinyjs)

############################## UI INTERFACE #############################
ui <- dashboardPage(
  skin = "red",
  
  # HEADER
  dashboardHeader(
    title = "Cell Files"
  ),
  
  # SIDEBAR
  dashboardSidebar(
    useShinyjs(),
    # uiOutput("userPanel"),
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
  
  # BODY
  dashboardBody(
    conditionalPanel(
      
      # PANEL HOME
      condition = "input.sidebar == 'home'",
      
      fluidRow(
        # BOX QUERY
        box(
          title ="", solidHeader = T, background = "black", collapsible = T,
          textInput("queryId", "Input query ID", ""),
          actionButton("search", "Search ID")
        ),
        # BOX FILE
        box(
          title ="", solidHeader = T, background = "black", collapsible = T,
          fileInput("celFile", "Upload file", accept = ".zip"),
          actionButton("upload", "Upload file")
        )
      ),
      fluidRow(
        #BOX PHENO CHOICE
        uiOutput("selectPlot")
      )
    ),
    
    # PANEL PLOTS
    conditionalPanel(
      condition= "input.sidebar == 'plot'",
      tabsetPanel(type="tabs",
                  tabPanel("Quality Control", 
                           fluidRow(
                             column(6,plotOutput("plot.raw1")
                                    ),
                             column(6,plotOutput("plot.rma1")
                                    )
                           ),
                           fluidRow(
                             column(width=8, offset=2, align="center",
                                    plotOutput("plot.MA")
                             )
                           )
                           
                  ),
                  tabPanel("Gene Expression",
                           fluidRow(
                             plotOutput("plot2")
                             ),
                           fluidRow(
                             plotOutput("plot.gene1")
                           )
                  )
      )
    )
  )
)

server <- function(input, output, session) {
  
  ##################################### UI THINGS #####################################
  
  #Switch to plot tab when click in Go to Plot button
  observeEvent(input$goToPlot, {
    newtab <- switch(input$sidebar, "home" = "plot","plot" = "home")
    updateTabItems(session, "sidebar", newtab)
  })
  
  # If input$check is null, hide the go to plot button
  observe({
    if(!is.null(input$check1)){
      shinyjs::show("goToPlot")
    } else {
      shinyjs::hide("goToPlot")
    }
  })

  
  ##################################### DATA #####################################
  
  # retrieving and launching query
  uQuery <- eventReactive(input$search, {
    query <- input$queryId
    #if query id is empty, show error
    validate(
      need(input$queryId != "", "Please enter a query ID")
    )
    
    data <- try(getGEO(query, destdir="."))
    validate(
      need(isClass(data) == TRUE, "Please insert a correct GEO ID")
    )
    return(data)
  })
  
  # raw data
  eset.raw <- reactive({
    req(uQuery)
    e <- uQuery()
    e <- GDS2eSet(e, do.log2=FALSE)
    e
  })
  
  # normalized data
  eset.rma <- reactive({
    req(uQuery)
    e <- uQuery()
    e <- GDS2eSet(e, do.log2=TRUE)
    e
  })
  
  # update checkbox with phenodata
  output$selectPlot <- renderUI({
    tagList(
      box(
        selectInput("check1", "Choose first pheno: ",
                    choices = colnames(pData(eset.rma()))[2:3]),
        actionButton("goToPlot", "Go to Plot") 
      )
    )
  })
  
  # groups, this will be choosen by user
  groups <- reactive({
    req(input$check1)
    x <- eset.rma()
    col_pheno <- input$check1
    pData(x)[,col_pheno]
  }) 
  
  # dataframe raw
  genes.raw <- reactive({
    req(eset.raw(), groups())
    sample <- sampleNames(eset.raw())
    y <- exprs(eset.raw())
    groups<- groups()
    g<- melt(y, varnames = c( "probe", "sample"))
    g$genotype <- groups[match(g$sample, sample)]
    return(g)
  })
  
  # dataframe normalized
  genes.rma <- reactive({
    req(eset.rma(), groups())
    sample <- sampleNames(eset.rma())
    y <- exprs(eset.rma())
    groups<- groups()
    g<- melt(y, varnames = c( "probe", "sample"))
    g$genotype <- groups[match(g$sample, sample)]
    return(g)
  })
  
  # data for the heatmap (normalized)
  ebayes <- reactive({
    req(groups(), eset.rma())
    y <- exprs(eset.rma())
    g <- groups()
    design <- model.matrix(~factor(g))
    fit <- lmFit(y, design)
    return(eBayes(fit))
  })
  
  tab <- reactive({
    req(ebayes())
    e <- ebayes()
    return(topTable(e, coef=2, adjust="fdr", n=150))
  })
  
  labCol<- reactive({
    req(groups())
    g <- groups()
    g <- as.character(g)
    return(g)
  }) 
  
  ##################################### PLOTS  ##################################### 
  
  ########### QUALITY PLOTS ##############
  
  ### Plots raw
  output$plot.raw1 <- renderPlot({
    req(genes.raw())
    data <- genes.raw()
    ggplot(data, aes(x=sample, y=value, fill=genotype)) + geom_boxplot()
  })
  
  ### Plots normalized
  
  output$plot.rma1 <- renderPlot({
    req(genes.rma())
    data <- genes.rma()
    ggplot(data, aes(x=sample, y=value, fill=genotype)) + geom_boxplot()
  })
  
  ## plot ma
  output$plot.MA <- renderPlot({
    req(eset.rma())
    g <- groups()
    Index <- as.numeric(g)
    y <- exprs(eset.rma())
    d <- rowMeans(y[,Index==2]) - rowMeans(y[, Index==1])
    a <- rowMeans(y)
    smoothScatter(a, d, main="MA plot", xlab="A", ylab="M")
    abline(h=c(-1,1), col="red")
  })
  
  
  ######## GENE EXPRESSION ##########
  ### Plot 2
  output$plot2 <- renderPlot({
    req(eset.rma())
    y<- exprs(eset.rma())
    tab <- tab()
    labCol<- labCol()
    heatmap(y[row.names(tab),], labCol = labCol, scale="none", cexRow=0.5)
  })
  
  output$plot.gene1 <- renderPlot({
    req(eset.rma(), groups())
    y<- exprs(eset.rma())
    g<- y[23,]
    group <- groups()
    dt <- data.frame("Index"=group, "Expr"=g)
    ggplot(dt, aes(x=group, y=Expr)) + geom_jitter(aes(colour=group)) +
      theme(legend.position = "none")
    
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