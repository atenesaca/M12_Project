## app.R ##
library(shinydashboard)
library(Biobase)
library(GEOquery)
library("reshape2")
library(ggplot2)
library(dplyr)
library(limma)
library(annotate)
library(shinycssloaders)
library(shinycustomloader)

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(
    title = "Cell Files"
  ),
  dashboardSidebar(
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
  dashboardBody(
    conditionalPanel(
      condition = "input.sidebar == 'home'",
      
      fluidRow(
        box(
          title ="", solidHeader = T, background = "black", collapsible = T,
          textInput("queryId", "Input query ID", ""),
          actionButton("search", "Search ID")
        ),
        box(
          title ="", solidHeader = T, background = "black", collapsible = T,
          fileInput("celFile", "Upload file", accept = ".zip"),
          actionButton("upload", "Upload file")
        )
      )
    ),
    conditionalPanel(
      condition= "input.sidebar == 'plot'",
      plotOutput("plot") %>% withSpinner(),
      withLoader(plotOutput("plot2"), type="html", loader="loader4")
    )
  )
)

server <- function(input, output, session) {
  
  #Switch to plot tab
  observeEvent(input$search, {
    newtab <- switch(input$sidebar, "home" = "plot","plot" = "home")
    updateTabItems(session, "sidebar", newtab)
  })
  
  
  ##### DATA #####
  
  # retrieving query
  uQuery <- eventReactive(input$search, {
    query <- input$queryId
    data <- getGEO(query, destdir=".")
    return(data)
  })
  
  # normalizing query
  eset <- reactive({
    req(uQuery)
    e <- uQuery()
    e <- GDS2eSet(e, do.log2=TRUE)
    e
  })
  
  # groups, this will be choosen by user
  groups <- reactive({
    req(eset())
    x <- eset()
    pData(x)[,2]
  }) 

  # dataframe for plot1
  genes <- reactive({
    req(eset(), groups())
    sample <- sampleNames(eset())
    y <- exprs(eset())
    groups<- groups()
    g<- melt(y, varnames = c( "probe", "sample"))
    g$genotype <- groups[match(g$sample, sample)]
    return(g)
  })
  
  # data for the heatmap
  ebayes <- reactive({
    req(groups(), eset())
    y <- exprs(eset())
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
  
  ########## PLOTS #########
  
  ### Plot 1
  output$plot <- renderPlot({
    req(genes())
    data <- genes()
    ggplot(data, aes(x=sample, y=value, fill=genotype)) + geom_boxplot()
  })
  
  ### Plot 2
  output$plot2 <- renderPlot({
    req(eset())
    y<- exprs(eset())
    tab <- tab()
    labCol<- labCol()
    heatmap(y[row.names(tab),], labCol = labCol, scale="none", cexRow=0.5)
  })
  
}

shinyApp(ui, server)
