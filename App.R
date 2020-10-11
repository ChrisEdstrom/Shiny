# #Upload to Shiny IO
# install.packages('rsconnect')
# library(rsconnect)
# install.packages(c('shiny','DT','ggplot2','purrr',dplyr','corrplot','plotly')
# 
# rsconnect::setAccountInfo(name='chrisedstrom', token='88D536E091400263E74E70661483E0F1', secret='OYmlIwBg/jZdy9htFnQf8Kgex0crEwWkYFQKLOf4')
# rsconnect::deployApp('C:\\Users\\Chris\\Desktop\\Shiny')
# deployApp()

#Load libraries
library(shiny)
library(DT)
library(ggplot2)
library(purrr)
library(dplyr)
library(corrplot)
library(plotly)

# Load data and create variable converting it to numeric for correlation
#df <-read.csv(file="C:\\Users\\Chris\\Desktop\\School\\HR_Cat.csv", header = T, check.names=F, fileEncoding="UTF-8-BOM")

df<-read.csv("./HR_Cat.csv")

#make some factors
#easier to let ggplot2 control plotting (color, fill) based on type
uvals<-sapply(df,function(x){length(unique(x))})
df<-map_if(df,uvals<4,as.factor) %>%
  as.data.frame()

#plotting theme for ggplot2
.theme <- theme(
  axis.line = element_line(colour = 'gray', size = .75),
  panel.background = element_blank(),
  plot.background = element_blank()
)

#########################################################################################################
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(width=2,
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),

# Horizontal line ----
    tags$hr(),

# Input: Select what to display
    selectInput("dataset",
                "Data:",
                choices =list(df="df",uploaded_file = "inFile"),
                              selected=NULL),
    selectInput("variable",
                "Variable (X):", 
                choices = NULL),
    selectInput("group",
                "Group (y):", 
                choices = NULL),
    selectInput("plot.type",
                "Plot Type:",
                list(boxplot = "boxplot", 
                     histogram = "histogram", 
                     density = "density", 
                     bar = "bar")
    ),
    checkboxInput("show.points", 
                  "show points", 
                  TRUE)
  ),

  mainPanel(

# Output: Tabset w/ correlation, summary, and table ----
      tabsetPanel(type = "tabs",
        tabPanel("Graphs", 
                 uiOutput("graph")),
        tabPanel("Data", 
                 DT::dataTableOutput("table")),
        tabPanel("Summary", 
                 verbatimTextOutput("summary")),
        tabPanel("Correlation", 
                 plotlyOutput("heat"),
                 plotlyOutput("scatterplot"),
                verbatimTextOutput("selection")
      ),
#          h3(textOutput("caption")),
          uiOutput("graph") # depends on input
      )
    )
  )
)


##############################################################################################################
##############################################################################################################
server <- function(input, output, session) {
  datasetInput <- reactive({
    switch(input$dataset,
           "df" = df,
           "inFile" = read.csv(input$file1$datapath,fileEncoding="UTF-8-BOM"))
  })
  
  #update group and variables based on the data
  observe({
    if(!exists(input$dataset)) return() #make sure upload exists
    var.opts<-colnames(get(input$dataset))
    updateSelectInput(session, 
                      "variable", 
                      choices = var.opts)
    updateSelectInput(session, 
                      "group", 
                      choices = var.opts)
  })
  
  output$caption<-renderText({
    switch(input$plot.type,
           "boxplot" 	= 	"Boxplot",
           "histogram" =	"Histogram",
           "density" 	=	"Density plot",
           "bar" 		=	"Bar graph")
  })
  
  output$graph <- renderUI({
    plotOutput("p")
  })
  
  #get data object
  get_data<-reactive({
    
    if(!exists(input$dataset)) return() # if no upload
    check<-function(x){is.null(x) || x==""}
    if(check(input$dataset)) return()
    obj<-list(data=get(input$dataset),
              variable=input$variable,
              group=input$group
    )
    
    #require all to be set to proceed
    if(any(sapply(obj,check))) return()
    #make sure choices had a chance to update
    check<-function(obj){
      !all(c(obj$variable,obj$group) %in% colnames(obj$data))
    }
    
    if(check(obj)) return()
    
    obj
  })
  
  #plotting function using ggplot2
  output$p <- renderPlot({
    plot.obj<-get_data()
    
    #conditions for plotting
    if(is.null(plot.obj)) return()
    
    #make sure variable and group have loaded
    if(plot.obj$variable == "" | plot.obj$group =="") return()
    
    #plot types
    plot.type<-switch(input$plot.type,
                      "boxplot" = geom_boxplot(),
                      "histogram" =	geom_histogram(alpha=0.5,position="identity"),
                      "density" =	geom_density(alpha=.75),
                      "bar" =	geom_bar(position="dodge")
    )
    
    if(input$plot.type=="boxplot")	{		#control for 1D or 2D graphs
      p<-ggplot(plot.obj$data,
                aes_string(
                  x 		= plot.obj$variable,
                  y 		= plot.obj$group,
                  fill 	= plot.obj$group # let type determine plotting
                )
      ) + plot.type
      
      if(input$show.points==TRUE)
      {
        p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
      }
      
    } else {
      
      p<-ggplot(plot.obj$data,
                aes_string(
                  x 		= plot.obj$variable,
                  fill 	= plot.obj$group,
                  group 	= plot.obj$group
                  #color 	= as.factor(plot.obj$variable)
                )
      ) + plot.type
    }
    
    p<-p+labs(
      fill 	= input$group,
      x 		= input$variable,
      y 		= ""
    )  +
      .theme
    print(p)
  })
  
  # set uploaded file
  upload_data<-reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #could also store in a reactiveValues
    read.csv(inFile$datapath,fileEncoding="UTF-8-BOM")
  })
  
  observeEvent(input$file1,{
    inFile<<-upload_data()
  })
  
# Generate a correlation matrix of the data ----
  output$heat <- renderPlotly({
    dfNum <- Filter(is.numeric, datasetInput())
  
  # compute a correlation matrix
  correlation <- round(cor(dfNum), 3)
  nms <- names(dfNum)
  
  plot_ly(x = nms, y = nms, z = correlation, 
          key = correlation, type = "heatmap", source = "heatplot",colors = colorRamp(c("red", "white", "green"))) %>%
    layout(xaxis = list(title = ""), 
           yaxis = list(title = ""))
})

output$selection <- renderPrint({
  s <- event_data("plotly_click")
  if (length(s) == 0) {
    "Click on a cell in the heatmap to display a scatterplot"
  } else {
    cat("You selected: \n\n")
    as.list(s)
  }
})

output$scatterplot <- renderPlotly({
  dfNum <- Filter(is.numeric, datasetInput())
  
  s <- event_data("plotly_click", source = "heatplot")
  if (length(s)) {
    vars <- c(s[["x"]], s[["y"]])
    d <- setNames(dfNum[vars], c("x", "y"))
    yhat <- fitted(lm(y ~ x, data = d))
    plot_ly(d, x = ~x) %>%
      add_markers(y = ~y) %>%
      add_lines(y = ~yhat) %>%
      layout(xaxis = list(title = s[["x"]]), 
             yaxis = list(title = s[["y"]]), 
             showlegend = FALSE)
  } else {
    plotly_empty()
  }
})
    # Show data in table
    output$table <- DT::renderDataTable({
      if (is.null(datasetInput))
        return(NULL)
      datasetInput()
    })
    
    #Get Summary #and Groupby
    output$summary <- renderPrint({
      if (is.null(datasetInput))
        return(NULL)
      summary(datasetInput())
    })
  }

shinyApp(ui, server)