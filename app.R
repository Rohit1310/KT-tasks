library(shiny)
library(leaflet)
library(sf)
library(readxl)

ui <- fluidPage(
  titlePanel(title = "Multiple Tab on clicking the Markers"),
  
  sidebarPanel(
    fileInput("in_data","coordinate excel file"),
    tableOutput(outputId = "my_tb2")
  ),
  #textInput("nTabs",label = "no. of tabs",value = NULL),
  mainPanel(
    leafletOutput("mymap"),
    uiOutput('mytabs')  
  )
)

server <- function(input, output, session) {
  
  input_file <- reactive({
    inFile <- input$in_data
    
    if (is.null(inFile))
      return(NULL)
    
    read_excel(inFile$datapath)
  })

  
  output$mymap <- renderLeaflet({
      if(is.null(input_file())){
      }else{
        leaflet(input_file()) %>% addTiles() %>%
          addAwesomeMarkers(layerId =~Name, ~long, ~lat, label=~Name)
      }
      
    })
    
  
  pd <- reactive({
    as.data.frame(input$mymap_marker_click)
  })
  
  data_of_click <- reactiveValues(clickedMarker = data.frame())
  observeEvent(input$mymap_marker_click,
               {
                 data_of_click$clickedMarker<-rbind(data_of_click$clickedMarker,
                                                     as.data.frame(input$mymap_marker_click)
                                                    )
                 print(levels(data_of_click$clickedMarker[,1]))
                 print("----------------------------------------")
                 output$my_tb2 <- renderTable(data_of_click$clickedMarker)
                 
                 output$mytabs = renderUI({
                   nTabs = nrow(data_of_click$clickedMarker)
                   myTabs = lapply(paste(levels(data_of_click$clickedMarker[,1])),tabPanel)
                   do.call(tabsetPanel, myTabs)
                 })
                 
               })
  
}

shinyApp(ui, server)