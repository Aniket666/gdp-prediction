library(shiny)

ui <- fluidPage(
  titlePanel(title = "GDP Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      
      textInput("DJI", "Enter DJI value"),
      actionButton("btnTest", "Predict GDP")
      
    ),
    
    
    mainPanel(
      textOutput("prediction"),
      plotOutput("DJIDist"),
      plotOutput("GDPDist")
    )
  )
)

server <- function(input, output){
  
  data <- read.csv(file = "data/GDP_DJI.csv", header = TRUE)
  reg <- lm(GDPC1~DJI_ADJ_CLOSE, data = data)
  model <- summary(reg)
  
  
  
  output$DJIDist <- renderPlot({
    d_DJI <- density(data$DJI_ADJ_CLOSE)
    plot(d_DJI, main = "Density of DJI")
    polygon(d_DJI, col = "red", border = "blue")
  })
  
  output$GDPDist <- renderPlot({
    d_GDP <- density(data$GDPC1)
    plot(d_GDP, main = "Density of GDP")
    polygon(d_GDP, col = "red", border = "blue")
  })
  
  predictGDP <- eventReactive(input$btnTest, {
    inputDJI <- as.numeric(input$DJI)
    
    exactPred <- model$coefficients[[1]] + model$coefficients[[2]]*inputDJI
    predLower <- exactPred - 2*model$sigma
    predUpper <- exactPred + 2*model$sigma
    
    Result <- paste("GDP would lie between", predLower, "and", predUpper)
  })
  
  output$prediction <- renderText(predictGDP())
}

shinyApp(ui = ui, server = server)