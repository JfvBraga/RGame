library(shiny)

ui <- fluidPage(
  actionButton("up", "Up"),
  actionButton("down", "Down"),
  actionButton("left", "Left"),
  actionButton("right", "Right"),
  numericInput("n", "n", 1),
  plotOutput("plot")
)

server <- function(input, output) {
  V <- reactiveValues(x = 15, y = 15, Pos = NULL)
  
  land_init <- matrix(sample(c(1,2,3),size = 30*30,replace = T),30,30)
  #starting position

  Pos <- observeEvent(input$up, {
    V$y <- V$y+input$n
  })
  
  Pos <- observeEvent(input$down, {
    V$y <- V$y-input$n
  })

  Pos <- observeEvent(input$left, {
    V$x <- V$x-input$n
  })
  
  Pos <- observeEvent(input$right, {
    V$x <- V$x+input$n
  })
 
  output$plot <- renderPlot({
    land_init[V$x,V$y] <- 4
    image(land_init, axes = F, col=c("green","grey","blue","black"))
  })
}

shinyApp(ui, server)

