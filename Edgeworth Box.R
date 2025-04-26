library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Interactive Edgeworth Box"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("endow_x1", "Endowment A of Good 1:", min = 0, max = 10, value = 5, step = 0.1),
      sliderInput("endow_x2", "Endowment A of Good 2:", min = 0, max = 10, value = 5, step = 0.1),
      sliderInput("alphaA", "Alpha A (preference for good 1):", min = 0.1, max = 0.9, value = 0.5),
      sliderInput("alphaB", "Alpha B (preference for good 1):", min = 0.1, max = 0.9, value = 0.5)
    ),
    
    mainPanel(
      plotOutput("edgePlot", height = "600px")
    )
  )
)

server <- function(input, output) {
  output$edgePlot <- renderPlot({
    x1_total <- 10
    x2_total <- 10
    
    x1A0 <- input$endow_x1
    x2A0 <- input$endow_x2
    x1B0 <- x1_total - x1A0
    x2B0 <- x2_total - x2A0
    
    alphaA <- input$alphaA
    alphaB <- input$alphaB
    
    uA0 <- (x1A0^alphaA) * (x2A0^(1 - alphaA))
    uB0 <- (x1B0^alphaB) * (x2B0^(1 - alphaB))
    
    x_seq <- seq(0.2, 9.8, length.out = 100)
    
    indif_A <- data.frame(
      x1 = x_seq,
      x2 = (uA0 / (x_seq^alphaA))^(1 / (1 - alphaA)),
      person = "A"
    )
    
    indif_B <- data.frame(
      x1 = x_seq,
      x2 = x2_total - (uB0 / ((x1_total - x_seq)^alphaB))^(1 / (1 - alphaB)),
      person = "B"
    )
    
    contract <- data.frame(
      x1 = x_seq,
      x2 = (x2_total / x1_total) * x_seq
    )
    
    ggplot() +
      geom_line(data = indif_A, aes(x = x1, y = x2), color = "blue") +
      geom_line(data = indif_B, aes(x = x1, y = x2), color = "red") +
      geom_line(data = contract, aes(x = x1, y = x2), linetype = "dashed", color = "darkgreen") +
      geom_point(aes(x = x1A0, y = x2A0), size = 3) +
      scale_x_continuous(
        name = "Good 1 (A →)     (← B)",
        limits = c(0, x1_total),
        sec.axis = dup_axis(trans = ~ x1_total - ., name = "")
      ) +
      scale_y_continuous(
        name = "Good 2 (A ↑)\n(B ↓)",
        limits = c(0, x2_total),
        sec.axis = dup_axis(trans = ~ x2_total - ., name = "")
      ) +
      coord_fixed() +
      theme_minimal(base_size = 14) +
      ggtitle("Interactive Edgeworth Box")
  })
}

shinyApp(ui = ui, server = server)
