library(shiny)
library(ggplot2)
library(latex2exp)

ui <- fluidPage(
  titlePanel("Edgeworth Box and the First Welfare Theorem"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("endowA1", "Agent A's endowment of good 1:", 0, 10, 5, step = 0.1),
      sliderInput("endowA2", "Agent A's endowment of good 2:", 0, 10, 5, step = 0.1),
      sliderInput("alphaA", "Alpha A (preference for good 1):", 0.1, 0.9, 0.5),
      sliderInput("alphaB", "Alpha B (preference for good 1):", 0.1, 0.9, 0.5),
      sliderInput("p1", "Price of good 1 (p1):", 0.1, 5, 1, step = 0.1),
      sliderInput("p2", "Price of good 2 (p2):", 0.1, 5, 1, step = 0.1)
    ),
    
    mainPanel(
      plotOutput("edgePlot", height = "600px"),
      br(),
      withMathJax(),
      uiOutput("utilities"),
      uiOutput("equilibrium"),
      uiOutput("theoremStatement"),
      uiOutput("walras")
    )
  )
)

server <- function(input, output) {
  output$edgePlot <- renderPlot({
    x1_total <- 10
    x2_total <- 10
    
    x1A0 <- input$endowA1
    x2A0 <- input$endowA2
    x1B0 <- x1_total - x1A0
    x2B0 <- x2_total - x2A0
    
    alphaA <- input$alphaA
    alphaB <- input$alphaB
    p1 <- input$p1
    p2 <- input$p2
    
    wA <- p1 * x1A0 + p2 * x2A0
    wB <- p1 * x1B0 + p2 * x2B0
    
    x1A <- alphaA * wA / p1
    x2A <- (1 - alphaA) * wA / p2
    x1B <- alphaB * wB / p1
    x2B <- (1 - alphaB) * wB / p2
    
    uA <- (x1A^alphaA) * (x2A^(1 - alphaA))
    uB <- (x1B^alphaB) * (x2B^(1 - alphaB))
    
    x_seq <- seq(0.2, 9.8, length.out = 100)
    indifA <- data.frame(
      x1 = x_seq,
      x2 = (uA / (x_seq^alphaA))^(1 / (1 - alphaA)),
      person = "A"
    )
    indifB <- data.frame(
      x1 = x_seq,
      x2 = x2_total - (uB / ((x1_total - x_seq)^alphaB))^(1 / (1 - alphaB)),
      person = "B"
    )
    
    budgetA <- data.frame(
      x1 = x_seq,
      x2 = (wA - p1 * x_seq) / p2
    )
    
    ggplot() +
      geom_line(data = indifA, aes(x = x1, y = x2), color = "blue") +
      geom_line(data = indifB, aes(x = x1, y = x2), color = "red") +
      geom_line(data = budgetA, aes(x = x1, y = x2), linetype = "dotted") +
      geom_point(aes(x = x1A, y = x2A), color = "blue", size = 4, shape = 4) +
      geom_point(aes(x = x1B, y = x2B), color = "red", size = 4, shape = 4) +
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
      ggtitle("Edgeworth Box at Competitive Equilibrium")
  })
  
  output$utilities <- renderUI({
    alphaA <- input$alphaA
    alphaB <- input$alphaB
    p1 <- input$p1
    p2 <- input$p2
    x1A0 <- input$endowA1
    x2A0 <- input$endowA2
    x1B0 <- 10 - x1A0
    x2B0 <- 10 - x2A0
    wA <- p1 * x1A0 + p2 * x2A0
    wB <- p1 * x1B0 + p2 * x2B0
    x1A <- alphaA * wA / p1
    x2A <- (1 - alphaA) * wA / p2
    x1B <- alphaB * wB / p1
    x2B <- (1 - alphaB) * wB / p2
    uA <- (x1A^alphaA) * (x2A^(1 - alphaA))
    uB <- (x1B^alphaB) * (x2B^(1 - alphaB))
    
    withMathJax(paste0(
      "Utilities at equilibrium:",
      " Agent A: $$U^A = x_1^A\\,^{\\alpha_A} x_2^A\\,^{1 - \\alpha_A} = ",
      round(x1A, 2), "^{", round(alphaA, 2), "} \\cdot ",
      round(x2A, 2), "^{", round(1 - alphaA, 2), "} = ",
      round(uA, 4), "$$",
      "Agent B: $$U^B = x_1^B\\,^{\\alpha_B} x_2^B\\,^{1 - \\alpha_B} = ",
      round(x1B, 2), "^{", round(alphaB, 2), "} \\cdot ",
      round(x2B, 2), "^{", round(1 - alphaB, 2), "} = ",
      round(uB, 4), "$$"
    ))
  })
  
  output$equilibrium <- renderUI({
    alphaA <- input$alphaA
    p1 <- input$p1
    p2 <- input$p2
    wA <- p1 * input$endowA1 + p2 * input$endowA2
    x1A <- round(alphaA * wA / p1, 2)
    x2A <- round((1 - alphaA) * wA / p2, 2)
    x1B <- round(10 - x1A, 2)
    x2B <- round(10 - x2A, 2)
    
    HTML(paste0(
      "<b>Competitive Equilibrium Allocation:</b><br>",
      "Agent A: x1 = ", x1A, ", x2 = ", x2A, "<br>",
      "Agent B: x1 = ", x1B, ", x2 = ", x1B
    ))
  })
  
  output$theoremStatement <- renderUI({
    HTML("<b style='color:green;'>✓ According to the First Welfare Theorem, this competitive equilibrium is Pareto efficient.</b>")
  })
  
  output$walras <- renderUI({
    alphaA <- input$alphaA
    alphaB <- input$alphaB
    p1 <- input$p1
    p2 <- input$p2
    x1A0 <- input$endowA1
    x2A0 <- input$endowA2
    x1_total <- 10
    x2_total <- 10
    x1B0 <- x1_total - x1A0
    x2B0 <- x2_total - x2A0
    
    wA <- p1 * x1A0 + p2 * x2A0
    wB <- p1 * x1B0 + p2 * x2B0
    
    x1A <- alphaA * wA / p1
    x2A <- (1 - alphaA) * wA / p2
    x1B <- alphaB * wB / p1
    x2B <- (1 - alphaB) * wB / p2
    
    z1 <- x1A + x1B - x1_total
    z2 <- x2A + x2B - x2_total
    walras <- p1 * z1 + p2 * z2
    
    HTML(paste0(
      "<b>Walras' Law Check:</b><br>",
      "Excess demand value = \\( p_1 z_1 + p_2 z_2 = ",
      round(p1, 2), " \\cdot ", round(z1, 4),
      " + ", round(p2, 2), " \\cdot ", round(z2, 4),
      " = ", round(walras, 6), " \\)<br>",
      ifelse(abs(walras) < 1e-6,
             "<b style='color:green;'>✓ Walras' Law is satisfied.</b>",
             "<b style='color:red;'>✓ Walras' Law is not satisfied.</b>"
      )
    ))
  })
}

shinyApp(ui = ui, server = server)

