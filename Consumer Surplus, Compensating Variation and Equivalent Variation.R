library(shiny)
library(latex2exp)

ui <- fluidPage(
  titlePanel("Consumer Surplus, Compensated Variation and Equivalent Variation"),
  withMathJax(helpText("$$u(x_1, x_2) = x_1^\\alpha \\cdot x_2^{1 - \\alpha}$$")),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("alpha", "Preference parameter (α):", 0.1, 0.9, 0.5, 0.01),
      
      h4("Initial Scenario"),
      numericInput("p1x1", "Price of x1 (p₁₁):", 2),
      numericInput("p1x2", "Price of x2 (p₁₂):", 1),
      numericInput("w1", "Initial income (w₁):", 100),
      
      h4("Final Scenario"),
      numericInput("p2x1", "Price of x1 (p₂₁):", 1),
      numericInput("p2x2", "Price of x2 (p₂₂):", 1),
      numericInput("w2", "Final income (w₂):", 100),
      
      h4("Graph Scale"),
      numericInput("xmax", "X-axis max (x1):", 100),
      numericInput("ymax", "Y-axis max (x2):", 100)
    ),
    
    mainPanel(
      plotOutput("grafico"),
      verbatimTextOutput("result")
    )
  )
)

server <- function(input, output) {
  output$result <- renderPrint({
    α <- input$alpha
    
    # Consumption and utility in initial scenario
    x1_1 <- α * input$w1 / input$p1x1
    x2_1 <- (1 - α) * input$w1 / input$p1x2
    U1 <- x1_1^α * x2_1^(1 - α)
    
    # Final scenario
    x1_2 <- α * input$w2 / input$p2x1
    x2_2 <- (1 - α) * input$w2 / input$p2x2
    U2 <- x1_2^α * x2_2^(1 - α)
    
    # Compensating Variation (CV)
    w_comp <- U1 / (α^α * (1 - α)^(1 - α)) * input$p2x1^α * input$p2x2^(1 - α)
    CV <- w_comp - input$w2
    
    # Equivalent Variation (EV)
    w_equiv <- U2 / (α^α * (1 - α)^(1 - α)) * input$p1x1^α * input$p1x2^(1 - α)
    EV <- w_equiv - input$w1
    
    # Consumer Surplus (approx.)
    CS <- (input$p1x1 - input$p2x1) * (x1_1 + x1_2) / 2
    
    cat("=== Equilibrium ===\n")
    cat(sprintf("Initial Scenario: x1 = %.2f, x2 = %.2f, u = %.2f\n", x1_1, x2_1, U1))
    cat(sprintf("Final Scenario:   x1 = %.2f, x2 = %.2f, u = %.2f\n", x1_2, x2_2, U2))
    cat(sprintf("Consumer Surplus (CS): %.2f\n", CS))
    cat(sprintf("Compensating Variation (CV): %.2f\n", CV))
    cat(sprintf("Equivalent Variation (EV): %.2f\n", EV))
  })
  
  output$grafico <- renderPlot({
    α <- input$alpha
    
    # Consumption and utility
    x1_1 <- α * input$w1 / input$p1x1
    x2_1 <- (1 - α) * input$w1 / input$p1x2
    U1 <- x1_1^α * x2_1^(1 - α)
    
    x1_2 <- α * input$w2 / input$p2x1
    x2_2 <- (1 - α) * input$w2 / input$p2x2
    U2 <- x1_2^α * x2_2^(1 - α)
    
    # Budget constraints
    bc1 <- function(x) (input$w1 - input$p1x1 * x) / input$p1x2
    bc2 <- function(x) (input$w2 - input$p2x1 * x) / input$p2x2
    
    # Indifference curves
    u1 <- U1
    u2 <- U2
    indiff1 <- function(x) (u1 / x^α)^(1 / (1 - α))
    indiff2 <- function(x) (u2 / x^α)^(1 / (1 - α))
    
    x_vals <- seq(0.01, input$xmax, length.out = 300)
    
    plot(x_vals, bc1(x_vals), type = "l", lwd = 2, col = "red",
         ylim = c(0, input$ymax), xlim = c(0, input$xmax),
         xlab = "Good x₁", ylab = "Good x₂", main = "Indifference Curves and Budgets")
    lines(x_vals, bc2(x_vals), col = "blue", lwd = 2)
    lines(x_vals, indiff1(x_vals), col = "darkgreen", lty = 2, lwd = 2)
    lines(x_vals, indiff2(x_vals), col = "purple", lty = 2, lwd = 2)
    
    points(x1_1, x2_1, pch = 19, col = "red")
    points(x1_2, x2_2, pch = 19, col = "blue")
    
    legend("topright", legend = c("Initial Budget", "Final Budget", 
                                  "Initial Indifference", "Final Indifference"),
           col = c("red", "blue", "darkgreen", "purple"),
           lty = c(1, 1, 2, 2), lwd = 2, bty = "n")
  })
}

shinyApp(ui, server)

