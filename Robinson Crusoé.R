library(shiny)

# Production Function
f <- function(L, A, beta) A * L^beta

# Utility
utility <- function(C, L, alpha) C^alpha * (24 - L)^(1 - alpha)

# Indifference Curve
indiff_curve <- function(L, U_star, alpha) {
  (U_star / (24 - L)^(1 - alpha))^(1 / alpha)
}

# UI
ui <- fluidPage(
  titlePanel("General Equilibrium: Robinson Crusoe"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("alpha", "Preference Parameter (α):", 0.1, 0.9, 0.5, 0.01),
      sliderInput("beta", "Output-Labor Elasticity (β):", 0.1, 0.9, 0.7, 0.01),
      sliderInput("A", "Total Productivity Factor (A):", 0.5, 2, 1, 0.1),
      numericInput("xmax", "Max X-axis (L):", value = 20, min = 1, max = 24),
      numericInput("ymax", "Max Y-axis (C):", value = 10, min = 1, max = 50)
    ),
    mainPanel(
      withMathJax(),
      h4("Model Equations"),
      helpText("Production: $$C = A L^{\\beta}$$"),
      helpText("Indifference Curve: $$U^* = C^{\\alpha}(24 - L)^{1 - \\alpha}$$"),
      helpText("Profit Line: $$C = \\pi^* + wL$$"),
      plotOutput("plot", height = "500px"),
      verbatimTextOutput("info")
    )
  )
)

# Server
server <- function(input, output) {
  output$plot <- renderPlot({
    alpha <- input$alpha
    beta <- input$beta
    A <- input$A
    xmax <- input$xmax
    ymax <- input$ymax
    
    # === Tangency condition: MRS = MPL ===
    L_star <- uniroot(function(L) {
      C <- f(L, A, beta)
      lhs <- (1 - alpha) / alpha * (C / (24 - L))
      rhs <- A * beta * L^(beta - 1)
      lhs - rhs
    }, c(0.01, 23.99))$root
    
    # Equilibrium C and utility
    C_star <- f(L_star, A, beta)
    U_star <- utility(C_star, L_star, alpha)
    w_star <- A * beta * L_star^(beta - 1)
    pi_star <- C_star - w_star * L_star
    
    # Sequences for plotting
    L_seq <- seq(0.01, xmax, length.out = 300)
    C_prod <- f(L_seq, A, beta)
    C_profit_line <- pi_star + w_star * L_seq
    C_indiff <- indiff_curve(L_seq, U_star, alpha)
    
    plot(L_seq, C_prod, type = "l", col = "darkgreen", lwd = 2,
         xlab = "Labor (L)", ylab = "Consumption (C)",
         xlim = c(0, xmax), ylim = c(0, ymax),
         main = "Robinson Crusoe's Equilibrium")
    lines(L_seq, C_profit_line, col = "gray40", lty = 2, lwd = 2)
    lines(L_seq, C_indiff, col = "blue", lty = 3, lwd = 2)
    points(L_star, C_star, col = "red", pch = 19, cex = 1.5)
    
    legend("topleft",
           legend = c("Production", "Profit Line", "Indifference"),
           col = c("darkgreen", "gray40", "blue"),
           lty = c(1, 2, 3), lwd = 2, bty = "n")
  })
  
  output$info <- renderPrint({
    alpha <- input$alpha
    beta <- input$beta
    A <- input$A
    
    L_star <- uniroot(function(L) {
      C <- f(L, A, beta)
      lhs <- (1 - alpha) / alpha * (C / (24 - L))
      rhs <- A * beta * L^(beta - 1)
      lhs - rhs
    }, c(0.01, 23.99))$root
    
    C_star <- f(L_star, A, beta)
    U_star <- utility(C_star, L_star, alpha)
    w_star <- A * beta * L_star^(beta - 1)
    pi_star <- C_star - w_star * L_star
    
    cat("=== Equilibrium ===\n")
    cat(sprintf("Labor (L*) = %.4f\n", L_star))
    cat(sprintf("Consumption (C*) = %.4f\n", C_star))
    cat(sprintf("Utility (U*) = %.4f\n", U_star))
    cat(sprintf("Wage (w*) = %.4f\n", w_star))
    cat(sprintf("Profit (π*) = %.4f\n", pi_star))
  })
}

shinyApp(ui, server)
