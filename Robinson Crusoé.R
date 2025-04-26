library(shiny)

# Interface
ui <- fluidPage(
  titlePanel("General Equilibrium Model - Robinson Crusoe"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("alpha", "Consumption Preference (α):", min = 0.1, max = 0.9, value = 0.5, step = 0.05),
      sliderInput("A", "Firm Productivity (A):", min = 0.5, max = 2, value = 1, step = 0.1),
      sliderInput("beta", "Production Elasticity (β):", min = 0.3, max = 0.99, value = 0.7, step = 0.01),
      width = 4
    ),
    
    mainPanel(
      plotOutput("grafico"),
      verbatimTextOutput("resultados")
    )
  )
)

# Server
server <- function(input, output) {
  output$resultados <- renderPrint({
    alpha <- input$alpha
    A <- input$A
    beta <- input$beta
    T <- 1  # total time
    
    f <- function(L) A * L^beta
    optimal_firm <- function(w) (w / (A * beta))^(1 / (beta - 1))
    optimal_consumer <- function(w) {
      l <- (1 - alpha) * T
      L <- T - l
      c <- w * L
      list(c = c, l = l, L = L)
    }
    
    excess <- function(w) optimal_firm(w) - optimal_consumer(w)$L
    w_star <- uniroot(excess, c(0.01, 10))$root
    
    cons <- optimal_consumer(w_star)
    y_star <- f(cons$L)
    
    cat("=== Model Result ===\n")
    cat(sprintf("Equilibrium Wage (w*): %.4f\n", w_star))
    cat(sprintf("Consumption (c): %.4f\n", cons$c))
    cat(sprintf("Leisure (l): %.4f\n", cons$l))
    cat(sprintf("Labor (L): %.4f\n", cons$L))
    cat(sprintf("Production (y = f(L)): %.4f\n", y_star))
  })
  
  output$grafico <- renderPlot({
    alpha <- input$alpha
    A <- input$A
    beta <- input$beta
    T <- 1
    
    f <- function(L) A * L^beta
    U <- function(c, l) c^alpha * l^(1 - alpha)
    optimal_firm <- function(w) (w / (A * beta))^(1 / (beta - 1))
    optimal_consumer <- function(w) {
      l <- (1 - alpha) * T
      L <- T - l
      c <- w * L
      list(c = c, l = l, L = L)
    }
    
    excess <- function(w) optimal_firm(w) - optimal_consumer(w)$L
    w_star <- uniroot(excess, c(0.01, 10))$root
    cons <- optimal_consumer(w_star)
    
    # Curves
    l_seq <- seq(0.01, T, length.out = 200)
    c_util <- (U(cons$c, cons$l) / l_seq^(1 - alpha))^(1 / alpha)
    c_constraint <- w_star * (T - l_seq)
    c_prod <- f(T - l_seq)
    
    plot(
      l_seq, c_util, type = "l", lwd = 2, col = "blue",
      xlab = "Leisure", ylab = "Consumption", ylim = c(0, max(c_util, c_constraint, c_prod) * 1.1),
      main = "Robinson Crusoe Equilibrium"
    )
    lines(l_seq, c_constraint, col = "gray40", lty = 2, lwd = 2)
    lines(l_seq, c_prod, col = "darkgreen", lty = 3, lwd = 2)
    points(cons$l, cons$c, pch = 19, cex = 1.5)
    
    legend("topright", legend = c("Utility Curve", "Budget Constraint", "Production Frontier"),
           col = c("blue", "gray40", "darkgreen"), lty = c(1, 2, 3), lwd = 2, bty = "n")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
