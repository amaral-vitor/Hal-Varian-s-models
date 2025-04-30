library(shiny)
library(plotly)

# Cobb-Douglas Production Function
cobb_douglas_prod <- function(K, L, A, alpha, beta) {
  A * K^alpha * L^beta
}

# Long-run cost minimization (optimal K and L)
minimum_cost <- function(Y, A, alpha, beta, w, r) {
  L_star <- (Y / (A * ((alpha * w / (beta * r))^alpha)))^(1 / (alpha + beta))
  K_star <- (alpha * w / (beta * r)) * L_star
  C_star <- w * L_star + r * K_star
  list(K = K_star, L = L_star, C = C_star)
}

# Short-run cost minimization (K fixed, optimize L)
short_run_cost <- function(Y, K_fixed, A, alpha, beta, w, r) {
  L_star <- (Y / (A * K_fixed^alpha))^(1 / beta)
  C_star <- w * L_star + r * K_fixed
  VC_star <- w * L_star
  list(K = K_fixed, L = L_star, C = C_star, VC = VC_star)
}

# UI
ui <- fluidPage(
  titlePanel("Cost Minimization with Cobb-Douglas Technology"),
  
  tabsetPanel(
    tabPanel("Cost Minimization",
             uiOutput("latex_output"),
             sidebarLayout(
               sidebarPanel(
                 numericInput("A", "Technological Parameter (A)", 1, min = 0.1),
                 numericInput("alpha", "Capital Elasticity (α)", 0.5, min = 0.01, max = 1),
                 numericInput("beta", "Labor Elasticity (β)", 0.5, min = 0.01, max = 1),
                 numericInput("w", "Wage (w)", 1, min = 0.1),
                 numericInput("r", "Capital Cost (r)", 1, min = 0.1),
                 numericInput("Y", "Target Output (Ŷ)", 10, min = 0.1),
                 numericInput("K_fixed", "Fixed Capital (short-run)", 5, min = 0.1),
                 numericInput("Kmax", "Max Capital", 100),
                 numericInput("Lmax", "Max Labor", 100),
                 numericInput("Ymax", "Max Output (Ymax)", 100, min = 0.1)
               ),
               mainPanel(
                 plotlyOutput("cost_surface", height = "600px"),
                 br(),
                 verbatimTextOutput("optimal_output")
               )
             )
    ),
    tabPanel("Long-Run Cost Curves",
             mainPanel(
               plotlyOutput("long_run_curves", height = "600px")
             )
    ),
    tabPanel("Short-Run Cost Curves",
             mainPanel(
               plotlyOutput("short_run_curves", height = "600px")
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  optimal_long <- reactive({
    minimum_cost(input$Y, input$A, input$alpha, input$beta, input$w, input$r)
  })
  
  optimal_short <- reactive({
    short_run_cost(input$Y, input$K_fixed, input$A, input$alpha, input$beta, input$w, input$r)
  })
  
  output$latex_output <- renderUI({
    withMathJax(
      helpText("**Long-run cost minimization:**"),
      helpText('$$\\min_{K,L} \\, C = wL + rK \\quad \\text{s.t.} \\quad \\hat{Y} = A K^\\alpha L^\\beta$$'),
      helpText("**Short-run cost minimization (K fixed):**"),
      helpText('$$\\min_{L} \\, C = wL + rK \\quad \\text{s.t.} \\quad \\hat{Y} = A K_{fixed}^\\alpha L^\\beta$$')
    )
  })
  
  output$cost_surface <- renderPlotly({
    K_seq <- seq(0.1, input$Kmax, length.out = 50)
    L_seq <- seq(0.1, input$Lmax, length.out = 50)
    grid <- expand.grid(K = K_seq, L = L_seq)
    grid$Y <- cobb_douglas_prod(grid$K, grid$L, input$A, input$alpha, input$beta)
    grid$Cost <- input$r * grid$K + input$w * grid$L
    
    plot_ly() %>%
      add_surface(x = ~L_seq, y = ~K_seq,
                  z = ~matrix(grid$Cost, nrow = 50),
                  opacity = 0.7, colorscale = "Blues",
                  showscale = FALSE) %>%
      add_markers(x = optimal_long()$L, y = optimal_long()$K, z = optimal_long()$C,
                  marker = list(color = "green", size = 8),
                  name = "Long-run Optimum") %>%
      add_markers(x = optimal_short()$L, y = optimal_short()$K, z = optimal_short()$C,
                  marker = list(color = "orange", size = 8),
                  name = "Short-run Optimum") %>%
      layout(scene = list(
        xaxis = list(title = "Labor (L)", range = c(0, input$Lmax)),
        yaxis = list(title = "Capital (K)", range = c(0, input$Kmax)),
        zaxis = list(title = "Total Cost", range = c(0, max(grid$Cost)))
      ),
      title = list(text = "Cost Surface", x = 0.5))
  })
  
  output$optimal_output <- renderPrint({
    long <- optimal_long()
    short <- optimal_short()
    cat("Long-run Equilibrium:\n")
    cat(sprintf("  L*: %.2f\n", long$L))
    cat(sprintf("  K*: %.2f\n", long$K))
    cat(sprintf("  C*: %.2f\n\n", long$C))
    
    cat("Short-run Equilibrium (K fixed = ", input$K_fixed, "):\n", sep = "")
    cat(sprintf("  L*: %.2f\n", short$L))
    cat(sprintf("  C*: %.2f\n", short$C))
  })
  
  output$long_run_curves <- renderPlotly({
    Y_seq <- seq(0.1, input$Ymax, length.out = 200)
    C <- sapply(Y_seq, function(Y) minimum_cost(Y, input$A, input$alpha, input$beta, input$w, input$r)$C)
    MC <- c(NA, diff(C) / diff(Y_seq))
    AC <- C / Y_seq
    
    plot_ly() %>%
      add_lines(x = Y_seq, y = MC, name = "Marginal Cost (MC)", line = list(color = "red")) %>%
      add_lines(x = Y_seq, y = AC, name = "Average Cost (AC)", line = list(color = "blue")) %>%
      layout(title = "Long-Run Cost Curves",
             xaxis = list(title = "Output (Y)"),
             yaxis = list(title = "Cost"))
  })
  
  output$short_run_curves <- renderPlotly({
    Y_seq <- seq(0.1, input$Ymax, length.out = 200)
    results <- lapply(Y_seq, function(Y) short_run_cost(Y, input$K_fixed, input$A, input$alpha, input$beta, input$w, input$r))
    C <- sapply(results, function(res) res$C)
    VC <- sapply(results, function(res) res$VC)
    AVC <- VC / Y_seq
    MC <- c(NA, diff(C) / diff(Y_seq))
    AC <- C / Y_seq
    
    plot_ly() %>%
      add_lines(x = Y_seq, y = MC, name = "Marginal Cost (MC)", line = list(color = "red")) %>%
      add_lines(x = Y_seq, y = AC, name = "Average Cost (AC)", line = list(color = "blue")) %>%
      add_lines(x = Y_seq, y = AVC, name = "Average Variable Cost (AVC)", line = list(color = "green")) %>%
      layout(title = "Short-Run Cost Curves",
             xaxis = list(title = "Output (Y)"),
             yaxis = list(title = "Cost"))
  })
}

# Run the app
shinyApp(ui, server)
