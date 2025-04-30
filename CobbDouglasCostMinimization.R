library(shiny)
library(plotly)

# Cobb-Douglas Production Function
cobb_douglas_prod <- function(K, L, A, alpha, beta) {
  A * K^alpha * L^beta
}

# Long-run cost minimization (optimal K and L)
minimum_cost <- function(Ybarra, A, alpha, beta, w, r) {
  L_star <- (Ybarra / (A * ((alpha * w / (beta * r))^alpha)))^(1 / (alpha + beta))
  K_star <- (alpha * w / (beta * r)) * L_star
  C_star <- w * L_star + r * K_star
  list(K = K_star, L = L_star, C = C_star)
}

# Short-run cost minimization (K fixed, optimize L)
short_run_cost <- function(Ybarra, K_fixed, A, alpha, beta, w, r) {
  L_star <- (Ybarra / (A * K_fixed^alpha))^(1 / beta)
  C_star <- w * L_star + r * K_fixed
  list(K = K_fixed, L = L_star, C = C_star)
}

# Cost in function of Y (long run)
cost_in_terms_of_y <- function(Y, A, alpha, beta, w, r) {
  res <- minimum_cost(Y, A, alpha, beta, w, r)
  res$C
}

# Cost in function of Y (short run)
cost_in_terms_of_y_sr <- function(Y, K_fixed, A, alpha, beta, w, r) {
  L_star <- (Y / (A * K_fixed^alpha))^(1 / beta)
  C_star <- w * L_star + r * K_fixed
  C_star
}

# UI
ui <- fluidPage(
  titlePanel("Cost Minimization Problem"),
  
  tabsetPanel(
    tabPanel("Cost Minimization",
             uiOutput("latex_output"),
             sidebarLayout(
               sidebarPanel(
                 numericInput("A", "Technological Parameter (A)", 1, min = 0.1),
                 numericInput("alpha", "Output-Capital Elasticity (α)", 0.5, min = 0.01, max = 1),
                 numericInput("beta", "Output-Labor Elasticity (β)", 0.5, min = 0.01, max = 1),
                 numericInput("w", "Wage (w)", 1, min = 0.1),
                 numericInput("r", "Capital Cost (r)", 1, min = 0.1),
                 numericInput("Ybarra", "Production Level (Ŷ)", 10, min = 0.1),
                 numericInput("K_fixed", "Fixed Capital (Short-run)", 5, min = 0.1),
                 numericInput("Kmax", "Maximum Capital (K)", 100),
                 numericInput("Lmax", "Maximum Labor (L)", 100)
               ),
               mainPanel(
                 plotlyOutput("grafico3D", height = "600px"),
                 br(),
                 verbatimTextOutput("saida_optimo")
               )
             )
    ),
    tabPanel("Cost Functions",
             mainPanel(
               plotlyOutput("graficoCusto", height = "600px")
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  optimal_long <- reactive({
    minimum_cost(input$Ybarra, input$A, input$alpha, input$beta, input$w, input$r)
  })
  
  optimal_short <- reactive({
    short_run_cost(input$Ybarra, input$K_fixed, input$A, input$alpha, input$beta, input$w, input$r)
  })
  
  output$latex_output <- renderUI({
    withMathJax(
      helpText("Long-run Cost Minimization:"),
      helpText('$$\\min_{K,L} \\, C = wL + rK \\quad \\text{s.t.} \\quad \\hat{Y} = A K^\\alpha L^\\beta$$'),
      helpText("Short-run Cost Minimization (fixed K):"),
      helpText('$$\\min_{L} \\, C = wL + rK \\quad \\text{s.t.} \\quad \\hat{Y} = A K_{fixed}^\\alpha L^\\beta$$')
    )
  })
  
  output$grafico3D <- renderPlotly({
    K_seq <- seq(0.1, input$Kmax, length.out = 50)
    L_seq <- seq(0.1, input$Lmax, length.out = 50)
    grid <- expand.grid(K = K_seq, L = L_seq)
    grid$Y <- cobb_douglas_prod(grid$K, grid$L, input$A, input$alpha, input$beta)
    grid$Custo <- input$r * grid$K + input$w * grid$L
    
    plot_ly() %>%
      add_surface(x = ~L_seq, y = ~K_seq,
                  z = ~matrix(grid$Custo, nrow = 50),
                  opacity = 0.7, colorscale = "Blues",
                  showscale = FALSE) %>%
      add_markers(x = optimal_long()$L, y = optimal_long()$K, z = optimal_long()$C,
                  marker = list(color = "green", size = 8),
                  name = "Long-run Optimum") %>%
      add_markers(x = optimal_short()$L, y = optimal_short()$K, z = optimal_short()$C,
                  marker = list(color = "orange", size = 8),
                  name = "Short-run Optimum") %>%
      layout(scene = list(
        xaxis = list(title = "Labor (L)"),
        yaxis = list(title = "Capital (K)"),
        zaxis = list(title = "Total Cost")
      ),
      title = list(text = "Cost Surface", x = 0.5))
  })
  
  output$graficoCusto <- renderPlotly({
    Y_seq <- seq(0.1, 20, length.out = 200)
    C_lr <- sapply(Y_seq, cost_in_terms_of_y, A = input$A, alpha = input$alpha,
                   beta = input$beta, w = input$w, r = input$r)
    C_sr <- sapply(Y_seq, cost_in_terms_of_y_sr, K_fixed = input$K_fixed,
                   A = input$A, alpha = input$alpha, beta = input$beta, w = input$w, r = input$r)
    
    plot_ly() %>%
      add_trace(x = Y_seq, y = C_lr, type = 'scatter', mode = 'lines', name = 'Long-run Cost',
                line = list(color = 'blue')) %>%
      add_trace(x = Y_seq, y = C_sr, type = 'scatter', mode = 'lines', name = 'Short-run Cost',
                line = list(color = 'orange', dash = 'dash')) %>%
      layout(xaxis = list(title = "Output (Y)"),
             yaxis = list(title = "Cost (C)"),
             title = "Cost Functions: Long-run vs Short-run")
  })
  
  output$saida_optimo <- renderPrint({
    long <- optimal_long()
    short <- optimal_short()
    cat("Long-run Equilibrium:\n")
    cat(sprintf("  L*: %.2f\n", long$L))
    cat(sprintf("  K*: %.2f\n", long$K))
    cat(sprintf("  C*: %.2f\n\n", long$C))
    
    cat("Short-run Equilibrium (fixed K = ", input$K_fixed, "):\n", sep = "")
    cat(sprintf("  L*: %.2f\n", short$L))
    cat(sprintf("  C*: %.2f\n", short$C))
  })
}

# Run
shinyApp(ui, server)
