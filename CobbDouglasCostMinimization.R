# Packages
library(shiny)
library(plotly)

# Cobb-Douglas Production Function
cobb_douglas_prod <- function(K, L, A, alpha, beta) {
  return(A * K^alpha * L^beta)
}

# Cost Function
minimum_cost <- function(Ybarra, A, alpha, beta, w, r) {
  L_star <- (Ybarra / (A * ((alpha * w / (beta * r))^alpha)))^(1 / (alpha + beta))
  K_star <- (alpha * w / (beta * r)) * L_star
  C_star <- w * L_star + r * K_star
  list(K = K_star, L = L_star, C = C_star)
}

# Cost Function in terms of Y
cost_in_terms_of_y <- function(Y, A, alpha, beta, w, r) {
  L_star <- (Y / (A * ((alpha * w / (beta * r))^alpha)))^(1 / (alpha + beta))
  K_star <- (alpha * w / (beta * r)) * L_star
  C_star <- w * L_star + r * K_star
  return(C_star)
}

# UI
ui <- fluidPage(
  titlePanel("Cobb-Douglas Cost Minimization"),
  
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
    tabPanel("Cost Function",
             mainPanel(
               plotlyOutput("graficoCusto", height = "600px")
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  optimal_result <- reactive({
    minimum_cost(input$Ybarra, input$A, input$alpha, input$beta, input$w, input$r)
  })
  
  output$latex_output <- renderUI({
    withMathJax(
      helpText('Cost Minimization Problem:'),
      helpText('$$\\min_{K,L} C = wL + rK$$'),
      helpText('Subject to: $$\\hat{Y} = A K^\\alpha L^\\beta$$')
    )
  })
  
  output$grafico3D <- renderPlotly({
    K_seq <- seq(0.1, input$Kmax, length.out = 50)
    L_seq <- seq(0.1, input$Lmax, length.out = 50)
    grid <- expand.grid(K = K_seq, L = L_seq)
    grid$Y <- mapply(cobb_douglas_prod, grid$K, grid$L, MoreArgs = list(A = input$A, alpha = input$alpha, beta = input$beta))
    grid$Custo <- input$r * grid$K + input$w * grid$L
    
    optimal <- optimal_result()
    
    K_iso <- seq(0.1, input$Kmax, length.out = 200)
    Y_optimal <- cobb_douglas_prod(optimal_result()$K, optimal_result()$L,
                                   A = input$A, alpha = input$alpha, beta = input$beta)
    L_iso <- sapply(K_iso, resolve_L, Ybarra = Y_optimal, A = input$A, alpha = input$alpha, beta = input$beta)
    C_iso <- input$r * K_iso + input$w * L_iso
    
    valid <- (L_iso > 0 & L_iso < input$Lmax)
    K_iso <- K_iso[valid]
    L_iso <- L_iso[valid]
    C_iso <- C_iso[valid]
    
    plot_ly() %>%
      add_surface(x = ~L_seq, y = ~K_seq,
                  z = ~matrix(grid$Custo, nrow = 50),
                  opacity = 0.7, colorscale = "Blues",
                  showscale = FALSE) %>%
      add_trace(x = L_iso, y = K_iso, z = C_iso,
                type = 'scatter3d', mode = 'lines',
                line = list(color = 'red', width = 5),
                name = "Isoquant") %>%
      add_markers(x = optimal$L, y = optimal$K, z = optimal$C,
                  marker = list(color = "green", size = 8),
                  name = "Optimum") %>%
      layout(scene = list(
        xaxis = list(title = "Labor (L)"),
        yaxis = list(title = "Capital (K)"),
        zaxis = list(title = "Total Cost")
      ),
      title = list(text = "Cost Surface", x = 0.5))
  })
  
  output$graficoCusto <- renderPlotly({
    Y_seq <- seq(0.1, 20, length.out = 200)
    C_seq <- sapply(Y_seq, cost_in_terms_of_y, A = input$A, alpha = input$alpha, beta = input$beta, w = input$w, r = input$r)
    
    plot_ly(x = Y_seq, y = C_seq, type = 'scatter', mode = 'lines', name = 'Cost Function') %>%
      layout(xaxis = list(title = "Output (Y)"),
             yaxis = list(title = "Cost (C)"),
             title = "Cost Function C(Y)")
  })
  
  output$saida_optimo <- renderPrint({
    optimal <- optimal_result()
    cat("Optimal Result:\n")
    cat(sprintf("Optimal Labor (L*): %.2f\n", optimal$L))
    cat(sprintf("Optimal Capital (K*): %.2f\n", optimal$K))
    cat(sprintf("Optimal Cost (C*): %.2f\n", optimal$C))
  })
}

# Run the app
shinyApp(ui, server)
