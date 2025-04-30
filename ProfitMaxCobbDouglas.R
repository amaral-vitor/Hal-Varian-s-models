library(shiny)
library(plotly)

# Cobb-Douglas production function
cobb_douglas <- function(K, L, A, alpha, beta) {
  A * K^alpha * L^beta
}

# Profit function
profit <- function(K, L, A, alpha, beta, P, r, w) {
  P * cobb_douglas(K, L, A, alpha, beta) - r * K - w * L
}

# Calculation of optimal values
calculate_optimal <- function(A, alpha, beta, P, r, w) {
  sum <- alpha + beta
  if (abs(sum - 1) < 1e-6) {
    return(list(indeterminate = TRUE))
  }
  
  exponent <- 1 / (1 - sum)
  
  y <- A * (P * alpha / r)^ (alpha * exponent) * (P * beta / w)^ (beta * exponent)
  K <- (P * alpha / r) * y
  L <- (P * beta / w) * y
  pi <- P * y - r * K - w * L
  
  list(y = y, K = K, L = L, profit = pi, indeterminate = FALSE)
}

# UI
ui <- fluidPage(
  titlePanel("Profit Maximization Problem - Cobb-Douglas"),
  
  # Displaying the profit function in LaTeX below the title
  withMathJax(
    helpText("Profit function: $$\\pi(K, L) = P \\cdot A K^{\\alpha} L^{\\beta} - rK - wL$$")
  ),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("alpha", "Output-Capital Elasticity (α)", value = 0.3, min = 0.01, max = 0.99, step = 0.01),
      numericInput("beta", "Output-Labor Elasticity (β)", value = 0.6, min = 0.01, max = 0.99, step = 0.01),
      numericInput("A", "Technology Parameter (A)", value = 1, min = 0.1),
      numericInput("P", "Product Price (P)", value = 1, min = 0.1),
      numericInput("r", "Capital Cost (r)", value = 1, min = 0.01),
      numericInput("w", "Wage (w)", value = 1, min = 0.01)
    ),
    
    mainPanel(
      plotlyOutput("grafico_3d", height = "500px"),
      plotlyOutput("grafico_contorno", height = "300px"),
      verbatimTextOutput("resultado")
    )
  )
)

# Server
server <- function(input, output) {
  
  profit_matrix <- reactive({
    K_vals <- seq(0.1, 10, length.out = 30)
    L_vals <- seq(0.1, 10, length.out = 30)
    outer(K_vals, L_vals, function(K, L) 
      profit(K, L, input$A, input$alpha, input$beta, input$P, input$r, input$w))
  })
  
  output$grafico_3d <- renderPlotly({
    K_vals <- seq(0.1, 10, length.out = 30)
    L_vals <- seq(0.1, 10, length.out = 30)
    profit_vals <- profit_matrix()
    
    plot_ly(x = L_vals, y = K_vals, z = profit_vals,
            type = "surface", colorscale = "Viridis") %>%
      layout(
        title = "Profit",
        scene = list(
          xaxis = list(title = "Labor (L)"),
          yaxis = list(title = "Capital (K)"),
          zaxis = list(title = "Profit", range = c(-10, 2)) # Setting the z-axis range from -10 to 2
        )
      )
  })
  
  output$grafico_contorno <- renderPlotly({
    K_vals <- seq(0.1, 10, length.out = 30)
    L_vals <- seq(0.1, 10, length.out = 30)
    profit_vals <- profit_matrix()
    
    plot_ly(x = L_vals, y = K_vals, z = profit_vals, type = "contour", colorscale = "Viridis") %>%
      layout(
        title = "Iso-profit Curves",
        xaxis = list(title = "Labor (L)"),
        yaxis = list(title = "Capital (K)")
      )
  })
  
  output$resultado <- renderText({
    res <- calculate_optimal(input$A, input$alpha, input$beta, input$P, input$r, input$w)
    
    if (res$indeterminate) {
      return("The function has first-degree homogeneity (α + β = 1). The supply is indeterminate.")
    }
    
    paste0("Optimal equilibrium:\n",
           "Output (Y*): ", signif(res$y, 4), "\n",
           "Capital (K*): ", signif(res$K, 4), "\n",
           "Labor (L*): ", signif(res$L, 4), "\n",
           "Profit (π*): ", signif(res$profit, 4))
  })
}

# Run
shinyApp(ui, server)
