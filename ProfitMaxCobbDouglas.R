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
  sum_ab <- alpha + beta
  if (abs(sum_ab - 1) < 1e-6) {
    return(list(indeterminate = TRUE))
  }
  
  exponent <- 1 / (1 - sum_ab)
  
  y <- A * (P * alpha / r)^(alpha * exponent) * (P * beta / w)^(beta * exponent)
  K <- (P * alpha / r) * y
  L <- (P * beta / w) * y
  pi <- P * y - r * K - w * L
  
  list(y = y, K = K, L = L, profit = pi, indeterminate = FALSE)
}

# UI
ui <- fluidPage(
  titlePanel("Profit Maximization Problem - Cobb-Douglas"),
  
  withMathJax(
    helpText("Profit function: $$\\pi(K, L) = P \\cdot A K^{\\alpha} L^{\\beta} - rK - wL$$")
  ),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("alpha", "Capital-Output Elasticity (α)", value = 0.3, min = 0.01, max = 0.99, step = 0.01),
      numericInput("beta", "Labor-Output Elasticity (β)", value = 0.6, min = 0.01, max = 0.99, step = 0.01),
      numericInput("A", "Technology (A)", value = 1, min = 0.1),
      numericInput("P", "Output Price (P)", value = 1, min = 0.1),
      numericInput("r", "Capital Cost (r)", value = 1, min = 0.01),
      numericInput("w", "Wage (w)", value = 1, min = 0.01)
    ),
    
    mainPanel(
      plotlyOutput("plot_3d", height = "500px"),
      plotlyOutput("contour_plot", height = "300px"),
      verbatimTextOutput("result_output")
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
  
  calculate_opt <- reactive({
    calculate_optimal(input$A, input$alpha, input$beta, input$P, input$r, input$w)
  })
  
  output$plot_3d <- renderPlotly({
    K_vals <- seq(0.1, 10, length.out = 30)
    L_vals <- seq(0.1, 10, length.out = 30)
    profit_vals <- profit_matrix()
    res <- calculate_opt()
    sum_ab <- input$alpha + input$beta
    
    p <- plot_ly(x = L_vals, y = K_vals, z = profit_vals,
                 type = "surface", colorscale = "Viridis") %>%
      layout(
        title = "Profit Surface",
        scene = list(
          xaxis = list(title = "Labor (L)"),
          yaxis = list(title = "Capital (K)"),
          zaxis = list(title = "Profit")
        )
      )
    
    if (!res$indeterminate && sum_ab < 1) {
      p <- p %>%
        add_markers(x = res$L, y = res$K, z = res$profit,
                    marker = list(color = "red", size = 5),
                    name = "Optimum")
    }
    
    p
  })
  
  output$contour_plot <- renderPlotly({
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
  
  output$result_output <- renderText({
    res <- calculate_opt()
    sum_ab <- input$alpha + input$beta
    
    if (abs(sum_ab - 1) < 1e-6) {
      return("The problem has constant returns to scale (α + β = 1).\nSupply is indeterminate, the profit tends to zero")
    } else if (sum_ab > 1) {
      return("The problem has increasing returns to scale (α + β > 1).\nThe function is convex and profits increase indefinitely with inputs.\nThere is no interior optimum.")
    } else {
      paste0("Optimum:\n",
             "Output (Y*): ", signif(res$y, 4), "\n",
             "Capital (K*): ", signif(res$K, 4), "\n",
             "Labor (L*): ", signif(res$L, 4), "\n",
             "Profit (π*): ", signif(res$profit, 4))
    }
  })
}

# Run
shinyApp(ui, server)
