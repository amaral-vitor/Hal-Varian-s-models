## For this code, I used only a Cobb-Douglas to simplify the calculations. I plan to create a code with a CRRA utility function. In the CRRA, the labor supply is an inverted parabola.

library(shiny)
library(plotly)

# Cobb-Douglas utility function
cobb_douglas_util <- function(l, c, sigma) {
  return(l^sigma * c^(1 - sigma))  # Cobb-Douglas
}

# Function to calculate the optimal point
get_optimal_bundle <- function(sigma, w, p, M, T) {
  f <- function(l) {
    # Calculate N from l (N + l = T)
    N <- T - l
    c <- (M + w * N) / p
    if (c < 0 || l < 0 || l > T) return(-Inf)
    -cobb_douglas_util(l, c, sigma)
  }
  result <- optimize(f, c(0, T), maximum = FALSE)
  l_opt <- result$minimum
  N_opt <- T - l_opt
  c_opt <- (M + w * N_opt) / p
  return(list(l = l_opt, c = c_opt, N = N_opt))
}

# Function to calculate the equilibrium N as a function of the wage w
get_N_equilibrium <- function(sigma, p, M, T, w) {
  # Corrected formula for N*
  N <- T - ((sigma * (M + w * T)) / w)
  return(N)
}

# User Interface (UI)
ui <- fluidPage(
  titlePanel("Labor Supply Model"),
  
  # Displaying the Cobb-Douglas utility function
  withMathJax(),
  helpText("Cobb-Douglas Utility functional form:"),
  helpText("$$ U(\\ell, c) = \\ell^{\\sigma} c^{1 - \\sigma} $$"),
  helpText("Budget Constraint: $$ pC = M + wN $$"),
  helpText("Where: $$ N + \\ell = T $$"),
  
  tabsetPanel(
    tabPanel("Consumption x Leisure",
             sidebarLayout(
               sidebarPanel(
                 numericInput("sigma", "Leisure Weight (σ)", value = 0.5, min = 0.1, max = 0.9, step = 0.05),
                 br(),
                 numericInput("w", "Wage (w)", value = 100),
                 br(),
                 numericInput("p", "Consumption Price (p)", value = 1),
                 br(),
                 numericInput("M", "Initial Money Endowment (M)", value = 100),
                 br(),
                 numericInput("T", "Total Available Time (T)", value = 24)
               ),
               
               mainPanel(
                 plotlyOutput("max_plot"),
                 verbatimTextOutput("info")
               )
             )
    ),
    tabPanel("Labor Supply Curve",
             withMathJax(),
             helpText("Equilibrium Labor Supply Function with Wage:"),
             helpText("$$ N^* = T - \\frac{\\sigma(M + w T)}{w} $$"),
             plotlyOutput("w_salary_plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  output$max_plot <- renderPlotly({
    sigma <- input$sigma
    w <- input$w
    p <- input$p
    M <- input$M
    T <- input$T
    
    # Values for L and C
    l_vals <- seq(0, T, length.out = 100)
    c_vals <- seq(0, (M + w*T)/p + 2, length.out = 100)
    grid <- expand.grid(l = l_vals, c = c_vals)
    grid$U <- mapply(cobb_douglas_util, grid$l, grid$c, MoreArgs = list(sigma = sigma))
    Z <- matrix(grid$U, nrow = length(l_vals), ncol = length(c_vals), byrow = TRUE)
    
    # Budget constraint pC = M + wN (where N = T - l)
    N_vals <- T - l_vals
    c_budget <- (M + w * N_vals) / p
    c_budget[c_budget < 0] <- NA
    
    # Plot with utility contours
    plot <- plot_ly() %>% 
      add_contour(x = l_vals, y = c_vals, z = Z, colorscale = "Viridis",
                  contours = list(showlabels = TRUE, start = 0)) %>%
      add_lines(x = l_vals, y = c_budget, line = list(color = "red"), name = "Budget Constraint") %>%
      add_markers(x = T, y = M / p, marker = list(size = 8, color = "black"), name = "Initial Endowment (T, M/p)")
    
    # Optimal point on the graph
    opt <- get_optimal_bundle(sigma, w, p, M, T)
    
    plot <- plot %>% 
      add_markers(x = opt$l, y = opt$c, marker = list(size = 10, color = "red"), name = "Optimal")
    
    # Layout adjustments
    plot %>%
      layout(
        title = list(text = "Consumption vs Leisure", x = 0.5),
        xaxis = list(title = "Leisure (ℓ)", range = c(0, T)),
        yaxis = list(title = "Consumption (C)")
      )
  })
  
  output$info <- renderText({
    sigma <- input$sigma
    w <- input$w
    p <- input$p
    M <- input$M
    T <- input$T
    
    opt <- get_optimal_bundle(sigma, w, p, M, T)
    
    paste0(
      "Equilibrium:\n",
      "Leisure (l*) = ", round(opt$l, 3), "\n",
      "Consumption (C*) = ", round(opt$c, 3), "\n",
      "Equilibrium Labor (N*) = ", round(opt$N, 3), "\n"
    )
  })
  
  output$w_salary_plot <- renderPlotly({
    sigma <- input$sigma
    p <- input$p
    M <- input$M
    T <- input$T
    
    # Wage (w) variation to plot N*
    w_vals <- seq(1, 200, length.out = 100)  
    
    # Formula for N*
    N_vals <- sapply(w_vals, function(w) get_N_equilibrium(sigma, p, M, T, w))
    
    # Plot labor supply curve
    plot_ly(x = N_vals, y = w_vals, type = 'scatter', mode = 'lines') %>%
      layout(
        title = "Labor Supply",
        xaxis = list(title = "Labor Supply (N*)", range = c(0, T)),
        yaxis = list(title = "Wage (w)")
      )
  })
}

# Run the app
shinyApp(ui, server)
