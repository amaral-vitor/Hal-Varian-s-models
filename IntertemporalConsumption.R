library(shiny)
library(plotly)

# CES utility function
ces_util <- function(c1, c2, beta, rho) {
  if (abs(rho) < 1e-6) {
    return(c1^beta * c2^(1 - beta))  # Cobb-Douglas
  } else if (rho <= -100) {
    return(pmin(c1, c2))  # Leontief
  } else if (rho == 1) {
    return(beta * c1 + (1 - beta) * c2)  # Perfect Substitutes
  } else {
    return((beta * c1^rho + (1 - beta) * c2^rho)^(1 / rho))
  }
}

# Function to optimize the intertemporal consumptions
get_optimal_bundle <- function(beta, rho, r, w1, w2) {
  w <- w1 + w2 / (1 + r)  # Intertemporal budget constraint
  
  # Solve for optimal consumption
  f <- function(c1) {
    c2 <- w - c1 - c1 / (1 + r)
    if (c2 < 0) return(-Inf)
    -ces_util(c1, c2, beta, rho)  # Negative to maximize
  }
  
  result <- optimize(f, c(0, w), maximum = FALSE)
  c1_opt <- result$minimum
  c2_opt <- w - c1_opt - c1_opt / (1 + r)
  U_opt <- ces_util(c1_opt, c2_opt, beta, rho)
  
  # Optimal savings
  savings_opt <- w1 - c1_opt
  
  # Determine if the agent is a saver or a borrower
  if (savings_opt > 0) {
    agent_status <- "Saver"
  } else {
    agent_status <- "Borrower"
  }
  
  return(list(c1 = c1_opt, c2 = c2_opt, U = U_opt, savings = savings_opt, status = agent_status))
}

# UI layout
ui <- fluidPage(
  titlePanel("Intertemporal Consumption Model"),
  withMathJax(),
  helpText("Utility function:"),
  helpText("$$ U(c_1, c_2) = \\left[ \\beta c_1^\\rho + (1 - \\beta) c_2^\\rho \\right]^{1/\\rho} $$"),
  
  helpText("Intertemporal budget constraint:"),
  helpText("$$ c_1 + \\frac{c_2}{1 + r} = w_1 + \\frac{w_2}{1 + r} $$"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("beta", "Intertemporal Discount Factor (β)", min = 0.1, max = 0.9, value = 0.5, step = 0.05),
      sliderInput("rho", "CES Parameter (ρ)", min = -20, max = 20, value = 0.5, step = 0.1),
      numericInput("r", "Interest Rate (r)", value = 0.05),
      numericInput("w1", "Endowment in Period 1 (w1)", value = 5),
      numericInput("w2", "Endowment in Period 2 (w2)", value = 5),
      
      actionButton("cobb_douglas", "Cobb-Douglas (ρ = 0)"),
      actionButton("leontief", "Leontief (ρ → -∞)"),
      actionButton("perfect_subs", "Perfect Substitutes (ρ = 1)")
    ),
    mainPanel(
      plotlyOutput("max_plot"),
      verbatimTextOutput("info")
    )
  )
)

# Server function
server <- function(input, output, session) {
  
  observeEvent(input$cobb_douglas, {
    updateSliderInput(session, "rho", value = 0)  # Cobb-Douglas
  })
  
  observeEvent(input$leontief, {
    updateSliderInput(session, "rho", value = -Inf)  # Leontief
  })
  
  observeEvent(input$perfect_subs, {
    updateSliderInput(session, "rho", value = 1)  # Perfect Substitutes
  })
  
  output$max_plot <- renderPlotly({
    beta <- input$beta
    rho <- input$rho
    r <- input$r
    w1 <- input$w1
    w2 <- input$w2
    
    opt <- get_optimal_bundle(beta, rho, r, w1, w2)
    
    # Define the max values of c1 and c2 based on the budget constraint
    max_c1 <- w1 + w2 / (1 + r)
    max_c2 <- w1 + w2 / (1 + r)
    
    # Adjust c1 and c2 values to cover the entire first quadrant
    c1_vals <- seq(0, max_c1, length.out = 200)
    c2_vals <- seq(0, max_c2, length.out = 200)
    
    # Generate grid and calculate utility only for c1 >= 0 and c2 >= 0
    grid <- expand.grid(c1 = c1_vals, c2 = c2_vals)
    grid <- grid[grid$c1 >= 0 & grid$c2 >= 0, ]  # Filter to first quadrant
    
    # Calculate utility
    grid$U <- mapply(ces_util, grid$c1, grid$c2, MoreArgs = list(beta = beta, rho = rho))
    Z <- matrix(grid$U, nrow = length(c1_vals), ncol = length(c2_vals), byrow = TRUE)
    
    # Budget constraint (red line)
    c2_restr <- w1 + w2 / (1 + r) - c1_vals - c1_vals / (1 + r)
    
    plot <- plot_ly() %>% 
      add_contour(x = c1_vals, y = c2_vals, z = Z, colorscale = "Viridis", contours = list(showlabels = TRUE)) %>%
      add_trace(
        x = c1_vals, y = c2_restr, 
        mode = 'lines', 
        line = list(color = 'red', width = 2),
        name = "Budget Constraint"
      ) %>%
      add_markers(x = opt$c1, y = opt$c2, marker = list(size = 10, color = "red"), name = "Optimal Point")
    
    plot %>% 
      layout(
        title = list(text = "Intertemporal Consumption Maximization", x = 0.2),
        xaxis = list(title = "Consumption in Period 1 (c1)", range = c(0, max_c1)),
        yaxis = list(title = "Consumption in Period 2 (c2)", range = c(0, max_c2)),
        showlegend = TRUE
      )
  })
  
  output$info <- renderText({
    beta <- input$beta
    rho <- input$rho
    r <- input$r
    w1 <- input$w1
    w2 <- input$w2
    
    opt <- get_optimal_bundle(beta, rho, r, w1, w2)
    
    paste0(
      "Optimal Consumption in Period 1 (c1): ", round(opt$c1, 2), "\n",
      "Optimal Consumption in Period 2 (c2): ", round(opt$c2, 2), "\n",
      "Optimal Savings: ", round(opt$savings, 2), "\n",
      "Agent Status: ", opt$status
    )
  })
}

# Run the app
shinyApp(ui, server)
