library(shiny)
library(plotly)

# CES Utility Function
ces_util <- function(x1, x2, delta, rho) {
  if (abs(rho) < 1e-6) {
    return(x1^delta * x2^(1 - delta))  # Cobb-Douglas
  } else if (rho <= -100) {
    return(pmin(x1, x2))  # Leontief
  } else if (rho == 1) {
    return(delta * x1 + (1 - delta) * x2)  # Perfect Substitutes
  } else {
    return((delta * x1^rho + (1 - delta) * x2^rho)^(1 / rho))
  }
}

# Function to find the optimal bundle given prices and income
get_optimal_bundle <- function(delta, rho, p1, p2, w) {
  f <- function(x1) {
    x2 <- (w - p1 * x1) / p2
    if (x2 < 0) return(-Inf)
    -ces_util(x1, x2, delta, rho)
  }
  result <- optimize(f, c(0, w / p1), maximum = FALSE)
  x1_opt <- result$minimum
  x2_opt <- (w - p1 * x1_opt) / p2
  U_opt <- ces_util(x1_opt, x2_opt, delta, rho)
  
  return(list(x1 = x1_opt, x2 = x2_opt, U = U_opt))
}

# Slutsky Decomposition Function
slutsky_decomposition <- function(delta, rho, p1_old, p1_new, p2, w) {
  # Initial optimal
  opt_initial <- get_optimal_bundle(delta, rho, p1_old, p2, w)
  
  # Need to find compensated income: keeping initial utility but with new p1
  target_U <- opt_initial$U
  
  f_compensated_income <- function(w_comp) {
    opt_comp <- get_optimal_bundle(delta, rho, p1_new, p2, w_comp)
    return(abs(opt_comp$U - target_U))
  }
  
  w_compensated <- optimize(f_compensated_income, c(0.01, 10*w), tol = 1e-6)$minimum
  
  # Optimal with compensated income
  opt_compensated <- get_optimal_bundle(delta, rho, p1_new, p2, w_compensated)
  
  # Final optimal
  opt_final <- get_optimal_bundle(delta, rho, p1_new, p2, w)
  
  # Effects
  subs_effect_x1 <- opt_compensated$x1 - opt_initial$x1
  subs_effect_x2 <- opt_compensated$x2 - opt_initial$x2
  
  income_effect_x1 <- (opt_final$x1 - opt_compensated$x1) * opt_final$x1
  income_effect_x2 <- (opt_final$x2 - opt_compensated$x2) * opt_final$x2
  
  total_effect_x1 <- opt_final$x1 - opt_initial$x1
  total_effect_x2 <- opt_final$x2 - opt_initial$x2
  
  return(list(
    opt_initial = opt_initial,
    opt_compensated = opt_compensated,
    opt_final = opt_final,
    w_compensated = w_compensated,
    subs_effect_x1 = subs_effect_x1,
    subs_effect_x2 = subs_effect_x2,
    income_effect_x1 = income_effect_x1,
    income_effect_x2 = income_effect_x2,
    total_effect_x1 = total_effect_x1,
    total_effect_x2 = total_effect_x2
  ))
}

# Shiny App
ui <- fluidPage(
  titlePanel("Slutsky Equation - CES Utility"),
  
  withMathJax(),
  helpText("Utility Functional Form: $$ U(x_1, x_2) = \\left[ \\delta x_1^\\rho + (1 - \\delta) x_2^\\rho \\right]^{1/\\rho} $$"),
  
  helpText("Slutsky Equation: $$ \\frac{\\partial x_1}{\\partial p_1} = \\frac{\\partial x_1}{\\partial p_1}^{\\text{s}} - \\left( \\frac{\\partial x_1}{\\partial w} \\cdot x_1 \\right) $$"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("delta", "Weight of Good 1 (δ)", 0.1, 0.9, 0.5, step = 0.05),
      sliderInput("rho", "CES Parameter (ρ)", -20, 20, 0.5, step = 0.1),
      
      # Buttons for special CES functions
      actionButton("cobb_douglas", "Cobb-Douglas (ρ = 0)"),
      actionButton("leontief", "Leontief (ρ → -∞)"),
      actionButton("perfect_substitutes", "Perfect Substitutes (ρ = 1)"),
      
      br(),  
      br(),  
      numericInput("w", "Income (w)", 100),
            numericInput("p1_old", "Initial Price of Good 1 (p1_old)", 10),
      numericInput("p1_new", "New Price of Good 1 (p1_new)", 8),
      numericInput("p2", "Price of Good 2 (p2)", 10)
    ),
    
    mainPanel(
      plotlyOutput("max_plot"),
      verbatimTextOutput("slutsky_info")
    )
  )
)

server <- function(input, output, session) {
  
  # When clicking on Cobb-Douglas, set rho to 0
  observeEvent(input$cobb_douglas, {
    updateSliderInput(session, "rho", value = 0)  # Set rho to 0
  })
  
  # When clicking on Leontief, set rho to -Inf
  observeEvent(input$leontief, {
    updateSliderInput(session, "rho", value = -Inf)  # Set rho to -∞
  })
  
  # When clicking on Perfect Substitutes, set rho to 1
  observeEvent(input$perfect_substitutes, {
    updateSliderInput(session, "rho", value = 1)  # Set rho to 1
  })
  
  output$max_plot <- renderPlotly({
    delta <- input$delta
    rho <- input$rho
    p1_old <- input$p1_old
    p1_new <- input$p1_new
    p2 <- input$p2
    w <- input$w
    
    decomposition <- slutsky_decomposition(delta, rho, p1_old, p1_new, p2, w)
    
    x1_vals <- seq(0, w / min(p1_old, p1_new) * 1.5, length.out = 100)
    x2_vals <- seq(0, w / p2 * 1.5, length.out = 100)
    grid <- expand.grid(x1 = x1_vals, x2 = x2_vals)
    grid$U <- mapply(ces_util, grid$x1, grid$x2, MoreArgs = list(delta = delta, rho = rho))
    Z <- matrix(grid$U, nrow = length(x1_vals), ncol = length(x2_vals), byrow = TRUE)
    
    budget_initial <- (w - p1_old * x1_vals) / p2
    budget_initial[budget_initial < 0] <- NA
    
    budget_compensated <- (decomposition$w_compensated - p1_new * x1_vals) / p2
    budget_compensated[budget_compensated < 0] <- NA
    
    budget_final <- (w - p1_new * x1_vals) / p2
    budget_final[budget_final < 0] <- NA
    
    plot <- plot_ly() %>% 
      add_contour(x = x1_vals, y = x2_vals, z = Z, colorscale = "Viridis", 
                  contours = list(showlabels = TRUE, start = 0)) %>%
      add_lines(x = x1_vals, y = budget_initial, line = list(color = "blue"), name = "Initial Budget") %>%
      add_lines(x = x1_vals, y = budget_compensated, line = list(color = "orange", dash = 'dash'), name = "Compensated Budget") %>%
      add_lines(x = x1_vals, y = budget_final, line = list(color = "red"), name = "Final Budget") %>%
      add_markers(x = decomposition$opt_initial$x1, y = decomposition$opt_initial$x2, 
                  marker = list(color = "blue", size = 10), name = "Initial Optimum") %>%
      add_markers(x = decomposition$opt_compensated$x1, y = decomposition$opt_compensated$x2, 
                  marker = list(color = "orange", size = 10), name = "Compensated Optimum") %>%
      add_markers(x = decomposition$opt_final$x1, y = decomposition$opt_final$x2, 
                  marker = list(color = "red", size = 10), name = "Final Optimum") %>%
      layout(
        title = list(text = "Slutsky Decomposition - CES Utility", x = 0.2),
        xaxis = list(title = "Good 1 (x1)"),
        yaxis = list(title = "Good 2 (x2)")
      )
    
    plot
  })
  
  output$slutsky_info <- renderPrint({
    delta <- input$delta
    rho <- input$rho
    p1_old <- input$p1_old
    p1_new <- input$p1_new
    p2 <- input$p2
    w <- input$w
    
    decomposition <- slutsky_decomposition(delta, rho, p1_old, p1_new, p2, w)
    
    cat("Initial Optimum: (", round(decomposition$opt_initial$x1, 2), ",", round(decomposition$opt_initial$x2, 2), ")\n")
    cat("Compensated Optimum: (", round(decomposition$opt_compensated$x1, 2), ",", round(decomposition$opt_compensated$x2, 2), ")\n")
    cat("Final Optimum: (", round(decomposition$opt_final$x1, 2), ",", round(decomposition$opt_final$x2, 2), ")\n")
    
    cat("\nSubstitution Effect:\n")
    cat("  x1: ", round(decomposition$subs_effect_x1, 2), "\n")
    cat("  x2: ", round(decomposition$subs_effect_x2, 2), "\n")
    
    cat("\nIncome Effect:\n")
    cat("  x1: ", round(decomposition$income_effect_x1, 2), "\n")
    cat("  x2: ", round(decomposition$income_effect_x2, 2), "\n")
    
    cat("\nTotal Effect:\n")
    cat("  x1: ", round(decomposition$total_effect_x1, 2), "\n")
    cat("  x2: ", round(decomposition$total_effect_x2, 2), "\n")
  })
}

shinyApp(ui, server)
