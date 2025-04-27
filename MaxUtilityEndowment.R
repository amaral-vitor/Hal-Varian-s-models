library(shiny)
library(plotly)

# CES utility function
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

# Optimization function
get_optimal_bundle <- function(delta, rho, p1, p2, w1, w2) {
  w <- p1 * w1 + p2 * w2
  price_ratio <- p1 / p2
  
  if (rho == 1) {
    MRS <- delta / (1 - delta)
    if (abs(MRS - price_ratio) < 1e-3) {
      return("infinite")  # infinite optimal bundles
    } else if (MRS > price_ratio) {
      return(list(x1 = w / p1, x2 = 0, U = ces_util(w / p1, 0, delta, rho)))
    } else {
      return(list(x1 = 0, x2 = w / p2, U = ces_util(0, w / p2, delta, rho)))
    }
  } else {
    f <- function(x1) {
      x2 <- (w - p1 * x1) / p2
      if (x2 < 0) return(-Inf)
      -ces_util(x1, x2, delta, rho)  # Negative to maximize
    }
    result <- optimize(f, c(0, w / p1 * 1.5), maximum = FALSE)
    x1_opt <- result$minimum
    x2_opt <- (w - p1 * x1_opt) / p2
    U_opt <- ces_util(x1_opt, x2_opt, delta, rho)
    
    return(list(x1 = x1_opt, x2 = x2_opt, U = U_opt))
  }
}

# Shiny UI
ui <- fluidPage(
  titlePanel("Utility Maximization - CES (with Endowment)"),
  withMathJax(),
  helpText("Functional form of the CES utility function:"),
  helpText("$$ U(x_1, x_2) = \\left[ \\delta x_1^\\rho + (1 - \\delta) x_2^\\rho \\right]^{1/\\rho} $$"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("delta", "Weight on Good 1 (δ)", min = 0.1, max = 0.9, value = 0.5, step = 0.05),
      sliderInput("rho", "CES Parameter (ρ)", min = -20, max = 20, value = 0.5, step = 0.1),
      actionButton("leontief", "Leontief (ρ → -∞)"),
      actionButton("substitutes", "Perfect Substitutes (ρ = 1)"),
      actionButton("cobbdouglas", "Cobb-Douglas (ρ → 0)"),
      br(), br(),
      numericInput("w1", "Endowment of Good 1 (w1)", value = 5),
      numericInput("w2", "Endowment of Good 2 (w2)", value = 5),
      numericInput("p1", "Price of Good 1 (p1)", value = 10),
      numericInput("p2", "Price of Good 2 (p2)", value = 10)
    ),
    mainPanel(
      plotlyOutput("max_plot"),
      verbatimTextOutput("info")
    )
  )
)

# Shiny server
server <- function(input, output, session) {
  observeEvent(input$leontief, {
    updateSliderInput(session, "rho", value = -100)
  })
  observeEvent(input$substitutes, {
    updateSliderInput(session, "rho", value = 1)
  })
  observeEvent(input$cobbdouglas, {
    updateSliderInput(session, "rho", value = 1e-6)
  })
  
  output$max_plot <- renderPlotly({
    delta <- input$delta
    rho <- input$rho
    p1 <- input$p1
    p2 <- input$p2
    w1 <- input$w1
    w2 <- input$w2
    w <- p1 * w1 + p2 * w2
    
    x1_vals <- seq(0, (w / p1) * 1.5, length.out = 100)
    x2_vals <- seq(0, (w / p2) * 1.5, length.out = 100)
    grid <- expand.grid(x1 = x1_vals, x2 = x2_vals)
    grid$U <- mapply(ces_util, grid$x1, grid$x2, MoreArgs = list(delta = delta, rho = rho))
    Z <- matrix(grid$U, nrow = length(x1_vals), ncol = length(x2_vals), byrow = TRUE)
    
    budget_x2 <- (w - p1 * x1_vals) / p2
    budget_x2[budget_x2 < 0] <- NA
    
    plot <- plot_ly() %>% 
      add_contour(x = x1_vals, y = x2_vals, z = Z, colorscale = "Viridis",
                  contours = list(showlabels = TRUE, start = 0)) %>%
      add_lines(x = x1_vals, y = budget_x2, line = list(color = "red"), name = "Budget Line") %>%
      add_markers(x = w1, y = w2, marker = list(size = 8, color = "black"), name = "Endowment")
    
    opt <- get_optimal_bundle(delta, rho, p1, p2, w1, w2)
    
    if (is.list(opt)) {
      plot <- plot %>% 
        add_markers(x = opt$x1, y = opt$x2, marker = list(size = 10, color = "red"), name = "Optimum")
    }
    
    plot %>% 
      layout(
        title = list(text = "Utility Maximization with Endowment", x = 0.2),
        xaxis = list(title = "Good 1 (x1)"),
        yaxis = list(title = "Good 2 (x2)")
      )
  })
  
  output$info <- renderText({
    delta <- input$delta
    rho <- input$rho
    p1 <- input$p1
    p2 <- input$p2
    w1 <- input$w1
    w2 <- input$w2
    opt <- get_optimal_bundle(delta, rho, p1, p2, w1, w2)
    
    if (is.list(opt)) {
      net_x1 <- opt$x1 - w1
      net_x2 <- opt$x2 - w2
      
      desc1 <- ifelse(abs(net_x1) < 1e-3, 
                      "Self-sufficient in good 1", 
                      ifelse(net_x1 > 0, "Net demander of good 1", "Net supplier of good 1"))
      desc2 <- ifelse(abs(net_x2) < 1e-3, 
                      "Self-sufficient in good 2", 
                      ifelse(net_x2 > 0, "Net demander of good 2", "Net supplier of good 2"))
      
      paste0(
        "Optimum: x1 = ", round(opt$x1, 2), ", x2 = ", round(opt$x2, 2), "\n",
        desc1, "\n", desc2
      )
    } else {
      "Any point along the budget line is optimal."
    }
  })
}

shinyApp(ui, server)
