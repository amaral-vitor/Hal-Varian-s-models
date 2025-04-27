library(shiny)
library(plotly)

# CES Production Function
ces_prod <- function(K, L, delta, rho) {
  if (abs(rho) < 1e-6) {
    return(K^delta * L^(1 - delta))  # Cobb-Douglas
  } else if (rho <= -100) {
    return(pmin(K, L))  # Leontief
  } else if (rho == 1) {
    return(delta * K + (1 - delta) * L)  # Perfect Substitutes
  } else {
    return((delta * K^rho + (1 - delta) * L^rho)^(1/rho))
  }
}

# UI
ui <- fluidPage(
  titlePanel("CES Production Function"),
  withMathJax(),
  helpText("Functional form of the CES production function:"),
  helpText("$$ Y = \\left[ \\delta K^{\\rho} + (1-\\delta) L^{\\rho} \\right]^{1/\\rho} $$"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("delta", "Capital Weight (δ)", min = 0.1, max = 0.9, value = 0.5, step = 0.05),
      sliderInput("rho", "CES Parameter (ρ)", min = -20, max = 20, value = 0.5, step = 0.1),
      actionButton("leontief", "Leontief (ρ → -∞)"),
      actionButton("perfect", "Perfect Substitutes (ρ = 1)"),
      actionButton("cobb", "Cobb-Douglas (ρ → 0)"),
      br(), br(),
      numericInput("Kmax", "Maximum Capital (K)", value = 10, min = 1, max = 50),
      numericInput("Lmax", "Maximum Labor (L)", value = 10, min = 1, max = 50)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Long Run Production",
                 br(),
                 plotlyOutput("grafico3D", height = "600px"),
                 br(),
                 plotlyOutput("isoquantas2D", height = "300px")
        ),
        tabPanel("Short Run",
                 br(),
                 numericInput("Kfixo", "Select Fixed Capital (K)", value = 5, min = 0.1, max = 50, step = 0.1),
                 plotlyOutput("curtoprazo", height = "500px")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Quick buttons to adjust ρ
  observeEvent(input$leontief, {
    updateSliderInput(session, "rho", value = -100)
  })
  
  observeEvent(input$perfect, {
    updateSliderInput(session, "rho", value = 1)
  })
  
  observeEvent(input$cobb, {
    updateSliderInput(session, "rho", value = 1e-6)
  })
  
  # 3D plot
  output$grafico3D <- renderPlotly({
    K_seq <- seq(0, input$Kmax, length.out = 50)
    L_seq <- seq(0, input$Lmax, length.out = 50)
    grid <- expand.grid(K = K_seq, L = L_seq)
    grid$Y <- mapply(ces_prod, grid$K, grid$L, 
                     MoreArgs = list(delta = input$delta, rho = input$rho))
    
    plot_ly(x = L_seq, y = K_seq, z = matrix(grid$Y, nrow = length(K_seq)), 
            type = "surface") %>%
      layout(
        scene = list(
          xaxis = list(title = "Labor (L)"),
          yaxis = list(title = "Capital (K)"),
          zaxis = list(title = "Production (Y)")
        ),
        title = list(text = "CES Production Function 3D", x = 0.5)
      )
  })
  
  # 2D Isoquants plot
  output$isoquantas2D <- renderPlotly({
    K_seq <- seq(0.1, input$Kmax, length.out = 100)
    L_seq <- seq(0.1, input$Lmax, length.out = 100)
    grid <- expand.grid(K = K_seq, L = L_seq)
    grid$Y <- mapply(ces_prod, grid$K, grid$L,
                     MoreArgs = list(delta = input$delta, rho = input$rho))
    
    plot_ly(
      x = grid$L, y = grid$K, z = grid$Y,
      type = "contour",
      colorscale = "Viridis",
      contours = list(showlabels = TRUE)
    ) %>%
      layout(
        title = list(text = "Isoquants Map", x = 0.5),
        xaxis = list(title = "Labor (L)"),
        yaxis = list(title = "Capital (K)"),
        # Make the plot more square-like
        xaxis = list(scaleanchor = "y")
      )
  })
  
  # Short Run plot (Fixed K)
  output$curtoprazo <- renderPlotly({
    L_seq <- seq(0.1, input$Lmax, length.out = 100)
    Y_seq <- sapply(L_seq, function(L) ces_prod(input$Kfixo, L, input$delta, input$rho))
    
    plot_ly(
      x = L_seq,
      y = Y_seq,
      type = "scatter",
      mode = "lines",
      line = list(color = "blue")
    ) %>%
      layout(
        title = list(text = paste0("Short Run Production (K fixed = ", input$Kfixo, ")"), x = 0.5),
        xaxis = list(title = "Labor (L)"),
        yaxis = list(title = "Production (Y)")
      )
  })
  
}

shinyApp(ui, server)
