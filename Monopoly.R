library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Monopoly with Linear Demand - DWL Calculated"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("a", "Demand Intercept (a)", min = 10, max = 100, value = 50),
      sliderInput("b", "Demand Slope (b)", min = 0.1, max = 5, value = 1, step = 0.1),
      sliderInput("c", "Marginal Cost (c)", min = 1, max = 50, value = 10)
    ),
    
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("values")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    a <- input$a
    b <- input$b
    c <- input$c
    
    q <- seq(0, a/b, length.out = 300)
    p <- a - b * q
    MR <- a - 2 * b * q
    MC <- rep(c, length(q))
    
    q_m <- (a - c) / (2 * b)
    p_m <- a - b * q_m
    
    df <- data.frame(q, p, MR, MC)
    
    ggplot(df, aes(x = q)) +
      geom_line(aes(y = p), color = "blue", size = 1.2) +       # Demand
      geom_line(aes(y = MR), color = "red", size = 1.2) +       # MR
      geom_line(aes(y = MC), color = "black", size = 1.2) +     # MC
      geom_vline(xintercept = q_m, linetype = "dashed", color = "purple") +
      geom_hline(yintercept = p_m, linetype = "dashed", color = "darkgreen") +
      annotate("point", x = q_m, y = p_m, size = 3, color = "darkgreen") +
      labs(title = "Monopoly Model with Linear Demand",
           subtitle = "Demand (blue), Marginal Revenue (red), Marginal Cost (black)",
           x = "Quantity (q)", y = "Price / Revenue / Cost") +
      coord_cartesian(xlim = c(0, a/b), ylim = c(0, a)) +
      theme_minimal()
  })
  
  output$values <- renderPrint({
    a <- input$a
    b <- input$b
    c <- input$c
    
    q_m <- (a - c) / (2 * b)
    p_m <- a - b * q_m
    profit <- (p_m - c) * q_m
    q_c <- (a - c) / b
    dwl <- 0.5 * (q_c - q_m) * (p_m - c)
    
    cat("📌 Results:\n")
    cat(sprintf("→ Monopoly Quantity (q*): %.2f\n", q_m))
    cat(sprintf("→ Monopoly Price (p*): %.2f\n", p_m))
    cat(sprintf("→ Monopoly Profit: %.2f\n", profit))
    cat(sprintf("→ Deadweight Loss (DWL): %.2f\n", dwl))
  })
}

shinyApp(ui = ui, server = server)

