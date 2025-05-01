library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Monopsony with Linear Supply"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("a", "Supply intercept (a)", min = 1, max = 50, value = 10),
      sliderInput("b", "Supply slope (b)", min = 0.1, max = 5, value = 1, step = 0.1),
      sliderInput("vmp", "Value of Marginal Product (VMP)", min = 10, max = 100, value = 50)
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
    VMP <- input$vmp
    
    # Check to avoid invalid values
    if (VMP <= a) {
      plot.new()
      text(0.5, 0.5, "⚠️ VMP must be greater than 'a' to hire labor.")
      return()
    }
    
    # Relevant quantities
    q_m <- (VMP - a) / (2 * b)
    q_c <- (VMP - a) / b
    q_max <- q_c * 1.1
    
    q <- seq(0, q_max, length.out = 300)
    supply <- a + b * q
    mfc <- a + 2 * b * q
    vmp_line <- rep(VMP, length(q))
    
    df <- data.frame(q, supply, mfc, vmp_line)
    
    ggplot(df, aes(x = q)) +
      geom_line(aes(y = supply), color = "blue", size = 1.2) +
      geom_line(aes(y = mfc), color = "red", size = 1.2) +
      geom_line(aes(y = vmp_line), color = "black", size = 1.2) +
      geom_vline(xintercept = q_m, linetype = "dashed", color = "purple") +
      geom_hline(yintercept = a + b * q_m, linetype = "dashed", color = "darkgreen") +
      annotate("point", x = q_m, y = a + b * q_m, size = 3, color = "darkgreen") +
      annotate("text", x = q_max * 0.6, y = VMP - 5,
               label = "Monopsony solution: MFC = VMP", size = 5) +
      annotate("text", x = q_max * 0.6, y = VMP - 10,
               label = paste0("q* = ", round(q_m, 2), ", w* = ", round(a + b * q_m, 2)), size = 4.5) +
      labs(
        title = "Monopsony Model with Linear Supply",
        subtitle = "Supply (blue), Marginal Factor Cost - MFC (red), VMP (black)",
        x = "Labor Quantity (q)",
        y = "Wage / Value"
      ) +
      coord_cartesian(xlim = c(0, q_max), ylim = c(0, VMP * 1.1)) +
      theme_minimal()
  })
  
  output$values <- renderPrint({
    a <- input$a
    b <- input$b
    VMP <- input$vmp
    
    if (VMP <= a) {
      cat("⚠️ VMP must be greater than 'a' to allow hiring.\n")
      return()
    }
    
    q_m <- (VMP - a) / (2 * b)
    w_m <- a + b * q_m
    q_c <- (VMP - a) / b
    w_c <- a + b * q_c
    dwl <- 0.5 * (q_c - q_m) * (VMP - w_m)
    
    cat("Results:\n")
    cat(sprintf("→ Monopsony Quantity (q*): %.2f\n", q_m))
    cat(sprintf("→ Monopsony Wage (w*): %.2f\n", w_m))
    cat(sprintf("→ Competitive Quantity: %.2f\n", q_c))
    cat(sprintf("→ Competitive Wage: %.2f\n", w_c))
    cat(sprintf("→ Deadweight Loss (DWL): %.2f\n", dwl))
  })
}

shinyApp(ui = ui, server = server)
