library(shiny)
library(plotly)

# Função CES
ces_util <- function(x1, x2, delta, rho) {
  if (abs(rho) < 1e-6) {
    return(x1^delta * x2^(1 - delta))  # Cobb-Douglas
  } else if (rho <= -100) {
    return(pmin(x1, x2))  # Leontief
  } else if (rho == 1) {
    return(delta * x1 + (1 - delta) * x2)  # Substitutos Perfeitos
  } else {
    return((delta * x1^rho + (1 - delta) * x2^rho)^(1 / rho))
  }
}

# Função para otimização
get_optimal_bundle <- function(delta, rho, p1, p2, w) {
  price_ratio <- p1 / p2
  
  if (rho == 1) {
    TMS <- delta / (1 - delta)
    if (abs(TMS - price_ratio) < 1e-3) {
      return("infinite")  # infinitos ótimos
    } else if (TMS > price_ratio) {
      return(list(x1 = w / p1, x2 = 0, U = ces_util(w / p1, 0, delta, rho)))
    } else {
      return(list(x1 = 0, x2 = w / p2, U = ces_util(0, w / p2, delta, rho)))
    }
  } else {
    f <- function(x1) {
      x2 <- (w - p1 * x1) / p2
      if (x2 < 0) return(-Inf)
      -ces_util(x1, x2, delta, rho)  # Negativo para maximizar
    }
    result <- optimize(f, c(0, w / p1), maximum = FALSE)
    x1_opt <- result$minimum
    x2_opt <- (w - p1 * x1_opt) / p2
    U_opt <- ces_util(x1_opt, x2_opt, delta, rho)
    
    # Verifica a concavidade e TMS = p1/p2
    if (rho > 1) {
      TMS <- delta / (1 - delta)
      if (abs(TMS - price_ratio) < 1e-3) {
        # Caso de duas soluções de canto
        x1_corner1 <- w / p1
        x2_corner1 <- 0
        x1_corner2 <- 0
        x2_corner2 <- w / p2
        return(list(
          x1 = c(x1_corner1, x1_corner2),
          x2 = c(x2_corner1, x2_corner2),
          U = NULL  # Não exibe a utilidade máxima
        ))
      }
    }
    
    return(list(x1 = x1_opt, x2 = x2_opt, U = U_opt))
  }
}

# Interface do Shiny
ui <- fluidPage(
  titlePanel("Maximização de Utilidade - CES"),
  withMathJax(),
  helpText("Forma funcional da função CES:"),
  helpText("$$ U(x_1, x_2) = \\left[ \\delta x_1^\\rho + (1 - \\delta) x_2^\\rho \\right]^{1/\\rho} $$"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("delta", "Peso do Bem 1 (δ)", min = 0.1, max = 0.9, value = 0.5, step = 0.05),
      sliderInput("rho", "Parâmetro CES (ρ)", min = -20, max = 20, value = 0.5, step = 0.1),
      actionButton("leontief", "Leontief (ρ → -∞)"),
      actionButton("substitutos", "Substitutos Perfeitos (ρ = 1)"),
      actionButton("cobbdouglas", "Cobb-Douglas (ρ → 0)"),
      numericInput("w", "Renda (w)", value = 100),
      numericInput("p1", "Preço do Bem 1 (p1)", value = 10),
      numericInput("p2", "Preço do Bem 2 (p2)", value = 15)
    ),
    mainPanel(
      plotlyOutput("max_plot"),
      verbatimTextOutput("info")
    )
  )
)

# Servidor
server <- function(input, output, session) {
  observeEvent(input$leontief, {
    updateSliderInput(session, "rho", value = -100)
  })
  observeEvent(input$substitutos, {
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
    w <- input$w
    
    x1_vals <- seq(0.1, w / p1 * 1.2, length.out = 100)
    x2_vals <- seq(0.1, w / p2 * 1.2, length.out = 100)
    grid <- expand.grid(x1 = x1_vals, x2 = x2_vals)
    grid$U <- mapply(ces_util, grid$x1, grid$x2, MoreArgs = list(delta = delta, rho = rho))
    Z <- matrix(grid$U, nrow = length(x1_vals), ncol = length(x2_vals), byrow = TRUE)
    
    budget_x2 <- (w - p1 * x1_vals) / p2
    budget_x2[budget_x2 < 0] <- NA
    
    plot <- plot_ly() %>%
      add_contour(x = x1_vals, y = x2_vals, z = Z, colorscale = "Viridis",
                  contours = list(showlabels = TRUE)) %>%
      add_lines(x = x1_vals, y = budget_x2, line = list(color = "red"), name = "Reta Orçamentária")
    
    opt <- get_optimal_bundle(delta, rho, p1, p2, w)
    
    if (is.list(opt)) {
      if (length(opt$x1) > 1) {
        # Exibe as duas soluções de canto
        plot <- plot %>%
          add_markers(x = opt$x1, y = opt$x2, marker = list(size = 10, color = "red"), name = "Ótimos de Canto")
      } else {
        plot <- plot %>%
          add_markers(x = opt$x1, y = opt$x2, marker = list(size = 10, color = "red"), name = "Ótimo")
      }
    } else if (opt == "infinite") {
      x1_range <- seq(0, w / p1, length.out = 20)
      x2_range <- (w - p1 * x1_range) / p2
      plot <- plot %>% add_markers(x = x1_range, y = x2_range, marker = list(color = "blue"), name = "Ótimos possíveis")
    }
    
    plot %>%
      layout(
        title = list(text = "Maximização de Utilidade", x = 0.2),
        xaxis = list(title = "Bem 1 (x1)"),
        yaxis = list(title = "Bem 2 (x2)")
      )
  })
  
  output$info <- renderText({
    delta <- input$delta
    rho <- input$rho
    p1 <- input$p1
    p2 <- input$p2
    w <- input$w
    opt <- get_optimal_bundle(delta, rho, p1, p2, w)
    
    if (is.list(opt)) {
      if (length(opt$x1) > 1) {
        # Exibe somente as coordenadas dos ótimos de canto, sem a utilidade máxima
        paste0("Ótimos de canto: x1 = ", round(opt$x1[1], 2), ", x2 = ", round(opt$x2[1], 2), 
               "\n e x1 = ", round(opt$x1[2], 2), ", x2 = ", round(opt$x2[2], 2))
      } else {
        paste0("Ótimo: x1 = ", round(opt$x1, 2), ", x2 = ", round(opt$x2, 2))
      }
    } else {
      "Todas as cestas sobre a reta orçamentária são ótimas."
    }
  })
}

shinyApp(ui, server)
