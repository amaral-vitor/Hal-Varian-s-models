# Limpa variáveis
rm(list = ls())

# Limpa gráficos
if (dev.cur() != 1) dev.off()

# Limpa o console (só visualmente)
cat("\014")

# Instale se necessário
if (!require(shiny)) install.packages("shiny")

library(shiny)

# Interface
ui <- fluidPage(
  titlePanel("Modelo de Equilíbrio Geral - Robinson Crusoé"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("alpha", "Preferência por consumo (α):", min = 0.1, max = 0.9, value = 0.5, step = 0.05),
      sliderInput("A", "Produtividade da firma (A):", min = 0.5, max = 2, value = 1, step = 0.1),
      sliderInput("beta", "Elasticidade da produção (β):", min = 0.3, max = 0.99, value = 0.7, step = 0.01),
      width = 4
    ),
    
    mainPanel(
      plotOutput("grafico"),
      verbatimTextOutput("resultados")
    )
  )
)

# Servidor
server <- function(input, output) {
  output$resultados <- renderPrint({
    alpha <- input$alpha
    A <- input$A
    beta <- input$beta
    T <- 1  # tempo total
    
    f <- function(L) A * L^beta
    firma_otima <- function(w) (w / (A * beta))^(1 / (beta - 1))
    consumidor_otimo <- function(w) {
      l <- (1 - alpha) * T
      L <- T - l
      c <- w * L
      list(c = c, l = l, L = L)
    }
    
    excesso <- function(w) firma_otima(w) - consumidor_otimo(w)$L
    w_star <- uniroot(excesso, c(0.01, 10))$root
    
    cons <- consumidor_otimo(w_star)
    y_star <- f(cons$L)
    
    cat("=== Resultado do Modelo ===\n")
    cat(sprintf("Salário de equilíbrio (w*): %.4f\n", w_star))
    cat(sprintf("Consumo (c): %.4f\n", cons$c))
    cat(sprintf("Lazer (l): %.4f\n", cons$l))
    cat(sprintf("Trabalho (L): %.4f\n", cons$L))
    cat(sprintf("Produção (y = f(L)): %.4f\n", y_star))
  })
  
  output$grafico <- renderPlot({
    alpha <- input$alpha
    A <- input$A
    beta <- input$beta
    T <- 1
    
    f <- function(L) A * L^beta
    U <- function(c, l) c^alpha * l^(1 - alpha)
    firma_otima <- function(w) (w / (A * beta))^(1 / (beta - 1))
    consumidor_otimo <- function(w) {
      l <- (1 - alpha) * T
      L <- T - l
      c <- w * L
      list(c = c, l = l, L = L)
    }
    
    excesso <- function(w) firma_otima(w) - consumidor_otimo(w)$L
    w_star <- uniroot(excesso, c(0.01, 10))$root
    cons <- consumidor_otimo(w_star)
    
    # Curvas
    l_seq <- seq(0.01, T, length.out = 200)
    c_util <- (U(cons$c, cons$l) / l_seq^(1 - alpha))^(1 / alpha)
    c_restricao <- w_star * (T - l_seq)
    c_prod <- f(T - l_seq)
    
    plot(
      l_seq, c_util, type = "l", lwd = 2, col = "blue",
      xlab = "Lazer", ylab = "Consumo", ylim = c(0, max(c_util, c_restricao, c_prod) * 1.1),
      main = "Equilíbrio de Robinson Crusoé"
    )
    lines(l_seq, c_restricao, col = "gray40", lty = 2, lwd = 2)
    lines(l_seq, c_prod, col = "darkgreen", lty = 3, lwd = 2)
    points(cons$l, cons$c, pch = 19, cex = 1.5)
    
    legend("topright", legend = c("Curva de Utilidade", "Restrição Orçamentária", "Fronteira de Produção"),
           col = c("blue", "gray40", "darkgreen"), lty = c(1, 2, 3), lwd = 2, bty = "n")
  })
}

# Executa o app
shinyApp(ui = ui, server = server)