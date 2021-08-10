pacman::p_load(ggplot2, latex2exp, reshape2, dplyr, ggpubr)

## FDA fornecida
pgev <- function(x, mean, sd, qsi) {
  if (qsi != 0) {
    return( exp(-(1 + qsi*(x-mean)/sd)^(-1/qsi)) )
  } else {
    return( exp(-exp(- (x-mean)/sd)) )
  }
}

## Inversa da FDA (Quantil)
qgev <- function(q, mean, sd, qsi) {
  if (qsi != 0) {
    return(mean - (sd/qsi) * ( 1 - (-log(q))^(-qsi)) )
  } else {
    return(mean - sd*log(-log(q))  )
  }
}

## Derivada da FDA (Densidade)
dgev <- function(x, mean, sd, qsi) {
  if (qsi != 0) {
    return ( exp(-(1 + qsi*(x-mean)/sd)^(-1/qsi)) *
               (1/qsi*(1 + qsi*(x-mean)/sd)^(-1/qsi-1) ) * qsi/sd )
  } else {
    return ( exp(-exp(- (x-mean)/sd)) * (1/sd)*(exp(- (x-mean)/sd)) )
  }
}

set.seed(2021.1)

qsi_g0 <- 0.1
qsi_l0 <- -0.5
qsi_e0 <- 0
s <- 1
m <- 0

y <- runif(100, 0, 1)
## Note que a funcao foi manualmente definida anteriormente
amostra_g0 <- qgev(q = y, mean = m, sd = s, qsi = qsi_g0)
amostra_l0 <- qgev(q = y, mean = m, sd = s, qsi = qsi_l0)
amostra_e0 <- qgev(q = y, mean = m, sd = s, qsi = qsi_e0)

amostra <- data.frame(
  "index" = seq(1,100),
  "Maior que 0" = amostra_g0,
  "Menor que 0" = amostra_l0,
  "Igual a 0" = amostra_e0) %>%
  melt(id.vars = "index") %>%
  rename_all(~ c("index", "qsi", "valor"))


## Valores reais para estudo
x.true <- seq(-2,8, 0.001)
y.trueg0 <- dgev(x.true, mean = m, sd = s, qsi = qsi_g0)
y.truel0 <- dgev(x.true, mean = m, sd = s, qsi = qsi_l0)
y.truee0 <- dgev(x.true, mean = m, sd = s, qsi = qsi_e0)


uni_plot <- function(amostra, x.true, y.true, escala_inf, escala_sup) {
  plot <- ggplot() +
    geom_histogram(aes(x = amostra, y = ..density..),
                   bins = 25, fill =  "green", color = "black") +
    geom_line(aes(x = x.true, y = y.true), size = 2) +
    xlab("x") + ylab("Densidade") +
    scale_x_continuous(breaks = seq(escala_inf, escala_sup, 2)) +
    ylim(0,0.45)

  return(plot)
}

plotg0 <- uni_plot(amostra_g0, x.true, y.trueg0, -2, 8)
plotl0 <- uni_plot(amostra_l0, x.true, y.truel0, -2, 8)
plote0 <- uni_plot(amostra_e0, x.true, y.truee0, -2, 8)

ggarrange(plotg0, plotl0, plote0,
          labels = c(">0", "<0", "=0"),
          ncol = 1, nrow = 3)
