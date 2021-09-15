### Arquivo contendo todas as funções customizadas utilizadas no trabalho 1
pacman::p_load('dplyr', 'purrr', 'ggplot2', 'stringr', 'stabledist')

tab_exp <- function (arr, decimals=4) {
  data.frame(
    "Media" = mean(arr),
    "Variancia" = var(arr),
    "Desvio.Padrao" = sqrt(var(arr)),
    "Minimo" = range(arr)[1],
    "Maximo" = range(arr)[2],
    "Prim.Quantil" = quantile(arr, 0.25),
    "Seg.Quantil" = quantile(arr, 0.50),
    "Ter.Quantil" = quantile(arr, 0.75)) %>%
    map_df(round, decimals)
}


Serie <- function (data, col_x, col_y) {
  plot <- ggplot(data) +
    geom_line(aes(x = data[col_x][[1]], y = data[col_y][[1]])) +
    xlab("Tempo") + ylab("Maximo valor diario alcançado") +
    scale_x_date(date_labels = "%b-%Y", breaks = seq(ano_inicio, ano_fim, 365/2)) +
    theme_bw()
  return(plot)
}


Hist_Fit <- function (data, values, fits=c(), fits_param=c()) {
  values <- data[values][[1]]
  fits <- map_chr(fits, str_to_lower)


  plot <- ggplot() +
    geom_histogram(aes(x = values, y = ..density..),
                   fill = "steelblue", color = "black",
                   bins = 30) +
    xlab("Log-retorno diário Samsung") + ylab("Densidade") +
    theme_bw()


  if (length(fits)) {
    x <- seq(min(values), max(values), length.out = 1000)
    fits_df <- data.frame(matrix(ncol = 0, nrow = 0))
    if ('gaussian' %in% fits) {
      gfit_df <- data.frame(
        "X" = x,
        "Y" = dnorm(x, mean = mean(values), sd = sd(values)),
        "Ajuste" = 'Normal'
      )
      fits_df <- rbind(fits_df, gfit_df)
    }
    if ('stable' %in% fits) {
      stfit_df <- data.frame(
        "X" = x,
        "Y" = stblfit <- dstable(x, alpha = fits_param['alpha'], beta = fits_param['beta'],
                                 gamma = fits_param['gamma'], delta = fits_param['delta']),
        "Ajuste" = 'Alfa-Estavel'
      )
      fits_df <- rbind(fits_df, stfit_df)
    }
    colors <- c("green3", "red3")
    plot <- plot + geom_line(data = fits_df, aes(x=X, y=Y, group=Ajuste, color=Ajuste), size=1.1) +
      scale_color_manual(values = colors[1:(length(fits))])
  }

  return(plot)
}