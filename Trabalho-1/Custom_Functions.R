### Arquivo contendo todas as funções customizadas utilizadas no trabalho 1
pacman::p_load('dplyr', 'purrr', 'ggplot2')

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
