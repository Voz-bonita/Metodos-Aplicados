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
    geom_line(aes(x = as.Date(.data[[col_x]]), y = .data[[col_y]])) +
    xlab("Tempo") + ylab("Maximo valor diario alcançado") +
    scale_x_date(date_labels = "%b-%Y", breaks = seq(ano_inicio, ano_fim, 365/2)) +
    theme_bw()
  return(plot)
}


Hist_Fit <- function (data, values, fits=c(), fits_param=c(), bins=30) {
  values <- data[[values]]
  fits <- map_chr(fits, str_to_lower)


  plot <- ggplot() +
    geom_histogram(aes(x = values, y = ..density..),
                   fill = "steelblue", color = "black",
                   bins = bins) +
    xlab("Log-retorno diario") + ylab("Densidade") +
    theme_bw()

  if ('self' %in% fits) {
    plot <- plot + geom_density(aes(values), color="turquoise3",
                                size=1.1)
    fits <- fits[!('self' == fits)]
  }
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


Bloco_maximo <- function (data, values, x_axis = "Date", conf = 0.05) {
  section <- function (n, values) {
    tau <- floor(length(values) / n)
    maximos <- numeric(tau)
    inicio <- 1
    for (i in 1:tau) {
      ## Pega-se o maior valor dado um tamanho de bloco
      ## Exemplo, se n = 3, entao pega-se
      ## as observacoes de 1 a 3 (incluso 3), de 4 a 6 (incluso 6), ...
      maximos[i] <- max(values[inicio: (inicio + n - 1)])
      # Atualiza o indice inicial do bloco
      inicio <- inicio + n
    }
    return(maximos)
  }

  values <- data[[values]]
  datas <- as.character(data[[x_axis]])


  # Data frame que acumula os testes para cada tamanho de bloco
  ans_tab <- data.frame(matrix(nrow = 0, ncol = 2))

  # n trata-se dos tamanhos de bloco
  for (n in 1:60) {
    maximos <- section(n, values)

    ## Teste de Ljung-Box para series historicas
    teste <- Box.test(maximos, lag = 1, type = "Ljung-Box", fitdf = 0)
    teste <- c(n, teste$p.value)
    # Coloca-se o resultado em um dataframe com todos os p-valores
    # para comparacao futura
    ans_tab <- rbind(ans_tab, teste)}

  ans_tab <- ans_tab %>%
    rename_all( ~ c("Tamanho", "P.valor"))

  if (TRUE %in% (ans_tab$P.valor < conf)) {
    # Retorna apenas o indice subsequente ao último indice em que o teste rejeitou independencia
    n <- last(dplyr::filter(ans_tab, P.valor < conf))[[1]] + 1
  } else {
    # Retorna apenas o valor de n que fornece o maior p-valor e o bloco é maior que 7 (arbitrario)
    n <- dplyr::filter(ans_tab, P.valor == max(ans_tab$P.valor) & Tamanho > 7)[["Tamanho"]]
    message("Nao foi encontrado nenhum p-valor mais\nsignificativo que o nivel de confiança")
  }

  Serie_temp <- data.frame(
    "Datas" = section(n, datas),
    "Retorno" = section(n, values)
  )

  ans <- list(n, ans_tab, Serie_temp)

  names(ans) <- c("n", "Teste", "Serie")
  return(ans)
}