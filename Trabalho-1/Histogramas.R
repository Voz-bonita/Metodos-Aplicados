pacman::p_load(ggplot2, cowplot, dplyr)


### Arquivos CSV com os conjuntos de dados escolhidos
Apple <- read.csv("Trabalho-1/AAPL.csv")
Samsung <- read.csv("Trabalho-1/SMSN.IL.csv") %>%
  filter(High != "null")

apl_plot <- ggplot(data = Apple) +
  geom_histogram(aes(x = High, y = ..density..),
                 fill = "darkslategray1", color = "black",
                 bins = 15) +
  xlab("Valor") + ylab("Densidade")

ssg_plot <- ggplot(data = Samsung) +
  geom_histogram(aes(x = as.double(High), y = ..density..),
                 fill = "seagreen1", color = "black",
                 bins = 15) +
  xlab("Valor") + ylab("Densidade")

ggdraw() +
  draw_plot(apl_plot, x = 0, y = 0.5, width = 1, height = 0.5) +
  draw_plot(ssg_plot, x = 0, y = 0, width = 1, height = 0.5) +
  draw_plot_label(label = c("Apple", "Samsung"), size = 12,
                  x = c(-0.025, -0.045), y = c(1.01, 0.52))
