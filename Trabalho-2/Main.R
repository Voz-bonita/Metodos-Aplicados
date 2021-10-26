pacman::p_load("ggplot2", "fExtremes", "extRemes", "dplyr", "purrr", "knitr", "kableExtra")
source("Trabalho-1/Custom_Functions.R")

SSG <- read.csv("Trabalho-2/SSG.csv")$x
APL <- read.csv("Trabalho-2/APL.csv")$x

teste.kendall <- cor.test(SSG, APL, method = "kendall")

ggplot() +
  geom_point(mapping = aes(x = SSG, y = APL), size = 2) +
  theme_bw() + xlab("SSG15") + ylab("APL15")


SSG_gevfit1 <- gevFit(SSG, type ="mle")
APL_gevfit1 <- gevFit(APL, type ="mle")

SSG_fit <- fevd(SSG, type="GEV")
APL_fit <- fevd(APL, type="GEV")


years <- c(5,10,20,100,1000)
SSG_Full_ret <- Retorno(SSG_fit, years, nome = "SSG15")
APL_Full_ret <- Retorno(APL_fit, years, nome = "APL15")

SSG_Full_ret %>%
  rbind(APL_Full_ret) %>%
  kbl(booktabs = T, caption = "\\label{tab:retssgdol}Retorno máximo para um investimento de \\$1000 em ações da SSG15 e APL15.") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)










