pacman::p_load('tidyverse','lubridate','copula',
               'fBasics','StableEstim','stabledist',
               'DT','kableExtra','PerformanceAnalytics',
               'extRemes', 'ismev', 'evmix', 'evd', 'extremis',
               'VGAM', 'fExtremes', 'dplyr', 'ggplot2', 'ggpubr')


ano_inicio <- as.Date("2017-06-30", format = "%Y-%m-%d")
ano_fim <-  as.Date("2021-06-30", format = "%Y-%m-%d")
### Carrega as funções customizadas
source("Trabalho-1/Custom_Functions.R")


SAMSUNG <- read_csv("Trabalho-1/SMSN.IL.csv") %>%
  summarise(Date = as.Date(Date, format = "%Y-%m-%d"), High = as.numeric(High)) %>%
  mutate(Retorno = log(High/dplyr::lag(High))) %>%
  dplyr::filter(Date >= ano_inicio & Date <= ano_fim)

APPLE <- read_csv("Trabalho-1/AAPL.csv") %>%
  summarise(Date = as.Date(Date, format = "%Y-%m-%d"), High = as.numeric(High)) %>%
  mutate(Retorno = log(High/dplyr::lag(High))) %>%
  dplyr::filter(Date >= ano_inicio & Date <= ano_fim)

SAMSUNG <- SAMSUNG[is.finite(SAMSUNG$Retorno), ]
APPLE <- APPLE[is.finite(APPLE$Retorno), ]

## Padronizacao do tamanho do banco
SAMSUNG <- SAMSUNG[-1:-5,]

### Analise exploratoria
head(SAMSUNG, 5) %>%
  kbl(caption="Alta e retorno das ações da Samsung.", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = F)

head(APPLE, 5) %>%
kbl(caption = "Alta e retorno das ações da Apple.", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = F)


Serie(data = SAMSUNG, col_x = "Date", col_y = "High")
Serie(data = APPLE, col_x = "Date", col_y = "High")

## Exploratoria do log-retorno
SSG_ret <- SAMSUNG$Retorno
APL_ret <- APPLE$Retorno

SSG_exp <- tab_exp(SSG_ret) %>%
  map_df(~format(.x, scientific = FALSE))
SSG_exp %>%
  kable(caption = "Medidas resumo do retorno das ações da Samsung.", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = F)

APL_exp <- tab_exp(APL_ret) %>%
  map_df(~format(.x, scientific = FALSE))
APL_exp %>%
  kbl(caption = "Medidas resumo do retorno das ações da Apple.", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = F)


Serie(SAMSUNG, col_x = "Date", col_y = "Retorno")
Serie(APPLE, col_x = "Date", col_y = "Retorno")



Hist_Fit(data = SAMSUNG, values = 'Retorno')
Hist_Fit(data = APPLE, values = 'Retorno')

SSG_stF1 <- stableFit(SSG_ret, "q", doplot = TRUE)
APL_stF1 <- stableFit(APL_ret, "q", doplot = TRUE)
# SSG_stF2 <- stableFit(SSG_ret, "mle", doplot = TRUE)
# APL_stF2 <- stableFit(APL_ret, "mle", doplot = TRUE)

SSG_alpha_params <- SSG_stF1@fit[["estimate"]]
Hist_Fit(data = SAMSUNG, values = 'Retorno',
         fits = c('gaussian', 'stable'), fits_param = list('stable' = SSG_alpha_params))

APL_alpha_params <- APL_stF1@fit[["estimate"]]
Hist_Fit(data = APPLE, values = 'Retorno',
         fits = c('gaussian', 'stable'), fits_param = list('stable' = APL_alpha_params))

### VaR
p <- c(0.95, 0.99, 0.999)
## Historico, Normal, Alfa-Estavel (respectivamente)
SSG_VaR <- tibble(
  "VaR Historico" = map_dbl(p, ~PerformanceAnalytics::VaR(SSG_ret, p=.x, method = "historical")),
  "VaR Gaussiano" = map_dbl(p, ~PerformanceAnalytics::VaR(SSG_ret, p=.x, method = "gaussian")),
  "VaR Alfa-Estavel" = map_dbl(p, qstable, alpha=SSG_alpha_params['alpha'], beta=SSG_alpha_params['beta'],
                               gamma=SSG_alpha_params['gamma'], delta=SSG_alpha_params['delta'])) %>%
  map_df(round, 4) %>%
  mutate("Confianca" = paste0(p*100,"%"), .before = "VaR Historico")
SSG_VaR %>%
  kbl(booktabs = T, caption = "\\label{tab:ssgvar3}VaR (Value at Risk) para os log-retornos diários da Samsung") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)

APL_VaR <- tibble(
  "VaR Historico" = map_dbl(p, ~PerformanceAnalytics::VaR(APL_ret, p=.x, method = "historical")),
  "VaR Gaussiano" = map_dbl(p, ~PerformanceAnalytics::VaR(APL_ret, p=.x, method = "gaussian")),
  "VaR Alfa-Estavel" = map_dbl(p, qstable, alpha=APL_alpha_params['alpha'], beta=APL_alpha_params['beta'],
                               gamma=APL_alpha_params['gamma'], delta=APL_alpha_params['delta'])) %>%
  map_df(round, 4) %>%
  mutate("Confianca" = paste0(p*100,"%"), .before = "VaR Historico")
APL_VaR %>%
  kbl(booktabs = T, caption = "\\label{tab:aplvar3}VaR (Value at Risk) para os log-retornos diários da Apple") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)

SSG_VaR[-1] %>%
  map_df(~round(exp(.x)*1000), 0) %>%
  mutate("Confianca" = paste0(p*100,"%"), .before = "VaR Historico") %>%
  rename_all( ~ c("Confiança", "VaR Historico($)", "VaR Gaussiano($)", "VaR Alfa-Estável($)")) %>%
  kbl(booktabs = T, caption = "\\label{tab:ssgvar3dol}Retorno máximo esperado de um dia para o outro, para um investimento de \\$1000 em ações da Samsung.") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)


APL_VaR[-1] %>%
  map_df(~round(exp(.x)*1000), 0) %>%
  mutate("Confianca" = paste0(p*100,"%"), .before = "VaR Historico") %>%
  rename_all( ~ c("Confiança", "VaR Historico($)", "VaR Gaussiano($)", "VaR Alfa-Estável($)")) %>%
  kbl(booktabs = T, caption = "\\label{tab:aplvar3dol}Retorno máximo esperado de um dia para o outro, para um investimento de \\$1000 em ações da Apple.") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)



Bmax <- Bloco_maximo(data = SAMSUNG, values = "Retorno")
SSG_n <- Bmax$n
SSG_tab <- Bmax$Teste %>%
  summarise(Tamanho = Tamanho,
            P.valor = round(P.valor, 2)) %>%
  as_tibble()
SSG_ts <- Bmax$Serie %>%
  dplyr::select(c("Date", "Retorno"))

Bmax <- Bloco_maximo(data = APPLE, values = "Retorno", force = c(TRUE, 30))
APL_n <- Bmax$n
APL_tab <- Bmax$Teste %>%
  summarise(Tamanho = Tamanho,
            P.valor = round(P.valor, 2)) %>%
  as_tibble()
APL_ts <- Bmax$Serie %>%
  dplyr::select(c("Date", "Retorno"))


kbl(SSG_tab[(SSG_n-2):(SSG_n+3),], booktabs = T,
    caption = "P-valores do teste de Ljung-Box para diferentes tamanhos de
     bloco máximo dos retornos das ações da Samsung") %>%
   kable_styling(latex_options = c("striped", "hold_position"), full_width = F)

kbl(APL_tab[(APL_n-2):(APL_n+3),], booktabs = T,
    caption = "P-valores do teste de Ljung-Box para diferentes tamanhos de
     bloco máximo dos retornos das ações da Apple") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)

ggarrange(Hist_Fit(data = SSG_ts, values = "Retorno", fits = "self", bins = 15),
          Serie(data = SSG_ts, col_x = "Date", col_y = "Retorno", 2),
          nrow = 1, ncol = 2)

ggarrange(Hist_Fit(data = APL_ts, values = "Retorno", fits = "self", bins = 10),
          Serie(data = APL_ts, col_x = "Date", col_y = "Retorno", 2),
          nrow = 1, ncol = 2)


## Qq plot
SSG_q <- c((1:SSG_n)/(SSG_n+1))
SSG_ginv <- -log(-log(SSG_q))
qqplot(SSG_ts$Retorno, SSG_ginv, xlab="Quantil empírico",ylab="Quantil da Fréchet",main="")
grid()

APL_q <- c((1:APL_n)/(APL_n+1))
APL_ginv <- -log(-log(APL_q))
qqplot(APL_ts$Retorno, APL_ginv, xlab="Quantil empírico",ylab="Quantil da Gumbel",main="")
grid()

## R MLE - GEV


SSG_gevfit1 <- gevFit(SSG_ts$Retorno, type ="mle")
SSG_gevfit2 <- gevFit(SSG_ts$Retorno, type ="pwm")


APL_gevfit1 <- gevFit(APL_ts$Retorno, type ="mle")
APL_gevfit2 <- gevFit(APL_ts$Retorno, type ="pwm")


SSG_gevmle <- SSG_gevfit1@fit[['par.ests']]
SSG_gevpwm <- SSG_gevfit2@fit[['par.ests']]

APL_gevmle <- APL_gevfit1@fit[['par.ests']]
APL_gevpwm <- APL_gevfit2@fit[['par.ests']]

Hist_Fit(data = SSG_ts, values = 'Retorno', bins = 15,
          fits = c("gevmle", "gevpwm"), fits_param = list("gevmle" = SSG_gevmle,
                                                          "gevpwm" = SSG_gevpwm))

Hist_Fit(data = APL_ts, values = 'Retorno', bins = 10,
         fits = c("gevmle", "gevpwm"), fits_param = list("gevmle" = APL_gevmle,
                                                         "gevpwm" = APL_gevpwm))

par(mfrow = c(2, 2))
summary(SSG_gevfit1)
summary(SSG_gevfit2)
summary(APL_gevfit1)
summary(APL_gevfit2)


par(mfrow=c(1,1))
SSG_fit <- fevd(SSG_ts$Retorno, type="GEV")
pars <- data.frame("Locacao" = SSG_fit$results$par['location'],
           "Escala" = SSG_fit$results$par['scale'],
           "Forma" = SSG_fit$results$par['shape'])
row.names(pars) <- ""
kbl(pars, booktabs = T,
    caption = "Parâmetros estimados para o ajuste de uma distribuição GEV para dados SSG15.") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)


APL_fit <- fevd(APL_ts$Retorno, type="GEV")
pars <- data.frame("Locacao" = APL_fit$results$par['location'],
                   "Escala" = APL_fit$results$par['scale'],
                   "Forma" = APL_fit$results$par['shape'])
row.names(pars) <- ""
kbl(pars, booktabs = T,
    caption = "Parâmetros estimados para o ajuste de uma distribuição GEV para dados APL57.") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)


SSG_fit$call <- ""
plot(SSG_fit)

APL_fit$call <- ""
plot(APL_fit)


ci(SSG_fit,type="parameter")

years <- c(2,5,10,20)
SSG_Full_ret <- Retorno(SSG_fit, years)
SSG_Full_ret[-1] %>%
  map_df(round, 4) %>%
  mutate(Tempo = paste(years, "Anos", sep = " "), .before = names(SSG_Full_ret)[2]) %>%
  kbl(booktabs = T, caption="\\label{tab:retssg}Intervalos de confiança para os retornos esperados da SSG15.") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)

APL_Full_ret <- Retorno(APL_fit, years)
APL_Full_ret[-1] %>%
  map_df(round, 4) %>%
  mutate(Tempo = paste(years, "Anos", sep = " "), .before = names(APL_Full_ret)[2]) %>%
  kbl(booktabs = T, caption="\\label{tab:retapl}Intervalos de confiança para os retornos esperados da APL57.") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)



SSG_Full_ret[-1] %>%
  map_df(~round(exp(.x)*1000), 0) %>%
  mutate(Tempo = paste(years, "Anos", sep = " "), .before = names(SSG_Full_ret)[2]) %>%
  rename_all( ~ names(SSG_Full_ret)) %>%
  kbl(booktabs = T, caption = "\\label{tab:retssgdol}Retorno máximo para um investimento de \\$1000 em ações da SSG15.") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)


APL_Full_ret[-1] %>%
  map_df(~round(exp(.x)*1000), 0) %>%
  mutate(Tempo = paste(years, "Anos", sep = " "), .before = names(APL_Full_ret)[2]) %>%
  rename_all( ~ names(APL_Full_ret)) %>%
  kbl(booktabs = T, caption = "\\label{tab:retapldol}Retorno máximo para um investimento de \\$1000 em ações da APL30.") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)
