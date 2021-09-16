pacman::p_load('tidyverse','lubridate','copula',
               'fBasics','StableEstim','stabledist',
               'DT','kableExtra','PerformanceAnalytics',
               'extRemes', 'ismev', 'evmix', 'evd', 'extremis',
               'VGAM', 'fExtremes', 'dplyr', 'ggplot2')


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

SSG_exp <- tab_exp(SSG_ret)
SSG_exp %>%
  kable(caption = "Medidas resumo do retorno das ações da Samsung.", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = F)

APL_exp <- tab_exp(APL_ret)
APL_exp %>%
  kbl(caption = "Medidas resumo do retorno das ações da Apple.", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = F)


Serie(SAMSUNG, col_x = "Date", col_y = "Retorno")
Serie(APPLE, col_x = "Date", col_y = "Retorno")



#### SAMSUNG ####
Hist_Fit(data = SAMSUNG, values = 'Retorno')

ssg_stF1 <- stableFit(SSG_ret, "q", doplot = TRUE)
ssg_stF2 <- stableFit(SSG_ret, "mle", doplot = TRUE)

alpha_params <- c(ssg_stF1@fit[["estimate"]][["alpha"]], ssg_stF1@fit[["estimate"]][["beta"]],
                  ssg_stF1@fit[["estimate"]][["gamma"]], ssg_stF1@fit[["estimate"]][["delta"]])
names(alpha_params) <- names(ssg_stF1@fit[["estimate"]])

Hist_Fit(data = SAMSUNG, values = 'Retorno',
         fits = c('gaussian', 'stable'), fits_param = alpha_params)

### VaR
p <- c(0.95, 0.99, 0.999)
## Historico, Normal, Alfa-Estavel (respectivamente)
map_dbl(p, ~PerformanceAnalytics::VaR(SSG_ret, p=.x, method = "historical"))
map_dbl(p, ~PerformanceAnalytics::VaR(SSG_ret, p=.x, method = "gaussian"))
map_dbl(p, qstable, alpha=alpha_params['alpha'], beta=alpha_params['beta'],
                    gamma=alpha_params['gamma'], delta=alpha_params['delta'])

Bmax <- Bloco_maximo(data = SAMSUNG, values = "Retorno")
SSG_n <- Bmax$n
SSG_tab <- Bmax$Teste %>%
  summarise(Tamanho = Tamanho,
            P.valor = round(P.valor, 2)) %>%
  as_tibble()
SSG_ts <- Bmax$Serie

Bmax <- Bloco_maximo(data = APPLE, values = "Retorno")
APL_n <- Bmax$n
APL_tab <- Bmax$Teste %>%
  summarise(Tamanho = Tamanho,
            P.valor = round(P.valor, 2)) %>%
  as_tibble()
APL_ts <- Bmax$Serie


kbl(SSG_tab[(SSG_n-2):(SSG_n+3),], bookSSG_tabs = T) %>%
   kable_styling(latex_options = c("striped", "hold_position"), full_width = F)

kbl(APL_tab[(APL_n-2):(APL_n+3),], bookAPL_tabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)


Hist_Fit(data = SSG_ts, values = "Retorno", fits = "self", bins = 20)
Serie(data = SSG_ts, col_x = "Datas", col_y = "Retorno")

Hist_Fit(data = APL_ts, values = "Retorno", fits = "self", bins = 20)
Serie(data = APL_ts, col_x = "Datas", col_y = "Retorno")


## Qq plot
SSG_q <- c((1:SSG_n)/(SSG_n+1))
SSG_ginv <- -log(-log(SSG_q))
qqplot(SSG_ts$Retorno, SSG_ginv, xlab="Quantil empírico",ylab="Quantil da Gumbel",main="")
grid()

APL_q <- c((1:APL_n)/(APL_n+1))
APL_ginv <- -log(-log(APL_q))
qqplot(APL_ts$Retorno, APL_ginv, xlab="Quantil empírico",ylab="Quantil da Gumbel",main="")
grid()

## R MLE - GEV

SSG_gevfit1 <- gevFit(SSG_ts$Retorno, type ="mle")
par(mfrow = c(2, 2))
summary(SSG_gevfit1)
SSG_gevfit2 <- gevFit(SSG_ts$Retorno, type ="pwm")
par(mfrow = c(2, 2))
summary(SSG_gevfit1)

APL_gevfit1 <- gevFit(APL_ts$Retorno, type ="mle")
par(mfrow = c(2, 2))
summary(APL_gevfit1)
APL_gevfit2 <- gevFit(APL_ts$Retorno, type ="pwm")
par(mfrow = c(2, 2))
summary(APL_gevfit2)

par(mfrow = c(2, 2))
summary(fitmv1)
## R PWM - GEV


par(mfrow = c(2, 2))
summary(fitpwm1)

xis11 <- fitmv1@fit[["par.ests"]][["xi"]]
mus11 <- fitmv1@fit[["par.ests"]][["mu"]]
betas11 <- fitmv1@fit[["par.ests"]][["beta"]]
xis12 <- fitpwm1@fit[["par.ests"]][["xi"]]
mus12 <- fitpwm1@fit[["par.ests"]][["mu"]]
betas12 <- fitpwm1@fit[["par.ests"]][["beta"]]

rm(fitmv1,fitpwm1)

hist(m1,prob=T,ylim=c(0,60),main='',cex.axis=1.5, cex.lab=1.5, cex=1.5, font.axis=2,lwd=2)
curve(dgev(x, xi = xis11 , mu = mus11, beta = betas11),col='blue', lwd=2, add=TRUE)
curve(dgev(x, xi = xis12, mu = mus12 , beta = betas12),col='green',lwd=2,add=TRUE)
legend('topright',legend=c('MLE','PWM'),col=c('blue','green'),lwd=2)

rm(xis11,mus11,betas11,xis12,mus12,betas12)
par(mfrow=c(1,1))
fit1 <- fevd(m1,type="GEV")
fit1 #positive shape estimate but fairly large standard error
plot(fit1) #The fit looks reasonable
ci(fit1,type="parameter") #As expected the 95% confidence interval includes negative values
return.level(fit1,do.ci=T)

rm(m1,fit1)




#### APPLE ####

ret2<-APPLE$retorno
plot(ret2,type="l")

hist(ret2, n = 50, probability = TRUE, border = "white",
     col = "steelblue", main="", ylim=c(0,40), xlab="log retorno diário Apple",
     ylab="Densidade")

st21<-stableFit(ret2, "q",doplot = TRUE)
st22<-stableFit(ret2, "mle",doplot = TRUE)

alpha2 = st21@fit[["estimate"]][["alpha"]]
beta2 = st21@fit[["estimate"]][["beta"]]
gamma2 = st21@fit[["estimate"]][["gamma"]]
delta2 = st21@fit[["estimate"]][["delta"]]

rm(st21,st22)

hist(ret2,n=50, prob=T,ylim=c(0,40))
lines(seq(min(ret2,na.rm=T),max(ret2,na.rm=T),length=1000),dnorm(seq(min(ret2,na.rm=T),max(ret2,na.rm=T),
                                                                     length=1000),mean(ret2,na.rm=T),sd(ret2,na.rm=T)),lwd=2, col = "blue")

curve(dstable(x, alpha = alpha2, beta = beta2, gamma = gamma2, delta = delta2), -1, 1, col = "red",add=TRUE)

#var historico
PerformanceAnalytics::VaR(ret2, p=0.95, method="historical")
PerformanceAnalytics::VaR(ret2, p=0.99, method="historical")
PerformanceAnalytics::VaR(ret2, p=0.999, method="historical")
# normal
PerformanceAnalytics::VaR(ret2, p=.95, method="gaussian")
PerformanceAnalytics::VaR(ret2, p=.99, method="gaussian")
PerformanceAnalytics::VaR(ret2, p=.999, method="gaussian")
##VaR alpha estavel
qstable(0.95,alpha = alpha2, beta = beta2, gamma = gamma2, delta = delta2, pm = 0,)
qstable(0.99,alpha = alpha2, beta = beta2, gamma = gamma2, delta = delta2, pm = 0,)
qstable(0.999,alpha = alpha2, beta = beta2, gamma = gamma2, delta = delta2, pm = 0,)

rm(alpha2,beta2,gamma2,delta2)

N2 <-length(APPLE$High)
result2 <- data.frame()
for (k in 20:60) {
  n2<-k
  tau2<-floor(N2/n2)
  m2<-numeric(tau2); j2<-1
  for (i in 1:tau2){
    m2[i]<-max(ret2[j2:(j2+n2-1)])
    j2<-j2+n2
  }
  m2 <- m2[-1]
  teste2 <- Box.test(m2, lag = 1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)
  teste2$indice <- k
  teste2 <- c(teste2$indice,teste2$p.value) #
  if(teste2[2]>0.05){
    result2 <- rbind(result2, teste2)}
}
result2 <- tibble(result2)
names(result2) <- c("Tam","Pval")
result2 <- rbind(result2, teste2)
rm(i,j2,k,m2,n2,tau2,teste2)

################## Bloco Máximo ###########
n2<-result2$Tam[result2$Pval == max(result2$Pval)][1]
tau2<-floor(N2/n2)
m2<-matrix(0,tau2,1)
j2<-1
for (i in 1:tau2){
  m2[i]<-max(ret2[j2:(j2+n2-1)])
  j2<-j2+n2}
head(m2)
sum(is.na(m2))
hist(m2,n=tau2, prob=T,ylim=c(0,70))
lines(density(m2),lwd=2)


rm(ret2,N2,result2,i,j2,n2)

par(mfrow=c(1,1))
plot(m2, type="l")

## Qq plot
p2<-c((1:tau2)/(tau2+1))
ginv2<- -log(-log(p2))
qqplot(m2,ginv2,xlab="Quantil empírico",ylab="Quantil da Gumbel",main="qqplot")
grid()

rm(tau2,ginv2,p2)
## R MLE - GEV
fitmv2 = gevFit(m2, type ="mle")
fitmv2
par(mfrow = c(2, 2))
summary(fitmv2)
## R PWM - GEV
fitpwm2 = gevFit(m2, type ="pwm")
fitpwm2
par(mfrow = c(2, 2))
summary(fitpwm2)

xis21 <- fitmv2@fit[["par.ests"]][["xi"]]
mus21 <- fitmv2@fit[["par.ests"]][["mu"]]
betas21 <- fitmv2@fit[["par.ests"]][["beta"]]
xis22 <- fitpwm2@fit[["par.ests"]][["xi"]]
mus22 <- fitpwm2@fit[["par.ests"]][["mu"]]
betas22 <- fitpwm2@fit[["par.ests"]][["beta"]]

rm(fitmv2,fitpwm2)

hist(m2,prob=T,ylim=c(0,60),main='',cex.axis=1.5, cex.lab=1.5, cex=1.5, font.axis=2,lwd=2)
curve(dgev(x, xi = xis21 , mu = mus21, beta = betas21),col='blue', lwd=2, add=TRUE)
curve(dgev(x, xi = xis22, mu = mus22 , beta = betas22),col='green',lwd=2,add=TRUE)
legend('topright',legend=c('MLE','PWM'),col=c('blue','green'),lwd=2)

rm(xis21,mus21,betas21,xis22,mus22,betas22)
par(mfrow=c(1,1))
fit2 <- fevd(m2,type="GEV")
fit2 #positive shape estimate but fairly large standard error
plot(fit2) #The fit looks reasonable
ci(fit2,type="parameter") #As expected the 95% confidence interval includes negative values
return.level(fit2,do.ci=T)

rm(m2,fit2)