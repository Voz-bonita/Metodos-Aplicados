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
ret1 <- SAMSUNG$retorno
plot(ret1,type="l")

hist(ret1, n = 50, probability = TRUE, border = "white",
     col = "steelblue", main="", ylim=c(0,40), xlab="log retorno diário Samsung",
     ylab="Densidade")

st11 <- stableFit(ret1, "q",doplot = TRUE)
st12 <- stableFit(ret1, "mle",doplot = TRUE)

alpha1 <- st11@fit[["estimate"]][["alpha"]]
beta1 <- st11@fit[["estimate"]][["beta"]]
gamma1 <- st11@fit[["estimate"]][["gamma"]]
delta1 <- st11@fit[["estimate"]][["delta"]]

rm(st11,st12)


hist(ret1, n = 50, probability = TRUE, border = "white",
     col = "steelblue", main="", ylim=c(0,40),
     xlab="log retorno diário Samsung", ylab="Densidade")

lines(seq(min(ret1,na.rm=T),max(ret1,na.rm=T),length=1000),
      dnorm(seq(min(ret1, na.rm=T), max(ret1, na.rm=T), length=1000),
            mean(ret1,na.rm=T),
            sd(ret1,na.rm=T)),
      lwd=2, col = "blue")

curve(dstable(x, alpha = alpha1, beta = beta1, gamma = gamma1, delta = delta1),
      -1, 1, col = "red",add=TRUE)

#var historico
PerformanceAnalytics::VaR(ret1, p=0.95, method="historical")
PerformanceAnalytics::VaR(ret1, p=0.99, method="historical")
PerformanceAnalytics::VaR(ret1, p=0.999, method="historical")
# normal
PerformanceAnalytics::VaR(ret1, p=.95, method="gaussian")
PerformanceAnalytics::VaR(ret1, p=.99, method="gaussian")
PerformanceAnalytics::VaR(ret1, p=.999, method="gaussian")
##VaR alpha estavel
qstable(0.95,alpha = alpha1, beta = beta1, gamma = gamma1, delta = delta1, pm = 0,)
qstable(0.99,alpha = alpha1, beta = beta1, gamma = gamma1, delta = delta1, pm = 0,)
qstable(0.999,alpha = alpha1, beta = beta1, gamma = gamma1, delta = delta1, pm = 0,)

rm(alpha1,beta1,gamma1,delta1)

N <- length(SAMSUNG$High)
result1 <- data.frame()
for (k in 1:60) {
  n1<-k
  tau1<-floor(N/n1)
  m1<-numeric(tau1); j1<-1
  for (i in 1:tau1){
    m1[i]<-max(ret1[j1:(j1+n1-1)])
    j1<-j1+n1
  }
  m1 <- m1[-1]
  teste1 <- Box.test(m1, lag = 1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)
  teste1$indice <- k
  teste1 <- c(teste1$indice,teste1$p.value) #
  if(teste1[2]>0.05){
    result1 <- rbind(result1, teste1)}
}
result1 <- tibble(result1)
names(result1) <- c("Tam","Pval")
result1 <- rbind(result1, teste1)
rm(i,j1,k,m1,n1,tau1,teste1)

################## Bloco Máximo ###########
if (TRUE %in% (result1$Tam < 0.05)) {
  n1 <- max(which((result1$Pval < 0.1) == TRUE))
} else {
  n1 <- 1
}

tau1 <- floor(N/n1)
m1 <- matrix(0,tau1,1)
j1 <- 1
for (i in 1:tau1){
  m1[i]<-max(ret1[j1:(j1+n1-1)])
  j1<-j1+n1}
head(m1)

hist(m1, n=1007, prob=T,ylim=c(0,70))
lines(density(m1),lwd=2, col="blue")


rm(ret1,result1,i,j1,n1)

par(mfrow=c(1,1))
plot(m1, type="l")

## Qq plot
p1 <- c((1:tau1)/(tau1+1))
ginv1<- -log(-log(p1))
qqplot(m1,ginv1,xlab="Quantil empírico",ylab="Quantil da Gumbel",main="qqplot")
grid()

rm(tau1,ginv1,p1)
## R MLE - GEV
fitmv1 <- gevFit(m1, type ="mle")
fitmv1
par(mfrow = c(2, 2))
summary(fitmv1)
## R PWM - GEV
fitpwm1 = gevFit(m1, type ="pwm")
fitpwm1
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