pacman::p_load("ggplot2", "fExtremes", "extRemes", "dplyr", "purrr", "knitr", "kableExtra", "copula",
               "xtable")
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


# Cuidado com o nome dos argumentos ao usar do.call
SSG_par <- as.list(SSG_fit$results$par)
APL_par <- as.list(APL_fit$results$par)
names(SSG_par) <- formalArgs(qgev)[-1]
names(APL_par) <- formalArgs(qgev)[-1]

# Chute inicial baseado no tau de kendall
a.0 <- sin(teste.kendall$estimate * pi/2)

udat<- cbind(
  do.call(pgev, c(list("x" = SSG), SSG_par)),
  do.call(pgev, c(list("x" = APL), APL_par))
)


clayton<- claytonCopula(dim=2)
gumbel<- gumbelCopula(dim=2)
frank<- frankCopula(dim=2)


fit.cl <- fitCopula(clayton, udat, start=a.0)
fit.gu <- fitCopula(gumbel, udat, start=a.0)
fit.fr <- fitCopula(frank,udat, start=a.0)


# AIC e BIC
# 1 parametro livre para AIC e BIC (theta)
n <- nrow(udat)
V <- 1
aicCl <- -2*fit.cl@loglik + 2*V
bicCl <- -2*fit.cl@loglik + V*log(n)

aicGu <- -2*fit.gu@loglik + 2*V
bicGu <- -2*fit.gu@loglik + V*log(n)

aicFr <- -2*fit.fr@loglik + 2*V
bicFr <- -2*fit.fr@loglik + V*log(n)

aic <- c(aicCl, aicGu, aicFr)
bic <- c(bicCl, bicGu, bicFr)

daic <- aic - min(aic)
dbic <- bic - min(bic)

# % De cada modelo ser o melhor modelo
waic <- round(exp(-1/2 * daic) / sum( exp(-1/2 * daic) ) * 100, 2)
wbic <- round(exp(-1/2 * dbic) / sum( exp(-1/2 * dbic) ) * 100, 2)

modelos <- c("Clayton", "Gumbel", "Frank")
pars <- c(fit.cl@copula@parameters, fit.gu@copula@parameters, fit.fr@copula@parameters)
copula.choose <- tibble(
  "Copula" = modelos,
  "Parametros" = pars,
  "$AIC$" = aic,
  "$\\text{w}(AIC)$" = paste0(waic, "\\%"),
  "$BIC$" = bic,
  "$\\text{w}(BIC)$" = paste0(wbic, "\\%"),
)

options(xtable.comment = FALSE)
print(xtable(copula.choose,
             caption = "\\label{tab:fitcop}Criterios de ajuste de modelos para cópulas arquimedianas sobre os dados da SSG15 e APL15."),
      sanitize.text.function = function(x){x},
      latex.environments = "center",
      caption.placement = "top",
      include.rownames=FALSE)


set.seed(2021)
cc <- gumbelCopula(fit.gu@copula@parameters)
sample <- rCopula(67, cc)
lam.true <- lambda(cc)

sample_SSG <- sample[,1]
sample_APL <- sample[,2]

x <- do.call(qgev, c(list("q" = sample_SSG), SSG_par))
y <- do.call(qgev, c(list("q" = sample_APL), APL_par))


plot(SSG, APL, pch = "../Trabalho-1 ", cex = 5 , col="black", xlab="Samsung" , ylab="Apple" )
points(x, y, ylab="Y", pch = "../Trabalho-1 ", cex = 5, col="red")
legend("topright", legend=c("Dados Ajustados", "Dados reais"), col=c("red", "black"), pch = 15)

colors <- c("Dados Reais" = "black", "Dados Simulados" = "red")
ggplot() +
  stat_density_2d(mapping = aes(x = SSG, y = APL, colour = "Dados Reais"), geom="polygon", fill="grey") +
  stat_density_2d(mapping = aes(x = x, y = y, colour = "Dados Simulados")) +
  scale_color_manual(values = colors) +
  guides(color = guide_legend(title = "Origem dos dados")) +
  theme_bw() + xlab("Samsung") + ylab("Apple")



alpha <- c(0.95, 0.975, 0.99)    #    alpha%
theta <- fit.gu@estimate    #    valor estimado do par�metro da c�pula
VaR.b <- data.frame("Confianca" =  paste0(alpha*100, "\\%"),
                    "APL15" = NA,
                    "SSG15" = NA)
#Gerar amostra de tamanho n de uma v�. S~Beta(1,1)=U[0,1]

set.seed(2021)
S <- runif(50, min=0, max=1 )




inversas <- map(alpha, ~inv_gen_gumbel(theta, .x))
estimativas_SSG <- map_dbl(inversas, ~mean( do.call(qgev, c(list("q" = .x), SSG_par))) )
estimativas_APL <- map_dbl(inversas, ~mean( do.call(qgev, c(list("q" = .x), APL_par))) )

VaR.b$SSG15 <- estimativas_SSG
VaR.b$APL15 <- estimativas_APL

kbl(VaR.b, booktabs = T, caption = "\\label{tab:VaRb}VaR bivariado para ações da APL15 e SSG15.") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)

investimento <- 1000
VaR.b$SSG15 <- round(exp(estimativas_SSG)*investimento, 2)
VaR.b$APL15 <- round(exp(estimativas_APL)*investimento, 2)
