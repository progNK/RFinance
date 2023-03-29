#####################################
asset <- read.table("/Users/NK/Downloads/KMAZ_200205_210205.csv",header=TRUE,sep=',')
asset[,1] <- as.Date(asset[,1],"%d/%m/%Y")
head(asset)
tail(asset)
imoex<- read.table("/Users/NK/Downloads/IMOEX_200205_210205.csv",header=TRUE,sep=',')
imoex[,1] <- as.Date(imoex[,1],"%d/%m/%Y")
head(imoex)
tail(imoex)

len<-  dim(asset)[1]
b <- 20

#логарифмические доходности
len<-  dim(asset)[1]
r1 <- diff(log(asset[,2]))
r2 <- diff(log(imoex[,2]))
rates.xts <- as.xts(  cbind(r1,r2), order.by = asset[2:len,1])
colnames(rates.xts)<- c("GAZPROM","MOEX")
head(rates.xts)

#коэффициент beta
beta_manual <- cov(rates.xts[, "GAZPROM"],rates.xts[, "MOEX"])/ var(rates.xts[, "MOEX"])

beta <- CAPM.beta(rates.xts[, "GAZPROM"],rates.xts[, "MOEX"])

#коэффициент alpha

Rf <- 0.07
(alpha_manual <- mean(rates.xts[, "GAZPROM"])-Rf- beta*(mean(rates.xts[, "MOEX"])- Rf))
alpha_manual
(alpha <- CAPM.alpha(rates.xts[, "GAZPROM"],rates.xts[, "MOEX"],Rf = 0.07))
alpha
#коэффициент sharp
sharp <- SharpeRatio(rates.xts[, "GAZPROM"], Rf = 0.04, p = 0.95, FUN = c("StdDev"))

#коэффициент нормального VaR уровня 0.95
var_gauss <- (VaR(rates.xts[, "GAZPROM"], p=.95, method="gaussian"))



#коэффициент исторического VaR уровня 0.95

var_hist <- (VaR(rates.xts[, "GAZPROM"], p=.95, method="historical"))
var_hist

#коэффициент исторического Shortfall уровня 0.95

shortfall_gauss <- ES(rates.xts, p=.95, method="gaussian")
shortfall_hist <- ES(rates.xts, p=.95, method="historical")
#коэффициент gaussian
ES(rates.xts, p=.95, method="gaussian")

res <- list(
  beta = beta,
  alpha = alpha,
  sharp=sharp[1],
  var_hist = var_hist,
  var_gauss= var_gauss,
  shortfall_hist=shortfall_hist,
  shortfall_gauss=shortfall_gauss
)
res

