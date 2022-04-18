#library
#####

library(quantmod)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(xts)
library(rugarch)
library(rmgarch)
library(car)
library(fitdistrplus)
library(cowplot)
library(ggplot2)
library(ellipse)
library(corrplot)
library(scales)
library(expss)
library(copula)

#####

#company portfolio making 
#####
companies_aggressive <- c("RIL", "TCS", "HB", "BFL")
companies_defensive <- c("SUNP", "IOCL","RAJEX", "HINU")
companies_rs <- c("LL", "FinoInd", "Suprajit", "Sonata")
weight <- c(0.25,0.25,0.25,0.25)
#####
toDate <- function(o) as.Date(o, origin = "2020-01-01", format = "%d-%m-%Y") #imp 

#importing data 
#####
#reliance
RIL <- read.csv("C:\\Users\\prati\\Downloads\\data\\reliance.csv")
#View(RIL)
RIL$Date <- as.Date(RIL$Date, format = "%d-%m-%Y")
z <- read.csv.zoo(RIL, header = TRUE, sep = ",", FUN = toDate)
RIL1 <- as.xts(z)
RIL1_ret <- dailyReturn(RIL1$Close)[-1]
#View(RIL1_ret)

#tata consultancy service
TCS <- read.csv("C:\\Users\\prati\\Downloads\\data\\tcs.csv")
#View(TCS)
TCS$Date <- as.Date(TCS$Date, format = "%d-%m-%Y")
y <- read.csv.zoo(TCS, header = TRUE, sep = ",", FUN = toDate)
TCS1 <- as.xts(y)
TCS1_ret <- dailyReturn(TCS1$Close)[-1]
#View(TCS1_ret)

#hdfcbank
HB <- read.csv("C:\\Users\\prati\\Downloads\\data\\hdfcbank.csv")
#View(HB)
HB$Date <- as.Date(HB$Date, format = "%d-%m-%Y")
x <- read.csv.zoo(HB, header = TRUE, sep = ",", FUN = toDate)
HB1 <- as.xts(x)
HB1_ret <- dailyReturn(HB1$Close)[-1]
#View(HB1_ret)

#bajajfinservice
BFL <- read.csv("C:\\Users\\prati\\Downloads\\data\\bajajfinserv.csv")
#View(BFL)
BFL$Date <- as.Date(BFL$Date, format = "%d-%m-%Y")
w <- read.csv.zoo(BFL, header = TRUE, sep = ",", FUN = toDate)
BFL1 <- as.xts(w)
BFL1_ret <- dailyReturn(BFL1$Close)[-1]
#View(BFL1_ret)

#SUN PHARMA
SUNP <- read.csv("C:\\Users\\prati\\Downloads\\data\\sun pharma.csv")
#View(SUNP)
SUNP$Date <- as.Date(SUNP$Date, format = "%d-%m-%Y")
Z <- read.csv.zoo(SUNP, header = TRUE, sep = ",", FUN = toDate)
SUNP1<- as.xts(Z)
SUNP1_ret <- dailyReturn(SUNP1$Close)[-1]
#View(SUNP1_ret)

#IOCL
IOCL <- read.csv("C:\\Users\\prati\\Downloads\\data\\iocl.csv")
#View(IOCL)
IOCL$Date <- as.Date(IOCL$Date, format = "%d-%m-%Y")
Y <- read.csv.zoo(IOCL, header = TRUE, sep = ",", FUN = toDate)
IOCL1 <- as.xts(Y)
IOCL1_ret <- dailyReturn(IOCL1$Close)[-1]
#View(IOCL1_ret)

#RAJESH EXPORT
RAJEX <- read.csv("C:\\Users\\prati\\Downloads\\data\\rajesh exports.csv")
#View(RAJEX)
RAJEX$Date <- as.Date(RAJEX$Date, format = "%d-%m-%Y")
X <- read.csv.zoo(RAJEX, header = TRUE, sep = ",", FUN = toDate)
RAJEX1 <- as.xts(X)
RAJEX1_ret <- dailyReturn(RAJEX1$Close)[-1]
#View(RAJEX1_ret)

#HINDUSTAN UNILIVER
HINU <- read.csv("C:\\Users\\prati\\Downloads\\data\\hindustan uniliver.csv")
#View(HINU)
HINU$Date <- as.Date(HINU$Date, format = "%d-%m-%Y")
W <- read.csv.zoo(HINU, header = TRUE, sep = ",", FUN = toDate)
HINU1 <- as.xts(W)
HINU1_ret <- dailyReturn(HINU1$Close)[-1]
#View(HINU1_ret)

#lauruslabs
LL <- read.csv("C:\\Users\\prati\\Downloads\\data\\LaurusLabs.csv")
#View(LL)
LL$Date <- as.Date(LL$Date, format = "%d-%m-%Y")
a <- read.csv.zoo(LL, header = TRUE, sep = ",", FUN = toDate)
LL1 <- as.xts(a)
LL1_ret <- dailyReturn(LL1$Close)[-1]
#View(LL1_ret)

#finolex pipe
FinoInd<- read.csv("C:\\Users\\prati\\Downloads\\data\\FI.csv")
#View(FinoInd)
FinoInd$Date <- as.Date(FinoInd$Date, format = "%d-%m-%Y")
b <- read.csv.zoo(FinoInd, header = TRUE, sep = ",", FUN = toDate)
FinoInd1 <- as.xts(b)
FinoInd1_ret <- dailyReturn(FinoInd1$Close)[-1]
#View(FinoInd1_ret)

#Suprajit
Suprajit <- read.csv("C:\\Users\\prati\\Downloads\\data\\Suprajit.csv")
#View(Suprajit)
Suprajit$Date <- as.Date(Suprajit$Date, format = "%d-%m-%Y")
c <- read.csv.zoo(Suprajit, header = TRUE, sep = ",", FUN = toDate)
Suprajit1 <- as.xts(c)
Suprajit1_ret <- dailyReturn(Suprajit1$Close)[-1]
#View(Suprajit1_ret)

#sonatasoftware
Sonata <- read.csv("C:\\Users\\prati\\Downloads\\data\\sonatasoftware.csv")
#View(Sonata)
Sonata$Date <- as.Date(Sonata$Date, format = "%d-%m-%Y")
d <- read.csv.zoo(Sonata, header = TRUE, sep = ",", FUN = toDate)
Sonata1 <- as.xts(d)
Sonata1_ret <- dailyReturn(Sonata1$Close)[-1]
#View(Sonata1_ret)


#####

#correlation
#####
cor_RIL_TCS <- cor(RIL1_ret, TCS1_ret)
cor_RIL_HB <- cor(RIL1_ret, HB1_ret)
cor_RIL_BFL <- cor(RIL1_ret, BFL1_ret)
cor_TCS_HB <- cor(TCS1_ret, HB1_ret)
cor_TCS_BFL <- cor(TCS1_ret, BFL1_ret)
cor_HB_BFL<- cor(HB1_ret, BFL1_ret)
correlation_agg <- c(cor_RIL_TCS, cor_RIL_HB, cor_RIL_BFL, cor_TCS_HB, cor_TCS_BFL, cor_HB_BFL)
correlation_agg

num_company_agg <- length(companies_aggressive)
cor_ret_agg <- matrix(NA, num_company_agg, num_company_agg) #defined a matrix
diag(cor_ret_agg) <- 1

k <- 1
for (i in 1 : (num_company_agg - 1)) {
  for (j in (i+1) : num_company_agg) {
    cor_ret_agg[i,j] <- cor_ret_agg[j,i] <- correlation_agg[k]
    k = k + 1
  }
  k = 1
}

corrplot(cor_ret_agg, method = "color",addCoef.col = "white",
         title="Correlation of Agressive stock")

cor_SUNP_IOCL <- cor(SUNP1_ret,IOCL1_ret)
cor_SUNP_RAJEX <- cor(SUNP1_ret,RAJEX1_ret)
cor_SUNP_HINU <- cor(SUNP1_ret,HINU1_ret)
cor_IOCL_RAJEX <- cor(IOCL1_ret,RAJEX1_ret)
cor_IOCL_HINU <- cor(IOCL1_ret,HINU1_ret)
cor_RAJEX_HINU<- cor(RAJEX1_ret,HINU1_ret)

correlation_def <- c(cor_SUNP_IOCL,cor_SUNP_RAJEX,cor_SUNP_HINU,cor_IOCL_RAJEX,
                 cor_IOCL_HINU,cor_RAJEX_HINU)
num_company_def <- length(companies_defensive)
cor_ret_def <- matrix(NA, num_company_def, num_company_def) #defined a matrix
diag(cor_ret_def) <- 1
k <- 1
for (i in 1 : (num_company_def - 1)) {
  for (j in (i+1) : num_company_def) {
    cor_ret_def[i,j] <- cor_ret_def[j,i] <- correlation_def[k]
    k = k + 1
  }
  k = 1
}
corrplot(cor_ret_def, method = "color",addCoef.col = "white",
         title="Correlation of Defensive stock")

cor_LL_FinoInd <- cor(LL1_ret, FinoInd1_ret)
cor_LL_Suprajit <- cor(LL1_ret, Suprajit1_ret)
cor_LL_Sonata <- cor(LL1_ret, Sonata1_ret)
cor_FinoInd_Suprajit <- cor(FinoInd1_ret, Suprajit1_ret)
cor_FinoInd_Sonata <- cor(FinoInd1_ret, Sonata1_ret)
cor_Suprajit_Sonata<- cor(Suprajit1_ret, Sonata1_ret)
correlation_rs <- c(cor_LL_FinoInd, cor_LL_Suprajit, cor_LL_Sonata, cor_FinoInd_Suprajit, cor_FinoInd_Sonata, cor_Suprajit_Sonata)
correlation_rs

num_company_rs <- length(companies_rs)
cor_ret_rs <- matrix(NA, num_company_rs, num_company_rs) #defined a matrix
diag(cor_ret_rs) <- 1

k <- 1
for (i in 1 : (num_company_rs - 1)) {
  for (j in (i+1) : num_company_rs) {
    cor_ret_rs[i,j] <- cor_ret_rs[j,i] <- correlation_rs[k]
    k = k + 1
  }
  k = 1
}

corrplot(cor_ret_rs, method = "color",addCoef.col = "white",
         title="Correlation of Rising Star stock")


#####

#Corvariance
#####
cov_RIL_TCS <- cov(RIL1_ret, TCS1_ret)
cov_RIL_HB <- cov(RIL1_ret, HB1_ret)
cov_RIL_BFL <- cov(RIL1_ret, BFL1_ret)
cov_TCS_HB <- cov(TCS1_ret, HB1_ret)
cov_TCS_BFL <- cov(TCS1_ret, BFL1_ret)
cov_HB_BFL<- cov(HB1_ret, BFL1_ret)
corvariance_agg <- c(cov_RIL_TCS, cov_RIL_HB, cov_RIL_BFL,
                 cov_TCS_HB, cov_TCS_BFL, cov_HB_BFL)

cov_ret_agg <- matrix(NA, num_company_agg,num_company_agg)
diag(cov_ret_agg) <- c(cov(RIL1_ret, RIL1_ret), cov(TCS1_ret, TCS1_ret), cov(HB1_ret, HB1_ret), cov(BFL1_ret, BFL1_ret))

k <- 1
for (i in 1 : (num_company_agg - 1)) {
  for (j in (i+1) : num_company_agg) {
    options(scipen = 999) # A penalty to be applied when deciding to print numeric values in fixed or exponential notation
    cov_ret_agg[i,j] <- cov_ret_agg[j,i] <- corvariance_agg[k]
    k = k + 1
  }
  k = 1
}

cov_SUNP_IOCL <- cov(SUNP1_ret,IOCL1_ret)
cov_SUNP_RAJEX <- cov(SUNP1_ret,RAJEX1_ret)
cov_SUNP_HINU <- cov(SUNP1_ret,HINU1_ret)
cov_IOCL_RAJEX <- cov(IOCL1_ret,RAJEX1_ret)
cov_IOCL_HINU <- cov(IOCL1_ret,HINU1_ret)
cov_RAJEX_HINU<- cov(RAJEX1_ret,HINU1_ret)
corvariance_def <- c(cov_SUNP_IOCL,cov_SUNP_RAJEX,cov_SUNP_HINU,cov_IOCL_RAJEX,
                 cov_IOCL_HINU,cov_RAJEX_HINU)
cov_ret_def <- matrix(NA, num_company_def,num_company_def)
diag(cov_ret_def) <- c( cov(SUNP1_ret,SUNP1_ret), cov(IOCL1_ret,IOCL1_ret),
                    cov(RAJEX1_ret,RAJEX1_ret), cov(HINU1_ret,HINU1_ret))
cov_ret_def <- matrix(NA, num_company_def,num_company_def)

k <- 1
for (i in 1 : (num_company_def - 1)) {
  for (j in (i+1) : num_company_def) {
    options(scipen = 999) # A penalty to be applied when deciding to print numeric values in fixed or exponential notation
    cov_ret_def[i,j] <- cov_ret_def[j,i] <- corvariance_def[k]
    k = k + 1
  }
  k = 1
}

cov_LL_FinoInd <- cov(LL1_ret, FinoInd1_ret)
cov_LL_Suprajit <- cov(LL1_ret, Suprajit1_ret)
cov_LL_Sonata <- cov(LL1_ret, Sonata1_ret)
cov_FinoInd_Suprajit <- cov(FinoInd1_ret, Suprajit1_ret)
cov_FinoInd_Sonata <- cov(FinoInd1_ret, Sonata1_ret)
cov_Suprajit_Sonata<- cov(Suprajit1_ret, Sonata1_ret)
corvariance_rs <- c(cov_LL_FinoInd, cov_LL_Suprajit, cov_LL_Sonata,
                 cov_FinoInd_Suprajit, cov_FinoInd_Sonata, cov_Suprajit_Sonata)

cov_ret_rs <- matrix(NA, num_company_rs,num_company_rs)
diag(cov_ret_rs) <- c(cov(LL1_ret, LL1_ret), cov(FinoInd1_ret, FinoInd1_ret), cov(Suprajit1_ret, Suprajit1_ret), cov(Sonata1_ret, Sonata1_ret))

k <- 1
for (i in 1 : (num_company_rs - 1)) {
  for (j in (i+1) : num_company_rs) {
    options(scipen = 999) # A penalty to be applied when deciding to print numeric values in fixed or exponential notation
    cov_ret_rs[i,j] <- cov_ret_rs[j,i] <- corvariance_rs[k]
    k = k + 1
  }
  k = 1
}
#####

#plotting the data
#####

RIL_plot <- ggplot() +
  geom_line(aes(x = RIL$Date, y =RIL$Close),
            colour = 'black') +
  ggtitle('RIL Stock Price') +
  xlab('TIME') +
  ylab('PRICE')

TCS_plot <- ggplot() +
  geom_line(aes(x = TCS$Date, y =TCS$Close),
            colour = 'black') +
  ggtitle('TCS Stock Price') +
  xlab('TIME') +
  ylab('PRICE')

HB_plot <- ggplot() +
  geom_line(aes(x = HB$Date, y =HB$Close),
            colour = 'black') +
  ggtitle('HDFC BANK Stock Price') +
  xlab('TIME') +
  ylab('PRICE')

BFL_plot <- ggplot() +
  geom_line(aes(x = BFL$Date, y =BFL$Close),
            colour = 'black') +
  ggtitle('Bajaj Finservice Stock Price') +
  xlab('TIME') +
  ylab('PRICE')

plot_grid(RIL_plot, TCS_plot, HB_plot, BFL_plot, labels = "AUTO")


SUNP_PLOT<- ggplot() +geom_line(aes(x = SUNP$Date, y =SUNP$Close),
                                colour = 'black') +
  ggtitle('SUN PHARMA STOCK PRICE') +
  xlab('TIME') +
  ylab('PRICE')

IOCL_PLOT <- ggplot() + geom_line(aes(x = IOCL$Date, y =IOCL$Close),
                                  colour = 'black') +
  ggtitle('IOCL STOCK PRICE') +
  xlab('TIME') +
  ylab('PRICE')

RAJEX_PLOT <- ggplot() + geom_line(aes(x = RAJEX$Date, y =RAJEX$Close),
                                   colour = 'black') +
  ggtitle('RAJESH EXPORTS STOCK PRICE') +
  xlab('TIME') +
  ylab('PRICE')

HINU_PLOT<- ggplot() +geom_line(aes(x = HINU$Date, y =HINU$Close),
                                colour = 'black') +
  ggtitle('HINDUSTAN UNILIVER STOCK PRICE') +
  xlab('TIME') +
  ylab('PRICE')

plot_grid( SUNP_PLOT,IOCL_PLOT,RAJEX_PLOT,HINU_PLOT, labels = "AUTO")

LL_plot <- ggplot() +
  geom_line(aes(x = LL$Date, y =LL$Close),
            colour = 'black') +
  ggtitle('LL Stock Price') +
  xlab('TIME') +
  ylab('PRICE')

FinoInd_plot <- ggplot() +
  geom_line(aes(x = FinoInd$Date, y =FinoInd$Close),
            colour = 'black') +
  ggtitle('FinoInd Stock Price') +
  xlab('TIME') +
  ylab('PRICE')

Suprajit_plot <- ggplot() +
  geom_line(aes(x = Suprajit$Date, y =Suprajit$Close),
            colour = 'black') +
  ggtitle('Suprajit Stock Price') +
  xlab('TIME') +
  ylab('PRICE')

Sonata_plot <- ggplot() +
  geom_line(aes(x = Sonata$Date, y =Sonata$Close),
            colour = 'black') +
  ggtitle('SONATA SOFTWARE Stock Price') +
  xlab('TIME') +
  ylab('PRICE')

plot_grid(LL_plot, FinoInd_plot, Suprajit_plot, Sonata_plot, labels = "AUTO")

#####

#importing data for nifty
#####
N50 <- read.csv("C:\\Users\\prati\\Downloads\\data\\NIFTY50.csv")
#View(N50)
N50$Date <- as.Date(N50$Date, format = "%d-%m-%Y")
v <- read.csv.zoo(N50, header = TRUE, sep = ",", FUN = toDate)
N501 <- as.xts(v)
N501_ret <- dailyReturn(N501$Close)[-1]
#View(N501_ret)
N50_PLOT<- ggplot() +geom_line(aes(x = N50$Date, y =N50$Close),
                                colour = 'black') +
  ggtitle('Nifty 50 Index PRICE') +
  xlab('TIME') +
  ylab('PRICE')
N50_PLOT
#####


#beta values
#####
beta_RIL <- cov(RIL1_ret, N501_ret)/var(N501_ret)
beta_TCS <- cov(TCS1_ret, N501_ret)/var(N501_ret)
beta_HB <- cov(HB1_ret, N501_ret)/var(N501_ret)
beta_BFL <- cov(BFL1_ret, N501_ret)/var(N501_ret)
beta_Agg_Port <- sum(c(beta_RIL,beta_TCS,beta_HB,beta_BFL)*weight)
beta_Agg_Port

Port_beta_A <- cbind(beta_RIL,beta_TCS,beta_HB,beta_BFL)
colnames(Port_beta_A) <- companies_aggressive
rownames(Port_beta_A) <- "Beta"
View(Port_beta_A)

beta_SUNP <- cov(SUNP1_ret, N501_ret)/var(N501_ret)
beta_IOCL <- cov(IOCL1_ret, N501_ret)/var(N501_ret)
beta_RAJEX <- cov(RAJEX1_ret, N501_ret)/var(N501_ret)
beta_HINU <- cov(HINU1_ret, N501_ret)/var(N501_ret)
beta_Def_Port <- sum(c(beta_SUNP,beta_IOCL,beta_RAJEX,beta_HINU)*weight)
beta_Def_Port

Port_beta_D <- cbind(beta_SUNP,beta_IOCL,beta_RAJEX,beta_HINU)
colnames(Port_beta_D) <- companies_defensive
rownames(Port_beta_D) <- "Beta"
View(Port_beta_D)

beta_LL <- cov(LL1_ret, N501_ret)/var(N501_ret)
beta_FinoInd <- cov(FinoInd1_ret, N501_ret)/var(N501_ret)
beta_Suprajit <- cov(Suprajit1_ret, N501_ret)/var(N501_ret)
beta_Sonata <- cov(Sonata1_ret, N501_ret)/var(N501_ret)
beta_rs_Port <- sum(c(beta_LL,beta_FinoInd,beta_Suprajit,beta_Sonata)*weight)
beta_rs_Port

Port_beta_rs <- cbind(beta_LL,beta_FinoInd,beta_Suprajit,beta_Sonata)
colnames(Port_beta_rs) <- companies_rs
rownames(Port_beta_rs) <- "Beta"
View(Port_beta_rs)

#####


#importing teasury bill data
#####
TBILL <- read.csv("C:\\Users\\prati\\Downloads\\data\\tbill_10yr.csv")
#View(TBILL)
TBILL$Date <- as.Date(TBILL$Date, format = "%d-%m-%Y")
u <- read.csv.zoo(TBILL, header = TRUE, sep = ",", FUN = toDate)
TBILL1 <- as.xts(u)
TBILL1_ret <- dailyReturn(TBILL1$Close)[-1]
#View(TBILL1_ret)

TBILL_plot <- ggplot() +
  geom_line(aes(x = TBILL$Date, 
                y = TBILL$Close),
            colour = 'black') +
  ggtitle('10 T Bill') +
  xlab('TIME') +
  ylab('PRICE')

plot(TBILL_plot)
#####
RF <- mean(TBILL$Close) #risk free return

#calculation of alpha value
#####
RIL_rf <- CAPM.alpha(RIL1_ret, N501_ret, RF)
TCS_rf <- CAPM.alpha(TCS1_ret, N501_ret, RF)
HB_rf <- CAPM.alpha(HB1_ret, N501_ret, RF)
BFL_rf <- CAPM.alpha(BFL1_ret, N501_ret, RF)
Port_rf_A <- cbind(RIL_rf, TCS_rf, HB_rf, BFL_rf)
alpha_Agg_Port <- sum(c(RIL_rf,TCS_rf,HB_rf,BFL_rf)*weight)
alpha_Agg_Port


colnames(Port_rf_A) <- companies_aggressive
rownames(Port_rf_A) <- "Alpha"
View(Port_rf_A)

SUNP_rf <- CAPM.alpha(SUNP1_ret, N501_ret, RF)
IOCL_rf <- CAPM.alpha(IOCL1_ret, N501_ret, RF)
RAJEX_rf <- CAPM.alpha(RAJEX1_ret, N501_ret, RF)
HINU_rf <- CAPM.alpha(HINU1_ret, N501_ret, RF)
Port_rf_D <- cbind(SUNP_rf,RAJEX_rf,HINU_rf,IOCL_rf)
alpha_Def_Port <- sum(c(SUNP_rf,IOCL_rf,RAJEX_rf,HINU_rf)*weight)
alpha_Def_Port


colnames(Port_rf_D) <- companies_defensive
rownames(Port_rf_D) <- "Alpha"
View(Port_rf_D)

LL_rf <- CAPM.alpha(LL1_ret, N501_ret, RF)
FinoInd_rf <- CAPM.alpha(FinoInd1_ret, N501_ret, RF)
Suprajit_rf <- CAPM.alpha(Suprajit1_ret, N501_ret, RF)
Sonata_rf <- CAPM.alpha(Sonata1_ret, N501_ret, RF)
Port_rf_rs <- cbind(LL_rf, FinoInd_rf, Suprajit_rf, Sonata_rf)
alpha_RS_Port <- sum(c(LL_rf,FinoInd_rf,Suprajit_rf,Sonata_rf)*weight)
alpha_RS_Port


colnames(Port_rf_rs) <- companies_rs
rownames(Port_rf_rs) <- "Alpha"
View(Port_rf_rs)

#####

#Calculate VaR by variance-covariance method/parametric method
#####
Port_ret_A <- cbind(RIL1_ret,TCS1_ret, HB1_ret, BFL1_ret)
mean_Port_A <- mean(Port_ret_A * weight)
sd_Port_A = sd(Port_ret_A * weight)
prob = qnorm(0.01)
VaR_cc_A = mean_Port_A + prob * sd_Port_A #value at risk multiple by -1 and show the value in percentage form 
VaR_cc_A
var.portfolio(Port_ret_A, weights = weight)

A_ret <- (Port_ret_A*weight)[,1] + (Port_ret_A*weight)[,2] +(Port_ret_A*weight)[,3] + (Port_ret_A*weight)[,4]

cor(TBILL1_ret,A_ret)

ggplot() +
  geom_point(aes(x = A_ret, 
                 y =TBILL1_ret),
             colour = 'black') +
  ggtitle('Correlation between return of 10 Year T Bill and Aggressive Portfolio') +
  xlab('Portfolio') +
  ylab('Tbill')

port_price_a <- cbind(RIL$Close, TCS$Close, HB$Close, BFL$Close) * weight
View(port_price_a)

Port_ret_D <- cbind(SUNP1_ret,RAJEX1_ret,HINU1_ret,IOCL1_ret)
mean_Port_D <- mean(Port_ret_D * weight)
sd_Port_D = sd(Port_ret_D * weight)
probd = qnorm(0.01)
VaR_cc_D= mean_Port_D + probd * sd_Port_D
VaR_cc_D
var.portfolio(Port_ret_D, weights = weight)

D_ret <- (Port_ret_D*weight)[,1] + (Port_ret_D*weight)[,2] +(Port_ret_D*weight)[,3] + (Port_ret_D*weight)[,4]

cor(TBILL1_ret,D_ret)

ggplot() +
  geom_point(aes(x = D_ret, 
                 y =TBILL1_ret),
             colour = 'black') +
  ggtitle('Correlation between return of 10 Year T Bill and Defensive Portfolio') +
  xlab('Portfolio') +
  ylab('Tbill')

port_price_d <- cbind(SUNP$Close, RAJEX$Close, HINU$Close, IOCL$Close) * weight
View(port_price_d)

Port_ret_rs <- cbind(LL1_ret,FinoInd1_ret, Suprajit1_ret, Sonata1_ret)
mean_Port_rs <- mean(Port_ret_rs * weight)
sd_Port_rs = sd(Port_ret_rs * weight)
prob = qnorm(0.01)
VaR_cc_rs = mean_Port_rs + prob * sd_Port_rs
VaR_cc_rs
var.portfolio(Port_ret_rs, weights = weight)

rs_ret <- (Port_ret_rs*weight)[,1] + (Port_ret_rs*weight)[,2] +(Port_ret_rs*weight)[,3] + (Port_ret_rs*weight)[,4]


cor(TBILL1_ret,rs_ret)

ggplot() +
  geom_point(aes(x = rs_ret, 
                 y =TBILL1_ret),
             colour = 'black') +
  ggtitle('Correlation between return of 10 Year T Bill and Rising Giant Portfolio') +
  xlab('Portfolio') +
  ylab('Tbill')



port_price_rs <- cbind(LL$Close, FinoInd$Close, Suprajit$Close, Sonata$Close) * weight
View(port_price_rs)
#####

#Fit into normal and t distribution
#####
#Define PDF, CDF and quantile of t distribution
dt_G <- function(l, mean, sd, nu){
  dt((l-mean)/sd,nu)/sd
}

pt_G <- function(l, mean, sd, nu){
  pt((l-mean)/sd,nu)
}
# 
qt_G <- function(l, mean, sd, nu){
  qt(l,nu) * sd + mean
}
#####

#Log return
#####
RIL1_log <- as.numeric(na.omit(diff(log(RIL1$Close))))
RIL1_G <- fitdist(RIL1_log, "norm", start = list(mean = mean(RIL1_log), sd = sd(RIL1_log)))
summary(RIL1_G)[6:7]
RIL1_t <- fitdist(RIL1_log, "t_G", start = list(mean = mean(RIL1_log), sd = sd(RIL1_log), nu = 5))
summary(RIL1_t)[6:7]

TCS1_log <- as.numeric(na.omit(diff(log(TCS1$Close))))
TCS1_G <- fitdist(TCS1_log, "norm", start = list(mean = mean(TCS1_log), sd = sd(TCS1_log)))
summary(TCS1_G)[6:7]
TCS1_t <- fitdist(TCS1_log, "t_G", start = list(mean = mean(TCS1_log), sd = sd(TCS1_log), nu = 5))
summary(TCS1_t)[6:7]

HB1_log <- as.numeric(na.omit(diff(log(HB$Close))))
HB1_G <- fitdist(HB1_log, "norm", start = list(mean = mean(HB1_log), sd = sd(HB1_log)))
summary(HB1_G)[6:7]
HB1_t <- fitdist(HB1_log, "t_G", start = list(mean = mean(HB1_log), sd = sd(HB1_log), nu = 5))
summary(HB1_t)[6:7]

BFL1_log <- as.numeric(na.omit(diff(log(BFL1$Close))))
BFL1_G <- fitdist(BFL1_log, "norm", start = list(mean = mean(BFL1_log), sd = sd(BFL1_log)))
summary(BFL1_G)[6:7]
BFL1_t <- fitdist(BFL1_log, "t_G", start = list(mean = mean(BFL1_log), sd = sd(BFL1_log), nu = 5))
summary(BFL1_t)[6:7]

table_G_A <- rbind(summary(RIL1_G)[6:7],summary(TCS1_G)[6:7],summary(HB1_G)[6:7],summary(BFL1_G)[6:7])
View(table_G_A)
table_t_A <- rbind(summary(RIL1_t)[6:7],summary(TCS1_t)[6:7],summary(HB1_t)[6:7],summary(BFL1_t)[6:7])
View(table_t_A)
table_compare_a <- cbind(table_G_A, table_t_A)
rownames(table_compare_a) <- companies_defensive
View(table_compare_a)


SUNP1_log <- as.numeric(na.omit(diff(log(SUNP1$Close))))
SUNP1_G <- fitdist(SUNP1_log, "norm", start = list(mean = mean(SUNP1_log), sd = sd(SUNP1_log)))
summary(SUNP1_G)[6:7]
SUNP1_t <- fitdist(SUNP1_log, "t_G", start = list(mean = mean(SUNP1_log), sd = sd(SUNP1_log), nu = 5))
summary(SUNP1_t)[6:7]

RAJEX1_log <- as.numeric(na.omit(diff(log(RAJEX1$Close))))
RAJEX1_G <- fitdist(RAJEX1_log, "norm", start = list(mean = mean(RAJEX1_log), sd = sd(RAJEX1_log)))
summary(RAJEX1_G)[6:7]
RAJEX1_t <- fitdist(RAJEX1_log, "t_G", start = list(mean = mean(RAJEX1_log), sd = sd(RAJEX1_log), nu = 5))
summary(RAJEX1_t)[6:7]

HINU1_log <- as.numeric(na.omit(diff(log(HINU1$Close))))
HINU1_G <- fitdist(HINU1_log, "norm", start = list(mean = mean(HINU1_log), sd = sd(HINU1_log)))
summary(HINU1_G)[6:7]
HINU1_t <- fitdist(HINU1_log, "t_G", start = list(mean = mean(HINU1_log), sd = sd(HINU1_log), nu = 5))
summary(HINU1_t)[6:7]

IOCL1_log <- as.numeric(na.omit(diff(log(IOCL1$Close))))
IOCL1_G <- fitdist(IOCL1_log, "norm", start = list(mean = mean(IOCL1_log), sd = sd(IOCL1_log)))
summary(IOCL1_G)[6:7]
IOCL1_t <- fitdist(IOCL1_log, "t_G", start = list(mean = mean(IOCL1_log), sd = sd(IOCL1_log), nu = 5))
summary(IOCL1_t)[6:7]

table_G_def <- rbind(summary(SUNP1_G)[6:7],summary(RAJEX1_G)[6:7],summary(HINU1_G)[6:7],summary(IOCL1_G)[6:7])
table_t_def <- rbind(summary(SUNP1_t)[6:7],summary(RAJEX1_t)[6:7],summary(HINU1_t)[6:7],summary(IOCL1_t)[6:7])

table_compare_d <- cbind(table_G_def, table_t_def)
rownames(table_compare_d) <- companies_defensive
View(table_compare_d)

LL1_log <- as.numeric(na.omit(diff(log(LL1$Close))))
LL1_G <- fitdist(LL1_log, "norm", start = list(mean = mean(LL1_log), sd = sd(LL1_log)))
summary(LL1_G)[6:7]
LL1_t <- fitdist(LL1_log, "t_G", start = list(mean = mean(LL1_log), sd = sd(LL1_log), nu = 5))
summary(LL1_t)[6:7]

FinoInd1_log <- as.numeric(na.omit(diff(log(FinoInd1$Close))))
FinoInd1_G <- fitdist(FinoInd1_log, "norm", start = list(mean = mean(FinoInd1_log), sd = sd(FinoInd1_log)))
summary(FinoInd1_G)[6:7]
FinoInd1_t <- fitdist(FinoInd1_log, "t_G", start = list(mean = mean(FinoInd1_log), sd = sd(FinoInd1_log), nu = 5))
summary(FinoInd1_t)[6:7]

Suprajit1_log <- as.numeric(na.omit(diff(log(Suprajit$Close))))
Suprajit1_G <- fitdist(Suprajit1_log, "norm", start = list(mean = mean(Suprajit1_log), sd = sd(Suprajit1_log)))
summary(Suprajit1_G)[6:7]
Suprajit1_t <- fitdist(Suprajit1_log, "t_G", start = list(mean = mean(Suprajit1_log), sd = sd(Suprajit1_log), nu = 5))
summary(Suprajit1_t)[6:7]

Sonata1_log <- as.numeric(na.omit(diff(log(Sonata1$Close))))
Sonata1_G <- fitdist(Sonata1_log, "norm", start = list(mean = mean(Sonata1_log), sd = sd(Sonata1_log)))
summary(Sonata1_G)[6:7]
Sonata1_t <- fitdist(Sonata1_log, "t_G", start = list(mean = mean(Sonata1_log), sd = sd(Sonata1_log), nu = 5))
summary(Sonata1_t)[6:7]

table_G_rs <- rbind(summary(LL1_G)[6:7],summary(FinoInd1_G)[6:7],summary(Suprajit1_G)[6:7],summary(Sonata1_G)[6:7])
View(table_G_rs)
table_t_rs <- rbind(summary(LL1_t)[6:7],summary(FinoInd1_t)[6:7],summary(Suprajit1_t)[6:7],summary(Sonata1_t)[6:7])
View(table_t_rs)
table_compare_rs <- cbind(table_G_rs, table_t_rs)
rownames(table_compare_rs) <- companies_rs
View(table_compare_rs)

#####


#histogram plot and density of t and Gaussian
#####
histt <- function(data, ft, location = "topleft", legend.cex = 1, xlab){
  # parameters of the fitted t distribution
  mean_t <- as.list(ft$estimate)$mean
  sd_t <- as.list(ft$estimate)$sd
  nu_t <- as.list(ft$estimate)$nu
  
  # drawing histogram and assigning to h so that we can get the breakpoints between histogram cells (we will use it!)
  h <- hist(data, breaks=30)
  
  # x sequence for the additional plots on the histogram
  x_seq <- seq(-3,3,length=10000)
  
  # y sequence: density of fitted t distr. at x_seq
  yhistt <- dt_G(x_seq, mean=mean_t, sd=sd_t, nu=nu_t)
  
  # y sequence: density of normal distr. with mean and standard deviation of log-returns at x_seq
  # Note. I did not fit the normal distribution as we would get almost the same mean and standard deviation
  yhistNorm <- dnorm(x_seq, mean=mean(data), sd=sd((data)))
  
  # drawing histogram but this time we draw its density as y axis (freq=FALSE) 
  hist(data,freq=FALSE,xlab=xlab, ylab="Density", breaks=h$breaks, main=paste(""),cex.lab=1.5)
  
  # adding the density of the fitted t distribution at x_seq
  lines(x_seq, yhistt, col=4)
  # adding the density of the normal distribution at x_seq	
  lines(x_seq, yhistNorm, lty = "dashed")
  
  # Legend 
  tmp.text <- c("t", "Gaussian")
  legend(location, legend = tmp.text, cex = legend.cex, lty = c(1,2), col=c(4,1))	
}

histt(RIL1_log, RIL1_t, xlab="Daily log returns of RIL")
histt(TCS1_log, TCS1_t, xlab="Daily log returns of TCS")
histt(HB1_log, HB1_t, xlab="Daily log returns of HB")
histt(BFL1_log, BFL1_t, xlab="Daily log returns of BFL")

histt(SUNP1_log, SUNP1_t, xlab="Daily log returns of SUNP")
histt(RAJEX1_log, RAJEX1_t, xlab="Daily log returns of RAJEX")
histt(HINU1_log, HINU1_t, xlab="Daily log returns of HINU")
histt(IOCL1_log, IOCL1_t, xlab="Daily log returns of IOCL")

histt(LL1_log, LL1_t, xlab="Daily log returns of LL")
histt(FinoInd1_log, FinoInd1_t, xlab="Daily log returns of FinoInd")
histt(Suprajit1_log, Suprajit1_t, xlab="Daily log returns of Suprajit")
histt(Sonata1_log, Sonata1_t, xlab="Daily log returns of Sonata")

#####

#Create the matrix of correlation of normal and t, compute the copula
#####
company_matrix_A <- matrix(nrow = length(RIL1_log) , ncol = 4)
company_matrix_A[,1] <- pt_G(RIL1_log,
                             mean = as.list(RIL1_t$estimate)$mean,
                             sd = as.list(RIL1_t$estimate)$sd,
                             nu = as.list(RIL1_t$estimate)$nu)
company_matrix_A[,2] <- pt_G(TCS1_log,
                             mean = as.list(TCS1_t$estimate)$mean,
                             sd = as.list(TCS1_t$estimate)$sd,
                             nu = as.list(TCS1_t$estimate)$nu)
company_matrix_A[,3] <- pt_G(HB1_log,
                             mean = as.list(HB1_t$estimate)$mean,
                             sd = as.list(HB1_t$estimate)$sd,
                             nu = as.list(HB1_t$estimate)$nu)
company_matrix_A[,4] <- pt_G(BFL1_log,
                             mean = as.list(BFL1_t$estimate)$mean,
                             sd = as.list(BFL1_t$estimate)$sd,
                             nu = as.list(BFL1_t$estimate)$nu)

norm.cop <- normalCopula(dim = 4,dispstr="un")
n.cop_agg <- fitCopula(norm.cop,company_matrix_A,method="ml")
n.cop_agg

t.cop <- tCopula(dim = 4, dispstr = "un")
t.cop_agg <- fitCopula(t.cop, company_matrix_A, method = "ml")
t.cop_agg

num_company_agg <- length(companies_aggressive)
cor_norm_agg <- matrix(NA, num_company_agg, num_company_agg)
diag(cor_norm_agg) <- 1
k <- 1
for (i in 1 : (num_company_agg - 1)) {
  for (j in (i+1) : num_company_agg) {
    cor_norm_agg[i,j] <- cor_norm_agg[j,i] <- as.numeric(coef(n.cop_agg)[k])
    k = k + 1
  }
  k = 1
}
View(cor_norm_agg)

cor_t_agg <- matrix(NA, num_company_agg, num_company_agg)
diag(cor_t_agg) <- 1
for (i in 1 : (num_company_agg - 1)) {
  for (j in (i+1) : num_company_agg) {
    cor_t_agg[i,j] <- cor_t_agg[j,i] <- as.numeric(coef(t.cop_agg)[k])
    k = k + 1
  }
  k = 1
}
View(cor_t_agg)

company_matrix_d <- matrix(nrow = length(SUNP1_log) , ncol = 4)
company_matrix_d[,1] <- pt_G(SUNP1_log,
                             mean = as.list(SUNP1_t$estimate)$mean,
                             sd = as.list(SUNP1_t$estimate)$sd,
                             nu = as.list(SUNP1_t$estimate)$nu)
company_matrix_d[,2] <- pt_G(RAJEX1_log,
                             mean = as.list(RAJEX1_t$estimate)$mean,
                             sd = as.list(RAJEX1_t$estimate)$sd,
                             nu = as.list(RAJEX1_t$estimate)$nu)
company_matrix_d[,3] <- pt_G(HINU1_log,
                             mean = as.list(HINU1_t$estimate)$mean,
                             sd = as.list(HINU1_t$estimate)$sd,
                             nu = as.list(HINU1_t$estimate)$nu)
company_matrix_d[,4] <- pt_G(IOCL1_log,
                             mean = as.list(IOCL1_t$estimate)$mean,
                             sd = as.list(IOCL1_t$estimate)$sd,
                             nu = as.list(IOCL1_t$estimate)$nu)

norm.cop <- normalCopula(dim = 4,dispstr="un")
n.cop_def <- fitCopula(norm.cop,company_matrix_d,method="ml")
n.cop_def

t.cop <- tCopula(dim = 4, dispstr = "un")
t.cop_def <- fitCopula(t.cop, company_matrix_d, method = "ml")
t.cop_def

num_company_def<- length(companies_defensive)
cor_norm_def <- matrix(NA, num_company_def, num_company_def)
diag(cor_norm_def) <- 1
k <- 1
for (i in 1 : (num_company_def - 1)) {
  for (j in (i+1) : num_company_def) {
    cor_norm_def[i,j] <- cor_norm_def[j,i] <- as.numeric(coef(n.cop_def)[k])
    k = k + 1
  }
  k = 1
}
View(cor_norm_def)

cor_t_def <- matrix(NA, num_company_def, num_company_def)
diag(cor_t_def) <- 1
for (i in 1 : (num_company_def - 1)) {
  for (j in (i+1) : num_company_def) {
    cor_t_def[i,j] <- cor_t_def[j,i] <- as.numeric(coef(t.cop_def)[k])
    k = k + 1
  }
  k = 1
}
View(cor_t_def)

company_matrix_rs <- matrix(nrow = length(LL1_log) , ncol = 4)
company_matrix_rs[,1] <- pt_G(LL1_log,
                             mean = as.list(LL1_t$estimate)$mean,
                             sd = as.list(LL1_t$estimate)$sd,
                             nu = as.list(LL1_t$estimate)$nu)
company_matrix_rs[,2] <- pt_G(FinoInd1_log,
                             mean = as.list(FinoInd1_t$estimate)$mean,
                             sd = as.list(FinoInd1_t$estimate)$sd,
                             nu = as.list(FinoInd1_t$estimate)$nu)
company_matrix_rs[,3] <- pt_G(Suprajit1_log,
                             mean = as.list(Suprajit1_t$estimate)$mean,
                             sd = as.list(Suprajit1_t$estimate)$sd,
                             nu = as.list(Suprajit1_t$estimate)$nu)
company_matrix_rs[,4] <- pt_G(Sonata1_log,
                             mean = as.list(Sonata1_t$estimate)$mean,
                             sd = as.list(Sonata1_t$estimate)$sd,
                             nu = as.list(Sonata1_t$estimate)$nu)

norm.cop <- normalCopula(dim = 4,dispstr="un")
n.cop <- fitCopula(norm.cop,company_matrix_rs,method="ml")
n.cop

t.cop <- tCopula(dim = 4, dispstr = "un")
t.cop <- fitCopula(t.cop, company_matrix_rs, method = "ml")
t.cop

num_company_rs <- length(companies_rs)
cor_norm_rs <- matrix(NA, num_company_rs, num_company_rs)
diag(cor_norm_rs) <- 1
k <- 1
for (i in 1 : (num_company_rs - 1)) {
  for (j in (i+1) : num_company_rs) {
    cor_norm_rs[i,j] <- cor_norm_rs[j,i] <- as.numeric(coef(n.cop)[k])
    k = k + 1
  }
  k = 1
}
View(cor_norm_rs)

cor_t_rs <- matrix(NA, num_company_rs, num_company_rs)
diag(cor_t_rs) <- 1
for (i in 1 : (num_company_rs - 1)) {
  for (j in (i+1) : num_company_rs) {
    cor_t_rs[i,j] <- cor_t_rs[j,i] <- as.numeric(coef(t.cop)[k])
    k = k + 1
  }
  k = 1
}
View(cor_t_rs)

#####

#GARCH
#####
date_A = as.Date(RIL$Date)[2:495]
blog <- cbind(RIL1_log, TCS1_log, HB1_log, BFL1_log) * weight
A_log <-xts(blog[,1] + blog[,2] + blog[,3] + blog[,4], date_A)

date_D = as.Date(SUNP$Date)[2:495]
blog_D <- cbind(SUNP1_log, RAJEX1_log, HINU1_log, IOCL1_log) * weight
D_log <-xts(blog_D[,1] + blog_D[,2] + blog_D[,3] + blog_D[,4], date_D)

date_rs = as.Date(LL$Date)[2:495]
blog_rs <- cbind(LL1_log, FinoInd1_log, Suprajit1_log, Sonata1_log) * weight
rs_log <-xts(blog_rs[,1] + blog_rs[,2] + blog_rs[,3] + blog_rs[,4], date_rs)



N50_log_A <- xts(N501_ret, date_A)
N50_log_A <- xts(N501_ret, date_D)
N50_log_A <- xts(N501_ret, date_rs)


#model univariate garch specification
gspec.ru <- ugarchspec(mean.model=list(armaOrder=c(0,0)), 
                       variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                       distribution.model="std") #std is t distribution and not normal
A_gar <- ugarchfit(gspec.ru, A_log)
D_gar <- ugarchfit(gspec.ru, D_log)
N50_gar <- ugarchfit(gspec.ru, N50_log_A)
rs_gar <- ugarchfit(gspec.ru, rs_log)


coef(A_gar)
coef(D_gar)
coef(rs_gar)

ggplot() +
  geom_line(aes(x= date_A,
                y= sqrt(252) * A_gar@fit$sigma),
            colour = "#FF3399",
            size = 1) + 
  geom_point(aes(x= date_A,
                 y= sqrt(252) * A_gar@fit$sigma,colour = "colbig",shape = "colbig"),
             
             size = 2
  ) + 
  geom_line(aes(x= date_D,
                y= sqrt(252) * D_gar@fit$sigma),
            colour = "#FF9999",
            size = 1) + 
  geom_point(aes(x= date_D,
                 y= sqrt(252) * D_gar@fit$sigma,colour = "coldef",shape = "coldef"),
             
             size = 2
  ) + 
  
  geom_line(aes(x= date_D,
                y= sqrt(252) * N50_gar@fit$sigma),
            colour = "#0000CC",
            size = 1) + 
  geom_point(aes(x= date_D,
                 y= sqrt(252) * N50_gar@fit$sigma,colour = "colsp",shape = "colsp"),
             size = 2)+
  geom_line(aes(x= date_rs,
                y= sqrt(252) * rs_gar@fit$sigma),
              colour = "#70ba71",
            size = 1) + 
  geom_point(aes(x= date_rs,
                 y= sqrt(252) * rs_gar@fit$sigma,colour = "colrs",shape = "colrs"),
             
             size = 2
  ) + 
  scale_x_date(breaks = pretty_breaks(15))+
  scale_colour_manual(name="Volatility", values=c("colbig" = "#FF3399", "coldef"="#FF9999", "colsp" = "#0000CC", "colrs" = "#70ba71"), 
                      labels=c("colbig"="Aggrressive", "coldef"="Defensive", "colsp" = "Nifty50", "colrs"="Rising Giant"))+
  scale_shape_manual(name="Volatility", values=c("colbig" = 17, "coldef"=19, "colsp" = 18, "colrs" = 20), 
                     labels=c("colbig"="Aggressive", "coldef"="Defensive","colsp" = "Nifty50", "colrs"="Rising Giant")
  )+
  ggtitle('Garch(1,1), Volatility of Aggressive, Defensive Portfolio, Rising Giant and Nifty50') +
  xlab('Time') +
  ylab('Volatility')

#####

#drawdowns
#####

#comapre drawdowns
com_dd <- merge(A_ret, D_ret, N501_ret, rs_ret)
colnames(com_dd) <- c("Aggressive", "Defensive", "Nifty50", "Rising Giant")
drawdowns <- table.Drawdowns(A_ret)
drawdowns.dates <- cbind(format(drawdowns$From),format(drawdowns$To))
drawdowns.dates[is.na(drawdowns.dates)] <- format(index(A_ret)[NROW(A_ret)])
drawdowns.dates <- lapply(seq_len(nrow(drawdowns.dates)), function(i) drawdowns.dates[i,])


charts.PerformanceSummary(com_dd,ylog=FALSE,
                          period.areas = drawdowns.dates,
                          colorset = c(2,3,4,5),
                          legend.loc = "topleft",
                          main = "Log Comparison among Aggressive, Defensive, Rising Giant and NIfty50" )

chart.CumReturns(com_dd,ylog=FALSE,
                 period.areas = drawdowns.dates,
                 colorset = c(2,3,4,5),
                 legend.loc = "topleft",
                 main = "Cumulative")


# advanced charts.PerforanceSummary based on ggplot
gg.charts.PerformanceSummary <- function(rtn.obj, geometric = TRUE, main = "", plot = TRUE)
{
  
  # load libraries
  suppressPackageStartupMessages(require(ggplot2))
  suppressPackageStartupMessages(require(scales))
  suppressPackageStartupMessages(require(reshape))
  suppressPackageStartupMessages(require(PerformanceAnalytics))
  
  # create function to clean returns if having NAs in data
  clean.rtn.xts <- function(univ.rtn.xts.obj,na.replace=0){
    univ.rtn.xts.obj[is.na(univ.rtn.xts.obj)]<- na.replace
    univ.rtn.xts.obj  
  }
  
  # Create cumulative return function
  cum.rtn <- function(clean.xts.obj, g = TRUE)
  {
    X <- clean.xts.obj
    if(g == TRUE){Y <- cumprod(X+1)-1} else {Y <- cumsum(X)}
    Y
  }
  
  # Create function to calculate drawdowns
  dd.xts <- function(clean.xts.obj, g = TRUE)
  {
    X <- clean.xts.obj
    if(g == TRUE){Y <- PerformanceAnalytics:::Drawdowns(X)} else {Y <- PerformanceAnalytics:::Drawdowns(X,geometric = FALSE)}
    Y
  }
  
  # create a function to create a dataframe to be usable in ggplot to replicate charts.PerformanceSummary
  cps.df <- function(xts.obj,geometric)
  {
    X <- clean.rtn.xts(xts.obj)
    series.name <- colnames(xts.obj)[1]
    tmp <- cum.rtn(X,geometric)
    tmp$rtn <- X
    tmp$dd <- dd.xts(X,geometric)
    colnames(tmp) <- c("Cumulative Return","Return","Drawdown") # names with space
    tmp.df <- as.data.frame(coredata(tmp))
    tmp.df$Date <- as.POSIXct(index(tmp))
    tmp.df.long <- melt(tmp.df,id.var="Date")
    tmp.df.long$asset <- rep(series.name,nrow(tmp.df.long))
    tmp.df.long
  }
  
  # A conditional statement altering the plot according to the number of assets
  if(ncol(rtn.obj)==1)
  {
    # using the cps.df function
    df <- cps.df(rtn.obj,geometric)
    # adding in a title string if need be
    if(main == ""){
      title.string <- paste("Asset Performance")
    } else {
      title.string <- main
    }
    
    gg.xts <- ggplot(df, aes_string( x = "Date", y = "value", group = "variable" )) +
      facet_grid(variable ~ ., scales = "free_y", space = "fixed") +
      geom_line(data = subset(df, variable == "Cumulative Return")) +
      geom_bar(data = subset(df, variable == "Return"), stat = "identity") +
      geom_line(data = subset(df, variable == "Drawdown")) +
      geom_hline(yintercept = 0, size = 0.5, colour = "black") +
      ggtitle(title.string) +
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
      scale_x_datetime(breaks = date_breaks("6 months"), labels = date_format("%m/%Y")) +
      ylab("") +
      xlab("")
    
  } 
  else 
  {
    # a few extra bits to deal with the added rtn columns
    no.of.assets <- ncol(rtn.obj)
    asset.names <- colnames(rtn.obj)
    df <- do.call(rbind,lapply(1:no.of.assets, function(x){cps.df(rtn.obj[,x],geometric)}))
    df$asset <- ordered(df$asset, levels=asset.names)
    if(main == ""){
      title.string <- paste("Asset",asset.names[1],asset.names[2],asset.names[3],asset.names[4],"Performance")
    } else {
      title.string <- main
    }
    
    if(no.of.assets>5){legend.rows <- 5} else {legend.rows <- no.of.assets}
    
    gg.xts <- ggplot(df, aes_string(x = "Date", y = "value" )) +
      
      # panel layout
      facet_grid(variable~., scales = "free_y", space = "fixed", shrink = TRUE, drop = TRUE, margin = 
                   , labeller = label_value) + # label_value is default
      
      # display points for Cumulative Return and Drawdown, but not for Return
      geom_point(data = subset(df, variable == c("Cumulative Return","Drawdown"))
                 , aes(colour = factor(asset), shape = factor(asset)), size = 1.2, show.legend = TRUE) + 
      
      # manually select shape of geom_point
      scale_shape_manual(values = c(1,2,3,4)) + 
      
      # line colours for the Index
      geom_line(data = subset(df, variable == "Cumulative Return"), aes(colour = factor(asset)), show.legend = FALSE) +
      
      # bar colours for the Return
      geom_bar(data = subset(df,variable == "Return"), stat = "identity"
               , aes(fill = factor(asset), colour = factor(asset)), position = "dodge", show.legend = FALSE) +
      
      # line colours for the Drawdown
      geom_line(data = subset(df, variable == "Drawdown"), aes(colour = factor(asset)), show.legend = FALSE) +
      
      # horizontal line to indicate zero values
      geom_hline(yintercept = 0, size = 0.5, colour = "black") +
      
      # horizontal ticks
      scale_x_datetime(breaks = date_breaks("3 months"), labels = date_format("%m/%Y")) +
      
      
      # main y-axis title
      ylab("") +
      
      # main x-axis title
      xlab("") +
      
      # main chart title
      ggtitle(title.string)
    
    # legend 
    
    gglegend <- guide_legend(override.aes = list(size = 3))
    
    gg.xts <- gg.xts + guides(colour = gglegend, size = "none") +
      
      #gglegend <- guide_legend(override.aes = list(size = 3), direction = "horizontal") # direction overwritten by legend.box?
      #gg.xts <- gg.xts + guides(colour = gglegend, size = "none", shape = gglegend) + # Warning: "Duplicated override.aes is ignored"
      
      theme( legend.title = element_blank()
             , legend.position = c(0,1)
             , legend.justification = c(0,1)
             , legend.background = element_rect(colour = 'grey')
             , legend.key = element_rect(fill = "white", colour = "white")
             , axis.text.x = element_text(angle = 0, hjust = 1)
             , strip.background = element_rect(fill = "white")
             , panel.background = element_rect(fill = "white", colour = "white")
             , panel.grid.major = element_line(colour = "grey", size = 0.5) 
             , panel.grid.minor = element_line(colour = NA, size = 0.0)
      )
    
  }
  
  assign("gg.xts", gg.xts,envir=.GlobalEnv)
  if(plot == TRUE){
    plot(gg.xts)
  } else {}
  
}

# display chart
gg.charts.PerformanceSummary(com_dd, geometric = TRUE)

######

##### optimised weights
#####

#weights
weightA <- c(0.22,0.32,0.10,0.36)
weightD <- c(0.38,0.24,0.13,0.25)
weightRS <- c(0.32,0.23,0.23,0.22)

#return composition
Port_ret_A1 <- cbind(RIL1_ret,TCS1_ret, HB1_ret, BFL1_ret)
mean_Port_A1 <- mean(Port_ret_A1 * weightA)
sd_Port_A1 = sd(Port_ret_A1 * weightA)
prob = qnorm(0.01)
VaR_cc_A1 = mean_Port_A1 + prob * sd_Port_A1 #value at risk multiple by -1 and show the value in percentage form 
VaR_cc_A1
A_ret1 <- (Port_ret_A1*weightA)[,1] + (Port_ret_A1*weightA)[,2] +(Port_ret_A1*weightA)[,3] + (Port_ret_A1*weightA)[,4]


Port_ret_D1 <- cbind(HINU1_ret,SUNP1_ret,RAJEX1_ret, IOCL1_ret)
mean_Port_D1 <- mean(Port_ret_D1 * weightD)
sd_Port_D1 = sd(Port_ret_D1 * weightD)
probd = qnorm(0.01)
VaR_cc_D1= mean_Port_D1 + probd * sd_Port_D1
VaR_cc_D1
D_ret1 <- (Port_ret_D1*weightD)[,1] + (Port_ret_D1*weightD)[,2] +(Port_ret_D1*weightD)[,3] + (Port_ret_D1*weightD)[,4]


Port_ret_rs1 <- cbind(LL1_ret,FinoInd1_ret, Suprajit1_ret, Sonata1_ret)
mean_Port_rs1 <- mean(Port_ret_rs1 * weightRS)
sd_Port_rs1 = sd(Port_ret_rs1 * weightRS)
prob = qnorm(0.01)
VaR_cc_rs1 = mean_Port_rs1 + prob * sd_Port_rs1
VaR_cc_rs1
rs_ret1 <- (Port_ret_rs1*weightRS)[,1] + (Port_ret_rs1*weightRS)[,2] +(Port_ret_rs1*weightRS)[,3] + (Port_ret_rs1*weightRS)[,4]


#garch
date_A = as.Date(RIL$Date)[2:495]
blog <- cbind(RIL1_log, TCS1_log, HB1_log, BFL1_log) * weight
A_log <-xts(blog[,1] + blog[,2] + blog[,3] + blog[,4], date_A)
blog1 <- cbind(RIL1_log, TCS1_log, HB1_log, BFL1_log) * weightA
A_log1 <-xts(blog1[,1] + blog1[,2] + blog1[,3] + blog1[,4], date_A)

date_D = as.Date(SUNP$Date)[2:495]
blog_D <- cbind(SUNP1_log, RAJEX1_log, HINU1_log, IOCL1_log) * weight
D_log <-xts(blog_D[,1] + blog_D[,2] + blog_D[,3] + blog_D[,4], date_D)
blog_D1 <- cbind(SUNP1_log, RAJEX1_log, HINU1_log, IOCL1_log) * weightD
D_log1 <-xts(blog_D1[,1] + blog_D1[,2] + blog_D1[,3] + blog_D1[,4], date_D)

date_rs = as.Date(LL$Date)[2:495]
blog_rs <- cbind(LL1_log, FinoInd1_log, Suprajit1_log, Sonata1_log) * weight
rs_log <-xts(blog_rs[,1] + blog_rs[,2] + blog_rs[,3] + blog_rs[,4], date_rs)
blog_rs1 <- cbind(LL1_log, FinoInd1_log, Suprajit1_log, Sonata1_log) * weightRS
rs_log1 <-xts(blog_rs1[,1] + blog_rs1[,2] + blog_rs1[,3] + blog_rs1[,4], date_rs)


N50_log_A <- xts(N501_ret, date_A)
N50_log_A <- xts(N501_ret, date_D)
N50_log_A <- xts(N501_ret, date_rs)


#model univariate garch specification
gspec.ru <- ugarchspec(mean.model=list(armaOrder=c(0,0)), 
                       variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                       distribution.model="std") #std is t distribution and not normal
A_gar <- ugarchfit(gspec.ru, A_log)
D_gar <- ugarchfit(gspec.ru, D_log)
N50_gar <- ugarchfit(gspec.ru, N50_log_A)
rs_gar <- ugarchfit(gspec.ru, rs_log)
A_gar1 <- ugarchfit(gspec.ru, A_log1)
D_gar1 <- ugarchfit(gspec.ru, D_log1)
rs_gar1 <- ugarchfit(gspec.ru, rs_log1)


coef(A_gar)
coef(D_gar)
coef(rs_gar)
coef(A_gar1)
coef(D_gar1)
coef(rs_gar1)

#volatility of aggressive
ggplot() +
  geom_line(aes(x= date_A,
                y= sqrt(252) * A_gar@fit$sigma),
            colour = "#FF3399",
            size = 1) + 
  geom_point(aes(x= date_A,
                 y= sqrt(252) * A_gar@fit$sigma,colour = "colbig",shape = "colbig"),
             
             size = 2
  ) + 
  geom_line(aes(x= date_A,
                y= sqrt(252) * A_gar1@fit$sigma),
            colour = "#FF3399",
            size = 1) + 
  geom_point(aes(x= date_A,
                 y= sqrt(252) * A_gar1@fit$sigma,colour = "colsp",shape = "colsp"),
             
             size = 2
  ) + 
  scale_x_date(breaks = pretty_breaks(15))+
  scale_colour_manual(name="Volatility", values=c("colbig" = "#FF3399", "colsp" = "#0000CC"))+
  
  scale_shape_manual(name="Volatility", values=c("colbig" = 17, "colsp" = 18), 
                     labels=c("colbig"="equal weight","colsp" = "optimized")
  )+
 
  ggtitle('Garch(1,1), Volatility of equal weight Aggressive & optimised weighted Aggressive') +
  xlab('Time') +
  ylab('Volatility')

#volatility of defensive
ggplot() +
  geom_line(aes(x= date_A,
                y= sqrt(252) * D_gar@fit$sigma),
            colour = "#FF3399",
            size = 1) + 
  geom_point(aes(x= date_A,
                 y= sqrt(252) * D_gar@fit$sigma,colour = "colbig",shape = "colbig"),
             
             size = 2
  ) + 
  geom_line(aes(x= date_A,
                y= sqrt(252) * D_gar1@fit$sigma),
            colour = "#FF3399",
            size = 1) + 
  geom_point(aes(x= date_A,
                 y= sqrt(252) * D_gar1@fit$sigma,colour = "colsp",shape = "colsp"),
             
             size = 2
  ) + 
  scale_x_date(breaks = pretty_breaks(15))+
  scale_colour_manual(name="Volatility", values=c("colbig" = "#FF3399", "colsp" = "#0000CC"))+
  
  scale_shape_manual(name="Volatility", values=c("colbig" = 17, "colsp" = 18), 
                     labels=c("colbig"="equal weight","colsp" = "optimized")
  )+
  
  ggtitle('Garch(1,1), Volatility of equal weight Defensive & optimised weighted Defensive') +
  xlab('Time') +
  ylab('Volatility')

#volatility of rising giant
ggplot() +
  geom_line(aes(x= date_A,
                y= sqrt(252) * rs_gar@fit$sigma),
            colour = "#FF3399",
            size = 1) + 
  geom_point(aes(x= date_A,
                 y= sqrt(252) * rs_gar@fit$sigma,colour = "colbig",shape = "colbig"),
             
             size = 2
  ) + 
  geom_line(aes(x= date_A,
                y= sqrt(252) * rs_gar1@fit$sigma),
            colour = "#FF3399",
            size = 1) + 
  geom_point(aes(x= date_A,
                 y= sqrt(252) * rs_gar1@fit$sigma,colour = "colsp",shape = "colsp"),
             
             size = 2
  ) + 
  scale_x_date(breaks = pretty_breaks(15))+
  scale_colour_manual(name="Volatility", values=c("colbig" = "#FF3399", "colsp" = "#0000CC"))+
  
  scale_shape_manual(name="Volatility", values=c("colbig" = 17, "colsp" = 18), 
                     labels=c("colbig"="equal weight","colsp" = "optimized")
  )+
  
  ggtitle('Garch(1,1), Volatility of equal weight rising giant & optimised weighted rising giant') +
  xlab('Time') +
  ylab('Volatility')


#drawdowns
#for aggressive
com_dd_agg <- merge(A_ret,A_ret1)
colnames(com_dd_agg) <- c("Equal weight", "Optimised weight")
drawdowns <- table.Drawdowns(A_ret)
drawdowns.dates <- cbind(format(drawdowns$From),format(drawdowns$To))
drawdowns.dates[is.na(drawdowns.dates)] <- format(index(A_ret)[NROW(A_ret)])
drawdowns.dates <- lapply(seq_len(nrow(drawdowns.dates)), function(i) drawdowns.dates[i,])


charts.PerformanceSummary(com_dd_agg,ylog=FALSE,
                          period.areas = drawdowns.dates,
                          colorset = c(2,4),
                          legend.loc = "topleft",
                          main = "Log Comparison among equal weighted and optimised weighted" )

chart.CumReturns(com_dd_agg,ylog=FALSE,
                 period.areas = drawdowns.dates,
                 colorset = c(2,4),
                 legend.loc = "topleft",
                 main = "Cumulative Returns Aggressive")

#for defensive
com_dd_def <- merge(D_ret,D_ret1)
colnames(com_dd_def) <- c("Equal weight", "Optimised weight")
drawdowns <- table.Drawdowns(A_ret)
drawdowns.dates <- cbind(format(drawdowns$From),format(drawdowns$To))
drawdowns.dates[is.na(drawdowns.dates)] <- format(index(A_ret)[NROW(A_ret)])
drawdowns.dates <- lapply(seq_len(nrow(drawdowns.dates)), function(i) drawdowns.dates[i,])


charts.PerformanceSummary(com_dd_def,ylog=FALSE,
                          period.areas = drawdowns.dates,
                          colorset = c(2,4),
                          legend.loc = "topleft",
                          main = "Log Comparison among equal weighted and optimised weighted" )

chart.CumReturns(com_dd_def,ylog=FALSE,
                 period.areas = drawdowns.dates,
                 colorset = c(2,4),
                 legend.loc = "topleft",
                 main = "Cumulative Returns Defensive")

#for rising giant
com_dd_rs <- merge(rs_ret,rs_ret1)
colnames(com_dd_rs) <- c("Equal weight", "Optimised weight")
drawdowns <- table.Drawdowns(A_ret)
drawdowns.dates <- cbind(format(drawdowns$From),format(drawdowns$To))
drawdowns.dates[is.na(drawdowns.dates)] <- format(index(A_ret)[NROW(A_ret)])
drawdowns.dates <- lapply(seq_len(nrow(drawdowns.dates)), function(i) drawdowns.dates[i,])


charts.PerformanceSummary(com_dd_rs,ylog=FALSE,
                          period.areas = drawdowns.dates,
                          colorset = c(2,4),
                          legend.loc = "topleft",
                          main = "Log Comparison among equal weighted and optimised weighted" )

chart.CumReturns(com_dd_rs,ylog=FALSE,
                 period.areas = drawdowns.dates,
                 colorset = c(2,4),
                 legend.loc = "topleft",
                 main = "Cumulative Returns Rising Giant")


#all 3 optimised
com_dd <- merge(A_ret1,D_ret1,rs_ret1)
colnames(com_dd) <- c("aggressive", "defensive", "rising giant")
drawdowns <- table.Drawdowns(A_ret)
drawdowns.dates <- cbind(format(drawdowns$From),format(drawdowns$To))
drawdowns.dates[is.na(drawdowns.dates)] <- format(index(A_ret)[NROW(A_ret)])
drawdowns.dates <- lapply(seq_len(nrow(drawdowns.dates)), function(i) drawdowns.dates[i,])


charts.PerformanceSummary(com_dd,ylog=FALSE,
                          period.areas = drawdowns.dates,
                          colorset = c(2,3,4),
                          legend.loc = "topleft",
                          main = "Log Comparison among optimised weighted" )

chart.CumReturns(com_dd,ylog=FALSE,
                 period.areas = drawdowns.dates,
                 colorset = c(2,3,4),
                 legend.loc = "topleft",
                 main = "Cumulative Returns of Optimised Weights")

#####

#forecast of volatility
#####

gspec.ru <- ugarchspec(mean.model=list(armaOrder=c(0,0)),
                       variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                       distribution.model = "std") #std is t distribution and not normal
A_gar1 <- ugarchfit(gspec.ru, A_log1)
D_gar1 <- ugarchfit(gspec.ru, D_log1)
rs_gar1 <- ugarchfit(gspec.ru, rs_log1)


library(forecast)
library(rugarch)
library(FinTS)
library(e1071)

plot(A_log1)
plot(D_log1)
plot(rs_log1)


DF=ugarchforecast(D_gar1, n.ahead=30)
plot(DF)
ArchTest(D_log1)

AF=ugarchforecast(A_gar1, n.ahead=30)
plot(AF)
ArchTest(A_log1)

RSF=ugarchforecast(rs_gar1, n.ahead=30)
plot(RSF)
ArchTest(rs_log1)

#####
