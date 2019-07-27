#### Load package ####
library(PerformanceAnalytics)
library(quadprog)
library(ggplot2)
library(ggpubr)

# Computation Rf T-note 10 years https://www.treasury.gov/resource-center/data-chart-center/interest-rates/pages/TextView.aspx?da 31/12/2018
Rf <- 0.0269
Rfdaily <- ((1+Rf)^(1/365))-1

# Returns
excess_return <- colMeans(Returns_ETF[,2:13]) - Rfdaily
covmat <- cov(Returns_ETF[,2:13])
mean_vect <- colMeans(Returns_ETF[,2:13]) - Rfdaily

#### Black Litterman ####
# Implied Risk Aversion Coefficient 
# Save market cap weight per day
ETF_MarketCap <- ETF_MarketCap[,-1]

MarketCapWeight <- data.frame()
for (i in 1:365) {
  for (j in 1:12) {
    MarketCapWeight[i,j] <- as.data.frame(ETF_MarketCap[i,j])/sum(ETF_MarketCap[i,1:12])
  }
}
colnames(MarketCapWeight) <- colnames(ETF_MarketCap)
Mean.MarketCapWeight <- as.matrix(colMeans(MarketCapWeight))

lamdba <- (mean(Benchmark$`Return S&P1200`[-1]) - Rfdaily)/ var(Benchmark$`Return S&P1200`[-1])

# Implied Excess Equilibrium Return Vector
IEE <- as.numeric(lamdba)*covmat%*%as.matrix(Mean.MarketCapWeight)


Pmatrix <- diag(12)

# Investor's view
Qviews <- matrix(Future_Return,nrow = 12, ncol = 1)
Omega <- Pmatrix*(0.025*covmat)*t(Pmatrix)

# Expected return Black Litterman
muBL <- solve(solve(0.025*covmat) + t(Pmatrix)%*%solve(Omega)%*%Pmatrix)%*%
  ((0.025*covmat)%*%IEE + t(Pmatrix)%*%solve(Omega)%*%Qviews)

# Variance Black Litterman
varBL <- solve(solve(0.025*covmat) + t(Pmatrix)%*%solve(Omega)%*%Pmatrix)  

# Minimum variance portfolio BL
min.var.BL <- min.var.portfolio(covmat, muBL,Rfdaily, short = "yes",0.5, -0.1, Names)

# Efficient portfolio of the investor BL
eff.portfolio.BL <- Eff.portfolio(covmat, muBL,Rfdaily,"yes",0.0001,0.5,-0.1, Names)
sum(eff.portfolio.BL[,1:6])
sum(eff.portfolio.BL[,7:12])

# Efficient frontier for BL
eff.fr.BL <- eff.frontier(covmat, muBL,Rfdaily, short = "yes", 0.5,-0.1, 5, 0.005)

eff.fr.IEE <- eff.frontier(covmat, IEE,Rfdaily, "yes", 0.5,-0.1, 5, 0.005)

# Comparison with views and no views
ggplot() +
  geom_line(eff.fr.IEE, mapping = aes(x = Std.Dev, y = Exp.Return, colour = "orange"), size =1) +
  geom_line(eff.fr.BL, mapping = aes(x = Std.Dev, y = Exp.Return, colour = "yellow"), size = 1) +
  #geom_abline(slope = max(eff.fr.BL$sharpe), intercept = Rfdaily) +
  #geom_abline(slope = max(eff.fr.IEE$sharpe), intercept = Rfdaily)
  scale_color_discrete(name = "Legend", labels = c("Black Litterman Model without views", "Black Litterman Model with views")) +
  labs(title = "Black Litterman Model with and without views", x = "Standard Deviation", y = "Expected Return") +
  theme(legend.position = "bottom")
  
# Comparison BLM vs. MVO
ggplot() +
  geom_line(eff.fr, mapping = aes(x = Std.Dev, y = Exp.Return, colour = "red"), size = 1) +
  geom_line(eff.fr.BL, mapping = aes(x = Std.Dev, y = Exp.Return, colour = "yellow"), size = 1) +
  #geom_point(eff.fr, mapping = aes(x = Std.Dev[which(eff.fr$sharpe==max(eff.fr$sharpe), arr.ind = TRUE)],
                                   #y = Exp.Return[which(eff.fr$sharpe==max(eff.fr$sharpe), arr.ind = TRUE)],color = "black")) +
  #geom_point(eff.fr.BL, mapping = aes(x = Std.Dev[which(eff.fr.BL$sharpe==max(eff.fr.BL$sharpe), arr.ind = TRUE)],
                                      #y = Exp.Return[which(eff.fr.BL$sharpe==max(eff.fr.BL$sharpe), arr.ind = TRUE)], color = "black")) +
  geom_point(eff.portfolio, mapping = aes(x = Std.Dev, y = Exp.Return, color = "Efficient Portfolio")) +
  geom_point(eff.portfolio.BL, mapping = aes(x = Std.Dev, y = Exp.Return, color = "Efficient Portfolio")) +
  #geom_abline(slope = max(eff.fr$sharpe), intercept = Rfdaily) +
  #geom_abline(slope = max(eff.fr.BL$sharpe), intercept = Rfdaily) +
  scale_color_discrete(name = "Legend", labels = c("Efficient Portfolio","Mean Variance Optimisation", "Black Litterman Model")) +
  labs(x = "Standard Deviation", y = "Expected Return") +
  theme(legend.position = "bottom")

# Comparison MVO vs. IEE
ggplot() +
  geom_line(eff.fr.IEE, mapping = aes(x = Std.Dev, y = Exp.Return, colour = "orange"), size =1) +
  geom_line(eff.fr, mapping = aes(x = Std.Dev, y = Exp.Return, colour = "yellow"), size = 1) +
  geom_point(eff.fr, mapping = aes(x = Std.Dev[which(eff.fr$sharpe==max(eff.fr$sharpe), arr.ind = TRUE)],
                                   y = Exp.Return[which(eff.fr$sharpe==max(eff.fr$sharpe), arr.ind = TRUE)],color = "black")) +
  geom_point(eff.fr.IEE, mapping = aes(x = Std.Dev[which(eff.fr.IEE$sharpe==max(eff.fr.IEE$sharpe), arr.ind = TRUE)],
                                      y = Exp.Return[which(eff.fr.IEE$sharpe==max(eff.fr.IEE$sharpe), arr.ind = TRUE)], color = "black")) +
  #geom_abline(slope = max(eff.fr$sharpe), intercept = Rfdaily) +
  #geom_abline(slope = max(eff.fr.IEE$sharpe), intercept = Rfdaily) +
  scale_color_discrete(name = "Legend", labels = c("Tangency Portfolio","Black Litterman Model without views","Mean Variance Optimisation")) +
  labs(title = "", x = "Standard Deviation", y = "Expected Return") +
  theme(legend.position = "bottom")

#### Comparison graph per asset MVO >< BL ####

# VTI
VTI_ggplot <- ggplot() +
  geom_line(eff.fr, mapping = aes(x = Exp.Return, y = VTI, color = "Mean Variance Optimisation"), size = 1) +
  geom_line(eff.fr.BL, mapping = aes(x = Exp.Return, y = VTI, color = "Black Litterman Model"), size = 1) +
  scale_color_discrete(name = "", labels = c("Black Litterman Model","Mean Variance Optimisation")) +
  labs(title = "VTI", x = "Expected Return", y = "Weight") +
  theme(legend.position = "bottom")

# VTV
VTV_ggplot <- ggplot() +
  geom_line(eff.fr, mapping = aes(x = Exp.Return, y = VTV, color = "Mean Variance Optimisation"), size = 1) +
  geom_line(eff.fr.BL, mapping = aes(x = Exp.Return, y = VTV, color = "Black Litterman Model"), size = 1) +
  scale_color_discrete(name = "", labels = c("Black Litterman Model","Mean Variance Optimisation")) +
  labs(title = "VTV", x = "Expected Return", y = "Weight") +
  theme(legend.position = "bottom")

# VOE
VOE_ggplot <- ggplot() +
  geom_line(eff.fr, mapping = aes(x = Exp.Return, y = VOE, color = "Mean Variance Optimisation"), size = 1) +
  geom_line(eff.fr.BL, mapping = aes(x = Exp.Return, y = VOE, color = "Black Litterman Model"), size = 1) +
  scale_color_discrete(name = "", labels = c("Black Litterman Model","Mean Variance Optimisation")) +
  labs(title = "VOE", x = "Expected Return", y = "Weight") +
  theme(legend.position = "bottom")

# VBR
VBR_ggplot <- ggplot() +
  geom_line(eff.fr, mapping = aes(x = Exp.Return, y = VBR, color = "Mean Variance Optimisation"), size = 1) +
  geom_line(eff.fr.BL, mapping = aes(x = Exp.Return, y = VBR, color = "Black Litterman Model"), size = 1) +
  scale_color_discrete(name = "", labels = c("Black Litterman Model","Mean Variance Optimisation")) +
  labs(title = "VBR", x = "Expected Return", y = "Weight") +
  theme(legend.position = "bottom")

# VEA
VEA_ggplot <- ggplot() +
  geom_line(eff.fr, mapping = aes(x = Exp.Return, y = VEA, color = "Mean Variance Optimisation"), size = 1) +
  geom_line(eff.fr.BL, mapping = aes(x = Exp.Return, y = VEA, color = "Black Litterman Model"), size = 1) +
  scale_color_discrete(name = "", labels = c("Black Litterman Model","Mean Variance Optimisation")) +
  labs(title = "VEA", x = "Expected Return", y = "Weight") +
  theme(legend.position = "bottom")

# VWO
VWO_ggplot <- ggplot() +
  geom_line(eff.fr, mapping = aes(x = Exp.Return, y = VWO, color = "Mean Variance Optimisation"), size = 1) +
  geom_line(eff.fr.BL, mapping = aes(x = Exp.Return, y = VWO, color = "Black Litterman Model"), size = 1) +
  scale_color_discrete(name = "", labels = c("Black Litterman Model","Mean Variance Optimisation")) +
  labs(title = "VWO", x = "Expected Return", y = "Weight") +
  theme(legend.position = "bottom")

# SHV
SHV_ggplot <- ggplot() +
  geom_line(eff.fr, mapping = aes(x = Exp.Return, y = SHV, color = "Mean Variance Optimisation"), size = 1) +
  geom_line(eff.fr.BL, mapping = aes(x = Exp.Return, y = SHV, color = "Black Litterman Model"), size = 1) +
  scale_color_discrete(name = "", labels = c("Black Litterman Model","Mean Variance Optimisation")) +
  labs(title = "SHV", x = "Expected Return", y = "Weight") +
  theme(legend.position = "bottom")

# VTIP
VTIP_ggplot <- ggplot() +
  geom_line(eff.fr, mapping = aes(x = Exp.Return, y = VTIP, color = "Mean Variance Optimisation"), size = 1) +
  geom_line(eff.fr.BL, mapping = aes(x = Exp.Return, y = VTIP, color = "Black Litterman Model"), size = 1) +
  scale_color_discrete(name = "", labels = c("Black Litterman Model","Mean Variance Optimisation")) +
  labs(title = "VTIP", x = "Expected Return", y = "Weight") +
  theme(legend.position = "bottom")

# MUB
MUB_ggplot <- ggplot() +
  geom_line(eff.fr, mapping = aes(x = Exp.Return, y = MUB, color = "Mean Variance Optimisation"), size = 1) +
  geom_line(eff.fr.BL, mapping = aes(x = Exp.Return, y = MUB, color = "Black Litterman Model"), size = 1) +
  scale_color_discrete(name = "", labels = c("Black Litterman Model","Mean Variance Optimisation")) +
  labs(title = "MUB", x = "Expected Return", y = "Weight") +
  theme(legend.position = "bottom")

# AGG
AGG_ggplot <- ggplot() +
  geom_line(eff.fr, mapping = aes(x = Exp.Return, y = AGG, color = "Mean Variance Optimisation"), size = 1) +
  geom_line(eff.fr.BL, mapping = aes(x = Exp.Return, y = AGG, color = "Black Litterman Model"), size = 1) +
  scale_color_discrete(name = "", labels = c("Black Litterman Model","Mean Variance Optimisation")) +
  labs(title = "AGG", x = "Expected Return", y = "Weight") +
  theme(legend.position = "bottom")

# BNDX
BNDX_ggplot <- ggplot() +
  geom_line(eff.fr, mapping = aes(x = Exp.Return, y = BNDX, color = "Mean Variance Optimisation"), size = 1) +
  geom_line(eff.fr.BL, mapping = aes(x = Exp.Return, y = BNDX, color = "Black Litterman Model"), size = 1) +
  scale_color_discrete(name = "", labels = c("Black Litterman Model","Mean Variance Optimisation")) +
  labs(title = "BNDX", x = "Expected Return", y = "Weight") +
  theme(legend.position = "bottom")

# EMB
EMB_ggplot <- ggplot() +
  geom_line(eff.fr, mapping = aes(x = Exp.Return, y = EMB, color = "Mean Variance Optimisation"), size = 1) +
  geom_line(eff.fr.BL, mapping = aes(x = Exp.Return, y = EMB, color = "Black Litterman Model"), size = 1) +
  scale_color_discrete(name = "", labels = c("Black Litterman Model","Mean Variance Optimisation")) +
  labs(title = "EMB", x = "Expected Return", y = "Weight") +
  theme(legend.position = "bottom")

ggarrange(VTI_ggplot,VTV_ggplot,VOE_ggplot,VBR_ggplot, VEA_ggplot, VWO_ggplot, SHV_ggplot, VTIP_ggplot, MUB_ggplot,
          AGG_ggplot, BNDX_ggplot, EMB_ggplot)


