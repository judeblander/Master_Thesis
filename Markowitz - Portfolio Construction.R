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

#### Markowitz ####
# Minimum Variance Portfolio MVO
min.var <- min.var.portfolio(covmat,mean_vect,Rfdaily, short = "yes" ,0.5, -0.1, Names)
sum(min.var[,1:6])
sum(min.var[,7:12])

# Efficient Portfolio MVO
eff.portfolio <- Eff.portfolio(covmat, mean_vect,Rfdaily, short = "yes", 0.0001, 0.50 , -0.1, Names)
sum(eff.portfolio[,1:6])
sum(eff.portfolio[,7:12])

# Efficient Frontier MVO
eff.fr <- eff.frontier(covmat, mean_vect,Rfdaily, short = "yes", 0.50,-0.1,0.50,-0.1, 5, 0.005)
eff.fr

# Look at the coordinates of x and y for tangency portfolio
return.tan.port <- eff.fr$Exp.Return[which(eff.fr$sharpe==max(eff.fr$sharpe), arr.ind = TRUE)]
std.tan.port <- eff.fr$Std.Dev[which(eff.fr$sharpe==max(eff.fr$sharpe), arr.ind = TRUE)]

# Plot efficient frontier and coordinates of points MVO
plot <- ggplot() +
  geom_line(eff.fr, mapping = aes(x = Std.Dev, y = Exp.Return)) + 
  geom_point(min.var, mapping = aes(x = Std.Dev, y = Exp.Return, color = "Minimum Variance Portfolio"))+
  geom_point(eff.fr, mapping = aes(x = Std.Dev[which(eff.fr$sharpe==max(eff.fr$sharpe), arr.ind = TRUE)], y = Exp.Return[which(eff.fr$sharpe==max(eff.fr$sharpe), arr.ind = TRUE)], color = "Tangency Portfolio"))+
  geom_point(eff.portfolio, mapping = aes(x = Std.Dev, y = Exp.Return, color = "Efficient Portfolio"))+
  geom_abline(slope = max(eff.fr$sharpe), intercept = Rfdaily)+
  scale_color_discrete( labels = c("Efficient Portfolio","Minimum Variance Portfolio","Tangency Portfolio")) +
  labs(x = "Standard Deviation", y = "Expected Return") +
  theme(legend.position = "bottom")
print(plot)
