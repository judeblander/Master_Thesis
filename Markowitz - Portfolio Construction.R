# Load package
library(PerformanceAnalytics)
library(quadprog)
library(ggplot2)

# Treat data as a time serie
ETF_Data <- as.data.frame(x = ETF_Data, order.by = as.Date(Stock_prices$Date))

Names <- colnames(Stock_prices)

# Returns
n <- nrow(Stock_prices)
returns <- ((Stock_prices[2:n,2:ncol(Stock_prices)] / Stock_prices[1:(n-1),2:ncol(Stock_prices)]) - 1)*100
cov_returns <- cov(returns)

# Minimum Variance Portfolio
min.var <- min.var.portfolio(excess_return[,2:13], short = "yes", 0.50, Names)
min.var


# Efficient Portfolio
eff.portfolio <- Eff.portfolio(excess_return[,2:13], short = "yes", 0.001, 0.50 , Names)
eff.portfolio

# Efficient Frontier
eff.fr <- eff.frontier(excess_return[,2:13], short = "yes", 0.50, 5, 0.005)
eff.fr

# Look at the coordinates of x and y for tangency portfolio
return.tan.port <- eff.fr$Exp.Return[which(eff.fr$sharpe==max(eff.fr$sharpe), arr.ind = TRUE)]
std.tan.port <- eff.fr$Std.Dev[which(eff.fr$sharpe==max(eff.fr$sharpe), arr.ind = TRUE)]

plot <- ggplot(eff.fr, aes(x = Std.Dev, y = Exp.Return)) +
  geom_line() + 
  geom_point(min.var, mapping = aes(x = Std.Dev, y = Exp.Return, color = "Minimum Variance Portfolio"))+
  geom_point(eff.fr, mapping = aes(x = Std.Dev[which(eff.fr$sharpe==max(eff.fr$sharpe), arr.ind = TRUE)], y = Exp.Return[which(eff.fr$sharpe==max(eff.fr$sharpe), arr.ind = TRUE)], color = "Tangency Portfolio"))+
  geom_point(eff.portfolio, mapping = aes(x = Std.Dev, y = Exp.Return, color = "Efficient Portfolio"))+
  geom_abline(slope = max(eff.fr$sharpe), intercept = Rfdaily)+
  labs(title = "Efficient Frontier", x = "Standard Deviation", y = "Expected Return") +
  theme(legend.position = "bottom")+
print(plot)
