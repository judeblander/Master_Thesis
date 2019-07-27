#### Implied Risk Aversion Coefficient ####
# Computation Rf T-note 10 years
Rf <- mean(Benchmark$`T-note`)
Rfdaily <- ((1+RfAnnual)^(1/365))-1

# Save market cap weight per day
ETF_MarketCap <- ETF_MarketCap[,-1]
sum <- matrix()
MarketCapWeight <- data.frame()
for (i in 1:365) {
  for (j in 1:12) {
     MarketCapWeight[i,j] <- as.data.frame(ETF_MarketCap[i,j])/sum(ETF_MarketCap[i,1:12])
  }
}
colnames(MarketCapWeight) <- colnames(ETF_MarketCap)
Mean.MarketCapWeight <- as.matrix(colMeans(MarketCapWeight))

# E(Rm) with 12 assets
ERm <- c()
for (i in 1:nrow(returns)) {
  ERm[i] <- sum(returns[i,]*t(MarketCapWeight[i,]))
}
Mean_ERm <- mean(ERm)
Varm <- t(Mean.MarketCapWeight)%*%covmat%*%Mean.MarketCapWeight
lamdba <- (Mean_ERm - Rfdaily)/Varm

#### Implied Beta ####
# Import market cap
ETF_MarketCap <- as.data.frame(ETF_MarketCap[,-1])

# Vector of implied betas
I.Beta <- (cov(returns)%*%mean.marketcap.weight)/as.vector(t(mean.marketcap.weight)%*%cov(returns)%*%mean.marketcap.weight)

#### Implied Excess Equilibrium Return Vector ####
IEE <- as.numeric(lamdba)*cov(returns)%*%as.matrix(Mean.MarketCapWeight)
CAPM <- I.Beta*(ERm-Rfdaily)
