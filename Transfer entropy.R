library(dplyr)
library(lmtest)
library(RTransferEntropy)

#### All possible combiniation of companies for VTI ####
# Create a table with all possibilities
VTI_Weights <- cbind(t(expand.grid(0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1)),VTI_Weights)

VTI_sentiment_w_NTUSD <- as.data.frame(matrix(NA))
Sum_VTI_Weights <- 0
VTI_Weights[,((2^ncol(VTI_NTUSD))+17)] <- NULL
row_num <- c()
All_possibilities_VTI <- list()
for (k in 2:(2^ncol(VTI_NTUSD))) {
  for (i in 1:nrow(VTI_NTUSD)) {
## To sum all weights where the possibility says 1    
    for (j in 1:ncol(VTI_NTUSD)) {
      if (VTI_Weights[j,k] == 1) {
        Sum_VTI_Weights <- Sum_VTI_Weights + VTI_Weights[j,((2^ncol(VTI_NTUSD))+16)]
      }
    }
## Divide the element with one with the sum above    
    for (l in 1:ncol(VTI_NTUSD)) {
      if (VTI_Weights[l,k]==1) {
      VTI_Weights[l,((2^ncol(VTI_NTUSD))+17)] <- VTI_Weights[l,((2^ncol(VTI_NTUSD))+16)]/Sum_VTI_Weights
      row_num <- which(VTI_Weights[,k]==1)
      }
    }
    
    for (m in 1:length(row_num)) {
      VTI_sentiment_w_NTUSD[i,m] <- VTI_NTUSD[i,row_num[m]]*VTI_Weights[row_num[m],((2^ncol(VTI_NTUSD))+17)]
  }
    }
VTI_sentiment_w_NTUSD[,ncol(VTI_sentiment_w_NTUSD)+1] <- rowSums(VTI_sentiment_w_NTUSD, na.rm = TRUE)
row.names(VTI_sentiment_w_NTUSD) <- row.names(VTI_NTUSD)
VTI_sentiment_w_NTUSD <- cbind(as.Date(row.names(VTI_sentiment_w_NTUSD)),VTI_sentiment_w_NTUSD)
colnames(VTI_sentiment_w_NTUSD)[1] <- "Date"
All_possibilities_VTI[[k]] <- as.data.frame(VTI_sentiment_w_NTUSD)  
# Reset everything to 0 for the next loop
Sum_VTI_Weights <- 0
row_num <- c()  
VTI_sentiment_w_NTUSD <- as.data.frame(matrix(NA))
VTI_Weights[,((2^ncol(VTI_NTUSD))+17)] <- NULL
}

test_causality_VTI <- list()
# Join 2 tables together to prepare for the non linear causality analysis
for (i in 2:length(All_possibilities_VTI)) {
  test_causality_VTI[[i]] <- left_join(Returns_ETF,All_possibilities_VTI[[i]], by =c("Date"="Date"))
}

#Replacing 0 by NA to not account for missing values during the test (the Shannon entropy test will ignore NA values)
for (i in 2:length(All_possibilities_VTI)) {
  is.na(test_causality_VTI[[i]][,ncol(test_causality_VTI[[i]])]) <- !test_causality_VTI[[i]][,ncol(test_causality_VTI[[i]])]
}

Entropy_VTI_1 <- c()
Entropy_VTI_2 <- c()
Entropy_VTI_3 <- c()
Entropy_VTI_4 <- c()

for (i in 2:(2^ncol(VTI_NTUSD))) {
  # Minimum number of days of 1/4 of the year => 91/364
  if (length(na.omit(pull(test_causality_VTI[[i]][,ncol(test_causality_VTI[[i]])]))) > 91 ) {
    # Perform the Shannon transfer entropy test
    Entropy_VTI_1[i] <- transfer_entropy(test_causality_VTI[[i]]$VTI,pull(test_causality_VTI[[i]][,ncol(test_causality_VTI[[i]])]),entropy = 'Shannon',lx = 1, ly = 1, nboot = 100)$coef[2,4]
    Entropy_VTI_2[i] <- transfer_entropy(test_causality_VTI[[i]]$VTI,pull(test_causality_VTI[[i]][,ncol(test_causality_VTI[[i]])]),entropy = 'Shannon',lx = 2, ly = 2, nboot = 100)$coef[2,4]
    Entropy_VTI_3[i] <- transfer_entropy(test_causality_VTI[[i]]$VTI,pull(test_causality_VTI[[i]][,ncol(test_causality_VTI[[i]])]),entropy = 'Shannon',lx = 3, ly = 3, nboot = 100)$coef[2,4]
    Entropy_VTI_4[i] <- transfer_entropy(test_causality_VTI[[i]]$VTI,pull(test_causality_VTI[[i]][,ncol(test_causality_VTI[[i]])]),entropy = 'Shannon',lx = 4, ly = 4, nboot = 100)$coef[2,4]
  }
}

# Retrieve the sequence of 0 and 1 that has the small p value of the Shannon transfer entropy test
VTI_Weights[,which.min(Entropy_VTI_1)] # 0 1 0 1 1 0 0 0 0 0 0 --> Microsoft + Facebook + Berkshire Hathaway
VTI_Weights[,which.min(Entropy_VTI_2)] # 0 1 0 0 1 1 0 0 0 0 0 --> Microsoft + Berkshire Hathaway + JPMorgan
VTI_Weights[,which.min(Entropy_VTI_3)] # 0 1 0 1 0 0 0 0 0 0 1 --> Microsoft + Facebook + Wells Fargo
VTI_Weights[,which.min(Entropy_VTI_4)] # 0 0 0 0 1 0 0 1 0 0 0 --> Bershire Hathaway + ExxonMobil

#### All possible combiniation of companies for VTV ####
VTV_Weights <- cbind(t(expand.grid(0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1)),VTV_Weights)

VTV_sentiment_w_NTUSD <- as.data.frame(matrix(NA))
Sum_VTV_Weights <- 0
VTV_Weights[,((2^ncol(VTV_NTUSD))+17)] <- NULL
row_num <- c()
All_possibilities_VTV <- list()
for (k in 2:(2^ncol(VTV_NTUSD))) {
  for (i in 1:nrow(VTV_NTUSD)) {
    ## To sum all weights where the possibility says 1    
    for (j in 1:ncol(VTV_NTUSD)) {
      if (VTV_Weights[j,k] == 1) {
        Sum_VTV_Weights <- Sum_VTV_Weights + VTV_Weights[j,((2^ncol(VTV_NTUSD))+16)]
      }
    }
    ## Divide the element with one with the sum above    
    for (l in 1:ncol(VTV_NTUSD)) {
      if (VTV_Weights[l,k]==1) {
        VTV_Weights[l,((2^ncol(VTV_NTUSD))+17)] <- VTV_Weights[l,((2^ncol(VTV_NTUSD))+16)]/Sum_VTV_Weights
        row_num <- which(VTV_Weights[,k]==1)
      }
    }
    
    for (m in 1:length(row_num)) {
      VTV_sentiment_w_NTUSD[i,m] <- VTV_NTUSD[i,row_num[m]]*VTV_Weights[row_num[m],((2^ncol(VTV_NTUSD))+17)]
    }
  }
  VTV_sentiment_w_NTUSD[,ncol(VTV_sentiment_w_NTUSD)+1] <- rowSums(VTV_sentiment_w_NTUSD, na.rm = TRUE)
  row.names(VTV_sentiment_w_NTUSD) <- row.names(VTV_NTUSD)
  VTV_sentiment_w_NTUSD <- cbind(as.Date(row.names(VTV_sentiment_w_NTUSD)),VTV_sentiment_w_NTUSD)
  colnames(VTV_sentiment_w_NTUSD)[1] <- "Date"
  All_possibilities_VTV[[k]] <- as.data.frame(VTV_sentiment_w_NTUSD)  
  Sum_VTV_Weights <- 0
  row_num <- c()  
  VTV_sentiment_w_NTUSD <- as.data.frame(matrix(NA))
  VTV_Weights[,((2^ncol(VTV_NTUSD))+17)] <- NULL
}

test_causality_VTV <- list()
for (i in 2:length(All_possibilities_VTV)) {
  test_causality_VTV[[i]] <- left_join(Returns_ETF,All_possibilities_VTV[[i]], by =c("Date"="Date"))
}

#Replacing 0 by NA
for (i in 2:length(All_possibilities_VTV)) {
  is.na(test_causality_VTV[[i]][,ncol(test_causality_VTV[[i]])]) <- !test_causality_VTV[[i]][,ncol(test_causality_VTV[[i]])]
}

Entropy_VTV_1 <- c()
Entropy_VTV_2 <- c()
Entropy_VTV_3 <- c()
Entropy_VTV_4 <- c()
for (i in 2:(2^ncol(VTV_NTUSD))) {
  if (length(na.omit(pull(test_causality_VTV[[i]][,ncol(test_causality_VTV[[i]])]))) > 91 ) {
    Entropy_VTV_1[i] <- transfer_entropy(test_causality_VTV[[i]]$VTV,pull(test_causality_VTV[[i]][,ncol(test_causality_VTV[[i]])]),entropy = 'Shannon',lx = 1, ly = 1, nboot = 100)$coef[2,4]
    Entropy_VTV_2[i] <- transfer_entropy(test_causality_VTV[[i]]$VTV,pull(test_causality_VTV[[i]][,ncol(test_causality_VTV[[i]])]),entropy = 'Shannon',lx = 2, ly = 2, nboot = 100)$coef[2,4]
    Entropy_VTV_3[i] <- transfer_entropy(test_causality_VTV[[i]]$VTV,pull(test_causality_VTV[[i]][,ncol(test_causality_VTV[[i]])]),entropy = 'Shannon',lx = 3, ly = 3, nboot = 100)$coef[2,4]
    Entropy_VTV_4[i] <- transfer_entropy(test_causality_VTV[[i]]$VTV,pull(test_causality_VTV[[i]][,ncol(test_causality_VTV[[i]])]),entropy = 'Shannon',lx = 4, ly = 4, nboot = 100)$coef[2,4]
  }
}

VTV_Weights[,which.min(Entropy_VTV_1)] # 1 1 1 0 0 0 0 0 0 0 --> Microsoft + Berkshire Hathaway +JPMorgan Chase
VTV_Weights[,which.min(Entropy_VTV_2)] # 1 0 0 0 0 0 0 0 0 0 --> Microsoft
VTV_Weights[,which.min(Entropy_VTV_3)] # 0 1 0 1 0 0 0 0 0 0 --> Berkshire Hathaway + JNJ
VTV_Weights[,which.min(Entropy_VTV_4)] # 0 1 0 0 1 0 0 0 0 0 --> Bershire Hathaway + Intel

#### All possible combiniation of companies for VOE ####
VOE_Weights <- cbind(t(expand.grid(0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1)),VOE_Weights)

VOE_sentiment_w_NTUSD <- as.data.frame(matrix(NA))
Sum_VOE_Weights <- 0
VOE_Weights[,((2^ncol(VOE_NTUSD))+17)] <- NULL
row_num <- c()
All_possibilities_VOE <- list()
for (k in 2:(2^ncol(VOE_NTUSD))) {
  for (i in 1:nrow(VOE_NTUSD)) {
    ## To sum all weights where the possibility says 1    
    for (j in 1:ncol(VOE_NTUSD)) {
      if (VOE_Weights[j,k] == 1) {
        Sum_VOE_Weights <- Sum_VOE_Weights + VOE_Weights[j,((2^ncol(VOE_NTUSD))+16)]
      }
    }
    ## Divide the element with one with the sum above    
    for (l in 1:ncol(VOE_NTUSD)) {
      if (VOE_Weights[l,k]==1) {
        VOE_Weights[l,((2^ncol(VOE_NTUSD))+17)] <- VOE_Weights[l,((2^ncol(VOE_NTUSD))+16)]/Sum_VOE_Weights
        row_num <- which(VOE_Weights[,k]==1)
        
      }
    }
    
    for (m in 1:length(row_num)) {
      VOE_sentiment_w_NTUSD[i,m] <- VOE_NTUSD[i,row_num[m]]*VOE_Weights[row_num[m],((2^ncol(VOE_NTUSD))+17)]
    }
  }
  VOE_sentiment_w_NTUSD[,ncol(VOE_sentiment_w_NTUSD)+1] <- rowSums(VOE_sentiment_w_NTUSD, na.rm = TRUE)
  row.names(VOE_sentiment_w_NTUSD) <- row.names(VOE_NTUSD)
  VOE_sentiment_w_NTUSD <- cbind(as.Date(row.names(VOE_sentiment_w_NTUSD)),VOE_sentiment_w_NTUSD)
  colnames(VOE_sentiment_w_NTUSD)[1] <- "Date"
  All_possibilities_VOE[[k]] <- as.data.frame(VOE_sentiment_w_NTUSD)  
  Sum_VOE_Weights <- 0
  row_num <- c()  
  VOE_sentiment_w_NTUSD <- as.data.frame(matrix(NA))
  VOE_Weights[,((2^ncol(VOE_NTUSD))+17)] <- NULL
}

test_causality_VOE <- list()
for (i in 2:length(All_possibilities_VOE)) {
  test_causality_VOE[[i]] <- left_join(Returns_ETF,All_possibilities_VOE[[i]], by =c("Date"="Date"))
}

#Replacing 0 by NA
for (i in 2:length(All_possibilities_VOE)) {
  is.na(test_causality_VOE[[i]][,ncol(test_causality_VOE[[i]])]) <- !test_causality_VOE[[i]][,ncol(test_causality_VOE[[i]])]
}


for (i in 2:(2^ncol(VOE_NTUSD))) {
  if (length(na.omit(pull(test_causality_VOE[[i]][,ncol(test_causality_VOE[[i]])]))) > 91 ) {
  Entropy_VOE_1[i] <- transfer_entropy(test_causality_VOE[[i]]$VOE,pull(test_causality_VOE[[i]][,ncol(test_causality_VOE[[i]])]),entropy = 'Shannon',lx = 1, ly = 1, nboot = 100)$coef[2,4]
  Entropy_VOE_2[i] <- transfer_entropy(test_causality_VOE[[i]]$VOE,pull(test_causality_VOE[[i]][,ncol(test_causality_VOE[[i]])]),entropy = 'Shannon',lx = 2, ly = 2, nboot = 100)$coef[2,4]
  Entropy_VOE_3[i] <- transfer_entropy(test_causality_VOE[[i]]$VOE,pull(test_causality_VOE[[i]][,ncol(test_causality_VOE[[i]])]),entropy = 'Shannon',lx = 3, ly = 3, nboot = 100)$coef[2,4]
  Entropy_VOE_4[i] <- transfer_entropy(test_causality_VOE[[i]]$VOE,pull(test_causality_VOE[[i]][,ncol(test_causality_VOE[[i]])]),entropy = 'Shannon',lx = 4, ly = 4, nboot = 100)$coef[2,4]
  }
}

VOE_Weights[,which.min(Entropy_VOE_1)] # 1 0 1 1 0 0 0 0 0 0 0 0 --> FreePort-McMoran + M&T Bank + Royal Caribbean
VOE_Weights[,which.min(Entropy_VOE_2)] # 0 1 1 0 1 1 1 0 1 0 0 0 --> Western Digital + M&T Bank + Citizens Financial + Regions Financial 
# + Newmont  + WEC 
VOE_Weights[,which.min(Entropy_VOE_3)] # 1 0 0 1 1 0 0 0 1 0 0 0 --> FreePort-McMoran + Royal Caribbean + Citizens Financial + WEC 
VOE_Weights[,which.min(Entropy_VOE_4)] # 1 0 0 1 1 0 0 0 1 0 0 0 --> FreePort-McMoran + Royal Caribbean + Citizens Financial + WEC

#### All possible combiniation of companies for VBR ####
VBR_Weights <- cbind(t(expand.grid(0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1)),VBR_Weights)

VBR_sentiment_w_NTUSD <- as.data.frame(matrix(NA))
Sum_VBR_Weights <- 0
VBR_Weights[,((2^ncol(VBR_NTUSD))+17)] <- NULL
row_num <- c()
All_possibilities_VBR <- list()
for (k in 2:(2^ncol(VBR_NTUSD))) {
  for (i in 1:nrow(VBR_NTUSD)) {
    ## To sum all weights where the possibility says 1    
    for (j in 1:ncol(VBR_NTUSD)) {
      if (VBR_Weights[j,k] == 1) {
        Sum_VBR_Weights <- Sum_VBR_Weights + VBR_Weights[j,((2^ncol(VBR_NTUSD))+16)]
      }
    }
    ## Divide the element with one with the sum above    
    for (l in 1:ncol(VBR_NTUSD)) {
      if (VBR_Weights[l,k]==1) {
        VBR_Weights[l,((2^ncol(VBR_NTUSD))+17)] <- VBR_Weights[l,((2^ncol(VBR_NTUSD))+16)]/Sum_VBR_Weights
        row_num <- which(VBR_Weights[,k]==1)
        
      }
    }
    
    for (m in 1:length(row_num)) {
      VBR_sentiment_w_NTUSD[i,m] <- VBR_NTUSD[i,row_num[m]]*VBR_Weights[row_num[m],((2^ncol(VBR_NTUSD))+17)]
    }
  }
  VBR_sentiment_w_NTUSD[,ncol(VBR_sentiment_w_NTUSD)+1] <- rowSums(VBR_sentiment_w_NTUSD, na.rm = TRUE)
  row.names(VBR_sentiment_w_NTUSD) <- row.names(VBR_NTUSD)
  VBR_sentiment_w_NTUSD <- cbind(as.Date(row.names(VBR_sentiment_w_NTUSD)),VBR_sentiment_w_NTUSD)
  colnames(VBR_sentiment_w_NTUSD)[1] <- "Date"
  All_possibilities_VBR[[k]] <- as.data.frame(VBR_sentiment_w_NTUSD)  
  Sum_VBR_Weights <- 0
  row_num <- c()  
  VBR_sentiment_w_NTUSD <- as.data.frame(matrix(NA))
  VBR_Weights[,((2^ncol(VBR_NTUSD))+17)] <- NULL
}

# Create a list of tables for the causality analysis by joining the return of the ETF with all the possibilities
test_causality_VBR <- list()
for (i in 2:length(All_possibilities_VBR)) {
  test_causality_VBR[[i]] <- left_join(Returns_ETF,All_possibilities_VBR[[i]], by =c("Date"="Date"))
}

# Replacing 0 by NA
for (i in 2:length(All_possibilities_VBR)) {
  is.na(test_causality_VBR[[i]][,ncol(test_causality_VBR[[i]])]) <- !test_causality_VBR[[i]][,ncol(test_causality_VBR[[i]])]
}


Entropy_VBR_1 <- c()
Entropy_VBR_2 <- c()
Entropy_VBR_3 <- c()
Entropy_VBR_4 <- c()
# Minimum number of days of 1/4 of the year => 91/364
for (i in 2:(2^ncol(VBR_NTUSD))) {
  if (length(na.omit(pull(test_causality_VBR[[i]][,ncol(test_causality_VBR[[i]])]))) > 91 ) {
    Entropy_VBR_1[i] <- transfer_entropy(test_causality_VBR[[i]]$VBR,pull(test_causality_VBR[[i]][,ncol(test_causality_VBR[[i]])]),entropy = 'Shannon',lx = 1, ly = 1, nboot = 100)$coef[2,4]
    Entropy_VBR_2[i] <- transfer_entropy(test_causality_VBR[[i]]$VBR,pull(test_causality_VBR[[i]][,ncol(test_causality_VBR[[i]])]),entropy = 'shannon',lx = 2, ly = 2, nboot = 100)$coef[2,4]
    Entropy_VBR_3[i] <- transfer_entropy(test_causality_VBR[[i]]$VBR,pull(test_causality_VBR[[i]][,ncol(test_causality_VBR[[i]])]),entropy = 'shannon',lx = 3, ly = 3, nboot = 100)$coef[2,4]
    Entropy_VBR_4[i] <- transfer_entropy(test_causality_VBR[[i]]$VBR,pull(test_causality_VBR[[i]][,ncol(test_causality_VBR[[i]])]),entropy = 'shannon',lx = 4, ly = 4, nboot = 100)$coef[2,4]
    }
}


VBR_Weights[,which.min(Entropy_VBR_1)] # 0 0 1 1 0 0 1 1 1 --> ON Semiconductor + Leidos + NRG + Wellcare + UGI Corp
VBR_Weights[,which.min(Entropy_VBR_2)] # 1 0 1 1 0 1 0 1 1 --> Spirit Aero + ON Semiconductor + Leidos + East West Bancorp + Wellcare + UGI Corp 
VBR_Weights[,which.min(Entropy_VBR_3)] # 1 0 1 1 1 1 0 0 1 --> Spirit Aero + ON Semiconductor + Leidos +Atmos +East West Bancorp + UGI Corp 
VBR_Weights[,which.min(Entropy_VBR_4)] # 1 1 1 1 1 0 1 0 0 --> Spirit Aero + IDEX + ON Semiconductor + Leidos + Atmos + NRG

#### All possible combiniation of companies for VEA ####
VEA_Weights <- cbind(t(expand.grid(0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1)),VEA_Weights)

VEA_sentiment_w_NTUSD <- as.data.frame(matrix(NA))
Sum_VEA_Weights <- 0
VEA_Weights[,((2^ncol(VEA_NTUSD))+17)] <- NULL
row_num <- c()
All_possibilities_VEA <- list()
for (k in 2:(2^ncol(VEA_NTUSD))) {
  for (i in 1:nrow(VEA_NTUSD)) {
    ## To sum all weights where the possibility says 1    
    for (j in 1:ncol(VEA_NTUSD)) {
      if (VEA_Weights[j,k] == 1) {
        Sum_VEA_Weights <- Sum_VEA_Weights + VEA_Weights[j,((2^ncol(VEA_NTUSD))+16)]
      }
    }
    ## Divide the element with one with the sum above    
    for (l in 1:ncol(VEA_NTUSD)) {
      if (VEA_Weights[l,k]==1) {
        VEA_Weights[l,((2^ncol(VEA_NTUSD))+17)] <- VEA_Weights[l,((2^ncol(VEA_NTUSD))+16)]/Sum_VEA_Weights
        row_num <- which(VEA_Weights[,k]==1)
        
      }
    }
    
    for (m in 1:length(row_num)) {
      VEA_sentiment_w_NTUSD[i,m] <- VEA_NTUSD[i,row_num[m]]*VEA_Weights[row_num[m],((2^ncol(VEA_NTUSD))+17)]
    }
  }
  VEA_sentiment_w_NTUSD[,ncol(VEA_sentiment_w_NTUSD)+1] <- rowSums(VEA_sentiment_w_NTUSD, na.rm = TRUE)
  row.names(VEA_sentiment_w_NTUSD) <- row.names(VEA_NTUSD)
  VEA_sentiment_w_NTUSD <- cbind(as.Date(row.names(VEA_sentiment_w_NTUSD)),VEA_sentiment_w_NTUSD)
  colnames(VEA_sentiment_w_NTUSD)[1] <- "Date"
  All_possibilities_VEA[[k]] <- as.data.frame(VEA_sentiment_w_NTUSD)  
  Sum_VEA_Weights <- 0
  row_num <- c()  
  VEA_sentiment_w_NTUSD <- as.data.frame(matrix(NA))
  VEA_Weights[,((2^ncol(VEA_NTUSD))+17)] <- NULL
}

test_causality_VEA <- list()
for (i in 2:length(All_possibilities_VEA)) {
  test_causality_VEA[[i]] <- left_join(Returns_ETF,All_possibilities_VEA[[i]], by =c("Date"="Date"))
}

# Replacing 0 by NA
for (i in 2:length(All_possibilities_VEA)) {
  is.na(test_causality_VEA[[i]][,ncol(test_causality_VEA[[i]])]) <- !test_causality_VEA[[i]][,ncol(test_causality_VEA[[i]])]
}

Entropy_VEA_1 <- c()
Entropy_VEA_2 <- c()
Entropy_VEA_3 <- c()
Entropy_VEA_4 <- c()
for (i in 2:(2^ncol(VEA_NTUSD))) {
  if (length(na.omit(pull(test_causality_VEA[[i]][,ncol(test_causality_VEA[[i]])]))) > 91 ) {
    Entropy_VEA_1[i] <- transfer_entropy(test_causality_VEA[[i]]$VEA,pull(test_causality_VEA[[i]][,ncol(test_causality_VEA[[i]])]),entropy = 'Shannon',lx = 1, ly = 1, nboot = 100)$coef[2,4]
    Entropy_VEA_2[i] <- transfer_entropy(test_causality_VEA[[i]]$VEA,pull(test_causality_VEA[[i]][,ncol(test_causality_VEA[[i]])]),entropy = 'Shannon',lx = 2, ly = 2, nboot = 100)$coef[2,4]
    Entropy_VEA_3[i] <- transfer_entropy(test_causality_VEA[[i]]$VEA,pull(test_causality_VEA[[i]][,ncol(test_causality_VEA[[i]])]),entropy = 'Shannon',lx = 3, ly = 3, nboot = 100)$coef[2,4]
    Entropy_VEA_4[i] <- transfer_entropy(test_causality_VEA[[i]]$VEA,pull(test_causality_VEA[[i]][,ncol(test_causality_VEA[[i]])]),entropy = 'Shannon',lx = 4, ly = 4, nboot = 100)$coef[2,4]
  }
}

VEA_Weights[,which.min(Entropy_VEA_1)] # 0 1 1 0 1 1 0 0 0 0 --> HSBC + Novartis + Toyota + Roche 
VEA_Weights[,which.min(Entropy_VEA_2)] # 0 1 1 0 1 1 0 0 0 0 --> HSBC + Novartis + Toyota + Roche 
VEA_Weights[,which.min(Entropy_VEA_3)] # 1 1 1 0 1 1 1 0 0 0 --> Nestle + HSBC + Novartis + Toyota + Roche + Shell
VEA_Weights[,which.min(Entropy_VEA_4)] # 1 1 1 0 1 1 0 0 0 0 --> Nestle + HSBC + Novartis + Toyota + Roche

#### All possible combiniation of companies for VWO ####
VWO_Weights <- cbind(t(expand.grid(0:1,0:1,0:1,0:1,0:1,0:1,0:1)),VWO_Weights)

VWO_sentiment_w_NTUSD <- as.data.frame(matrix(NA))
Sum_VWO_Weights <- 0
VWO_Weights[,((2^ncol(VWO_NTUSD))+17)] <- NULL
row_num <- c()
All_possibilities_VWO <- list()
for (k in 2:(2^ncol(VWO_NTUSD))) {
  for (i in 1:nrow(VWO_NTUSD)) {
    ## To sum all weights where the possibility says 1    
    for (j in 1:ncol(VWO_NTUSD)) {
      if (VWO_Weights[j,k] == 1) {
        Sum_VWO_Weights <- Sum_VWO_Weights + VWO_Weights[j,((2^ncol(VWO_NTUSD))+16)]
      }
    }
    ## Divide the element with one with the sum above    
    for (l in 1:ncol(VWO_NTUSD)) {
      if (VWO_Weights[l,k]==1) {
        VWO_Weights[l,((2^ncol(VWO_NTUSD))+17)] <- VWO_Weights[l,((2^ncol(VWO_NTUSD))+16)]/Sum_VWO_Weights
        row_num <- which(VWO_Weights[,k]==1)
        
      }
    }
    
    for (m in 1:length(row_num)) {
      VWO_sentiment_w_NTUSD[i,m] <- VWO_NTUSD[i,row_num[m]]*VWO_Weights[row_num[m],((2^ncol(VWO_NTUSD))+17)]
    }
  }
  VWO_sentiment_w_NTUSD[,ncol(VWO_sentiment_w_NTUSD)+1] <- rowSums(VWO_sentiment_w_NTUSD, na.rm = TRUE)
  row.names(VWO_sentiment_w_NTUSD) <- row.names(VWO_NTUSD)
  VWO_sentiment_w_NTUSD <- cbind(as.Date(row.names(VWO_sentiment_w_NTUSD)),VWO_sentiment_w_NTUSD)
  colnames(VWO_sentiment_w_NTUSD)[1] <- "Date"
  All_possibilities_VWO[[k]] <- as.data.frame(VWO_sentiment_w_NTUSD)  
  Sum_VWO_Weights <- 0
  row_num <- c()  
  VWO_sentiment_w_NTUSD <- as.data.frame(matrix(NA))
  VWO_Weights[,((2^ncol(VWO_NTUSD))+17)] <- NULL
}

test_causality_VWO <- list()
for (i in 2:length(All_possibilities_VWO)) {
  test_causality_VWO[[i]] <- left_join(Returns_ETF,All_possibilities_VWO[[i]], by =c("Date"="Date"))
}

# Replacing 0 by NA
for (i in 2:length(All_possibilities_VWO)) {
  is.na(test_causality_VWO[[i]][,ncol(test_causality_VWO[[i]])]) <- !test_causality_VWO[[i]][,ncol(test_causality_VWO[[i]])]
}

Entropy_VWO_1 <- c()
Entropy_VWO_2 <- c()
Entropy_VWO_3 <- c()
Entropy_VWO_4 <- c()
for (i in 2:(2^ncol(VWO_NTUSD))) {
  if (length(na.omit(pull(test_causality_VWO[[i]][,ncol(test_causality_VWO[[i]])]))) > 91 ) {
    Entropy_VWO_1[i] <- transfer_entropy(test_causality_VWO[[i]]$VWO,pull(test_causality_VWO[[i]][,ncol(test_causality_VWO[[i]])]),entropy = 'Shannon',lx = 1, ly = 1, nboot = 100)$coef[2,4]
    Entropy_VWO_2[i] <- transfer_entropy(test_causality_VWO[[i]]$VWO,pull(test_causality_VWO[[i]][,ncol(test_causality_VWO[[i]])]),entropy = 'Shannon',lx = 2, ly = 2, nboot = 100)$coef[2,4]
    Entropy_VWO_3[i] <- transfer_entropy(test_causality_VWO[[i]]$VWO,pull(test_causality_VWO[[i]][,ncol(test_causality_VWO[[i]])]),entropy = 'Shannon',lx = 3, ly = 3, nboot = 100)$coef[2,4]
    Entropy_VWO_4[i] <- transfer_entropy(test_causality_VWO[[i]]$VWO,pull(test_causality_VWO[[i]][,ncol(test_causality_VWO[[i]])]),entropy = 'Shannon',lx = 4, ly = 4, nboot = 100)$coef[2,4]
  }
}

List_Entropy_VWO <- list(Entropy_VWO_1,Entropy_VWO_2,Entropy_VWO_3,Entropy_VWO_4)
for (i in 1:4) {
  print(List_Entropy_VWO[[i]][which.min(Entropy_VWO_4)])
}

VWO_Weights[,which.min(Entropy_VWO_1)] #13 --> 0 0 1 1 0 0 0 --> Alibaba + China Mobile
VWO_Weights[,which.min(Entropy_VWO_1)] #106 --> 1 0 0 1 0 1 1 --> Tencent + China Mobile + Baidu + Reliance
VWO_Weights[,which.min(Entropy_VWO_3)] #6 --> 1 0 1 0 0 0 0 --> Tencent + Alibaba
VWO_Weights[,which.min(Entropy_VWO_4)] #74 --> 1 0 0 1 0 0 1 --> Tencent + China Mobile + Reliance

