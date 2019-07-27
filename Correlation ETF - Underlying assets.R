#### Correlation between the ETF and its underlying assets ####
### VTI ####
VTI_Underlying 
VTI_Returns <- VTI_Underlying$VTI
VTI_Underlying <- VTI_Underlying[,-1]
VTI_TE <- as.data.frame(matrix(NA, nrow = nrow(VTI_Underlying), ncol = (ncol(VTI_NTUSD))))

for (i in 1:nrow(VTI_Underlying)) {
  for (j in 1:ncol(VTI_NTUSD)) {
    VTI_TE[i,j] <- VTI_Underlying[i,j]*VTI_Weights[j,17]
  }
}
VTI_TE[,ncol(VTI_TE)+1] <- rowSums(VTI_TE[,1:11], na.rm = TRUE)

cor(VTI_Returns,VTI_TE$V12)

# VTV ####
VTV_Returns <- VTV_Underlying$VTV
VTV_Underlying <- VTV_Underlying[,-1]
VTV_Cor <- as.data.frame(matrix(NA, nrow = nrow(VTV_Underlying), ncol = 24))

for (i in 1:nrow(VTV_Underlying)) {
  for (j in 1:24) {
    VTV_Cor[i,j] <- VTV_Underlying[i,j]*VTV_Weights[j,17]
  }
}
VTV_Cor[,ncol(VTV_Cor)+1] <- rowSums(VTV_Cor[,1:24], na.rm = TRUE)

cor(VTV_Returns,rowSums(VTV_Cor[,1:12], na.rm = TRUE))

# VOE ####
VOE_Returns <- VOE_Underlying$VOE
VOE_Returns[is.na(VOE_Returns)] <- 0
VOE_Underlying <- VOE_Underlying[,-1]
VOE_Underlying <- VOE_Underlying[,-1]
VOE_Underlying <- VOE_Underlying[,-4]
VOE_Underlying <- VOE_Underlying[,-10]
VOE_Cor <- as.data.frame(matrix(NA, nrow = nrow(VOE_Underlying), ncol = ncol(VOE_Underlying)))

for (i in 1:nrow(VOE_Underlying)) {
  for (j in 1:ncol(VOE_Underlying)) {
    VOE_Cor[i,j] <- VOE_Underlying[i,j]*VOE_Weights[j,17]
  }
}
VOE_Cor[,ncol(VOE_Cor)+1] <- rowSums(VOE_Cor[,1:ncol(VOE_Underlying)], na.rm = TRUE)

cor(VOE_Returns,VOE_Cor$V13)

# VBR ####
VBR_Returns <- VBR_Underlying$VBR
VBR_Underlying <- VBR_Underlying[,-1]
VBR_Underlying <- VBR_Underlying[,-1]
VBR_Underlying <- VBR_Underlying[,-10]
VBR_Cor <- as.data.frame(matrix(NA, nrow = nrow(VBR_Underlying), ncol = ncol(VBR_Underlying)))
VBR_Weights <- VBR_Weights[-9,]

for (i in 1:nrow(VBR_Underlying)) {
  for (j in 1:ncol(VBR_Underlying)) {
    VBR_Cor[i,j] <- VBR_Underlying[i,j]*VBR_Weights[j,16]
  }
}

VBR_Cor[,ncol(VBR_Cor)+1] <- rowSums(VBR_Cor[,1:ncol(VBR_Underlying)], na.rm = TRUE)

cor(VBR_Returns,VBR_Cor$V10)

# VEA ####
VEA_Returns <- VEA_Underlying$VEA
VEA_Returns[is.na(VEA_Returns)] <- 0
VEA_Underlying <- VEA_Underlying[,-1]
VEA_Underlying <- VEA_Underlying[,-1]
VEA_Cor <- as.data.frame(matrix(NA, nrow = nrow(VEA_Underlying), ncol = ncol(VEA_Underlying)))

for (i in 1:nrow(VEA_Underlying)) {
  for (j in 1:ncol(VEA_Underlying)) {
    VEA_Cor[i,j] <- VEA_Underlying[i,j]*VEA_Weights[j,17]
  }
}

VEA_Cor[,ncol(VEA_Cor)+1] <- rowSums(VEA_Cor[,1:ncol(VEA_Underlying)], na.rm = TRUE)

cor(VEA_Returns,VEA_Cor$V6)

# VWO ####
VWO_Returns <- VWO_Underlying$VWO
VWO_Returns[is.na(VWO_Returns)] <- 0
VWO_Underlying <- VWO_Underlying[,-1]
VWO_Underlying <- VWO_Underlying[,-1]
VWO_Cor <- as.data.frame(matrix(NA, nrow = nrow(VWO_Underlying), ncol = ncol(VWO_Underlying)))

for (i in 1:nrow(VWO_Underlying)) {
  for (j in 1:ncol(VWO_Underlying)) {
    VWO_Cor[i,j] <- VWO_Underlying[i,j]*VWO_Weights[j,17]
  }
}

VWO_Cor[,ncol(VWO_Cor)+1] <- rowSums(VWO_Cor[,1:ncol(VWO_Underlying)], na.rm = TRUE)

VWO_Underlying$`0700.HK`[is.na(VWO_Underlying$`0700.HK`)] <- 0
cor(VWO_Returns,VWO_Cor$V4)
