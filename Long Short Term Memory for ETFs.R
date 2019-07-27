library(keras)
library(tensorflow)
library(MLmetrics)
library(plotly)
library(dplyr)

# Function normalization
range01 <- function(x){(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))}

#### VTI #####
# Take the model with the lowest p value in the transfer entropy
Model_VTI_sentiment <- list(test_causality_VTI[[which.min(Entropy_VTI_1)]]$V4,test_causality_VTI[[which.min(Entropy_VTI_2)]]$V4,
                            test_causality_VTI[[which.min(Entropy_VTI_3)]]$V4,test_causality_VTI[[which.min(Entropy_VTI_4)]]$V3)

# Initiate lists to stock accuracy measures
Model_MAE_VTI <- list()
Model_RMSE_VTI <- list()
Model_MSE_VTI <- list()

# Range 0 1 for values
for (k in 1:4) {
  y = range01(test_causality_VTI[[which.min(Entropy_VTI_1)]]$VTI)
  sentiment = range01(Model_VTI_sentiment[[k]])
  
  # If days with no sentence set NA to 2
  sentiment[is.na(sentiment)] <- 2
  
  # Create dataframe
  ts_VTI = data.frame(index = ETF_Data_NTUSD$Date, price = y, sentiment = sentiment)
  ts_VTI = ts_VTI[complete.cases(ts_VTI), ]
  ts_VTI$index = seq(nrow(ts_VTI))
  
  # Set lags of 2
  datalags = 2
  # Set the training set
  train_VTI = ts_VTI[seq(252 + datalags), ]
  # Set the test set
  test_VTI = ts_VTI[252 + datalags + seq(108 + datalags), ]
  # Set the batch size to 6 (needs to get an integer by which 252 and 108 can be divided --> required by the model)
  batch.size = 6
  
  # Divide variables x and y for the test and training set
  x.train_VTI = array(data = lag(cbind(train_VTI$sentiment),datalags)[-(1:datalags), ], dim = c(nrow(train_VTI) - datalags, datalags, 1))
  y.train_VTI = array(data = train_VTI$price[-(1:datalags)], dim = c(nrow(train_VTI)-datalags, 1))
  
  x.test_VTI = array(data = lag(cbind(test_VTI$sentiment), datalags)[-(1:datalags), ], dim = c(nrow(test_VTI) - datalags, datalags, 1))
  y.test_VTI = array(data = test_VTI$price[-(1:datalags)], dim = c(nrow(test_VTI) - datalags, 1))
  
  # Create folds for cross validation
  foldsx <- list(array(x.train_VTI[1:84],dim = c(84,2,1)),array(x.train_VTI[85:168],dim = c(84,2,1)),array(x.train_VTI[169:252],dim = c(84,2,1)))
  seqx <- list(seq(1,84),seq(85,168),seq(169,252))
  foldsy <- list(array(y.train_VTI[1:84],dim = c(84,1)),array(y.train_VTI[85:168],dim = c(84,1)),array(y.train_VTI[169:252],dim = c(84,1)))
  
  # Run 3 times the model for 3 folds 
  list_mae_VTI <- list()
  for(j in 1:3){
    model <- keras_model_sequential()
    
    model %>%
      # Sigmoid is used as recurrent activation function and tanh as output layer
      layer_lstm(units = 100,
                 input_shape = c(datalags, 1),
                 batch_size = batch.size,
                 return_sequences = TRUE,
                 stateful = TRUE,
                 unit_forget_bias = TRUE) %>%
      layer_dropout(rate = 0.5) %>% 
      layer_lstm(units = 50,
                 batch_size = batch.size,
                 return_sequences = FALSE,
                 stateful = TRUE,
                 unit_forget_bias = TRUE) %>%
      layer_dropout(rate = 0.5) %>%
      layer_dense(units = 1)
    
    model %>%
      compile(loss = 'mse', optimizer = 'adam', metrics = 'mae')
    
    model
    
    history = model %>% fit(x = array(x.train_VTI[-seqx[[j]],1,],dim = c(length(x.train_VTI[-seqx[[j]],1,]),2,1)),
                            y = array(y.train_VTI[-seqx[[j]]],dim = c(length(y.train_VTI[-seqx[[j]]]),1)),
                            batch_size = batch.size,
                            epochs = 100,
                            verbose = 1,
                            validation_data = list(foldsx[[j]],foldsy[[j]]),
                            shuffle = FALSE)
    
    # Stock the val_loss to check if there is overfitting
    list_mae_VTI[[j]] <- history$metrics$val_mean_absolute_error
  }
  
  # Compute the mean val_mean for 3 folds --> 100 values = 100 epochs 
  mean_mae_VTI <- c()
  for (i in 1:100) {
    mean_mae_VTI[i] <- rowMeans(cbind(list_mae_VTI[[1]][i],list_mae_VTI[[2]][i],list_mae_VTI[[3]][i]))
  }
  
  # Run the model with the entire training set
  model <- keras_model_sequential()
  
  model %>%
    # Sigmoid is used as recurrent activation function and tanh as output layer
    layer_lstm(units = 100,
               input_shape = c(datalags, 1),
               batch_size = batch.size,
               return_sequences = TRUE,
               stateful = TRUE,
               unit_forget_bias = TRUE) %>%
    layer_dropout(rate = 0.5) %>% 
    layer_lstm(units = 50,
               batch_size = batch.size,
               return_sequences = FALSE,
               stateful = TRUE,
               unit_forget_bias = TRUE) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1)
  
  model %>%
    compile(loss = 'mse', optimizer = 'adam', metrics = 'mae')
  
  model
  
  # Fit the model with the optimum number of epochs
  model %>% fit(x = x.train_VTI,
                y = y.train_VTI,
                batch_size = batch.size,
                epochs = which.min(mean_mae_VTI),
                verbose = 1,
                shuffle = FALSE)
  
  pred_out_VTI <- model %>% predict(x.test_VTI, batch_size = batch.size) %>% .[,1]
  

  Model_MAE_VTI[[k]] <- MAE(pred_out_VTI,y.test_VTI)
  Model_RMSE_VTI[[k]] <- RMSE(pred_out_VTI,y.test_VTI)
  Model_MSE_VTI[[k]] <-  MSE(pred_out_VTI,y.test_VTI)
}

#### VTV #####
Model_VTV_sentiment <- list(test_causality_VTV[[which.min(Entropy_VTV_1)]]$V4,test_causality_VTV[[which.min(Entropy_VTV_2)]]$V2,
                            test_causality_VTV[[which.min(Entropy_VTV_3)]]$V3,test_causality_VTV[[which.min(Entropy_VTV_4)]]$V3)
Model_MAE_VTV <- list()
Model_RMSE_VTV <- list()
Model_MSE_VTV <- list()

# Range 0 1 for values
for (k in 1:4) {
  y = range01(test_causality_VTV[[which.min(Entropy_VTV_1)]]$VTV)
  sentiment = range01(Model_VTV_sentiment[[k]])
  
  # If days with no sentence set NA to 2
  sentiment[is.na(sentiment)] <- 2
  
  # Create dataframe
  ts_VTV = data.frame(index = ETF_Data_NTUSD$Date, price = y, sentiment = sentiment)
  ts_VTV = ts_VTV[complete.cases(ts_VTV), ]
  ts_VTV$index = seq(nrow(ts_VTV))
  
  datalags = 2
  train_VTV = ts_VTV[seq(252 + datalags), ]
  test_VTV = ts_VTV[252 + datalags + seq(108 + datalags), ]
  batch.size = 6
  
  x.train_VTV = array(data = lag(cbind(train_VTV$sentiment), datalags)[-(1:datalags), ], dim = c(nrow(train_VTV) - datalags, datalags, 1))
  y.train_VTV = array(data = train_VTV$price[-(1:datalags)], dim = c(nrow(train_VTV)-datalags, 1))
  
  x.test_VTV = array(data = lag(cbind(test_VTV$sentiment), datalags)[-(1:datalags), ], dim = c(nrow(test_VTV) - datalags, datalags, 1))
  y.test_VTV = array(data = test_VTV$price[-(1:datalags)], dim = c(nrow(test_VTV) - datalags, 1))
  
  # Create folds for cross validation
  foldsx <- list(array(x.train_VTV[1:84],dim = c(84,2,1)),array(x.train_VTV[85:168],dim = c(84,2,1)),array(x.train_VTV[169:252],dim = c(84,2,1)))
  seqx <- list(seq(1,84),seq(85,168),seq(169,252))
  foldsy <- list(array(y.train_VTV[1:84],dim = c(84,1)),array(y.train_VTV[85:168],dim = c(84,1)),array(y.train_VTV[169:252],dim = c(84,1)))
  
  # Run 3 times the model for 3 folds 
  list_mae_VTV <- list()
  for(j in 1:3){
    model <- keras_model_sequential()
    
    model %>%
      # Sigmoid is used as recurrent activation function and tanh as output layer
      layer_lstm(units = 100,
                 input_shape = c(datalags, 1),
                 batch_size = batch.size,
                 return_sequences = TRUE,
                 stateful = TRUE,
                 unit_forget_bias = TRUE) %>%
      layer_dropout(rate = 0.5) %>% 
      layer_lstm(units = 50,
                 batch_size = batch.size,
                 return_sequences = FALSE,
                 stateful = TRUE,
                 unit_forget_bias = TRUE) %>%
      layer_dropout(rate = 0.5) %>%
      layer_dense(units = 1)
    
    model %>%
      compile(loss = 'mse', optimizer = 'adam', metrics = 'mae')
    
    model
    
    history = model %>% fit(x = array(x.train_VTV[-seqx[[j]],1,],dim = c(length(x.train_VTV[-seqx[[j]],1,]),2,1)),
                            y = array(y.train_VTV[-seqx[[j]]],dim = c(length(y.train_VTV[-seqx[[j]]]),1)),
                            batch_size = batch.size,
                            epochs = 100,
                            verbose = 1,
                            validation_data = list(foldsx[[j]],foldsy[[j]]),
                            shuffle = FALSE)
    
    # Stock the val_loss to check if there is overfitting
    list_mae_VTV[[j]] <- history$metrics$val_mean_absolute_error
  }
  
  # Compute the mean val_mean for 3 folds --> 100 values = 100 epochs 
  mean_mae_VTV <- c()
  for (i in 1:100) {
    mean_mae_VTV[i] <- rowMeans(cbind(list_mae_VTV[[1]][i],list_mae_VTV[[2]][i],list_mae_VTV[[3]][i]))
  }
  
  # Run the model with the entire training set
  model <- keras_model_sequential()
  
  model %>%
    # Sigmoid is used as recurrent activation function and tanh as output layer
    layer_lstm(units = 100,
               input_shape = c(datalags, 1),
               batch_size = batch.size,
               return_sequences = TRUE,
               stateful = TRUE,
               unit_forget_bias = TRUE) %>%
    layer_dropout(rate = 0.5) %>% 
    layer_lstm(units = 50,
               batch_size = batch.size,
               return_sequences = FALSE,
               stateful = TRUE,
               unit_forget_bias = TRUE) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1)
  
  model %>%
    compile(loss = 'mse', optimizer = 'adam', metrics = 'mae')
  
  model
  
  # Fit the model with the optimum number of epochs
  model %>% fit(x = x.train_VTV,
                y = y.train_VTV,
                batch_size = batch.size,
                epochs = which.min(mean_mae_VTV),
                verbose = 1,
                shuffle = FALSE)
  
  pred_out_VTV <- model %>% predict(x.test_VTV, batch_size = batch.size) %>% .[,1]
  
  Model_MAE_VTV[[k]] <- MAE(pred_out_VTV,y.test_VTV)
  Model_RMSE_VTV[[k]] <- RMSE(pred_out_VTV,y.test_VTV)
  Model_MSE_VTV[[k]] <-  MSE(pred_out_VTV,y.test_VTV)
}

#### VOE #####
Model_VOE_sentiment <- list(test_causality_VOE[[which.min(Entropy_VOE_1)]]$V4,test_causality_VOE[[which.min(Entropy_VOE_2)]]$V7,
                            test_causality_VOE[[which.min(Entropy_VOE_3)]]$V5)
Model_MAE_VOE <- list()
Model_RMSE_VOE <- list()
Model_MSE_VOE <- list()

# Range 0 1 for values
for (k in 1:3) {
  y = range01(test_causality_VOE[[which.min(Entropy_VOE_1)]]$VOE)
  sentiment = range01(Model_VOE_sentiment[[k]])
  
  # If days with no sentence set NA to 2
  sentiment[is.na(sentiment)] <- 2
  
  # Create dataframe
  ts_VOE = data.frame(index = ETF_Data_NTUSD$Date, price = y, sentiment = sentiment)
  ts_VOE = ts_VOE[complete.cases(ts_VOE), ]
  ts_VOE$index = seq(nrow(ts_VOE))
  
  datalags = 2
  train_VOE = ts_VOE[seq(252 + datalags), ]
  test_VOE = ts_VOE[252 + datalags + seq(108 + datalags), ]
  batch.size = 6
  
  x.train_VOE = array(data = lag(cbind(train_VOE$sentiment), datalags)[-(1:datalags), ], dim = c(nrow(train_VOE) - datalags, datalags, 1))
  y.train_VOE = array(data = train_VOE$price[-(1:datalags)], dim = c(nrow(train_VOE)-datalags, 1))
  
  x.test_VOE = array(data = lag(cbind(test_VOE$sentiment), datalags)[-(1:datalags), ], dim = c(nrow(test_VOE) - datalags, datalags, 1))
  y.test_VOE = array(data = test_VOE$price[-(1:datalags)], dim = c(nrow(test_VOE) - datalags, 1))
  
  # Create folds for cross validation
  foldsx <- list(array(x.train_VOE[1:84],dim = c(84,2,1)),array(x.train_VOE[85:168],dim = c(84,2,1)),array(x.train_VOE[169:252],dim = c(84,2,1)))
  seqx <- list(seq(1,84),seq(85,168),seq(169,252))
  foldsy <- list(array(y.train_VOE[1:84],dim = c(84,1)),array(y.train_VOE[85:168],dim = c(84,1)),array(y.train_VOE[169:252],dim = c(84,1)))
  
  # Run 3 times the model for 3 folds 
  list_mae_VOE <- list()
  for(j in 1:3){
    model <- keras_model_sequential()
    
    model %>%
      # Sigmoid is used as recurrent activation function and tanh as output layer
      layer_lstm(units = 100,
                 input_shape = c(datalags, 1),
                 batch_size = batch.size,
                 return_sequences = TRUE,
                 stateful = TRUE,
                 unit_forget_bias = TRUE) %>%
      layer_dropout(rate = 0.5) %>% 
      layer_lstm(units = 50,
                 batch_size = batch.size,
                 return_sequences = FALSE,
                 stateful = TRUE,
                 unit_forget_bias = TRUE) %>%
      layer_dropout(rate = 0.5) %>%
      layer_dense(units = 1)
    
    model %>%
      compile(loss = 'mse', optimizer = 'adam', metrics = 'mae')
    
    model
    
    history = model %>% fit(x = array(x.train_VOE[-seqx[[j]],1,],dim = c(length(x.train_VOE[-seqx[[j]],1,]),2,1)),
                            y = array(y.train_VOE[-seqx[[j]]],dim = c(length(y.train_VOE[-seqx[[j]]]),1)),
                            batch_size = batch.size,
                            epochs = 100,
                            verbose = 1,
                            validation_data = list(foldsx[[j]],foldsy[[j]]),
                            shuffle = FALSE)
    
    # Stock the val_loss to check if there is overfitting
    list_mae_VOE[[j]] <- history$metrics$val_mean_absolute_error
  }
  
  # Compute the mean val_mean for 3 folds --> 100 values = 100 epochs 
  mean_mae_VOE <- c()
  for (i in 1:100) {
    mean_mae_VOE[i] <- rowMeans(cbind(list_mae_VOE[[1]][i],list_mae_VOE[[2]][i],list_mae_VOE[[3]][i]))
  }
  
  # Run the model with the entire training set
  model <- keras_model_sequential()
  
  model %>%
    # Sigmoid is used as recurrent activation function and tanh as output layer
    layer_lstm(units = 100,
               input_shape = c(datalags, 1),
               batch_size = batch.size,
               return_sequences = TRUE,
               stateful = TRUE,
               unit_forget_bias = TRUE) %>%
    layer_dropout(rate = 0.5) %>% 
    layer_lstm(units = 50,
               batch_size = batch.size,
               return_sequences = FALSE,
               stateful = TRUE,
               unit_forget_bias = TRUE) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1)
  
  model %>%
    compile(loss = 'mse', optimizer = 'adam', metrics = 'mae')
  
  model
  
  # Fit the model with the optimum number of epochs
  model %>% fit(x = x.train_VOE,
                y = y.train_VOE,
                batch_size = batch.size,
                epochs = which.min(mean_mae_VOE),
                verbose = 1,
                shuffle = FALSE)
  
  pred_out_VOE <- model %>% predict(x.test_VOE, batch_size = batch.size) %>% .[,1]
  
  Model_MAE_VOE[[k]] <- MAE(pred_out_VOE,y.test_VOE)
  Model_RMSE_VOE[[k]] <- RMSE(pred_out_VOE,y.test_VOE)
  Model_MSE_VOE[[k]] <-  MSE(pred_out_VOE,y.test_VOE)
}

#### VBR #####
Model_VBR_sentiment <- list(test_causality_VBR[[which.min(Entropy_VBR_2)]]$V7, test_causality_VBR[[which.min(Entropy_VBR_3)]]$V7)

Model_MAE_VBR <- list()
Model_RMSE_VBR <- list()
Model_MSE_VBR <- list()

# Range 0 1 for values
for (k in 1:2) {
  y = range01(test_causality_VBR[[which.min(Entropy_VBR_1)]]$VBR)
  sentiment = range01(Model_VBR_sentiment[[k]])
  
  # If days with no sentence set NA to 2
  sentiment[is.na(sentiment)] <- 2
  
  # Create dataframe
  ts_VBR = data.frame(index = ETF_Data_NTUSD$Date, price = y, sentiment = sentiment)
  ts_VBR = ts_VBR[complete.cases(ts_VBR), ]
  ts_VBR$index = seq(nrow(ts_VBR))
  
  datalags = 2
  train_VBR = ts_VBR[seq(252 + datalags), ]
  test_VBR = ts_VBR[252 + datalags + seq(108 + datalags), ]
  batch.size = 6
  
  x.train_VBR = array(data = lag(cbind(train_VBR$sentiment), datalags)[-(1:datalags), ], dim = c(nrow(train_VBR) - datalags, datalags, 1))
  y.train_VBR = array(data = train_VBR$price[-(1:datalags)], dim = c(nrow(train_VBR)-datalags, 1))
  
  x.test_VBR = array(data = lag(cbind(test_VBR$sentiment), datalags)[-(1:datalags), ], dim = c(nrow(test_VBR) - datalags, datalags, 1))
  y.test_VBR = array(data = test_VBR$price[-(1:datalags)], dim = c(nrow(test_VBR) - datalags, 1))
  
  # Create folds for cross validation
  foldsx <- list(array(x.train_VBR[1:84],dim = c(84,2,1)),array(x.train_VBR[85:168],dim = c(84,2,1)),array(x.train_VBR[169:252],dim = c(84,2,1)))
  seqx <- list(seq(1,84),seq(85,168),seq(169,252))
  foldsy <- list(array(y.train_VBR[1:84],dim = c(84,1)),array(y.train_VBR[85:168],dim = c(84,1)),array(y.train_VBR[169:252],dim = c(84,1)))
  
  # Run 3 times the model for 3 folds 
  list_mae_VBR <- list()
  for(j in 1:3){
    model <- keras_model_sequential()
    
    model %>%
      # Sigmoid is used as recurrent activation function and tanh as output layer
      layer_lstm(units = 100,
                 input_shape = c(datalags, 1),
                 batch_size = batch.size,
                 return_sequences = TRUE,
                 stateful = TRUE,
                 unit_forget_bias = TRUE) %>%
      layer_dropout(rate = 0.5) %>% 
      layer_lstm(units = 50,
                 batch_size = batch.size,
                 return_sequences = FALSE,
                 stateful = TRUE,
                 unit_forget_bias = TRUE) %>%
      layer_dropout(rate = 0.5) %>%
      layer_dense(units = 1)
    
    model %>%
      compile(loss = 'mse', optimizer = 'adam', metrics = 'mae')
    
    model
    
    history = model %>% fit(x = array(x.train_VBR[-seqx[[j]],1,],dim = c(length(x.train_VBR[-seqx[[j]],1,]),2,1)),
                            y = array(y.train_VBR[-seqx[[j]]],dim = c(length(y.train_VBR[-seqx[[j]]]),1)),
                            batch_size = batch.size,
                            epochs = 100,
                            verbose = 1,
                            validation_data = list(foldsx[[j]],foldsy[[j]]),
                            shuffle = FALSE)
    
    # Stock the val_loss to check if there is overfitting
    list_mae_VBR[[j]] <- history$metrics$val_mean_absolute_error
  }
  
  # Compute the mean val_mean for 3 folds --> 100 values = 100 epochs 
  mean_mae_VBR <- c()
  for (i in 1:100) {
    mean_mae_VBR[i] <- rowMeans(cbind(list_mae_VBR[[1]][i],list_mae_VBR[[2]][i],list_mae_VBR[[3]][i]))
  }
  
  # Run the model with the entire training set
  model <- keras_model_sequential()
  
  model %>%
    # Sigmoid is used as recurrent activation function and tanh as output layer
    layer_lstm(units = 100,
               input_shape = c(datalags, 1),
               batch_size = batch.size,
               return_sequences = TRUE,
               stateful = TRUE,
               unit_forget_bias = TRUE) %>%
    layer_dropout(rate = 0.5) %>% 
    layer_lstm(units = 50,
               batch_size = batch.size,
               return_sequences = FALSE,
               stateful = TRUE,
               unit_forget_bias = TRUE) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1)
  
  model %>%
    compile(loss = 'mse', optimizer = 'adam', metrics = 'mae')
  
  model
  
  # Fit the model with the optimum number of epochs
  model %>% fit(x = x.train_VBR,
                y = y.train_VBR,
                batch_size = batch.size,
                epochs = which.min(mean_mae_VBR),
                verbose = 1,
                shuffle = FALSE)
  
  pred_out_VBR <- model %>% predict(x.test_VBR, batch_size = batch.size) %>% .[,1]

  Model_MAE_VBR[[k]] <- MAE(pred_out_VBR,y.test_VBR)
  Model_RMSE_VBR[[k]] <- RMSE(pred_out_VBR,y.test_VBR)
  Model_MSE_VBR[[k]] <-  MSE(pred_out_VBR,y.test_VBR)
}
#### VEA #####
Model_VEA_sentiment <- list(test_causality_VEA[[which.min(Entropy_VEA_1)]]$V5, test_causality_VEA[[which.min(Entropy_VEA_3)]]$V7,
                            test_causality_VEA[[which.min(Entropy_VEA_4)]]$V6)

Model_MAE_VEA <- list()
Model_RMSE_VEA <- list()
Model_MSE_VEA <- list()

# Range 0 1 for values
for (k in 1:3) {
  y = range01(test_causality_VEA[[which.min(Entropy_VEA_1)]]$VEA)
  sentiment = range01(Model_VEA_sentiment[[k]])
  
  # If days with no sentence set NA to 2
  sentiment[is.na(sentiment)] <- 2
  
  # Create dataframe
  ts_VEA = data.frame(index = ETF_Data_NTUSD$Date, price = y, sentiment = sentiment)
  ts_VEA = ts_VEA[complete.cases(ts_VEA), ]
  ts_VEA$index = seq(nrow(ts_VEA))
  
  datalags = 2
  train_VEA = ts_VEA[seq(252 + datalags), ]
  test_VEA = ts_VEA[252 + datalags + seq(108 + datalags), ]
  batch.size = 6
  
  x.train_VEA = array(data = lag(cbind(train_VEA$sentiment), datalags)[-(1:datalags), ], dim = c(nrow(train_VEA) - datalags, datalags, 1))
  y.train_VEA = array(data = train_VEA$price[-(1:datalags)], dim = c(nrow(train_VEA)-datalags, 1))
  
  x.test_VEA = array(data = lag(cbind(test_VEA$sentiment), datalags)[-(1:datalags), ], dim = c(nrow(test_VEA) - datalags, datalags, 1))
  y.test_VEA = array(data = test_VEA$price[-(1:datalags)], dim = c(nrow(test_VEA) - datalags, 1))
  
  # Create folds for cross validation
  foldsx <- list(array(x.train_VEA[1:84],dim = c(84,2,1)),array(x.train_VEA[85:168],dim = c(84,2,1)),array(x.train_VEA[169:252],dim = c(84,2,1)))
  seqx <- list(seq(1,84),seq(85,168),seq(169,252))
  foldsy <- list(array(y.train_VEA[1:84],dim = c(84,1)),array(y.train_VEA[85:168],dim = c(84,1)),array(y.train_VEA[169:252],dim = c(84,1)))
  
  # Run 3 times the model for 3 folds 
  list_mae_VEA <- list()
  for(j in 1:3){
    model <- keras_model_sequential()
    
    model %>%
      # Sigmoid is used as recurrent activation function and tanh as output layer
      layer_lstm(units = 100,
                 input_shape = c(datalags, 1),
                 batch_size = batch.size,
                 return_sequences = TRUE,
                 stateful = TRUE,
                 unit_forget_bias = TRUE) %>%
      layer_dropout(rate = 0.5) %>% 
      layer_lstm(units = 50,
                 batch_size = batch.size,
                 return_sequences = FALSE,
                 stateful = TRUE,
                 unit_forget_bias = TRUE) %>%
      layer_dropout(rate = 0.5) %>%
      layer_dense(units = 1)
    
    model %>%
      compile(loss = 'mse', optimizer = 'adam', metrics = 'mae')
    
    model
    
    history = model %>% fit(x = array(x.train_VEA[-seqx[[j]],1,],dim = c(length(x.train_VEA[-seqx[[j]],1,]),2,1)),
                            y = array(y.train_VEA[-seqx[[j]]],dim = c(length(y.train_VEA[-seqx[[j]]]),1)),
                            batch_size = batch.size,
                            epochs = 100,
                            verbose = 1,
                            validation_data = list(foldsx[[j]],foldsy[[j]]),
                            shuffle = FALSE)
    
    # Stock the val_loss to check if there is overfitting
    list_mae_VEA[[j]] <- history$metrics$val_mean_absolute_error
  }
  
  # Compute the mean val_mean for 3 folds --> 100 values = 100 epochs 
  mean_mae_VEA <- c()
  for (i in 1:100) {
    mean_mae_VEA[i] <- rowMeans(cbind(list_mae_VEA[[1]][i],list_mae_VEA[[2]][i],list_mae_VEA[[3]][i]))
  }
  
  # Run the model with the entire training set
  model <- keras_model_sequential()
  
  model %>%
    # Sigmoid is used as recurrent activation function and tanh as output layer
    layer_lstm(units = 100,
               input_shape = c(datalags, 1),
               batch_size = batch.size,
               return_sequences = TRUE,
               stateful = TRUE,
               unit_forget_bias = TRUE) %>%
    layer_dropout(rate = 0.5) %>% 
    layer_lstm(units = 50,
               batch_size = batch.size,
               return_sequences = FALSE,
               stateful = TRUE,
               unit_forget_bias = TRUE) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1)
  
  model %>%
    compile(loss = 'mse', optimizer = 'adam', metrics = 'mae')
  
  model
  
  # Fit the model with the optimum number of epochs
  model %>% fit(x = x.train_VEA,
                y = y.train_VEA,
                batch_size = batch.size,
                epochs = which.min(mean_mae_VEA),
                verbose = 1,
                shuffle = FALSE)
  
  pred_out_VEA <- model %>% predict(x.test_VEA, batch_size = batch.size) %>% .[,1]
  
  Model_MAE_VEA[[k]] <- MAE(pred_out_VEA,y.test_VEA)
  Model_RMSE_VEA[[k]] <- RMSE(pred_out_VEA,y.test_VEA)
  Model_MSE_VEA[[k]] <-  MSE(pred_out_VEA,y.test_VEA)
}
#### VWO #####
Model_VWO_sentiment <- list(test_causality_VWO[[which.min(Entropy_VWO_1)]]$V3, test_causality_VWO[[which.min(Entropy_VWO_3)]]$V3)

Model_MAE_VWO <- list()
Model_RMSE_VWO <- list()
Model_MSE_VWO <- list()

# Range 0 1 for values
for (k in 1:2) {
  y = range01(test_causality_VWO[[which.min(Entropy_VWO_1)]]$VWO)
  sentiment = range01(Model_VWO_sentiment[[k]])
  
  # If days with no sentence set NA to 2
  sentiment[is.na(sentiment)] <- 2
  
  # Create dataframe
  ts_VWO = data.frame(index = ETF_Data_NTUSD$Date, price = y, sentiment = sentiment)
  ts_VWO = ts_VWO[complete.cases(ts_VWO), ]
  ts_VWO$index = seq(nrow(ts_VWO))
  
  datalags = 2
  train_VWO = ts_VWO[seq(252 + datalags), ]
  test_VWO = ts_VWO[252 + datalags + seq(108 + datalags), ]
  batch.size = 6
  
  x.train_VWO = array(data = lag(cbind(train_VWO$sentiment), datalags)[-(1:datalags), ], dim = c(nrow(train_VWO) - datalags, datalags, 1))
  y.train_VWO = array(data = train_VWO$price[-(1:datalags)], dim = c(nrow(train_VWO)-datalags, 1))
  
  x.test_VWO = array(data = lag(cbind(test_VWO$sentiment), datalags)[-(1:datalags), ], dim = c(nrow(test_VWO) - datalags, datalags, 1))
  y.test_VWO = array(data = test_VWO$price[-(1:datalags)], dim = c(nrow(test_VWO) - datalags, 1))
  
  # Create folds for cross validation
  foldsx <- list(array(x.train_VWO[1:84],dim = c(84,2,1)),array(x.train_VWO[85:168],dim = c(84,2,1)),array(x.train_VWO[169:252],dim = c(84,2,1)))
  seqx <- list(seq(1,84),seq(85,168),seq(169,252))
  foldsy <- list(array(y.train_VWO[1:84],dim = c(84,1)),array(y.train_VWO[85:168],dim = c(84,1)),array(y.train_VWO[169:252],dim = c(84,1)))
  
  # Run 3 times the model for 3 folds 
  list_mae_VWO <- list()
  for(j in 1:3){
    model <- keras_model_sequential()
    
    model %>%
      # Sigmoid is used as recurrent activation function and tanh as output layer
      layer_lstm(units = 100,
                 input_shape = c(datalags, 1),
                 batch_size = batch.size,
                 return_sequences = TRUE,
                 stateful = TRUE,
                 unit_forget_bias = TRUE) %>%
      layer_dropout(rate = 0.5) %>% 
      layer_lstm(units = 50,
                 batch_size = batch.size,
                 return_sequences = FALSE,
                 stateful = TRUE,
                 unit_forget_bias = TRUE) %>%
      layer_dropout(rate = 0.5) %>%
      layer_dense(units = 1)
    
    model %>%
      compile(loss = 'mse', optimizer = 'adam', metrics = 'mae')
    
    model
    
    history = model %>% fit(x = array(x.train_VWO[-seqx[[j]],1,],dim = c(length(x.train_VWO[-seqx[[j]],1,]),2,1)),
                            y = array(y.train_VWO[-seqx[[j]]],dim = c(length(y.train_VWO[-seqx[[j]]]),1)),
                            batch_size = batch.size,
                            epochs = 100,
                            verbose = 1,
                            validation_data = list(foldsx[[j]],foldsy[[j]]),
                            shuffle = FALSE)
    
    # Stock the val_loss to check if there is overfitting
    list_mae_VWO[[j]] <- history$metrics$val_mean_absolute_error
  }
  
  # Compute the mean val_mean for 3 folds --> 100 values = 100 epochs 
  mean_mae_VWO <- c()
  for (i in 1:100) {
    mean_mae_VWO[i] <- rowMeans(cbind(list_mae_VWO[[1]][i],list_mae_VWO[[2]][i],list_mae_VWO[[3]][i]))
  }
  
  # Run the model with the entire training set
  model <- keras_model_sequential()
  
  model %>%
    # Sigmoid is used as recurrent activation function and tanh as output layer
    layer_lstm(units = 100,
               input_shape = c(datalags, 1),
               batch_size = batch.size,
               return_sequences = TRUE,
               stateful = TRUE,
               unit_forget_bias = TRUE) %>%
    layer_dropout(rate = 0.5) %>% 
    layer_lstm(units = 50,
               batch_size = batch.size,
               return_sequences = FALSE,
               stateful = TRUE,
               unit_forget_bias = TRUE) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1)
  
  model %>%
    compile(loss = 'mse', optimizer = 'adam', metrics = 'mae')
  
  model
  
  # Fit the model with the optimum number of epochs
  model %>% fit(x = x.train_VWO,
                y = y.train_VWO,
                batch_size = batch.size,
                epochs = which.min(mean_mae_VWO),
                verbose = 1,
                shuffle = FALSE)
  
  pred_out_VWO <- model %>% predict(x.test_VWO, batch_size = batch.size) %>% .[,1]

  Model_MAE_VWO[[k]] <- MAE(pred_out_VWO,y.test_VWO)
  Model_RMSE_VWO[[k]] <- RMSE(pred_out_VWO,y.test_VWO)
  Model_MSE_VWO[[k]] <-  MSE(pred_out_VWO,y.test_VWO)
}




########## VTI Prediction ##############################
# Capture min and max for denormalisation
min_VTI_M1 <- min(test_causality_VTI[[which.min(Entropy_VTI_1)]]$VTI)
max_VTI_M1 <- max(test_causality_VTI[[which.min(Entropy_VTI_1)]]$VTI)
y_VTI = range01(test_causality_VTI[[which.min(Entropy_VTI_1)]]$VTI)
sentiment_VTI = range01(Model_VTI_sentiment[[1]])
sentiment_VTI[is.na(sentiment_VTI)] <- 2
vol_VTI = range01(ETF_Data_NTUSD$VTI_Volume)

ts_VTI_M1 = data.frame(index = ETF_Data_NTUSD$Date, price = y_VTI, sentiment = sentiment_VTI, vol = vol_VTI)

datalags = 2
train_VTI_M1 = ts_VTI_M1[seq(252 + datalags), ]
test_VTI_M1 = ts_VTI_M1[252 + datalags + seq(108 + datalags), ]
batch.size = 6

Model_MAE_VTI_M1 <- list()
Model_RMSE_VTI_M1 <- list()
Model_MSE_VTI_M1 <- list()

  x.train_VTI_M1 = array(data = lag(cbind(train_VTI_M1$sentiment), datalags )[-(1:datalags)], dim = c(nrow(train_VTI_M1) - datalags, datalags, 1))
  y.train_VTI_M1 = array(data = train_VTI_M1$price[-(1:datalags)], dim = c(nrow(train_VTI_M1)-datalags, 1))
  
  x.test_VTI_M1 = array(data = lag(cbind(test_VTI_M1$sentiment), datalags)[-(1:datalags)], dim = c(nrow(test_VTI_M1) - datalags, datalags, 1))
  y.test_VTI_M1 = array(data = test_VTI_M1$price[-(1:datalags)], dim = c(nrow(test_VTI_M1) - datalags, 1))
  
  model <- keras_model_sequential()
  
  model %>%
    # Sigmoid is used as recurrent activation function and tanh as output layer
    layer_lstm(units = 100,
               input_shape = c(datalags, 1),
               batch_size = batch.size,
               return_sequences = TRUE,
               stateful = TRUE,
               unit_forget_bias = TRUE) %>%
    layer_dropout(rate = 0.5) %>% 
    layer_lstm(units = 50,
               batch_size = batch.size,
               return_sequences = FALSE,
               stateful = TRUE,
               unit_forget_bias = TRUE) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1)
  
  model %>%
    compile(loss = 'mse', optimizer = 'adam', metrics = 'mae')
  
  model
  
  
  model %>% fit(x = x.train_VTI_M1,
                y = y.train_VTI_M1,
                batch_size = batch.size,
                epochs = 100,
                verbose = 1,
                shuffle = FALSE)
  
  pred_out_VTI_M1 <- model %>% predict(x.test_VTI_M1, batch_size = batch.size) %>% .[,1]
  
Future_return_VTI <- pred_out_VTI_M1[108]*(max_VTI_M1-min_VTI_M1)+min_VTI_M1


########## VTV Prediction ##############################
# Capture min and max for denormalisation
min_VTV_M1 <- min(test_causality_VTV[[which.min(Entropy_VTV_1)]]$VTV)
max_VTV_M1 <- max(test_causality_VTV[[which.min(Entropy_VTV_1)]]$VTV)
y_VTV = range01(test_causality_VTV[[which.min(Entropy_VTV_1)]]$VTV)
sentiment_VTV = range01(Model_VTV_sentiment[[1]])
sentiment_VTV[is.na(sentiment_VTV)] <- 2
vol_VTV = range01(ETF_Data_NTUSD$VTV_Volume)

ts_VTV_M1 = data.frame(index = ETF_Data_NTUSD$Date, price = y_VTV, sentiment = sentiment_VTV, vol = vol_VTV)

datalags = 2
train_VTV_M1 = ts_VTV_M1[seq(252 + datalags), ]
test_VTV_M1 = ts_VTV_M1[252 + datalags + seq(108 + datalags), ]
batch.size = 6

Model_MAE_VTV_M1 <- list()
Model_RMSE_VTV_M1 <- list()
Model_MSE_VTV_M1 <- list()

x.train_VTV_M1 = array(data = lag(cbind(train_VTV_M1$sentiment), datalags )[-(1:datalags)], dim = c(nrow(train_VTV_M1) - datalags, datalags, 1))
y.train_VTV_M1 = array(data = train_VTV_M1$price[-(1:datalags)], dim = c(nrow(train_VTV_M1)-datalags, 1))

x.test_VTV_M1 = array(data = lag(cbind(test_VTV_M1$sentiment), datalags)[-(1:datalags)], dim = c(nrow(test_VTV_M1) - datalags, datalags, 1))
y.test_VTV_M1 = array(data = test_VTV_M1$price[-(1:datalags)], dim = c(nrow(test_VTV_M1) - datalags, 1))

model <- keras_model_sequential()

model %>%
  # Sigmoid is used as recurrent activation function and tanh as output layer
  layer_lstm(units = 100,
             input_shape = c(datalags, 1),
             batch_size = batch.size,
             return_sequences = TRUE,
             stateful = TRUE,
             unit_forget_bias = TRUE) %>%
  layer_dropout(rate = 0.5) %>% 
  layer_lstm(units = 50,
             batch_size = batch.size,
             return_sequences = FALSE,
             stateful = TRUE,
             unit_forget_bias = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)

model %>%
  compile(loss = 'mse', optimizer = 'adam', metrics = 'mae')

model


model %>% fit(x = x.train_VTV_M1,
              y = y.train_VTV_M1,
              batch_size = batch.size,
              epochs = 100,
              verbose = 1,
              shuffle = FALSE)

pred_out_VTV_M1 <- model %>% predict(x.test_VTV_M1, batch_size = batch.size) %>% .[,1]

Future_return_VTV <- pred_out_VTV_M1[108]*(max_VTV_M1-min_VTV_M1)+min_VTV_M1

########## VOE Prediction ##############################
# Capture min and max for denormalisation
min_VOE_M1 <- min(test_causality_VOE[[which.min(Entropy_VOE_1)]]$VOE)
max_VOE_M1 <- max(test_causality_VOE[[which.min(Entropy_VOE_1)]]$VOE)
y_VOE = range01(test_causality_VOE[[which.min(Entropy_VOE_1)]]$VOE)
sentiment_VOE = range01(Model_VOE_sentiment[[2]])
sentiment_VOE[is.na(sentiment_VOE)] <- 2
vol_VOE = range01(ETF_Data_NTUSD$VOE_Volume)

ts_VOE_M1 = data.frame(index = ETF_Data_NTUSD$Date, price = y_VOE, sentiment = sentiment_VOE, vol = vol_VOE)

datalags = 2
train_VOE_M1 = ts_VOE_M1[seq(252 + datalags), ]
test_VOE_M1 = ts_VOE_M1[252 + datalags + seq(108 + datalags), ]
batch.size = 6

Model_MAE_VOE_M1 <- list()
Model_RMSE_VOE_M1 <- list()
Model_MSE_VOE_M1 <- list()

x.train_VOE_M1 = array(data = lag(cbind(train_VOE_M1$sentiment), datalags )[-(1:datalags)], dim = c(nrow(train_VOE_M1) - datalags, datalags, 1))
y.train_VOE_M1 = array(data = train_VOE_M1$price[-(1:datalags)], dim = c(nrow(train_VOE_M1)-datalags, 1))

x.test_VOE_M1 = array(data = lag(cbind(test_VOE_M1$sentiment), datalags)[-(1:datalags)], dim = c(nrow(test_VOE_M1) - datalags, datalags, 1))
y.test_VOE_M1 = array(data = test_VOE_M1$price[-(1:datalags)], dim = c(nrow(test_VOE_M1) - datalags, 1))

model <- keras_model_sequential()

model %>%
  # Sigmoid is used as recurrent activation function and tanh as output layer
  layer_lstm(units = 100,
             input_shape = c(datalags, 1),
             batch_size = batch.size,
             return_sequences = TRUE,
             stateful = TRUE,
             unit_forget_bias = TRUE) %>%
  layer_dropout(rate = 0.5) %>% 
  layer_lstm(units = 50,
             batch_size = batch.size,
             return_sequences = FALSE,
             stateful = TRUE,
             unit_forget_bias = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)

model %>%
  compile(loss = 'mse', optimizer = 'adam', metrics = 'mae')

model


model %>% fit(x = x.train_VOE_M1,
              y = y.train_VOE_M1,
              batch_size = batch.size,
              epochs = 100,
              verbose = 1,
              shuffle = FALSE)

pred_out_VOE_M1 <- model %>% predict(x.test_VOE_M1, batch_size = batch.size) %>% .[,1]

Future_return_VOE <- pred_out_VOE_M1[108]*(max_VOE_M1-min_VOE_M1)+min_VOE_M1
########## VBR Prediction ##############################
# Capture min and max for denormalisation
min_VBR_M1 <- min(test_causality_VBR[[which.min(Entropy_VBR_1)]]$VBR)
max_VBR_M1 <- max(test_causality_VBR[[which.min(Entropy_VBR_1)]]$VBR)
y_VBR = range01(test_causality_VBR[[which.min(Entropy_VBR_1)]]$VBR)
sentiment_VBR = range01(Model_VBR_sentiment[[2]])
sentiment_VBR[is.na(sentiment_VBR)] <- 2
vol_VBR = range01(ETF_Data_NTUSD$VBR_Volume)

ts_VBR_M1 = data.frame(index = ETF_Data_NTUSD$Date, price = y_VBR, sentiment = sentiment_VBR, vol = vol_VBR)

datalags = 2
train_VBR_M1 = ts_VBR_M1[seq(252 + datalags), ]
test_VBR_M1 = ts_VBR_M1[252 + datalags + seq(108 + datalags), ]
batch.size = 6

Model_MAE_VBR_M1 <- list()
Model_RMSE_VBR_M1 <- list()
Model_MSE_VBR_M1 <- list()

x.train_VBR_M1 = array(data = lag(cbind(train_VBR_M1$sentiment), datalags )[-(1:datalags)], dim = c(nrow(train_VBR_M1) - datalags, datalags, 1))
y.train_VBR_M1 = array(data = train_VBR_M1$price[-(1:datalags)], dim = c(nrow(train_VBR_M1)-datalags, 1))

x.test_VBR_M1 = array(data = lag(cbind(test_VBR_M1$sentiment), datalags)[-(1:datalags)], dim = c(nrow(test_VBR_M1) - datalags, datalags, 1))
y.test_VBR_M1 = array(data = test_VBR_M1$price[-(1:datalags)], dim = c(nrow(test_VBR_M1) - datalags, 1))

model <- keras_model_sequential()

model %>%
  # Sigmoid is used as recurrent activation function and tanh as output layer
  layer_lstm(units = 100,
             input_shape = c(datalags, 1),
             batch_size = batch.size,
             return_sequences = TRUE,
             stateful = TRUE,
             unit_forget_bias = TRUE) %>%
  layer_dropout(rate = 0.5) %>% 
  layer_lstm(units = 50,
             batch_size = batch.size,
             return_sequences = FALSE,
             stateful = TRUE,
             unit_forget_bias = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)

model %>%
  compile(loss = 'mse', optimizer = 'adam', metrics = 'mae')

model


model %>% fit(x = x.train_VBR_M1,
              y = y.train_VBR_M1,
              batch_size = batch.size,
              epochs = 100,
              verbose = 1,
              shuffle = FALSE)

pred_out_VBR_M1 <- model %>% predict(x.test_VBR_M1, batch_size = batch.size) %>% .[,1]

Future_return_VBR <- pred_out_VBR_M1[108]*(max_VBR_M1-min_VBR_M1)+min_VBR_M1

########## VEA Prediction ##############################
# Capture min and max for denormalisation
min_VEA_M1 <- min(test_causality_VEA[[which.min(Entropy_VEA_1)]]$VEA)
max_VEA_M1 <- max(test_causality_VEA[[which.min(Entropy_VEA_1)]]$VEA)
y_VEA = range01(test_causality_VEA[[which.min(Entropy_VEA_1)]]$VEA)
sentiment_VEA = range01(Model_VEA_sentiment[[1]])
sentiment_VEA[is.na(sentiment_VEA)] <- 2
vol_VEA = range01(ETF_Data_NTUSD$VEA_Volume)

ts_VEA_M1 = data.frame(index = ETF_Data_NTUSD$Date, price = y_VEA, sentiment = sentiment_VEA, vol = vol_VEA)

datalags = 2
train_VEA_M1 = ts_VEA_M1[seq(252 + datalags), ]
test_VEA_M1 = ts_VEA_M1[252 + datalags + seq(108 + datalags), ]
batch.size = 6

Model_MAE_VEA_M1 <- list()
Model_RMSE_VEA_M1 <- list()
Model_MSE_VEA_M1 <- list()

x.train_VEA_M1 = array(data = lag(cbind(train_VEA_M1$sentiment), datalags )[-(1:datalags)], dim = c(nrow(train_VEA_M1) - datalags, datalags, 1))
y.train_VEA_M1 = array(data = train_VEA_M1$price[-(1:datalags)], dim = c(nrow(train_VEA_M1)-datalags, 1))

x.test_VEA_M1 = array(data = lag(cbind(test_VEA_M1$sentiment), datalags)[-(1:datalags)], dim = c(nrow(test_VEA_M1) - datalags, datalags, 1))
y.test_VEA_M1 = array(data = test_VEA_M1$price[-(1:datalags)], dim = c(nrow(test_VEA_M1) - datalags, 1))

model <- keras_model_sequential()

model %>%
  # Sigmoid is used as recurrent activation function and tanh as output layer
  layer_lstm(units = 100,
             input_shape = c(datalags, 1),
             batch_size = batch.size,
             return_sequences = TRUE,
             stateful = TRUE,
             unit_forget_bias = TRUE) %>%
  layer_dropout(rate = 0.5) %>% 
  layer_lstm(units = 50,
             batch_size = batch.size,
             return_sequences = FALSE,
             stateful = TRUE,
             unit_forget_bias = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)

model %>%
  compile(loss = 'mse', optimizer = 'adam', metrics = 'mae')

model


model %>% fit(x = x.train_VEA_M1,
              y = y.train_VEA_M1,
              batch_size = batch.size,
              epochs = 100,
              verbose = 1,
              shuffle = FALSE)

pred_out_VEA_M1 <- model %>% predict(x.test_VEA_M1, batch_size = batch.size) %>% .[,1]

Future_return_VEA <- pred_out_VEA_M1[108]*(max_VEA_M1-min_VEA_M1)+min_VEA_M1

########## VWO Prediction ##############################
# Capture min and max for denormalisation
min_VWO_M1 <- min(test_causality_VWO[[which.min(Entropy_VWO_1)]]$VWO)
max_VWO_M1 <- max(test_causality_VWO[[which.min(Entropy_VWO_1)]]$VWO)
y_VWO = range01(test_causality_VWO[[which.min(Entropy_VWO_1)]]$VWO)
sentiment_VWO = range01(Model_VWO_sentiment[[1]])
sentiment_VWO[is.na(sentiment_VWO)] <- 2
vol_VWO = range01(ETF_Data_NTUSD$VWO_Volume)

ts_VWO_M1 = data.frame(index = ETF_Data_NTUSD$Date, price = y_VWO, sentiment = sentiment_VWO, vol = vol_VWO)

datalags = 2
train_VWO_M1 = ts_VWO_M1[seq(252 + datalags), ]
test_VWO_M1 = ts_VWO_M1[252 + datalags + seq(108 + datalags), ]
batch.size = 6

Model_MAE_VWO_M1 <- list()
Model_RMSE_VWO_M1 <- list()
Model_MSE_VWO_M1 <- list()

x.train_VWO_M1 = array(data = lag(cbind(train_VWO_M1$sentiment), datalags )[-(1:datalags)], dim = c(nrow(train_VWO_M1) - datalags, datalags, 1))
y.train_VWO_M1 = array(data = train_VWO_M1$price[-(1:datalags)], dim = c(nrow(train_VWO_M1)-datalags, 1))

x.test_VWO_M1 = array(data = lag(cbind(test_VWO_M1$sentiment), datalags)[-(1:datalags)], dim = c(nrow(test_VWO_M1) - datalags, datalags, 1))
y.test_VWO_M1 = array(data = test_VWO_M1$price[-(1:datalags)], dim = c(nrow(test_VWO_M1) - datalags, 1))

model <- keras_model_sequential()

model %>%
  # Sigmoid is used as recurrent activation function and tanh as output layer
  layer_lstm(units = 100,
             input_shape = c(datalags, 1),
             batch_size = batch.size,
             return_sequences = TRUE,
             stateful = TRUE,
             unit_forget_bias = TRUE) %>%
  layer_dropout(rate = 0.5) %>% 
  layer_lstm(units = 50,
             batch_size = batch.size,
             return_sequences = FALSE,
             stateful = TRUE,
             unit_forget_bias = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)

model %>%
  compile(loss = 'mse', optimizer = 'adam', metrics = 'mae')

model


model %>% fit(x = x.train_VWO_M1,
              y = y.train_VWO_M1,
              batch_size = batch.size,
              epochs = 100,
              verbose = 1,
              shuffle = FALSE)

pred_out_VWO_M1 <- model %>% predict(x.test_VWO_M1, batch_size = batch.size) %>% .[,1]

Future_return_VWO <- pred_out_VWO_M1[108]*(max_VWO_M1-min_VWO_M1)+min_VWO_M1
########## SHV Prediction ##############################
# Capture min and max to inverse normalisation
min_SHV <- min(ETF_Data_NTUSD$SHV)
max_SHV <- max(ETF_Data_NTUSD$SHV)
# Return as y variable for VTI
y_SHV = range01(ETF_Data_NTUSD$SHV)
vol_SHV = range01(ETF_Data_NTUSD$SHV_Volume)
# Create a dataframe with the normalised sentiments
ts = data.frame(index = ETF_Data_NTUSD$Date, price = y_SHV, vol = vol_SHV )


datalags = 2
train = ts[seq(252 + datalags), ]
nrow(train)
test = ts[252 + datalags + seq(108 + datalags), ]
nrow(test)
batch.size = 6

x.train = array(data = lag(cbind(train$price, train$vol), datalags)[-(1:datalags), ], dim = c(nrow(train) - datalags, datalags, 2))
y.train = array(data = train$price[-(1:datalags)], dim = c(nrow(train)-datalags, 1))

x.test = array(data = lag(cbind(test$price, test$vol), datalags)[-(1:datalags), ], dim = c(nrow(test) - datalags, datalags, 2))
y.test = array(data = test$price[-(1:datalags)], dim = c(nrow(test) - datalags, 1))

model <- keras_model_sequential()

model %>%
  # Sigmoid is used as recurrent activation function and tanh as output layer
  layer_lstm(units = 100,
             input_shape = c(datalags, 2),
             batch_size = batch.size,
             return_sequences = TRUE,
             stateful = TRUE,
             unit_forget_bias = TRUE) %>%
  layer_dropout(rate = 0.5) %>% 
  layer_lstm(units = 50,
             batch_size = batch.size,
             return_sequences = FALSE,
             stateful = TRUE,
             unit_forget_bias = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)

model %>%
  compile(loss = 'mse', optimizer = 'adam', metrics = 'mae')

model

  model %>% fit(x = x.train,
                y = y.train,
                batch_size = batch.size,
                epochs = 100,
                verbose = 1,
                shuffle = FALSE)


pred_out <-  predict(model,x.test, batch_size = batch.size, verbose =1) %>% .[,1]
Future_return_SHV <- pred_out[108]*(max_SHV- min_SHV) + min_SHV

########## VTIP Prediction ####
# Capture min and max to inverse normalisation
min_VTIP <- min(ETF_Data_NTUSD$VTIP)
max_VTIP <- max(ETF_Data_NTUSD$VTIP)
# Return as y variable for VTI
y_VTIP = range01(ETF_Data_NTUSD$VTIP)
vol_VTIP = range01(ETF_Data_NTUSD$VTIP_Volume)
# Create a dataframe with the normalised sentiments
ts_VTIP = data.frame(index = ETF_Data_NTUSD$Date, price = y_VTIP, vol = vol_VTIP)

datalags = 2
train_VTIP = ts_VTIP[seq(252 + datalags), ]
test_VTIP = ts_VTIP[252 + datalags + seq(108 + datalags), ]
batch.size = 6

x.train_VTIP = array(data = lag(cbind(train_VTIP$price, train_VTIP$vol), datalags)[-(1:datalags), ], dim = c(nrow(train_VTIP) - datalags, datalags, 2))
y.train_VTIP = array(data = train_VTIP$price[-(1:datalags)], dim = c(nrow(train_VTIP)-datalags, 1))

x.test_VTIP = array(data = lag(cbind(test_VTIP$price, test_VTIP$vol), datalags)[-(1:datalags), ], dim = c(nrow(test_VTIP) - datalags, datalags, 2))
y.test_VTIP = array(data = test_VTIP$price[-(1:datalags)], dim = c(nrow(test_VTIP) - datalags, 1))

model <- keras_model_sequential()

model %>%
  # Sigmoid is used as recurrent activation function and tanh as output layer
  layer_lstm(units = 100,
             input_shape = c(datalags, 2),
             batch_size = batch.size,
             return_sequences = TRUE,
             stateful = TRUE,
             unit_forget_bias = TRUE) %>%
  layer_dropout(rate = 0.5) %>% 
  layer_lstm(units = 50,
             batch_size = batch.size,
             return_sequences = FALSE,
             stateful = TRUE,
             unit_forget_bias = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)

model %>%
  compile(loss = 'mse', optimizer = 'adam', metrics = 'mae')

model


  model %>% fit(x = x.train_VTIP,
                y = y.train_VTIP,
                batch_size = batch.size,
                epochs = 100,
                verbose = 1,
                shuffle = FALSE)


pred_out_VTIP <-  predict(model,x.test_VTIP, batch_size = batch.size, verbose =1) %>% .[,1]

plot(x = y.test_VTIP, y = pred_out_VTIP)

Future_return_VTIP <- pred_out_VTIP[108]*(max_VTIP-min_VTIP) + min_VTIP

########## MUB Prediction ####
# Capture min and max to inverse normalisation
min_MUB <- min(ETF_Data_NTUSD$MUB)
max_MUB <- max(ETF_Data_NTUSD$MUB)
# Return as y variable for VTI
y_MUB = range01(ETF_Data_NTUSD$MUB)
vol_MUB = range01(ETF_Data_NTUSD$MUB_Volume)
# Create a dataframe with the normalised sentiments
ts_MUB = data.frame(index = ETF_Data_NTUSD$Date, price = y_MUB, vol = vol_MUB )

datalags = 2
train_MUB = ts_MUB[seq(252 + datalags), ]
test_MUB = ts_MUB[252 + datalags + seq(108 + datalags), ]
batch.size = 6

x.train_MUB = array(data = lag(cbind(train_MUB$price,train_MUB$vol), datalags)[-(1:datalags), ], dim = c(nrow(train_MUB) - datalags, datalags, 2))
y.train_MUB = array(data = train_MUB$price[-(1:datalags)], dim = c(nrow(train_MUB)-datalags, 1))

x.test_MUB = array(data = lag(cbind(test_MUB$price,test_MUB$vol), datalags)[-(1:datalags), ], dim = c(nrow(test_MUB) - datalags, datalags, 2))
y.test_MUB = array(data = test_MUB$price[-(1:datalags)], dim = c(nrow(test_MUB) - datalags, 1))

model <- keras_model_sequential()

model %>%
  # Sigmoid is used as recurrent activation function and tanh as output layer
  layer_lstm(units = 100,
             input_shape = c(datalags, 2),
             batch_size = batch.size,
             return_sequences = TRUE,
             stateful = TRUE,
             unit_forget_bias = TRUE) %>%
  layer_dropout(rate = 0.5) %>% 
  layer_lstm(units = 50,
             batch_size = batch.size,
             return_sequences = FALSE,
             stateful = TRUE,
             unit_forget_bias = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)

model %>%
  compile(loss = 'mse', optimizer = 'adam', metrics = 'mae')

model

  model %>% fit(x = x.train_MUB,
                y = y.train_MUB,
                batch_size = batch.size,
                epochs = 100,
                verbose = 1,
                shuffle = FALSE)


pred_out_MUB <-  predict(model,x.test_MUB, batch_size = batch.size, verbose =1) %>% .[,1]

plot(x = y.test_MUB, y = pred_out_MUB)

Future_return_MUB <- pred_out_MUB[108]*(max_MUB-min_MUB) + min_MUB

########## AGG Prediction ####
# Capture min and max to inverse normalisation
min_AGG <- min(ETF_Data_NTUSD$AGG)
max_AGG <- max(ETF_Data_NTUSD$AGG)
# Return as y variable for VTI
y_AGG = range01(ETF_Data_NTUSD$AGG)
# Create a dataframe with the normalised sentiments
ts_AGG = data.frame(index = ETF_Data_NTUSD$Date, price = y_AGG, vol = range01(ETF_Data_NTUSD$AGG_Volume))

datalags = 2
train_AGG = ts_AGG[seq(252 + datalags), ]
test_AGG = ts_AGG[252 + datalags + seq(108 + datalags), ]
batch.size = 6

x.train_AGG = array(data = lag(cbind(train_AGG$price, train_AGG$vol), datalags)[-(1:datalags), ], dim = c(nrow(train_AGG) - datalags, datalags, 2))
y.train_AGG = array(data = train_AGG$price[-(1:datalags)], dim = c(nrow(train_AGG)-datalags, 1))

x.test_AGG = array(data = lag(cbind(test_AGG$price, test_AGG$vol), datalags)[-(1:datalags), ], dim = c(nrow(test_AGG) - datalags, datalags, 2))
y.test_AGG = array(data = test_AGG$price[-(1:datalags)], dim = c(nrow(test_AGG) - datalags, 1))

model <- keras_model_sequential()

model %>%
  # Sigmoid is used as recurrent activation function and tanh as output layer
  layer_lstm(units = 100,
             input_shape = c(datalags, 2),
             batch_size = batch.size,
             return_sequences = TRUE,
             stateful = TRUE,
             unit_forget_bias = TRUE) %>%
  layer_dropout(rate = 0.5) %>% 
  layer_lstm(units = 50,
             batch_size = batch.size,
             return_sequences = FALSE,
             stateful = TRUE,
             unit_forget_bias = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)

model %>%
  compile(loss = 'mae', optimizer = 'adam')

model


  model %>% fit(x = x.train_AGG,
                y = y.train_AGG,
                batch_size = batch.size,
                epochs = 100,
                verbose = 1,
                shuffle = FALSE)

pred_out_AGG <-  predict(model,x.test_AGG, batch_size = batch.size, verbose =1) %>% .[,1]
Future_return_AGG <- pred_out_AGG[108]*(max_AGG-min_AGG) + min_AGG

plot(x = y.test_AGG, y = pred_out_AGG)


########## BNDX Prediction ####
# Capture min and max to inverse normalisation
min_BNDX <- min(ETF_Data_NTUSD$BNDX)
max_BNDX <- max(ETF_Data_NTUSD$BNDX)
# Return as y variable for VTI
y_BNDX = range01(ETF_Data_NTUSD$BNDX)
# Create a dataframe with the normalised sentiments
ts_BNDX = data.frame(index = ETF_Data_NTUSD$Date, price = y_BNDX, vol = range01(ETF_Data_NTUSD$BNDX_Volume))

datalags = 2
train_BNDX = ts_BNDX[seq(252 + datalags), ]
test_BNDX = ts_BNDX[252 + datalags + seq(108 + datalags), ]
batch.size = 6

x.train_BNDX = array(data = lag(cbind(train_BNDX$price,train_BNDX$vol), datalags)[-(1:datalags), ], dim = c(nrow(train_BNDX) - datalags, datalags, 2))
y.train_BNDX = array(data = train_BNDX$price[-(1:datalags)], dim = c(nrow(train_BNDX)-datalags, 1))

x.test_BNDX = array(data = lag(cbind(test_BNDX$price,test_BNDX$vol ), datalags)[-(1:datalags), ], dim = c(nrow(test_BNDX) - datalags, datalags, 2))
y.test_BNDX = array(data = test_BNDX$price[-(1:datalags)], dim = c(nrow(test_BNDX) - datalags, 1))

model <- keras_model_sequential()

model %>%
  # Sigmoid is used as recurrent activation function and tanh as output layer
  layer_lstm(units = 100,
             input_shape = c(datalags, 2),
             batch_size = batch.size,
             return_sequences = TRUE,
             stateful = TRUE,
             unit_forget_bias = TRUE) %>%
  layer_dropout(rate = 0.5) %>% 
  layer_lstm(units = 50,
             batch_size = batch.size,
             return_sequences = FALSE,
             stateful = TRUE,
             unit_forget_bias = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)

model %>%
  compile(loss = 'mse', optimizer = 'adam', metrics = 'mae')

model


  model %>% fit(x = x.train_BNDX,
                y = y.train_BNDX,
                batch_size = batch.size,
                epochs = 100,
                verbose = 1,
                shuffle = FALSE)


pred_out_BNDX <-  predict(model,x.test_BNDX, batch_size = batch.size, verbose =1) %>% .[,1]
Future_return_BNDX <- pred_out_BNDX[108]*(max_BNDX-min_BNDX) + min_BNDX

plot(x = y.test_BNDX, y = pred_out_BNDX)


########## EMB Prediction ####
# Capture min and max to inverse normalisation
min_EMB <- min(ETF_Data_NTUSD$EMB)
max_EMB <- max(ETF_Data_NTUSD$EMB)
# Return as y variable for VTI
y_EMB = range01(ETF_Data_NTUSD$EMB)
# Create a dataframe with the normalised sentiments
ts_EMB = data.frame(index = ETF_Data_NTUSD$Date, price = y_EMB, vol = range01(ETF_Data_NTUSD$EMB_Volume))

datalags = 2
train_EMB = ts_EMB[seq(252 + datalags), ]
test_EMB = ts_EMB[252 + datalags + seq(108 + datalags), ]
batch.size = 6

x.train_EMB = array(data = lag(cbind(train_EMB$price, train_EMB$vol), datalags)[-(1:datalags), ], dim = c(nrow(train_EMB) - datalags, datalags, 2))
y.train_EMB = array(data = train_EMB$price[-(1:datalags)], dim = c(nrow(train_EMB)-datalags, 1))

x.test_EMB = array(data = lag(cbind(test_EMB$price, test_EMB$vol), datalags)[-(1:datalags), ], dim = c(nrow(test_EMB) - datalags, datalags, 2))
y.test_EMB = array(data = test_EMB$price[-(1:datalags)], dim = c(nrow(test_EMB) - datalags, 1))

model <- keras_model_sequential()

model %>%
  # Sigmoid is used as recurrent activation function and tanh as output layer
  layer_lstm(units = 100,
             input_shape = c(datalags, 2),
             batch_size = batch.size,
             return_sequences = TRUE,
             stateful = TRUE,
             unit_forget_bias = TRUE) %>%
  layer_dropout(rate = 0.5) %>% 
  layer_lstm(units = 50,
             batch_size = batch.size,
             return_sequences = FALSE,
             stateful = TRUE,
             unit_forget_bias = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)

model %>%
  compile(loss = 'mse', optimizer = 'adam', metrics = 'mae')

model


  model %>% fit(x = x.train_EMB,
                y = y.train_EMB,
                batch_size = batch.size,
                epochs = 100,
                verbose = 1,
                shuffle = FALSE)


pred_out_EMB <-  predict(model,x.test_EMB, batch_size = batch.size, verbose =1) %>% .[,1]
Future_return_EMB <- pred_out_EMB[108]*(max_EMB-min_EMB) + min_EMB

plot(x = y.test_EMB, y = pred_out_EMB)

MAE(pred_out_EMB,y.test_EMB)
RMSE(pred_out_EMB,y.test_EMB)
MSE(pred_out_EMB,y.test_EMB)


Future_Return <- c(Future_return_VTI,Future_return_VTV,Future_return_VOE,Future_return_VBR,Future_return_VEA,Future_return_VWO,
                           Future_return_SHV,Future_return_VTIP,Future_return_MUB,Future_return_AGG,Future_return_BNDX,Future_return_EMB)