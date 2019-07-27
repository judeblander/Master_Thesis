#### Install and load packages ####
install.packages("tm")
install.packages("qdap")
install.packages("tidytext")
install.packages("lexicon")
install.packages("RWeka")
library(tm)
library(qdap)
library(magrittr)
library(tidyr)
library(tidytext)
library(dplyr)
library(lexicon)
library(ggplot2)
library(RWeka)
library(stringr)
library(sentimentr)
library(readr)
library(plyr)
library(stringr)
library(PerformanceAnalytics)
library(NlinTS)
library(tseries)

#### Importing .csv files ####
# set working directory
setwd("C:/Users/Julien/Desktop/Master Thesis")

# create a list from these files
list.files<-list.files(pattern=".csv$")

# create an empty list
myfiles<-list()

# create a loop to read data
for (i in 1:length(list.files))
{
  myfiles[[i]]<- read_delim(list.files[i], ";", escape_double = FALSE, trim_ws = TRUE)
}

# add the names of data to the list
names(myfiles)<-list.files


#### Cleaning files ####
# Selecting tweets that only express a sentiment
Feeling_text <- list()

for (i in 1:length(myfiles)) {
  Feeling_text[[i]] <- rbind.data.frame(myfiles[[i]] %>% filter(str_detect(as.character(text),"I'm") == TRUE),
                                        myfiles[[i]] %>% filter(str_detect(text,"I am") == TRUE),
                                        myfiles[[i]] %>% filter(str_detect(text,"makes me") == TRUE),
                                        myfiles[[i]] %>% filter(str_detect(text,"my guess") == TRUE),
                                        myfiles[[i]] %>% filter(str_detect(text,"I believe") == TRUE),
                                        myfiles[[i]] %>% filter(str_detect(text,"i dont feel") == TRUE),
                                        myfiles[[i]] %>% filter(str_detect(text,"i'm feeling") == TRUE),
                                        myfiles[[i]] %>% filter(str_detect(text,"i'm") == TRUE),
                                        myfiles[[i]] %>% filter(str_detect(text,"i am") == TRUE),
                                        myfiles[[i]] %>% filter(str_detect(text,"i dont") == TRUE),
                                        myfiles[[i]] %>% filter(str_detect(text,"i don't") == TRUE),
                                        myfiles[[i]] %>% filter(str_detect(text,"feel") == TRUE),
                                        myfiles[[i]] %>% filter(str_detect(text,"feeling") == TRUE),
                                        myfiles[[i]] %>% filter(str_detect(text,"I didn't") == TRUE),
                                        myfiles[[i]] %>% filter(str_detect(text,"i didnt") == TRUE),
                                        myfiles[[i]] %>% filter(str_detect(text,"I didnt") == TRUE),
                                        myfiles[[i]] %>% filter(str_detect(text,"i didn't") == TRUE)) 
}
names(Feeling_text)<-list.files

# Cleaning tweets function
clean_character <- function(x) {
# Replace @UserName
x <- gsub("@ \\w+", "", x)
# Replace cashtag
x <- gsub("\\$", "", x)
# Remove punctuation --> must not be taken into account as it is used for the sentiment function
# x <- gsub("[[:punct:]]", "", x)
# Remove pic link from twitter
x <- gsub("pic.twitter.*", "", x)
# Remove links http
x <- gsub("http.*", "", x)
# Remove links https
x <- gsub("https.*", "", x)
# Remove blank spaces at the beginning
x <- gsub("^ ", "", x)
# Remove blank spaces at the end
x <- gsub(" $", "", x)
#convert all text to lower case
x <- tolower(x)

}

# Apply the cleaning for all the csv files
my_texts <- NULL
for(i in 1:length(Feeling_text)){
  
    my_texts[i] <- as.data.frame(lapply(Feeling_text[[i]][2],clean_character))
    
}

#### Analysing data and sentiment dictionnary ####

NTUSD_dataframe %>% count(market_sentiment < 0)
NTUSD_dataframe %>% summarise( max(market_sentiment), min(market_sentiment))

# Return the number of tweets before cleaning
inbtweets <- c()
for (i in 1:length(Feeling_text)) {
  inbtweets[i] <- nrow(myfiles[[i]])
}
sum(inbtweets)

# Return the number of tweets after cleaning
fnbtweets <- c()
for (i in 1:length(Feeling_text)) {
  fnbtweets[i] <- length(my_texts[[i]])
}
sum(fnbtweets)

#### From NTUSD.JSON dictionnary ####

library(rjson)
NTUSD <- fromJSON(file = "NTUSD.json")
NTUSD_hashtag <- fromJSON(file = "NTUSD_hashtag.json")

# Combine two lists
NTUSD_list <- c(NTUSD,NTUSD_hashtag)

# Retrieve data
NTUSD_dataframe <- data.frame()
for (i in 1:length(NTUSD_list)) {
  NTUSD_dataframe[i,1] <- NTUSD_list[[i]][7]
  NTUSD_dataframe[i,2] <- NTUSD_list[[i]][8]
}

#### Sentiment NTUSD dictionnary ####
# Create a table for the sentiment 
senti_NTUSD <- NULL
for (i in 1:length(my_texts)) {
  senti_NTUSD[[i]] <- as.data.frame(list(matrix(NA, nrow = length(my_texts[[i]]), ncol = 2)))
  senti_NTUSD[[i]][,2] <- as.Date(Feeling_text[[i]]$date, tryFormats = "%d/%m/%Y")
}

# Put the polarity of the sentiment of each tweet for each element of list
for (i in 1:length(Feeling_text)) {
  for (j in 1:(nrow(senti_NTUSD[[i]]))) {
    senti_NTUSD[[i]][j,1] <- sentiment_by(as.character(my_texts[[i]][j]), polarity_dt = as_key(NTUSD_dataframe), valence_shifters_dt = hash_valence_shifters)$ave_sentiment
  }
}
names(senti_NTUSD)<-list.files

# Compute the score for each day NTUSD
score_day_NTUSD <- list()
for (i in 1:length(Feeling_text)) {
  score_day_NTUSD[[i]] <- data.frame(Date = unique(senti_NTUSD[[i]][,2]),
                               mean_score = tapply(senti_NTUSD[[i]][,1],senti_NTUSD[[i]][,2], FUN = mean))
  score_day_NTUSD[[i]][,1] <- row.names(score_day_NTUSD[[i]])
}
names(score_day_NTUSD)<-list.files

#### Regroup table by ETF ####
VTI_NTUSD <- full_join(score_day_NTUSD$Apple.csv,score_day_NTUSD$Microsoft.csv,by = c("Date"="Date"))
VTI_NTUSD <- full_join(VTI_NTUSD,score_day_NTUSD$Amazon.csv,by = c("Date"="Date"))
VTI_NTUSD <- full_join(VTI_NTUSD,score_day_NTUSD$Facebook.csv,by = c("Date"="Date"))
VTI_NTUSD <- full_join(VTI_NTUSD,score_day_NTUSD$Berkshire.csv,by = c("Date"="Date"))
VTI_NTUSD <- full_join(VTI_NTUSD,score_day_NTUSD$JPMorgan.csv,by = c("Date"="Date"))
VTI_NTUSD <- full_join(VTI_NTUSD,score_day_NTUSD$`Johnson&Johnson.csv`,by = c("Date"="Date"))
VTI_NTUSD <- full_join(VTI_NTUSD,score_day_NTUSD$ExxonMobil.csv,by = c("Date"="Date"))
VTI_NTUSD <- full_join(VTI_NTUSD,score_day_NTUSD$Google.csv,by = c("Date"="Date"))
VTI_NTUSD <- full_join(VTI_NTUSD,score_day_NTUSD$BankOfAmerica.csv,by = c("Date"="Date"))
VTI_NTUSD <- full_join(VTI_NTUSD,score_day_NTUSD$Wells_Fargo.csv,by = c("Date"="Date"))
VTI_NTUSD <- VTI_NTUSD[order(as.Date(VTI_NTUSD$Date, format = "%Y-%m-%d")),]
row.names(VTI_NTUSD) <- VTI_NTUSD$Date
VTI_NTUSD <- VTI_NTUSD[,-1]
colnames(VTI_NTUSD) <- c( "Apple","Microsoft","Amazon","Facebook","Johnson&Johnson","Berkshire Hathaway","JPMorgan","Exxon Mobil",
                    "Google","Bank of America", "Wells Fargo")

# VTV
VTV_NTUSD <- full_join(score_day_NTUSD$Microsoft.csv,score_day_NTUSD$Berkshire.csv, by = c("Date"="Date"))
VTV_NTUSD <- full_join(VTV_NTUSD,score_day_NTUSD$JPMorgan.csv, by = c("Date"="Date"))
VTV_NTUSD <- full_join(VTV_NTUSD,score_day_NTUSD$`Johnson&Johnson.csv`, by = c("Date"="Date"))
VTV_NTUSD <- full_join(VTV_NTUSD,score_day_NTUSD$ExxonMobil.csv, by = c("Date"="Date"))
VTV_NTUSD <- full_join(VTV_NTUSD,score_day_NTUSD$BankOfAmerica.csv, by = c("Date"="Date"))
VTV_NTUSD <- full_join(VTV_NTUSD,score_day_NTUSD$Wells_Fargo.csv, by = c("Date"="Date"))
VTV_NTUSD <- full_join(VTV_NTUSD,score_day_NTUSD$UnitedHealth.csv, by = c("Date"="Date"))
VTV_NTUSD <- full_join(VTV_NTUSD,score_day_NTUSD$Intel.csv, by = c("Date"="Date"))
VTV_NTUSD <- full_join(VTV_NTUSD,score_day_NTUSD$Pfizer.csv, by = c("Date"="Date"))
VTV_NTUSD <- VTV_NTUSD[order(as.Date(VTV_NTUSD$Date, format = "%Y-%m-%d")),]
row.names(VTV_NTUSD) <- VTV_NTUSD$Date
VTV_NTUSD <- VTV_NTUSD[,-1]
colnames(VTV_NTUSD) <- c("Microsoft","Berkshire Hathaway","JPMorgan","Johnson&Johnson","Exxon Mobil","Bank of America",
                   "Wells Fargo","United Health","Intel","Pfizer")

# VOE
VOE_NTUSD <- full_join(score_day_NTUSD$`FreePort-McMoran.csv`,score_day_NTUSD$WesternDigital.csv, by = c("Date" = "Date"))
VOE_NTUSD <- full_join(VOE_NTUSD,score_day_NTUSD$`M&TBank.csv`, by = c("Date" = "Date"))
VOE_NTUSD <- full_join(VOE_NTUSD,score_day_NTUSD$RoyalCarribean.csv, by = c("Date" = "Date"))
VOE_NTUSD <- full_join(VOE_NTUSD,score_day_NTUSD$Citizens.csv, by = c("Date" = "Date"))
VOE_NTUSD <- full_join(VOE_NTUSD,score_day_NTUSD$RegionsFinancial.csv, by = c("Date" = "Date"))
VOE_NTUSD <- full_join(VOE_NTUSD,score_day_NTUSD$Newmont_Goldcorp.csv, by = c("Date" = "Date"))
VOE_NTUSD <- full_join(VOE_NTUSD,score_day_NTUSD$WillisTower.csv, by = c("Date" = "Date"))
VOE_NTUSD <- full_join(VOE_NTUSD,score_day_NTUSD$WecEnergy.csv, by = c("Date" = "Date"))
VOE_NTUSD <- full_join(VOE_NTUSD,score_day_NTUSD$NetApp.csv, by = c("Date" = "Date"))
VOE_NTUSD <- full_join(VOE_NTUSD,score_day_NTUSD$DTEEnergy.csv, by = c("Date" = "Date"))
VOE_NTUSD <- full_join(VOE_NTUSD,score_day_NTUSD$Eversource.csv, by = c("Date" = "Date"))
VOE_NTUSD <- VOE_NTUSD[order(as.Date(VOE_NTUSD$Date, format = "%Y-%m-%d")),]
row.names(VOE_NTUSD) <- VOE_NTUSD$Date
VOE_NTUSD <- VOE_NTUSD[,-1]
colnames(VOE_NTUSD) <- c("FreePort-McMoran","Western Digital","M&T Bank","Royal Carribean","Citizens","Regions Financial",
                         "Newmont Goldcorp","Willis Tower","Wec Energy","NetApp","Dte Energy","Eversource")

# VBR
VBR_NTUSD <- full_join(score_day_NTUSD$SpiritAero.csv,score_day_NTUSD$IDEX.csv, by = c("Date" = "Date"))
VBR_NTUSD <- full_join(VBR_NTUSD,score_day_NTUSD$ONSemiconductor.csv, by = c("Date" = "Date"))
VBR_NTUSD <- full_join(VBR_NTUSD,score_day_NTUSD$Leidos.csv, by = c("Date" = "Date"))
VBR_NTUSD <- full_join(VBR_NTUSD,score_day_NTUSD$AtmosEnergy.csv, by = c("Date" = "Date"))
VBR_NTUSD <- full_join(VBR_NTUSD,score_day_NTUSD$EastWestBancorp.csv, by = c("Date" = "Date"))
VBR_NTUSD <- full_join(VBR_NTUSD,score_day_NTUSD$NRGEnergy.csv, by = c("Date" = "Date"))
VBR_NTUSD <- full_join(VBR_NTUSD,score_day_NTUSD$Wellcare.csv, by = c("Date" = "Date"))
VBR_NTUSD <- full_join(VBR_NTUSD,score_day_NTUSD$UGICorp.csv, by = c("Date" = "Date"))
VBR_NTUSD <- VBR_NTUSD[order(as.Date(VBR_NTUSD$Date, format = "%Y-%m-%d")),]
row.names(VBR_NTUSD) <- VBR_NTUSD$Date
VBR_NTUSD <- VBR_NTUSD[,-1]
colnames(VBR_NTUSD) <- c("Spirit Aerosystems","IDEX","On Semiconductor","Leidos","Atmos Energy","East West Bancorp",
                         "NRG Energy","Wellcare Health","UGI Corp")

# VEA
VEA_NTUSD <- full_join(score_day_NTUSD$Nestle.csv,score_day_NTUSD$HSBC.csv, by = c("Date" = "Date"))
VEA_NTUSD <- full_join(VEA_NTUSD,score_day_NTUSD$Novartis.csv, by = c("Date" = "Date"))
VEA_NTUSD <- full_join(VEA_NTUSD,score_day_NTUSD$Samsung.csv, by = c("Date" = "Date"))
VEA_NTUSD <- full_join(VEA_NTUSD,score_day_NTUSD$Toyota.csv, by = c("Date" = "Date"))
VEA_NTUSD <- full_join(VEA_NTUSD,score_day_NTUSD$Roche.csv, by = c("Date" = "Date"))
VEA_NTUSD <- full_join(VEA_NTUSD,score_day_NTUSD$Shell.csv, by = c("Date" = "Date"))
VEA_NTUSD <- full_join(VEA_NTUSD,score_day_NTUSD$BritishAmericanTabacco.csv, by = c("Date" = "Date"))
VEA_NTUSD <- full_join(VEA_NTUSD,score_day_NTUSD$BP.csv, by = c("Date" = "Date"))
VEA_NTUSD <- full_join(VEA_NTUSD,score_day_NTUSD$Total.csv, by = c("Date" = "Date"))
VEA_NTUSD <- VEA_NTUSD[order(as.Date(VEA_NTUSD$Date, format = "%Y-%m-%d")),]
row.names(VEA_NTUSD) <- VEA_NTUSD$Date
VEA_NTUSD <- VEA_NTUSD[,-1]
colnames(VEA_NTUSD) <- c("Nestle","HSBC Holdings","Novartis","Samsung","Toyota","Roche","Shell","British American","BP",
                         "Total")

# VWO
VWO_NTUSD <- full_join(score_day_NTUSD$Tencent.csv,score_day_NTUSD$Napsers.csv, by = c("Date"="Date"))
VWO_NTUSD <- full_join(VWO_NTUSD,score_day_NTUSD$Alibaba.csv, by = c("Date"="Date"))
VWO_NTUSD <- full_join(VWO_NTUSD,score_day_NTUSD$ChinaMobile.csv, by = c("Date"="Date"))
VWO_NTUSD <- full_join(VWO_NTUSD,score_day_NTUSD$Sberbank.csv, by = c("Date"="Date"))
VWO_NTUSD <- full_join(VWO_NTUSD,score_day_NTUSD$Baidu.csv, by = c("Date"="Date"))
VWO_NTUSD <- full_join(VWO_NTUSD,score_day_NTUSD$Reliance.csv, by = c("Date"="Date"))
VWO_NTUSD <- VWO_NTUSD[order(as.Date(VWO_NTUSD$Date, format = "%Y-%m-%d")),]
row.names(VWO_NTUSD) <- VWO_NTUSD$Date
VWO_NTUSD <- VWO_NTUSD[,-1]
colnames(VWO_NTUSD) <- c("Tencent Holdings","Naspers","Alibaba","China Mobile","Sberbank","Baidu","Reliance")

#### Create one big table with sentiment for each ETF #####
Sentiment_NTUSD <- data.frame()
Sentiment_NTUSD <- full_join(as.data.frame(VTI_sentiment_w_NTUSD),as.data.frame(VTV_sentiment_w_NTUSD),by = c("Date"="Date"))
Sentiment_NTUSD <- full_join(Sentiment_NTUSD,as.data.frame(VOE_sentiment_w_NTUSD),by = c("Date"="Date"))
Sentiment_NTUSD <- full_join(Sentiment_NTUSD,as.data.frame(VBR_sentiment_w_NTUSD),by = c("Date"="Date"))
Sentiment_NTUSD <- full_join(Sentiment_NTUSD,as.data.frame(VEA_sentiment_w_NTUSD),by = c("Date"="Date"))
Sentiment_NTUSD <- full_join(Sentiment_NTUSD,as.data.frame(VWO_sentiment_w_NTUSD),by = c("Date"="Date"))
Sentiment_NTUSD <- cbind.data.frame(Sentiment_NTUSD$Date,Sentiment_NTUSD$`Sentiment VTI`,Sentiment_NTUSD$`Sentiment VTV`,
                                    Sentiment_NTUSD$`Sentiment VOE`,Sentiment_NTUSD$`Sentiment VBR`,
                                    Sentiment_NTUSD$`Sentiment VEA`,Sentiment_NTUSD$`Sentiment VWO`)
colnames(Sentiment_NTUSD) <- c("Date", "Sentiment VTI NTUSD", "Sentiment VTV NTUSD", "Sentiment VOE NTUSD",
                               "Sentiment VBR NTUSD", "Sentiment VEA NTUSD","Sentiment VWO NTUSD")


#### Add into ETF_Data_NTUSD table ####
Returns_ETF[,1] <- as.Date(Returns_ETF$Date)
Volume_ETF[,1] <- as.Date(Volume_ETF$Date)
ETF_Data_NTUSD <- left_join(Returns_ETF,as.data.frame(Sentiment_NTUSD),by = c("Date"="Date"))
ETF_Data_NTUSD <- left_join(ETF_Data_NTUSD,Volume_ETF,by = c("Date"="Date"))
