library(ggplot2)
library(dplyr)
library(themes)
library(gganimate)

text_vector <- VectorSource(my_texts)
text_corpus <- VCorpus(text_vector)
tdm <- TermDocumentMatrix(text_corpus)
Zipf_plot(tdm, main = "Zipf's law", ylab = "Log frenquency of words", xlab = "Log rank of words", type = 'l')

