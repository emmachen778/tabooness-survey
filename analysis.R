# Tabooness Survey
# Analysis
library(dplyr)

results <- read.csv("tabooness-survey.csv")


demo.titles <- c('Time', 'Age', 'English', 'OlderSib', 'YoungerSib', 'Edu', 'EduFather', 'EduMother', 
          'GenderBirth', 'GenderNow', 'Sexuality', 'Ethnicity')
words.titles <- c('Pos.Fuck', 'Neg.Fuck', 'Den.Fuck', 'Pos.Shit', 'Neg.Shit', 'Den.Shit', 
                  'Pos.Bitch', 'Neg.Bitch', 'Den.Bitch', 'Pos.Damn', 'Neg.Damn', 'Den.Damn',
                  'Pos.Hell', 'Neg.Hell', 'Den.Hell', 'Neg.Gay', 'Pos.Gay', 'Den.Gay',
                  'Pos.Pussy', 'Neg.Pussy', 'Den.Pussy')
colnames(results) <- c(demo.titles, words.titles)
results <- results[c(1:27, 29, 28, 30:33)]

words.only <- data.frame(t(data.frame(results[, 13:33])))
words.only$mean <- rowMeans(words.only)
words.avg <- words.only %>% select(mean)

GetMeansDF <- function(df, pattern) {
  df <-data.frame(t(data.frame(df[, grep(pattern, colnames(df))])))
  df$mean <- rowMeans(df)
  return (select(df, mean))
}


