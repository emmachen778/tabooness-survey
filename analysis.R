# Tabooness Survey
# Analysis
library(dplyr)

results <- read.csv("tabooness-survey.csv")

demo.titles <- c('Time', 'Age', 'English', 'OlderSib', 'YoungerSib', 'Edu', 'EduFather', 'EduMother', 
          'GenderBirth', 'GenderNow', 'Sexuality', 'Ethnicity')
words.titles <- c('Pos.Fuck', 'Neg.Fuck', 'Den.Fuck', 'Pos.Shit', 'Neg.Shit', 'Den.Shit', 
                  'Pos.Bitch', 'Neg.Bitch', 'Den.Bitch', 'Pos.Damn', 'Neg.Damn', 'Den.Damn',
                  'Pos.Hell', 'Neg.Hell', 'Den.Hell', 'Pos.Gay', 'Neg.Gay', 'Den.Gay',
                  'Pos.Pussy', 'Neg.Pussy', 'Den.Pussy')
names <- c(demo.titles, words.titles)
colnames(results) <- names

words.only <- data.frame(t(data.frame(results[, 13:33])))
words.only$means <- rowMeans(words.only)
words.avg <- words.only %>% select(means)

GetMeansDF <- function(df, pattern) {
  df <-data.frame(t(data.frame(df[, grep(pattern, colnames(df))])))
  df$means <- rowMeans(df)
  return (select(df, means))
}

pos.only <- GetMeansDF(results,'Pos')
neg.only <- GetMeansDF(results, 'Neg')
den.only <- GetMeansDF(results, 'Den')

pos.mean <- mean(pos.only$means)
neg.mean <- mean(neg.only$means)
den.mean <- mean(den.only$means)

