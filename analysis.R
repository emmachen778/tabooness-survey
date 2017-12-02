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
words <- c('Fuck', 'Fuck', 'Fuck', 'Shit', 'Shit', 'Shit', 'Bitch', 'Bitch', 'Bitch', 'Damn', 'Damn', 'Damn',
           'Hell', 'Hell', 'Hell', 'Gay', 'Gay', 'Gay', 'Pussy', 'Pussy', 'Pussy')
contexts <- c('Positive', 'Negative', 'Denotative', 'Positive', 'Negative', 'Denotative', 'Positive', 'Negative', 'Denotative', 
              'Positive', 'Negative', 'Denotative', 'Positive', 'Negative', 'Denotative', 'Positive', 'Negative', 'Denotative',
              'Positive', 'Negative', 'Denotative')
colnames(results) <- c(demo.titles, words.titles)
results <- results[c(1:27, 29, 28, 30:33)]

words.only <- data.frame(t(data.frame(results[, 13:33])))
words.only$mean <- rowMeans(words.only)
words.avg <- words.only %>% select(mean) %>% mutate(Word = words, Context = contexts)
words.avg <- words.avg[c(2,3,1)]

################ Functions ############################

GetAvgVector <- function(df) {
  x.only <- data.frame(t(data.frame(df[, 13:33])))
  x.only$mean <- rowMeans(x.only)
  x.avg <- x.only %>% select(mean)
  return(x.avg$mean)
}

GetMeansDF <- function(df, pattern) {
  df <-data.frame(t(data.frame(df[, grep(pattern, colnames(df))])))
  df$mean <- rowMeans(df)
  return (select(df, mean))
}

################### Gender Data ####################

female.avg <- filter(results, GenderNow == 'Female') %>% GetAvgVector()
male.avg <- filter(results, GenderNow == 'Male') %>% GetAvgVector()

gender.avg.word <- data.frame(Word = words, Context = contexts, Male = male.avg, Female = female.avg, All = words.avg[,3]) %>%
  group_by(Word) %>% summarise(Female = mean(Female), Male = mean(Male), All = mean(All))

gender.avg.cont <- data.frame(Word = words, Context = contexts, Male = male.avg, Female = female.avg, All = words.avg[,3]) %>%
  group_by(Context) %>% summarise(Female = mean(Female), Male = mean(Male), All = mean(All))

################## Sexuality Data ##################

straight.avg <- filter(results, Sexuality == 'Straight (heterosexual)') %>% GetAvgVector()
lgbt.avg <- filter(results, Sexuality != 'Straight (heterosexual)') %>% GetAvgVector()

sexuality.avg.word <- data.frame(Word = words, Context = contexts, Straight = straight.avg, LGBTQIA = lgbt.avg, All = words.avg[,3]) %>%
  group_by(Word) %>% summarise(Heterosexual = mean(Straight), LGBTQIA = mean(LGBTQIA), All = mean(All))

sexuality.avg.cont <- data.frame(Word = words, Context = contexts, Straight = straight.avg, LGBTQIA = lgbt.avg, All = words.avg[,3]) %>%
  group_by(Context) %>% summarise(Heterosexual = mean(Straight), LGBTQIA = mean(LGBTQIA), All = mean(All))

################ Education Data #####################

assoc.avg <- filter(results, Edu == 'Completed associates degree') %>% GetAvgVector()
bach.avg <- filter(results, Edu == "Completed bachelor's degree") %>% GetAvgVector()
hs.avg <- filter(results, Edu == 'Graduated high school') %>% GetAvgVector()

edu.avg.word <- data.frame(Word = words, Context = contexts, Associates = assoc.avg, Bachelors = bach.avg, HS = hs.avg) %>%
  group_by(Word) %>% summarise(Bachelors = mean(Bachelors), Associates = mean(Associates), HS = mean(HS))

edu.avg.cont <- data.frame(Word = words, Context = contexts, Associates = assoc.avg, Bachelors = bach.avg, HS = hs.avg) %>%
  group_by(Context) %>% summarise(Bachelors = mean(Bachelors), Associates = mean(Associates), HS = mean(HS))

################# Primary Language #################

eng.avg <- filter(results, English == 'Yes') %>% GetAvgVector()
not.eng.avg <- filter(results, English == 'No') %>% GetAvgVector()

eng.avg.word <- data.frame(Word = words, Context = contexts, English = eng.avg, Not.English = not.eng.avg) %>%
  group_by(Word) %>% summarise(English = mean(English), Not.English = mean(Not.English))

eng.avg.cont <- data.frame(Word = words, Context = contexts, English = eng.avg, Not.English = not.eng.avg) %>%
  group_by(Context) %>% summarise(English = mean(English), Not.English = mean(Not.English))

