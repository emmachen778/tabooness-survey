# Tabooness Survey
# Analysis
library(dplyr)
library(tidyr)

results <- read.csv("tabooness-survey.csv")


demo.titles <- c('Time', 'Age', 'English', 'OlderSib', 'YoungerSib', 'Edu', 'EduFather', 'EduMother', 
          'GenderBirth', 'GenderNow', 'Sexuality', 'Ethnicity')
words.titles <- c('Pos.Fuck', 'Neg.Fuck', 'Den.Fuck', 'Pos.Shit', 'Neg.Shit', 'Den.Shit', 
                  'Pos.Bitch', 'Neg.Bitch', 'Den.Bitch', 'Pos.Damn', 'Neg.Damn', 'Den.Damn',
                  'Pos.Hell', 'Neg.Hell', 'Den.Hell', 'Neg.Gay', 'Pos.Gay', 'Den.Gay',
                  'Pos.Pussy', 'Neg.Pussy', 'Den.Pussy')
words <- c('Fuck', 'Fuck', 'Fuck', 'Shit', 'Shit', 'Shit', 'Bitch', 'Bitch', 'Bitch', 'Damn', 'Damn', 'Damn',
           'Hell', 'Hell', 'Hell', 'Gay', 'Gay', 'Gay', 'Pussy', 'Pussy', 'Pussy', 'All')
contexts <- c('Positive', 'Negative', 'Denotative', 'Positive', 'Negative', 'Denotative', 'Positive', 'Negative', 'Denotative', 
              'Positive', 'Negative', 'Denotative', 'Positive', 'Negative', 'Denotative', 'Positive', 'Negative', 'Denotative',
              'Positive', 'Negative', 'Denotative', 'All')
colnames(results) <- c(demo.titles, words.titles)
results <- results[c(1:27, 29, 28, 30:33)]

words.only <- data.frame(t(data.frame(results[, 13:33])))
words.only$mean <- rowMeans(words.only)
words.avg <- words.only %>% select(mean) %>% mutate(Word = words[1:21], Context = contexts[1:21])
words.avg <- words.avg[c(2,3,1)]

target.word <- c('Bitch', 'Damn', 'Fuck', 'Gay', 'Hell', 'Pussy', 'Shit', 'All')
target.cont <- c('Positive', 'Negative', 'Denotative', 'All')
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
female.avg <- c(female.avg, mean(female.avg))
male.avg <- filter(results, GenderNow == 'Male') %>% GetAvgVector()
male.avg <- c(male.avg, mean(male.avg))

gender.avg.word <- data.frame(Word = words, Context = contexts, Male = male.avg, Female = female.avg) %>%
  group_by(Word) %>% summarise(Female = mean(Female), Male = mean(Male))
gender.avg.word <- gender.avg.word[match(target.word, gender.avg.word$Word),]

gender.avg.cont <- data.frame(Word = words, Context = contexts, Male = male.avg, Female = female.avg) %>%
  group_by(Context) %>% summarise(Female = mean(Female), Male = mean(Male))
gender.avg.cont <- gender.avg.cont[match(target.cont, gender.avg.cont$Context),]

################## Sexuality Data ##################

straight.avg <- filter(results, Sexuality == 'Straight (heterosexual)') %>% GetAvgVector()
straight.avg <- c(straight.avg, mean(straight.avg))
lgbt.avg <- filter(results, Sexuality != 'Straight (heterosexual)') %>% GetAvgVector()
lgbt.avg <- c(lgbt.avg, mean(lgbt.avg))

sexuality.avg.word <- data.frame(Word = words, Context = contexts, Straight = straight.avg, LGBTQIA = lgbt.avg) %>%
  group_by(Word) %>% summarise(Heterosexual = mean(Straight), LGBTQIA = mean(LGBTQIA))
sexuality.avg.word <- sexuality.avg.word[match(target.word, sexuality.avg.word$Word),]

sexuality.avg.cont <- data.frame(Word = words, Context = contexts, Straight = straight.avg, LGBTQIA = lgbt.avg) %>%
  group_by(Context) %>% summarise(Heterosexual = mean(Straight), LGBTQIA = mean(LGBTQIA))
sexuality.avg.cont <- sexuality.avg.cont[match(target.cont, sexuality.avg.cont$Context),]

gay.only <- data.frame(Word = words, Context = contexts, Straight = straight.avg, LGBTQIA = lgbt.avg) %>% 
  filter(Word == 'Gay') %>% select(Context, Straight, LGBTQIA)

################ Education Data #####################

assoc.avg <- filter(results, Edu == 'Completed associates degree') %>% GetAvgVector()
assoc.avg <- c(assoc.avg,mean(assoc.avg))
bach.avg <- filter(results, Edu == "Completed bachelor's degree") %>% GetAvgVector()
bach.avg <- c(bach.avg,mean(bach.avg))
hs.avg <- filter(results, Edu == 'Graduated high school') %>% GetAvgVector()
hs.avg <- c(hs.avg, mean(hs.avg))

edu.avg.word <- data.frame(Word = words, Context = contexts, Associates = assoc.avg, Bachelors = bach.avg, HS = hs.avg) %>%
  group_by(Word) %>% summarise(Bachelors = mean(Bachelors), Associates = mean(Associates), HS = mean(HS))
edu.avg.word <- edu.avg.word[match(target.word, edu.avg.word$Word),]

edu.avg.cont <- data.frame(Word = words, Context = contexts, Associates = assoc.avg, Bachelors = bach.avg, HS = hs.avg) %>%
  group_by(Context) %>% summarise(Bachelors = mean(Bachelors), Associates = mean(Associates), HS = mean(HS))
edu.avg.cont <- edu.avg.cont[match(target.cont, edu.avg.cont$Context),]

################# Primary Language #################

eng.avg <- filter(results, English == 'Yes') %>% GetAvgVector()
eng.avg <- c(eng.avg, mean(eng.avg))
not.eng.avg <- filter(results, English == 'No') %>% GetAvgVector()
not.eng.avg <- c(not.eng.avg, mean(not.eng.avg))

eng.avg.word <- data.frame(Word = words, Context = contexts, English = eng.avg, Not.English = not.eng.avg) %>%
  group_by(Word) %>% summarise(English = mean(English), Not.English = mean(Not.English))
eng.avg.word <- eng.avg.word[match(target.word, eng.avg.word$Word),]

eng.avg.cont <- data.frame(Word = words, Context = contexts, English = eng.avg, Not.English = not.eng.avg) %>%
  group_by(Context) %>% summarise(English = mean(English), Not.English = mean(Not.English))
eng.avg.cont <- eng.avg.cont[match(target.cont, eng.avg.cont$Context),]

################ Parent's Education ##################

ped.results <- filter(results, EduFather != 'I do not know' & EduMother != 'I do not know')

above.hs <- filter(ped.results, (EduFather != 'Graduated high school' &  EduFather != 'Did not complete high school') &
                     (EduMother != 'Graduated high school' & EduMother != 'Did not complete high school')) %>% GetAvgVector()
above.hs <- c(above.hs, mean(above.hs))
below.hs <- filter(ped.results, (EduFather == 'Graduated high school' |  EduFather == 'Did not complete high school') &
                     (EduMother == 'Graduated high school' | EduMother == 'Did not complete high school')) %>% GetAvgVector()
below.hs <- c(below.hs, mean(below.hs))

ped.avg.word <- data.frame(Word = words, Context = contexts, Above.HS = above.hs, Below.HS = below.hs) %>%
  group_by(Word) %>% summarise(Above.HS = mean(Above.HS), Below.HS = mean(Below.HS))
ped.avg.word <- ped.avg.word[match(target.word, ped.avg.word$Word),]

ped.avg.cont <- data.frame(Word = words, Context = contexts, Above.HS = above.hs, Below.HS = below.hs) %>%
  group_by(Context) %>% summarise(Above.HS = mean(Above.HS), Below.HS = mean(Below.HS))
ped.avg.cont <- ped.avg.cont[match(target.cont, ped.avg.cont$Context),]

############## Siblings ########################333

no.sibs <- filter(results, YoungerSib == 'None'& OlderSib == 'None') %>% GetAvgVector()
no.sibs <- c(no.sibs, mean(no.sibs))
only.young <- filter(results, YoungerSib != 'None' & OlderSib == 'None') %>% GetAvgVector()
only.young <- c(only.young, mean(only.young))
only.older <- filter(results, YoungerSib == 'None' & OlderSib != 'None') %>% GetAvgVector()
only.older <- c(only.older, mean(only.older))
both.sibs <- filter(results, YoungerSib != 'None' & OlderSib != 'None') %>% GetAvgVector()
both.sibs <- c(both.sibs, mean(both.sibs))

sib.avg.word <- data.frame(Word = words, Context = contexts, None = no.sibs, Young = only.young, Old = only.older, Both = both.sibs) %>%
  group_by(Word) %>% summarise(No.Siblings = mean(None), Only.Younger = mean(Young), Only.Older = mean(Old), Both = mean(Both))
sib.avg.word <- sib.avg.word[match(target.word, sib.avg.word$Word),]

sib.avg.cont <- data.frame(Word = words, Context = contexts, None = no.sibs, Young = only.young, Old = only.older, Both = both.sibs) %>%
  group_by(Context) %>% summarise(No.Siblings = mean(None), Only.Younger = mean(Young), Only.Older = mean(Old), Both = mean(Both))
sib.avg.cont <- sib.avg.cont[match(target.cont, sib.avg.cont$Context),]
