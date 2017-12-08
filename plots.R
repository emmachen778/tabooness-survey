# plots
library(plotly)

source('analysis.R')

gender.word.plot <- plot_ly(data = gender.avg.word[1:7,], type = "scatter", mode = "lines+markers", x = ~Word, y = ~Male, name = 'Male (23)') %>%
  add_trace(y = ~Female, name = 'Female (65)', mode = 'lines+markers') %>%
  layout(xaxis = list(title = "Word"), 
         yaxis = list(title = "Tabooness"),
         title = 'Mean Tabooness of Words by Gender')

sex.word.plot <- plot_ly(data = sexuality.avg.word[1:7,], type = "scatter", mode = "lines+markers", x = ~Word, y = ~Heterosexual, name = 'Heterosexual (75)') %>%
  add_trace(y = ~LGBTQIA, name = 'LGBTQIA+ (15)', mode = 'lines+markers') %>%
  layout(xaxis = list(title = "Word"), 
         yaxis = list(title = "Tabooness"),
         title = 'Mean Tabooness of Words by Sexuality')

edu.word.plot <-plot_ly(data = edu.avg.word[1:7,], type = "scatter", mode = "lines+markers", x = ~Word, y = ~Bachelors, name = "Completed Bachelor's degree (8)") %>%
  add_trace(y = ~Associates, name = "Completed associates degree (6)", mode = 'lines+markers') %>%
  add_trace(y = ~HS, name = 'Completed high school (76)', mode = 'lines+markers') %>%
  layout(xaxis = list(title = "Word"), 
         yaxis = list(title = "Tabooness"),
         title = 'Mean Tabooness of Words by Education Level')

eng.word.plot <-plot_ly(data = eng.avg.word[1:7,], type = "scatter", mode = "lines+markers", x = ~Word, y = ~English, name = "Is Primary Language (87)") %>%
  add_trace(y = ~Not.English, name = "Is Not Primary Language (3)", mode = 'lines+markers') %>%
  layout(xaxis = list(title = "Word"), 
         yaxis = list(title = "Tabooness"),
         title = 'Mean Tabooness of Words by Primary English (English or not)')

ped.word.plot <- plot_ly(data = ped.avg.word[1:7,], type = "scatter", mode = "lines+markers", x = ~Word, y = ~Above.HS, name = "Both Parents' Edu. Level > HS (65)") %>%
  add_trace(y = ~Below.HS, name = "Both Parents' Edu. Level <= HS (11)", mode = 'lines+markers') %>%
  layout(xaxis = list(title = "Word"), 
         yaxis = list(title = "Tabooness"),
         title = "Mean Tabooness of Words by Parent's Education (More than High School or Not)")

sib.word.plot <- plot_ly(data = sib.avg.word[1:7,], type = "scatter", mode = "lines+markers", x = ~Word, y = ~No.Siblings, name = "No Siblings") %>%
  add_trace(y = ~Only.Younger, name = "Only Younger Siblings", mode = 'lines+markers') %>%
  add_trace(y = ~Only.Older, name = "Only Older Siblings", mode = 'lines+markers') %>%
  add_trace(y = ~Both, name = "Both Older and Younger Siblings", mode = 'lines+markers') %>%
  layout(xaxis = list(title = "Word"), 
         yaxis = list(title = "Tabooness"),
         title = "Mean Tabooness of Words by Siblings")
