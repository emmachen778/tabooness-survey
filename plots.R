# plots
library(plotly)

source('analysis.R')

gender.word.plot <- plot_ly(data = gender.avg.word, type = "scatter", mode = "lines+markers", x = ~Word, y = ~Male, name = 'Male') %>%
  add_trace(y = ~Female, name = 'Female', mode = 'lines+markers') %>%
  layout(xaxis = list(title = "Word"), 
         yaxis = list(title = "Tabooness"),
         title = 'Mean Tabooness of Words by Gender')

sex.word.plot <- plot_ly(data = sexuality.avg.word, type = "scatter", mode = "lines+markers", x = ~Word, y = ~Heterosexual, name = 'Heterosexual') %>%
  add_trace(y = ~LGBTQIA, name = 'LGBTQIA+', mode = 'lines+markers') %>%
  layout(xaxis = list(title = "Word"), 
         yaxis = list(title = "Tabooness"),
         title = 'Mean Tabooness of Words by Sexuality')

edu.word.plot <-plot_ly(data = edu.avg.word, type = "scatter", mode = "lines+markers", x = ~Word, y = ~Bachelors, name = "Completed Bachelor's degree") %>%
  add_trace(y = ~Associates, name = "Completed associates degree", mode = 'lines+markers') %>%
  add_trace(y = ~HS, name = 'Completed high school', mode = 'lines+markers') %>%
  layout(xaxis = list(title = "Word"), 
         yaxis = list(title = "Tabooness"),
         title = 'Mean Tabooness of Words by Education Level')

eng.word.plot <-plot_ly(data = eng.avg.word, type = "scatter", mode = "lines+markers", x = ~Word, y = ~English, name = "Is Primary Language") %>%
  add_trace(y = ~Not.English, name = "Is Not Primary Language", mode = 'lines+markers') %>%
  layout(xaxis = list(title = "Word"), 
         yaxis = list(title = "Tabooness"),
         title = 'Mean Tabooness of Words by Primary English (English or not)')
