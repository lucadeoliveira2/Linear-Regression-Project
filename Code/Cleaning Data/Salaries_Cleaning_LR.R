# Loading Libraries #

install.packages('dplyr')
install.packages('tidyr')
install.packages('Amelia')
library(dplyr)
library(tidyr)
library(Amelia)

# Data #

salaries <- read.csv("Raw Data/nba_salaries.csv")
salaries <- salaries[,-1]
salaries$Salaries <- as.numeric(gsub("[$,]", "", salaries$Salaries)) # Changing format of salaries ($123,456,789 to numeric form)

missmap(salaries, col = c('yellow', 'black')) # We see that the problems come from shooting percentages
my.function <- function(column){
  column[is.na(column)] <- 0
  return(column)
}
salaries <- as.data.frame(sapply(salaries, my.function))

salaries$Pos[salaries$Pos == 'C-PF'] <- 'C'
salaries$Pos[salaries$Pos == 'PF-SF'] <- 'PF'
salaries$Pos[salaries$Pos == 'SF-PF'] <- 'SF'
salaries$Pos[salaries$Pos == 'SF-SG'] <- 'SF'

salaries <- salaries %>%
  mutate(Pos = factor(Pos, levels = (c('PG', 'SG', 'SF', 'PF', 'C')))) %>%
  mutate(across(c('Player', 'Tm'), as.factor))%>%
  mutate(across(where(is.character), as.numeric))%>%
  mutate(Player = as.character(Player))
str(salaries)


