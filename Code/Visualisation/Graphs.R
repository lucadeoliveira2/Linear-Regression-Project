# Import Libraries #
install.packages('ggplot2')
install.packages('ggthemes')
install.packages('plotly')
install.packages('ggridges')
install.packages('ggrepel')
library(ggplot2)
library(ggthemes)
library(plotly)
library(ggridges)
library(ggrepel)

names(salaries)

# Plots #

sample_players <- sample(1:nrow(salaries)/5, 15)

# SALARY VS PPG
pl1 <- ggplot(salaries, aes(x = PTS, y = Salaries, col = Age)) + geom_point() + scale_color_gradientn(colours = c('salmon', 'turquoise'))
pl2 <- pl1 +geom_label_repel(aes(label = Player), data = salaries[sample_players,], col = 'black', size = 3) + theme_bw() + labs(x = 'Points per Game', title = 'Salaries Dependent on PPG') + theme(plot.title = element_text(hjust = 0.5)) 
pl2

# SALARIES BY POSITION
pl1 <- salaries %>%
  group_by(Pos) %>%
  summarise(pos.salary = median(Salaries)) %>%
  ggplot(aes(x = Pos, y = pos.salary, fill = Pos)) + geom_col(show.legend = F)
pl2 <- pl1 + labs(x = '', y = 'Median Yearly Salary ($)', title = 'Median Salary per Position in NBA') + scale_fill_manual(values = rainbow(5)) + theme_classic()
pl3 <- pl2 + annotate('text', label = 'Are PGs heavily overpaid?',x = 'PF', y = 6000000)
pl3

# PPG with AGE
pl1 <- salaries %>%
  ggplot(aes(x = Age, y = PTS)) + geom_point(position = 'jitter') + facet_wrap(.~Pos) + geom_smooth(col = 'red', se = F) + theme_economist_white()
pl2 <- pl1 + theme(axis.line.x = element_blank(), plot.title = element_text(hjust = 0.5, vjust = 2)) + labs(y = 'Points per Game', title = 'Does Position Affect PPG Growth with Age?')
pl2



