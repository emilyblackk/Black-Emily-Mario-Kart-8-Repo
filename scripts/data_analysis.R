#Data Analysis for Mario Kart Project
#Emily Black 

library(groundhog)
groundhog.library(tidyverse, "2022-09-14")


#Read in the data
mario_kart_data <- read.csv("data/raw_data/mario_kart_character_stats.csv")
head(mario_kart_data)
summary(mario_kart_data)

#what stats do we care most about?\
#Most important for winning: speed, acceleration, turbo (all speed related)
mario_kart_data <- mario_kart_data %>%
  mutate(avg_performance_score = rowMeans(select(., speed,acceleration,turbo)))

#Mario kart best karts
mario_kart_summary <- mario_kart_data %>%
  group_by(characters) %>%
    filter(avg_performance_score == max(avg_performance_score))

#Note: plotting is a work in progress!
#Plot the data
mario_kart_plot <- mario_kart_summary %>%
  ggplot(aes(x=characters, y=avg_performance_score)) +
  geom_point(aes(colour=kart, shape=glider) )+ 
  geom_jitter()+
  theme_classic()
mario_kart_plot