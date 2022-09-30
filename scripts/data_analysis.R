
#Data Analysis for Mario Kart Project
#Script by Emily Black (emily@zoology.ubc.ca)

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#clear R's brain
rm(list=ls()) 

#Part 0. Installing and opening necessary packages

#List of necessary packages 
pkgs <- c("tidyverse", "groundhog", "rPref", "plotly")
#Note: Groundhog is a useful package for R package version control

#Install packages
#Note: this line of code is commented out, as installing packages
#already downloaded can cause issues
#Please un-comment if packages are needed.
    #install.packages(pkgs)
rm(pkgs)

#Read packages into library
library(groundhog)
groundhog.library(tidyverse, "2022-09-14") #load version of tidyverse in use
                                            #when script was written
groundhog.library(rPref, "2022-09-14")
groundhog.library("plotly", "2022-09-14")

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 1. Reading in and cleaning data

#Read in the data downloaded into our Github folders
mario_kart_data <- read.csv("data/raw_data/mario_kart_character_stats.csv")
#(Original data retrieved from https://query.data.world/s/q4bodlodyhwkim5ader5prnl67gz5d)

#Get a preview of our data 
head(mario_kart_data)
summary(mario_kart_data)
#We see that we have information on our characters, our kart, tires, and glider
#as well as the stats scores for each build, and a cumulative score for each build


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 2. Finding the optimized build for each character 

#Another analysis uses the Pareto optimum to find the best kart build
#(https://www.youtube.com/watch?v=xo-Qt2mkjQs)
#This is when you find the combination of statistics in which gains in one
#statistic are not lost in another (e.g. higher speed doesn't mean 
#comprimising on lower accel.)

#However, I would rather first find the overall best build based on speed, 
#rather than try to balance all stats right off the bat. 
#Thus, I will begin by filtering to builds that maximize speed-related scores,
#and then calculating the pareto-optimal combination of speed-related scores


#What stats do we care most about?
#While traction and handling are important, a good driver
#should be able to make up for these in good steering
#So, the most important for winning: speed, acceleration, turbo (all speed related)
#However, acceleration is not as important as speed and turbo
#So, our first step will be calculating a score for each build 
#based on weighted average, with acceleration being 
#worth half as much as speed and turbo. 

#First, we average the speed, acceleration, and turbo scores for each character and build
mario_kart_data <- mario_kart_data %>%
  mutate(avg_performance_score = (0.4*speed)+(0.2*acceleration) +(0.4*turbo))

#Then, we can find the highest average score for each character 
mario_kart_summary <- mario_kart_data %>%
  group_by(characters) %>%
    filter(avg_performance_score == max(avg_performance_score))

#Now that we've narrowed our builds down to the fastest, 
#we can use pareto optimization to also make sure we have limited decreases
#in handling and traction. 
#I.e., there is the least amount of compromise in our kart. 
#We can use the rPrep package to find our pareto-optimal frontier for all stats
optimal_builds <- mario_kart_summary %>%
  group_by(characters) %>%
  psel(., high(avg_performance_score)*high(handling)*high(traction))
#Note: weight is co-dependent with the other statistics, so it has been removed


#Since we are only interested in builds, we should consolidate 
#our character, kart, tires, and glider info to get a single column, "build"
optimal_builds <- optimal_builds %>%
  unite(., build, c( kart, tires, glider), sep=",", remove=FALSE)

#What percent of our builds are part of our "optimal" group?
nrow(optimal_builds)/nrow(mario_kart_data) *100
#That's less than 1% of all possible builds! 

#To clean up our data, we can split the rows into each individual character 
#so it is easier to interpret
optimal_builds <- optimal_builds %>%
  mutate(characters= strsplit(characters, ",")) %>%
  unnest(., characters)


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 3. Plotting and Exporting Data

#Let's compare the optimal builds of each character
#Since there are a lot that are tied, I will randomly sample 3 builds from each character
#set, and jitter in the plot
set.seed(111) #make sure samples are the same every time
optimal_build_sample <- optimal_builds %>%
  group_by(characters)%>%
  slice_sample(n=3)

#arrange the dataset in alphabetical order for plotting
optimal_build_sample[order(optimal_build_sample[,'characters']), ]

#Plot the sample  dataset
mario_kart_plot <- optimal_build_sample %>%
  ggplot(aes(x=characters, y=avg_performance_score, )) + #specify vairables
  geom_point(position=position_jitter(w = 0, h = 0.005),  aes(colour=characters, 
                                 shape=build))+  #plot builds as points with labels
  labs(x="Character", 
       y=" Best Performance Score (after Pareto Optimization)")+ #label axes
  ggtitle("Sample of Optimal Builds by Character for Mario Kart 8 Deluxe")+ #label plot
  scale_shape_manual(values=c(1:30))+
  theme_bw()+ #start with predefined theme in ggplot
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), 
        legend.position="none", 
        axis.title = element_text(size=16, face="bold"), 
        title=element_text(size=16, face="bold")) #adjust text size and legends
ggplotly(mario_kart_plot, tooltip = c("x", "y", "shape")) #make plot interactive to see build

#save plotly plot as an interactive HTML
p <- ggplotly(mario_kart_plot, tooltip = c("x", "y", "shape"))
htmlwidgets::saveWidget(as_widget(p), "plots/optimal_build_plot.html")
htmlwidgets::saveWidget(as_widget(p), "output_files/optimal_build_plot.html")


#Export the list of optimal builds to CSV
write.csv(optimal_builds, "data/cleaned_data/optimal_builds.csv")
write.csv(optimal_builds, "output_files/optimal_builds.csv")