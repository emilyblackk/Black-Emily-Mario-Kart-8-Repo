#load packages
library(groundhog)
groundhog.library(tidyverse, "2022-09-14")
#library(rdryad) ##if downloading data from dryad

dir.create("data/")
dir.create("data/raw_data")
dir.create("data/cleaned_data")
dir.create("scripts/")

dir.create("data/url")
dir.create("data/dryad")
dir.create("data/github")


###Download via URL
#Downloaded on Sept. 14th 2022
data_url <- "https://query.data.world/s/q4bodlodyhwkim5ader5prnl67gz5d"
dest_file <- "data/url/mario_kart_character_stats.csv"

#No metadata associated with this file 
download.file(url=data_url, destfile=dest_file)
#Add to the raw data folder
download.file(url=data_url, destfile="data/raw_data/mario_kart_character_stats.csv")

mario_kart_stats <- read.csv("data/url/mario_kart_character_stats.csv")

