# tidy the raw data

library(tidyverse)
library(lubridate)

# import data
dayeuh_kolot <- read_csv("1_data/hymos_dayeuh_kolot.csv")

# cek data hasil import
glimpse(dayeuh_kolot)

#ambil kolom yang penting, filter terhadap data yang ada. rubah format.
dayeuh_kolot_filter <- dayeuh_kolot %>% select(date:station) %>% 
  filter(value != "-")

dayeuh_kolot_filter$value <- as.numeric(dayeuh_kolot_filter$value)
dayeuh_kolot_filter$date <- strptime(dayeuh_kolot_filter$date,format="%d-%b-%y")
dayeuh_kolot_filter$date <- as.Date(dayeuh_kolot_filter$date,"%Y-%m-%d")

# cek data hasil import
glimpse(dayeuh_kolot_filter)

# cek plot
ggplot(data=dayeuh_kolot_filter, aes(x=date, y=value))+geom_line()
