# tidy the raw data

# load_library ------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(fasstr)

# import and checking -----------------------------------------------------

dayeuh_kolot <- read_csv("1_data/hymos_dayeuh_kolot.csv")
sapan <- read_csv("1_data/hymos_sapan.csv")
nanjung <- read_csv("1_data/hymos_nanjung.csv",
                    col_types = cols(value = col_character(), 
                                     station = col_character()))

## cek data hasil import
glimpse(dayeuh_kolot)
glimpse(sapan)
glimpse(nanjung)

# filter, tidy, fix data type ---------------------------------------------

## ambil kolom yang penting, filter terhadap data yang ada. rubah format.
dayeuh_kolot_filter <- dayeuh_kolot %>% select(date:station) %>% 
  filter(value != "-")
sapan_filter <- sapan %>% select(date:station) %>% 
  filter(value != "-")
nanjung_filter <- nanjung %>% select(date:station) %>% 
  filter(value != "-")

## convert kolom debit menjadi numeric
dayeuh_kolot_filter$value <- as.numeric(dayeuh_kolot_filter$value)
sapan_filter$value <- as.numeric(sapan_filter$value)
nanjung_filter$value <- as.numeric(nanjung_filter$value)
nanjung_filter$station <- as.character(nanjung_filter$station)

## convert kolom tanggal menjadi format apa adanya di excel
dayeuh_kolot_filter$date <- strptime(dayeuh_kolot_filter$date,format="%d-%b-%y")
sapan_filter$date <- strptime(sapan_filter$date,format="%d-%b-%y")
nanjung_filter$date <- strptime(nanjung_filter$date,format="%d-%b-%y")

## convert kolom tanggal menjadi format seperti yang diinginkan
dayeuh_kolot_filter$date <- as.Date(dayeuh_kolot_filter$date,"%Y-%m-%d")
sapan_filter$date <- as.Date(sapan_filter$date,"%Y-%m-%d")
nanjung_filter$date <- as.Date(nanjung_filter$date,"%Y-%m-%d")

## cek data hasil import
glimpse(dayeuh_kolot_filter)
glimpse(sapan_filter)
glimpse(nanjung_filter)

# plotting ----------------------------------------------------------------
ggplot(data=dayeuh_kolot_filter, aes(x=date, y=value))+geom_line()

ggplot() +
  geom_line(data=dayeuh_kolot_filter, aes(x=date, y=value),color='green')+
  geom_line(data=sapan_filter, aes(x=date, y=value),color='blue')+
  geom_line(data=nanjung_filter, aes(x=date, y=value),color='orange')

# analisa_debit -----------------------------------------------------------
## rename column to adjust the fasstr package
dy_kolot_rename <- dayeuh_kolot_filter %>% 
  rename(Date=date,
         Value=value,
         groups=station)

calc_longterm_daily_stats(data = dy_kolot_rename)

plot_flow_data(data=dy_kolot_rename)
plot_longterm_monthly_stats(data=dy_kolot_rename)
plot_monthly_stats(data = dy_kolot_rename)
screen_flow_data(data = dy_kolot_rename)
plot_data_screening(data = dy_kolot_rename)

## monthly calculation
calc_monthly_stats(data = dy_kolot_rename)
plot_monthly_stats(data = dy_kolot_rename)
plot_monthly_stats2(data = dy_kolot_rename)

## daily calculation
calc_daily_stats(data = dy_kolot_rename)
plot_daily_stats(data = dy_kolot_rename)

## long term statistic
plot_flow_duration(data = dy_kolot_rename)
calc_flow_percentile(data = dy_kolot_rename,
                     flow_value = 20, #untuk Q80
                     months = 1:6) # bulan 1 sampai 6.
