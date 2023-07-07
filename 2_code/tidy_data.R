# tidy the raw data

# load_library ------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(fasstr)
library(scales)

# import and checking -----------------------------------------------------

dayeuh_kolot <- read_csv("1_data/hymos_dayeuh_kolot.csv")
sapan <- read_csv("1_data/hymos_sapan.csv")
nanjung <- read_csv("1_data/hymos_nanjung.csv",
                    col_types = cols(value = col_character(), 
                                     station = col_character()))
majalaya <- read_csv("1_data/hymos_majalaya.csv")

## cek data hasil import
glimpse(dayeuh_kolot)
glimpse(sapan)
glimpse(nanjung)
glimpse(majalaya)

# filter, tidy, fix data type ---------------------------------------------

## ambil kolom yang penting, filter terhadap data yang ada. rubah format.
dayeuh_kolot_filter <- dayeuh_kolot %>% select(date:station) %>% 
  filter(value != "-")
sapan_filter <- sapan %>% select(date:station) %>% 
  filter(value != "-")
nanjung_filter <- nanjung %>% select(date:station) %>% 
  filter(value != "-")
majalaya_filter <- majalaya %>% select(date:station) %>% 
  filter(value != "-")

## convert kolom debit menjadi numeric
dayeuh_kolot_filter$value <- as.numeric(dayeuh_kolot_filter$value)
sapan_filter$value <- as.numeric(sapan_filter$value)
nanjung_filter$value <- as.numeric(nanjung_filter$value)
nanjung_filter$station <- as.character(nanjung_filter$station)
majalaya_filter$value <- as.numeric(majalaya_filter$value)

## convert kolom tanggal menjadi format apa adanya di excel
dayeuh_kolot_filter$date <- strptime(dayeuh_kolot_filter$date,format="%d-%b-%y")
sapan_filter$date <- strptime(sapan_filter$date,format="%d-%b-%y")
nanjung_filter$date <- strptime(nanjung_filter$date,format="%d-%b-%y")
majalaya_filter$date <- strptime(majalaya_filter$date,format="%d-%b-%y")

## convert kolom tanggal menjadi format seperti yang diinginkan
dayeuh_kolot_filter$date <- as.Date(dayeuh_kolot_filter$date,"%Y-%m-%d")
sapan_filter$date <- as.Date(sapan_filter$date,"%Y-%m-%d")
nanjung_filter$date <- as.Date(nanjung_filter$date,"%Y-%m-%d")
majalaya_filter$date <- as.Date(majalaya_filter$date,"%Y-%m-%d")

## cek data hasil import
glimpse(dayeuh_kolot_filter)
glimpse(sapan_filter)
glimpse(nanjung_filter)
glimpse(majalaya_filter)

screen_flow_data(data = majalaya_rename)
plot_data_screening(data = majalaya_rename)

# plotting ----------------------------------------------------------------
ggplot(data=dayeuh_kolot_filter, aes(x=date, y=value))+geom_line()

plot_semua_pda <- ggplot() +
  geom_line(data=dayeuh_kolot_filter, aes(x=date, y=value,color='PDA Dayeuh Kolot'))+
  geom_line(data=sapan_filter, aes(x=date, y=value,color='PDA Sapan'))+
  geom_line(data=nanjung_filter, aes(x=date, y=value,color='PDA Nanjung'))+
  geom_line(data=majalaya_filter, aes(x=date, y=value,color='PDA Majalaya'))+
  ggtitle("Data Debit Harian PDA Majalaya, Sapan, Dayeuhkolot, dan Nanjung")+
  xlab(NULL)+ylab(quote(m^3/dtk))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name='Lokasi Pos Duga Air',
                     breaks=c('PDA Majalaya', 'PDA Sapan','PDA Dayeuh Kolot',
                              'PDA Nanjung'),
                     values=c('PDA Majalaya'='orange','PDA Sapan'='blue',
                              'PDA Dayeuh Kolot'='green',  
                              'PDA Nanjung'='purple'))+
  theme(legend.title=element_text(size=10),
        legend.text=element_text(size=8))+
  theme(legend.position = c(0.9,0.8))

plot_semua_pda + scale_x_date(date_labels = "%b %Y",
                              breaks = range(dayeuh_kolot_filter$date) )+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0))
  

# analisa_debit -----------------------------------------------------------
## rename column to adjust the fasstr package
dy_kolot_rename <- dayeuh_kolot_filter %>% 
  rename(Date=date,
         Value=value,
         groups=station)
sapan_rename <- sapan_filter %>% 
  rename(Date=date,
         Value=value,
         groups=station)
majalaya_rename <- majalaya_filter %>% 
  rename(Date=date,
         Value=value,
         groups=station)
nanjung_rename <- nanjung_filter %>% 
  rename(Date=date,
         Value=value,
         groups=station)


## longterm calculation
long_term_dy_kolot <- calc_longterm_daily_stats(data = dy_kolot_rename)
plot_longterm_monthly_stats(data=dy_kolot_rename)
write.csv(long_term_dy_kolot,"4_output/long_term_dayeuh_kolot.csv")

long_term_sapan <- calc_longterm_daily_stats(data = sapan_rename)
plot_longterm_monthly_stats(data=sapan_rename)
write.csv(long_term_sapan,"4_output/long_term_sapan.csv")

long_term_nanjung <- calc_longterm_daily_stats(data = nanjung_rename)
plot_longterm_monthly_stats(data=nanjung_rename)
write.csv(long_term_nanjung,"4_output/long_term_nanjung.csv")

long_term_majalaya <- calc_longterm_daily_stats(data = majalaya_rename,ignore_missing = T)
plot_longterm_monthly_stats(data=majalaya_rename,ignore_missing = T)
write.csv(long_term_majalaya,"4_output/long_term_majalaya.csv")


## monthly calculation
calc_monthly_stats(data = dy_kolot_rename)
plot_monthly_stats2(data = dy_kolot_rename)

calc_monthly_stats(data = sapan_rename)
plot_monthly_stats2(data = sapan_rename)

calc_monthly_stats(data = nanjung_rename)
plot_monthly_stats2(data = nanjung_rename)

calc_monthly_stats(data = majalaya_rename, allowed_missing = T)
plot_monthly_stats2(data = majalaya_rename, ignore_missing = T)


## daily calculation
calc_daily_stats(data = dy_kolot_rename)
plot_daily_stats(data = dy_kolot_rename, include_title = TRUE)
plot_daily_stats(data = dy_kolot_rename, roll_days = 7) #interval 7 hari.

## flow duration curve
plot_flow_duration(data = dy_kolot_rename)
calc_flow_percentile(data = dy_kolot_rename,
                     flow_value = 20, #untuk Q80
                     months = 1:12) # bulan 1 sampai 6.


