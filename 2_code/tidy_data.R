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

write.csv(nanjung_filter,"4_output/nanjung_filter.csv")

screen_flow_data(data = majalaya_rename)
plot_data_screening(data = majalaya_rename)
plot_missing_dates(data = majalaya_rename)
mjly_fill <- fill_missing_dates(data = majalaya_rename)
plot_missing_dates(data = mjly_fill)

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

long_term_majalaya <- calc_longterm_daily_stats(data = mjly_fill,ignore_missing = T)
plot_longterm_monthly_stats(data=mjly_fill,ignore_missing = T)
write.csv(long_term_majalaya,"4_output/long_term_majalaya.csv")


## monthly calculation
calc_monthly_stats(data = dy_kolot_rename)
plot_monthly_stats2(data = dy_kolot_rename)

calc_monthly_stats(data = sapan_rename)
plot_monthly_stats2(data = sapan_rename)

calc_monthly_stats(data = nanjung_rename)
plot_monthly_stats2(data = nanjung_rename)

calc_monthly_stats(data = mjly_fill,ignore_missing = T)
plot_monthly_stats2(data = mjly_fill, ignore_missing = T)


## daily calculation
calc_daily_stats(data = dy_kolot_rename)
plot_daily_stats(data = dy_kolot_rename, include_title = TRUE)
plot_daily_stats(data = dy_kolot_rename, roll_days = 7) #interval 7 hari.

plot_daily_stats(data = majalaya_rename, ignore_missing = T, include_title = TRUE)

plot_daily_stats(data = sapan_rename, include_title = TRUE)

plot_daily_stats(data = nanjung_rename, include_title = TRUE)

## flow duration curve
plot_flow_duration(data = dy_kolot_rename)
calc_flow_percentile(data = dy_kolot_rename,
                     flow_value = 5, 
                     months = 1:12) 

plot_flow_duration(data = majalaya_rename,ignore_missing = T)
calc_flow_percentile(data = majalaya_rename,
                     flow_value = 5, #untuk debit "X",brp percentilenya
                     months = 1:12) 

plot_flow_duration(data = sapan_rename)
calc_flow_percentile(data = sapan_rename,
                     flow_value = 5)

plot_flow_duration(data = nanjung_rename)
calc_flow_percentile(data = nanjung_rename,
                     flow_value = 5,
                     months = 1:12) 


Q_majalaya <- calc_longterm_percentile(data = majalaya_rename,
                         percentiles = c(5,10,20,30))

Q_sapan <- calc_longterm_percentile(data = sapan_rename,
                         percentiles = c(5,10,20,30))

Q_dy_kolot <- calc_longterm_percentile(data = dy_kolot_rename,
                         percentiles = c(5,10,20,30))

Q_nanjung <- calc_longterm_percentile(data = nanjung_rename,
                         percentiles = c(5,10,20,30))


write.csv(rbind(Q_majalaya, Q_sapan, Q_dy_kolot, Q_nanjung), 
          "4_output/Q_andalan.csv")

## debit per bulan 

# debit_perbulan_majalaya -------------------------------------------------

Q_m_jan <- calc_longterm_percentile(data = majalaya_rename,
                                       percentiles = c(5,10,20,30),
                                       months = 1)
Q_m_feb <- calc_longterm_percentile(data = majalaya_rename,
                                           percentiles = c(5,10,20,30),
                                           months = 2)
Q_m_mar <- calc_longterm_percentile(data = majalaya_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 3)
Q_m_apr <- calc_longterm_percentile(data = majalaya_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 4)
Q_m_may <- calc_longterm_percentile(data = majalaya_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 5)
Q_m_jun <- calc_longterm_percentile(data = majalaya_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 6)
Q_m_jul <- calc_longterm_percentile(data = majalaya_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 7)
Q_m_aug <- calc_longterm_percentile(data = majalaya_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 8)
Q_m_sep <- calc_longterm_percentile(data = majalaya_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 9)
Q_m_oct <- calc_longterm_percentile(data = majalaya_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 10)
Q_m_nov <- calc_longterm_percentile(data = majalaya_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 11)
Q_m_dec <- calc_longterm_percentile(data = majalaya_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 12)


# debit_perbulan_sapan ----------------------------------------------------
Q_s_jan <- calc_longterm_percentile(data = sapan_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 1)
Q_s_feb <- calc_longterm_percentile(data = sapan_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 2)
Q_s_mar <- calc_longterm_percentile(data = sapan_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 3)
Q_s_apr <- calc_longterm_percentile(data = sapan_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 4)
Q_s_may <- calc_longterm_percentile(data = sapan_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 5)
Q_s_jun <- calc_longterm_percentile(data = sapan_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 6)
Q_s_jul <- calc_longterm_percentile(data = sapan_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 7)
Q_s_aug <- calc_longterm_percentile(data = sapan_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 8)
Q_s_sep <- calc_longterm_percentile(data = sapan_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 9)
Q_s_oct <- calc_longterm_percentile(data = sapan_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 10)
Q_s_nov <- calc_longterm_percentile(data = sapan_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 11)
Q_s_dec <- calc_longterm_percentile(data = sapan_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 12)

# debit_perbulan_dy_kolot -------------------------------------------------
Q_dy_jan <- calc_longterm_percentile(data = dy_kolot_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 1)
Q_dy_feb <- calc_longterm_percentile(data = dy_kolot_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 2)
Q_dy_mar <- calc_longterm_percentile(data = dy_kolot_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 3)
Q_dy_apr <- calc_longterm_percentile(data = dy_kolot_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 4)
Q_dy_may <- calc_longterm_percentile(data = dy_kolot_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 5)
Q_dy_jun <- calc_longterm_percentile(data = dy_kolot_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 6)
Q_dy_jul <- calc_longterm_percentile(data = dy_kolot_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 7)
Q_dy_aug <- calc_longterm_percentile(data = dy_kolot_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 8)
Q_dy_sep <- calc_longterm_percentile(data = dy_kolot_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 9)
Q_dy_oct <- calc_longterm_percentile(data = dy_kolot_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 10)
Q_dy_nov <- calc_longterm_percentile(data = dy_kolot_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 11)
Q_dy_dec <- calc_longterm_percentile(data = dy_kolot_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 12)

# debit_perbulan_nanjung --------------------------------------------------
Q_n_jan <- calc_longterm_percentile(data = nanjung_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 1)
Q_n_feb <- calc_longterm_percentile(data = nanjung_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 2)
Q_n_mar <- calc_longterm_percentile(data = nanjung_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 3)
Q_n_apr <- calc_longterm_percentile(data = nanjung_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 4)
Q_n_may <- calc_longterm_percentile(data = nanjung_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 5)
Q_n_jun <- calc_longterm_percentile(data = nanjung_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 6)
Q_n_jul <- calc_longterm_percentile(data = nanjung_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 7)
Q_n_aug <- calc_longterm_percentile(data = nanjung_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 8)
Q_n_sep <- calc_longterm_percentile(data = nanjung_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 9)
Q_n_oct <- calc_longterm_percentile(data = nanjung_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 10)
Q_n_nov <- calc_longterm_percentile(data = nanjung_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 11)
Q_n_dec <- calc_longterm_percentile(data = nanjung_rename,
                                    percentiles = c(5,10,20,30),
                                    months = 12)



# output_debit_perbulan ---------------------------------------------------

write.csv(rbind(Q_m_jan, Q_m_feb, Q_m_mar,Q_m_apr,Q_m_may,Q_m_jun,
                Q_m_jul,Q_m_aug,Q_m_sep,Q_m_oct,Q_m_nov,Q_m_dec), 
          "4_output/Q_bulanan_majalaya.csv")

write.csv(rbind(Q_s_jan, Q_s_feb, Q_s_mar,Q_s_apr,Q_s_may,Q_s_jun,
                Q_s_jul,Q_s_aug,Q_s_sep,Q_s_oct,Q_s_nov,Q_s_dec), 
          "4_output/Q_bulanan_sapan.csv")

write.csv(rbind(Q_dy_jan, Q_dy_feb, Q_dy_mar,Q_dy_apr,Q_dy_may,Q_dy_jun,
                Q_dy_jul,Q_dy_aug,Q_dy_sep,Q_dy_oct,Q_dy_nov,Q_dy_dec), 
          "4_output/Q_bulanan_dy_kolot.csv")

write.csv(rbind(Q_n_jan, Q_n_feb, Q_n_mar,Q_n_apr,Q_n_may,Q_n_jun,
                Q_n_jul,Q_n_aug,Q_n_sep,Q_n_oct,Q_n_nov,Q_n_dec), 
          "4_output/Q_bulanan_nanjung.csv")


# hitung annual trend -----------------------------------------------------

compute_annual_trends(data = nanjung_rename,zyp_method = "zhang",
                      basin_area = 1718, ignore_missing = T)
