library(dplyr)
library(readxl)
library(ggplot2)
library(reshape2)
library(lubridate)
library(stringr)
library(scales)
library(ggpubr)
setwd('C:\\Users\\ppodl\\Desktop\\praca\\20221226_oil_price')

usdpln = read.csv('usdpln_w_stooq.csv')
usdpln$week <- 
  strftime(usdpln$Date, format = "%YW%V")

brent <-
  read_xls('RBRTEw_eiga_gov.xls',sheet = "Data 1",skip = 2)

colnames(brent) <- 
  c('date','price')

brent$date <- 
  as.Date(brent$date, origin = "1899-12-30")

brent$week <- 
  strftime(brent$date, format = "%YW%V")

ON <- 
  read_xlsx('ON_PL_bankier.xlsx')

colnames(ON) <- 
  c('Data','ON_price_PLN')
  

ON$week <- 
  substr(ON$Data,14,24) %>% 
  strftime(format = "%YW%V")  


df <- 
  ON %>% 
  select(week, ON_price_PLN) %>% 
  merge(brent %>% select(week,price),by = 'week') %>% 
  rename(brent_price_USD = price) %>% 
  merge(usdpln %>% select(week,Close,Date),by='week') %>% 
  mutate(brent_price_PLN = brent_price_USD * Close) %>% 
  select(Date,week, ON_price_PLN,brent_price_PLN) %>% 
  mutate(Date = Date %>% as.Date)

start_date <- 
  '2020-01-01'
start_week <- 
  min(df$week[df$Date >= start_date])

plot <- 
  df %>% 
    filter(Date >= start_date) %>%
    mutate(ON_price_PLN_proc = ON_price_PLN / df$ON_price_PLN[df$week == start_week],
           brent_price_PLN_proc = brent_price_PLN / df$brent_price_PLN[df$week == start_week]) %>%
    select(week,Date,ON_price_PLN_proc, brent_price_PLN_proc) %>% 
    melt(id.vars = c('Date','week')) %>%
    mutate(variable = str_replace_all(variable,'_',' ')) %>% 
    ggplot(aes(x=Date)) +
      geom_line(aes(y = value,col = variable)) +
      geom_hline(yintercept = 1) +
      theme_bw() +
      scale_x_date(date_breaks = "6 month", date_minor_breaks = "1 month",limits = c(start_date %>% as.Date - months(2),'2023-01-01' %>% as.Date),date_labels = '%y-%m') +
      scale_y_continuous(labels = scales::percent) +
      theme(legend.position = "top",
            legend.title = element_blank()) +
      labs(title = "Cena baryłki ropy 'Europe Brent' vs cena litra ON na stacjach paliw w PL",
           subtitle = '1W2020 = 100%',
           #caption = 'źródła danych: cena baryłki brent: eia.gov, ceny ON na stacjach w PL: bankier.pl, kursy USD/PLN: stooq.pl',
           x='',
           y='')
plot2 <- 
  df %>% 
    filter(Date >= start_date) %>%
    #mutate(ON_price_PLN_proc = ON_price_PLN / df$ON_price_PLN[df$week == start_week],
    #       brent_price_PLN_proc = brent_price_PLN / df$brent_price_PLN[df$week == start_week]) %>%
    #mutate(ratio = ON_price_PLN_proc / brent_price_PLN_proc) %>%
    mutate(ratio = ON_price_PLN / brent_price_PLN) %>%
    select(week,Date,ratio) %>% 
    ggplot(aes(x=Date,y=ratio)) +
      geom_line() +
      #geom_hline(yintercept = 1) +
      theme_bw() +
      scale_x_date(date_breaks = "6 month", date_minor_breaks = "1 month",limits = c(start_date %>% as.Date - months(2),'2023-01-01' %>% as.Date),date_labels = '%y-%m') +
      scale_y_continuous(labels = scales::percent) +
      theme(legend.position = "top",
            legend.title = element_blank()) +
      labs(title = "Stosunek ceny litra ON na stacjach paliw w PL do ceny baryłki ropy 'Europe Brent'",
           #subtitle = 'ceny z 1W2020 = 100% dla licznika i mianownika',
           caption = 'źródła danych: cena baryłki brent: eia.gov, ceny ON na stacjach w PL: bankier.pl, kursy USD/PLN: stooq.pl\nopracowanie: Przemysław Podlasin',
           x='',
           y='')+
      coord_cartesian(ylim=c(0,0.04))

plots <- 
  ggarrange(plot,plot2,ncol=1)
ggsave('plots.jpg',plots,scale = 1.5,width = 5, height = 5)

