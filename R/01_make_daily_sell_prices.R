# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(microbenchmark)
library(forecast)
library(smooth)
library(rAzureBatch)
library(doAzureParallel)
library(hts)
library(parallel)
library(BBmisc)
library(progress)
library(purrr)

weekly_prices <- paste(getwd(), "dataset", "sell_prices.csv", sep="/") %>% 
  read_csv(.) %>% 
  suppressMessages() %>% 
  mutate(time_series = paste(item_id, store_id, sep="_") ) %>% 
  dplyr::select(-item_id, -store_id) %>% 
  pivot_wider(id_cols = wm_yr_wk, names_from = time_series, values_from = sell_price)

daily_prices <- paste(getwd(), "dataset", "calendar.csv", sep="/") %>% 
  read_csv(.) %>% 
  suppressMessages() %>% 
  dplyr::select(wm_yr_wk) %>% 
  left_join(., weekly_prices, by="wm_yr_wk") %>% 
  dplyr::select(-wm_yr_wk)

daily_prices_file <- "daily_sell_prices.csv" %>% 
  paste(getwd(), "dataset", ., sep="/")

write_csv(x = daily_prices, file = daily_prices_file)

rm(weekly_prices, daily_prices)
