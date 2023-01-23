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

# Inputs

fcast_origin = 1941

bool_quant <- TRUE

use_sell_prices <- FALSE

agg_lvl_df <- paste(getwd(), "dataset", sep="/") %>%
  paste(., "aggregation_levels.csv", sep="/") %>%
  read_csv(.) %>%
  suppressMessages()

sales_df <- paste(getwd(), "dataset", sep="/") %>%
  paste(., "sales_all_levels.csv", sep="/") %>%
  read_csv(.) %>%
  suppressMessages()

exog_vars_df <- paste(getwd(), "dataset", sep="/") %>%
  paste(., "exogenous_variables.csv", sep="/") %>%
  read_csv(.) %>%
  suppressMessages()

daily_prices_df <- paste(getwd(), "dataset", sep="/") %>%
  paste(., "daily_sell_prices.csv", sep="/") %>%
  read_csv(.) %>%
  suppressMessages()

ts_vec <- agg_lvl_df %>%
  dplyr::filter(agg_lvl %in% seq(1:5) ) %>%
  pull(time_series)

# ts_vec <- agg_lvl_df %>%
#   dplyr::filter(agg_lvl %in% 12) %>%
#   pull(time_series)


# Make point forecasts without parallel computing
results <- make_pt_fcasts(sales_df = sales_df, exog_vars_df = exog_vars_df, daily_prices_df = daily_prices_df, time_series = unlist(ts_vec), fcast_origin = fcast_origin, h_out = 28, quantiles = bool_quant, use_sell_prices = use_sell_prices)

rm(agg_lvl_df, sales_df, exog_vars_df, daily_prices_df)