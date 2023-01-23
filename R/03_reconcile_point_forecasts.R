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

dataset_dir <- paste(getwd(), "dataset", sep="/")

pt_fcasts_df <- paste(getwd(), "point-forecasts", sep="/") %>%
  paste(., "all_point_forecasts.csv", sep="/") %>%
  read_csv(.) %>%
  suppressMessages()

# Reconcile original point forecasts for the evaluation period

pt_fcasts_eval <- pt_fcasts_df %>% 
  suppressMessages() %>% 
  dplyr::filter(fcast_origin == 1941) %>% 
  dplyr::select(-fcast_origin)

pt_fcasts_comb_eval <- reconcile_pt_fcasts(pt_fcast_matrix = pt_fcasts_eval, fcast_origin = 1941, h_out = 28, dataset_dir = dataset_dir)

# Reconcile original point forecasts for the validation period

pt_fcasts_valid <- pt_fcasts_df %>% 
  suppressMessages() %>% 
  dplyr::filter(fcast_origin == 1913) %>% 
  dplyr::select(-fcast_origin)

pt_fcasts_comb_valid <- reconcile_pt_fcasts(pt_fcast_matrix = pt_fcasts_valid, fcast_origin = 1913, h_out = 28, dataset_dir = dataset_dir)

rm(pt_fcasts_df)