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

# Prepare original accuracy submission (without sell prices)

pt_fcast_subm_eval <- pt_fcasts_comb_eval %>% 
  mutate(time_series = paste(time_series, "evaluation", sep="_") ) %>% 
  rename_with(~str_replace(.x, "h_", "F") )

pt_fcast_subm_valid <- pt_fcasts_comb_valid %>% 
  mutate(time_series = paste(time_series, "validation", sep="_") ) %>% 
  rename_with(~str_replace(.x, "h_", "F") )

pt_fcast_subm_all <- pt_fcast_subm_valid %>% 
  rbind(., pt_fcast_subm_eval)

pt_fcast_subm <- paste0(getwd(),"/dataset") %>% 
  paste(., "sample_submission_accuracy.csv", sep="/") %>% 
  read_csv(.) %>% 
  suppressMessages() %>% 
  dplyr::select(id) %>% 
  left_join(., pt_fcast_subm_all, by=c("id"="time_series") )

submission_file <- "accuracy_submission.csv" %>% 
  paste(getwd(), "point-forecasts", ., sep="/")

write_csv(x=pt_fcast_subm, file=submission_file)