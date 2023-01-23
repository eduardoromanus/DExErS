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

# Evaluate original point forecasts (without sell prices)

acc_subm_df <- paste(getwd(), "point-forecasts", sep="/") %>% 
  paste(., "accuracy_submission.csv", sep="/") %>% 
  read_csv(.) %>% 
  suppressMessages()

test_period <- "evaluation"

pt_fcast_eval_results <- evaluate_pt_fcasts(acc_subm_df = acc_subm_df,
                                            test_period = test_period,
                                            dataset_dir = dataset_dir)

test_period <- "validation"

pt_fcast_valid_results <- evaluate_pt_fcasts(acc_subm_df = acc_subm_df,
                                             test_period = test_period,
                                             dataset_dir = dataset_dir)

rm(acc_subm_df)

# Evaluate original point forecasts (with sell prices)

acc_subm_df <- paste(getwd(), "point-forecasts", sep="/") %>% 
  paste(., "accuracy_submission_w_sell_prices.csv", sep="/") %>% 
  read_csv(.) %>% 
  suppressMessages()

test_period <- "evaluation"

pt_fcast_eval_results_w_sell_prices <- evaluate_pt_fcasts(acc_subm_df = acc_subm_df,
                                                          test_period = test_period,
                                                          dataset_dir = dataset_dir)

test_period <- "validation"

pt_fcast_valid_results_w_sell_prices <- evaluate_pt_fcasts(acc_subm_df = acc_subm_df,
                                                           test_period = test_period,
                                                           dataset_dir = dataset_dir)

rm(acc_subm_df)

# Compare results with and without sell prices

print(paste0("Evaluation WRMSSE (w/out sell prices): ", round(pt_fcast_eval_results$WRMSSE, 5)))
print(paste0("Evaluation WRMSSE (w/ sell prices): ", round(pt_fcast_eval_results_w_sell_prices$WRMSSE, 5)))

delta_perc_eval <- round(100*(pt_fcast_eval_results_w_sell_prices$WRMSSE - pt_fcast_eval_results$WRMSSE)/pt_fcast_eval_results$WRMSSE, 2)

print(paste0("Evaluation % delta (w/ -> w/out sell prices): ", delta_perc_eval, "%"))

print(paste0("Validation WRMSSE (w/out sell prices): ", round(pt_fcast_valid_results$WRMSSE, 5)))
print(paste0("Validation WRMSSE (w/ sell prices): ", round(pt_fcast_valid_results_w_sell_prices$WRMSSE, 5)))

delta_perc_valid <- round(100*(pt_fcast_valid_results_w_sell_prices$WRMSSE - pt_fcast_valid_results$WRMSSE)/pt_fcast_valid_results$WRMSSE, 2)

print(paste0("Validation % delta (w/ -> w/out sell prices): ", delta_perc_valid, "%"))