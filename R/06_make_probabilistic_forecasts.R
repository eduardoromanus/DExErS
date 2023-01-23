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

# Problem variables

q_vec <- c(0.005, 0.025, 0.165, 0.25, 0.5, 0.75, 0.835, 0.975, 0.995)

# Directories

dataset_dir <- paste0(getwd(),"/dataset")

pt_fcasts_dir <- paste0(getwd(),"/point-forecasts")

# Generate forecasts for the evaluation period

prob_fcasts_eval_list <- make_prob_fcasts(dataset_dir = dataset_dir,
                                          pt_fcasts_dir = pt_fcasts_dir,
                                          method = "DExErS",
                                          error_measure = "signed",
                                          M = 100,
                                          N_min = 28,
                                          test_period = "evaluation",
                                          quantiles = q_vec)

# Generate forecasts for the validation period

prob_fcasts_valid_list <- make_prob_fcasts(dataset_dir = dataset_dir,
                                           pt_fcasts_dir = pt_fcasts_dir,
                                           method = "DExErS",
                                           error_measure = "signed",
                                           M = 100,
                                           N_min = 28,
                                           test_period = "validation",
                                           quantiles = q_vec)

# Prepare submission

prob_fcasts_eval_df <- extract_prob_fcasts(prob_fcasts_eval_list)

prob_fcasts_valid_df <- extract_prob_fcasts(prob_fcasts_valid_list)

prob_fcast_subm <- prep_prob_fcast_subm(prob_fcasts_valid_df = prob_fcasts_valid_df,
                                        prob_fcasts_eval_df = prob_fcasts_eval_df,
                                        dataset_dir = dataset_dir)

submission_file <- "prob_fcast_DExErS_sign.csv" %>% 
  paste(getwd(), "probabilistic-forecasts/DExErS", ., sep="/")

write_csv(x = prob_fcast_subm, file = submission_file)