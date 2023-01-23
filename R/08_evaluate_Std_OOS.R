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

# Initialize dataframes

q_vec <- c(0.005, 0.025, 0.165, 0.25, 0.5, 0.75, 0.835, 0.975, 0.995)

q_cols <- paste0("p_", q_vec)

agg_lvl_cols <- paste0("lvl_", 1:12)

WSPL_by_quantile_Std_OOS <- tibble(method=c("Std. OOS (signed)",
                                            "Std. OOS (percentage)"),
                                   p_0.005=NA,
                                   p_0.025=NA,
                                   p_0.165=NA,
                                   p_0.25=NA,
                                   p_0.5=NA,
                                   p_0.75=NA,
                                   p_0.835=NA,
                                   p_0.975=NA,
                                   p_0.995=NA)

WSPL_by_agg_level_Std_OOS <- tibble(method=c("Std. OOS (signed)",
                                             "Std. OOS (percentage)"),
                                    lvl_1=NA,
                                    lvl_2=NA,
                                    lvl_3=NA,
                                    lvl_4=NA,
                                    lvl_5=NA,
                                    lvl_6=NA,
                                    lvl_7=NA,
                                    lvl_8=NA,
                                    lvl_9=NA,
                                    lvl_10=NA,
                                    lvl_11=NA,
                                    lvl_12=NA)

RF_by_quantile_Std_OOS <- tibble(method=c("Std. OOS (signed)", 
                                          "Std. OOS (percentage)"),
                                 p_0.005=NA,
                                 p_0.025=NA,
                                 p_0.165=NA,
                                 p_0.25=NA,
                                 p_0.5=NA,
                                 p_0.75=NA,
                                 p_0.835=NA,
                                 p_0.975=NA,
                                 p_0.995=NA)

PICP_by_PI_Std_OOS <- tibble(method=c("Std. OOS (signed)",
                                      "Std. OOS (percentage)"),
                             PI_50=NA,
                             PI_67=NA,
                             PI_95=NA,
                             PI_99=NA)

PI_cols <- colnames(PICP_by_PI_Std_OOS[,-1])

for(error_measure in c("signed", "percentage") ){
  
  # Inputs
  
  submission_dir <- paste(getwd(), "probabilistic-forecasts/Standard-OOS", sep="/")
  
  error_measure_short <- str_sub(error_measure, end=4)
  
  submission_file <- paste0("prob_fcast_Std_OOS_", error_measure_short, ".csv") %>% 
    paste(submission_dir, ., sep="/")
  
  dataset_dir <- paste0(getwd(),"/dataset")
  
  test_period <- "evaluation"
  
  # Evaluation
  
  prob_fcast_scores <- evaluate_prob_fcasts(submission_file, test_period, dataset_dir)
  
  fcast_method <- paste0("Std. OOS (", error_measure, ")")
  
  WSPL_by_quantile_Std_OOS[which(WSPL_by_quantile_Std_OOS$method==fcast_method), q_cols] <- t(prob_fcast_scores$WSPL_by_quantile[["WSPL"]])
  
  WSPL_by_agg_level_Std_OOS[which(WSPL_by_agg_level_Std_OOS$method==fcast_method), agg_lvl_cols] <- t(prob_fcast_scores$WSPL_by_agg_level[["WSPL"]])
  
  RF_by_quantile_Std_OOS[which(RF_by_quantile_Std_OOS$method==fcast_method), q_cols] <- t(prob_fcast_scores$RF_by_quantile[["RF"]])
  
  PICP_by_PI_Std_OOS[which(PICP_by_PI_Std_OOS$method==fcast_method), PI_cols] <- t(prob_fcast_scores$PICP_by_PI[["PICP"]])
  
}