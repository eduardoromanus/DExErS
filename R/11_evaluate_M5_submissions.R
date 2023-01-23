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

M5_top_50_dir <- paste(getwd(), "probabilistic-forecasts/M5", sep="/")

M5_top_50_files <- list.files(path = M5_top_50_dir,
                              pattern = ".csv",
                              all.files = TRUE,
                              full.names = FALSE)

M5_top_50_teams <- str_remove(M5_top_50_files, ".csv")

q_vec <- c(0.005, 0.025, 0.165, 0.25, 0.5, 0.75, 0.835, 0.975, 0.995)

q_cols <- paste0("p_", q_vec)

WSPL_by_quantile_M5 <- tibble(team=M5_top_50_teams,
                              p_0.005=NA,
                              p_0.025=NA,
                              p_0.165=NA,
                              p_0.25=NA,
                              p_0.5=NA,
                              p_0.75=NA,
                              p_0.835=NA,
                              p_0.975=NA,
                              p_0.995=NA)

agg_lvl_cols <- paste0("lvl_", 1:12)

WSPL_by_agg_level_M5 <- tibble(team=M5_top_50_teams,
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

RF_by_quantile_M5 <- tibble(team=M5_top_50_teams,
                            p_0.005=NA,
                            p_0.025=NA,
                            p_0.165=NA,
                            p_0.25=NA,
                            p_0.5=NA,
                            p_0.75=NA, 
                            p_0.835=NA,
                            p_0.975=NA,
                            p_0.995=NA)

PICP_by_PI_M5 <- tibble(team=M5_top_50_teams,
                        PI_50=NA,
                        PI_67=NA,
                        PI_95=NA,
                        PI_99=NA)

PI_cols <- colnames(PICP_by_PI_M5[,-1])

pb <- progress::progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                                 total = length(M5_top_50_teams),
                                 complete = "=",
                                 incomplete = "-",
                                 current = ">",
                                 clear = FALSE)

for(team in M5_top_50_teams){
  
  pb$tick()
  
  # print(team)
  
  submission_file <- paste0(team, ".csv") %>% 
    paste(M5_top_50_dir, ., sep="/")
  
  dataset_dir <- paste0(getwd(),"/dataset")
  
  # Execution
  
  prob_fcast_scores <- evaluate_prob_fcasts(submission_file, test_period="evaluation", dataset_dir)
  
  WSPL_by_quantile_M5[which(WSPL_by_quantile_M5$team==team), q_cols] <- t(prob_fcast_scores$WSPL_by_quantile[["WSPL"]])
  
  WSPL_by_agg_level_M5[which(WSPL_by_agg_level_M5$team==team), agg_lvl_cols] <- t(prob_fcast_scores$WSPL_by_agg_level[["WSPL"]])
  
  RF_by_quantile_M5[which(RF_by_quantile_M5$team==team), q_cols] <- t(prob_fcast_scores$RF_by_quantile[["RF"]])
  
  PICP_by_PI_M5[which(PICP_by_PI_M5$team==team), PI_cols] <- t(prob_fcast_scores$PICP_by_PI[["PICP"]])
  
}