# Function for making point forecasts using the es() function of the 'smooth' package
make_pt_fcasts <- function(sales_df, exog_vars_df, daily_prices_df, time_series="Total", fcast_origin=1941, h_out=28, quantiles=FALSE, use_sell_prices=FALSE){
  
  ts_vec <- time_series
  
  sales_df <- sales_df[, ts_vec]
  
  first_day_df <- sales_df %>% 
    mutate(day = 1:n() ) %>% 
    pivot_longer(cols = !day, names_to = "time_series", values_to = "sales_count") %>%
    group_by(time_series) %>% 
    dplyr::arrange(day) %>% 
    dplyr::summarise(first_day = min(row_number()[sales_count > 0.5])) %>% 
    ungroup()
  
  exog_vars <- exog_vars_df %>% 
    mutate(day = 1:n() ) %>% 
    dplyr::filter(day <= fcast_origin + h_out) %>% 
    pivot_longer(., cols = !day, names_to = "variable", values_to = "value") %>% 
    mutate(is_day_test = (day > fcast_origin) ) %>% 
    group_by(., variable) %>%
    mutate(occurs_in_test = (sum(value*is_day_test) >= 1) ) %>% 
    ungroup() %>% 
    dplyr::filter(occurs_in_test) %>% 
    dplyr::select(-is_day_test, -occurs_in_test)
  
  if(quantiles == TRUE){
    
    results_names_fcast <- c("pt_fcast", "p_0.005", "p_0.025", "p_0.165", "p_0.25", "p_0.5", "p_0.75", "p_0.835", "p_0.975", "p_0.995", "sp_0.005", "sp_0.025", "sp_0.165", "sp_0.25", "sp_0.5", "sp_0.75", "sp_0.835", "sp_0.975", "sp_0.995", "np_0.005", "np_0.025", "np_0.165", "np_0.25", "np_0.5", "np_0.75", "np_0.835", "np_0.975", "np_0.995")
    
    results_names_times <- c("pt_fcast_time", "p_prob_fcast_time", "sp_prob_fcast_time", "np_prob_fcast_time")
    
  } else {
    
    results_names_fcast <- c("pt_fcast")
    
    results_names_times <- c("pt_fcast_time")
    
  }
  
  result_list <- c(rep(list(rep(list(rep(NA, h_out)), length(ts_vec))), length(results_names_fcast) ),
                   rep(list(rep(list(NA), length(ts_vec))), length(results_names_times)))
  
  names(result_list) <- c(results_names_fcast, results_names_times)
  
  for(list_index in 1:length(result_list)){
    
    names(result_list[[list_index]]) <- ts_vec
    
  }
  
  for(ts_name in ts_vec){
    
    ts_index <- match(ts_name, ts_vec)
    
    print(paste0("Started forecasting ", ts_name, " (", ts_index, "/", length(ts_vec), ")"), quote = FALSE)
    
    first_day <- first_day_df %>% 
      dplyr::filter(time_series == ts_name) %>% 
      pull(first_day)
    
    state <- case_when(
      str_detect(ts_name, "CA") ~ "CA",
      str_detect(ts_name, "TX") ~ "TX",
      str_detect(ts_name, "WI") ~ "WI",
      T ~ as.character(NA) )
    
    target_ts <- sales_df[, ts_name] %>% 
      setNames("sales_count") %>% 
      mutate(day = 1:n() ) %>% 
      dplyr::filter(day <= fcast_origin) %>% 
      dplyr::arrange(day) %>% 
      # add white noise (prevent non-singular matrices)
      mutate(noise = rnorm(n = n(), mean = 0, sd = 0.01) ) %>%
      rowwise() %>% 
      mutate(noise = max(noise, -0.03) ) %>% 
      mutate(noise = min(noise, 0.03) ) %>% 
      mutate(sales_count = max(sales_count + noise, 0) ) %>% 
      dplyr::select(-noise) %>% 
      ungroup()
    
    if((use_sell_prices == TRUE) & (ts_name %in% colnames(daily_prices_df) ) ){
      
      daily_prices_ts <- daily_prices_df[, ts_name] %>%
        setNames("value") %>%
        mutate(variable = "sell_price", day = 1:n() ) %>%
        relocate(day, variable, value) %>%
        dplyr::filter(day <= fcast_origin + h_out) %>%
        mutate(value = if_else(is.na(value), 0, value) )
      
      exog_vars_ts <- exog_vars %>%
        rbind(daily_prices_ts)
      
    } else{
      
      exog_vars_ts <- exog_vars
      
    }
    
    exog_vars_ts <- exog_vars_ts %>% 
      dplyr::filter(case_when(state=="CA" ~ !(variable %in% c("boolsnapTX", "boolsnapWI") ),
                              state=="TX" ~ !(variable %in% c("boolsnapCA", "boolsnapWI") ),
                              state=="WI" ~ !(variable %in% c("boolsnapCA", "boolsnapTX") ),
                              T ~ !(variable %in% NA) ) ) %>%
      # add white noise (prevent non-singular matrices)
      mutate(noise = rnorm(n = n(), mean = 0, sd = 0.01) ) %>%
      rowwise() %>% 
      mutate(noise = max(noise, -0.03) ) %>% 
      mutate(noise = min(noise, 0.03) ) %>% 
      mutate(value = max(value + noise, 0) ) %>%
      dplyr::select(-noise) %>% 
      ungroup()
    
    if(first_day > fcast_origin){
      
      target_ts <- target_ts %>%
        pull(sales_count) %>% 
        ts(., frequency = 7)
      
      exog_vars_ts <- exog_vars_ts %>%
        pivot_wider(., names_from = "variable", values_from = "value") %>% 
        dplyr::select(-day)
      
    } else{
      
      target_ts <- target_ts %>% 
        dplyr::filter(row_number() >= first_day) %>% 
        pull(sales_count) %>% 
        ts(., frequency = 7)
      
      exog_vars_ts <- exog_vars_ts %>%
        dplyr::filter(day >= first_day) %>% 
        pivot_wider(., names_from = "variable", values_from = "value") %>% 
        dplyr::select(-day)
      
    }
    
    tryCatch({
      
      if(first_day > fcast_origin){
        
        print(paste0("Time series starts at ", first_day, " and forecast origin is at ", fcast_origin), quote = FALSE)
        
        stop("Time series starts after forecast origin") 
        
      }
      
      model_type <- "XXX"
      
      pt_fcast_time <- microbenchmark(
        
        esx_model <- es(y = target_ts,
                        model = model_type,
                        initial = "optimal",
                        ic = "AICc",
                        loss = "MSE",
                        h = h_out,
                        holdout = FALSE,
                        interval = "none",
                        xreg = exog_vars_ts,
                        xregDo = "select",
                        rounded = FALSE),
        times = 1,
        unit = "s")
      
      if (grepl("M", esx_model$model)) {
        
        print(paste0("Selected model: ", esx_model$model), quote = FALSE)
        
        stop("The es() function could not find a purely additive model (not enough nonzero observations)") 
        
      }
      
      pt_fcast <- pmax(as.numeric(esx_model$forecast), 0.00)
      
      result_list$pt_fcast[[ts_name]] <- pt_fcast
      
      result_list$pt_fcast_time[[ts_name]] <- pt_fcast_time$time/10^9
      
      if(quantiles == TRUE){
        
        PI_vec <- c(0.5, 0.67, 0.95, 0.99, 0.01)
        
        PI_method_vec <- c("nonparametric", "semiparametric", "parametric")
        
        for(PI_method in PI_method_vec){
          
          prob_fcast_time <- microbenchmark(
            
            for(PI in PI_vec){
              
              esx_model_PI <- es(y = target_ts,
                                 model = esx_model,
                                 h = h_out,
                                 holdout = FALSE,
                                 interval = PI_method,
                                 level = PI)
              
              PI_prefix <- case_when(
                PI_method == "nonparametric" ~ "np",
                PI_method == "semiparametric" ~ "sp",
                PI_method == "parametric" ~ "p",
                T ~ as.character(NA)
              )
              
              if(PI == 0.01){
                
                median_name <- paste(PI_prefix, "0.5", sep = "_")
                
                median_value <- case_when(
                  PI_method == "nonparametric" ~ as.numeric((esx_model_PI$lower + esx_model_PI$upper)/2),
                  PI_method == "semiparametric" ~ pt_fcast,
                  PI_method == "parametric" ~ pt_fcast
                )
                
                result_list[[median_name]][[ts_name]] <- pmax(median_value, 0.00)
                
              } else{
                
                lower_name <- paste(PI_prefix, (1-PI)/2, sep = "_")
                
                lower_value <- as.numeric(esx_model_PI$lower)
                
                result_list[[lower_name]][[ts_name]] <- pmax(lower_value, 0.00)
                
                upper_name <- paste(PI_prefix, (1+PI)/2, sep = "_")
                
                upper_value <- as.numeric(esx_model_PI$upper)
                
                result_list[[upper_name]][[ts_name]] <- pmax(upper_value, 0.00)
                
              }
              
            },
            
            times = 1,
            unit = "s")
          
          time_name <- paste(PI_prefix, "prob_fcast_time", sep = "_")
          
          result_list[[time_name]][[ts_name]] <- prob_fcast_time$time/10^9
          
        }
        
      }
      
    }, error = function(err){
      
      print(err)
      
      pt_fcast_time <- microbenchmark(
        
        esx_model <<- naive(target_ts, h = h_out, level = c(0.50, 0.67, 0.95, 0.99, 0.01)),
        
        times = 1,
        unit = "s")
      
      pt_fcast <- pmax(as.numeric(esx_model$mean), 0.00)
      
      result_list$pt_fcast[[ts_name]] <<- pt_fcast
      
      result_list$pt_fcast_time[[ts_name]] <<- pt_fcast_time$time/10^9
      
      if(quantiles == TRUE){
        
        PI_vec <- c(0.5, 0.67, 0.95, 0.99, 0.01)
        
        PI_method_vec <- c("nonparametric", "semiparametric", "parametric")
        
        for(PI_method in PI_method_vec){
          
          PI_prefix <- case_when(
            PI_method == "nonparametric" ~ "np",
            PI_method == "semiparametric" ~ "sp",
            PI_method == "parametric" ~ "p",
            T ~ as.character(NA)
          )
          
          for(PI_index in 1:length(PI_vec) ){
            
            PI <- PI_vec[PI_index]
            
            if(PI_index == 5){
              
              median_name <- paste(PI_prefix, "0.5", sep = "_")
              
              median_value <- as.numeric((esx_model$upper[, 5] + esx_model$lower[, 5])/2)
              
              result_list[[median_name]][[ts_name]] <<- pmax(median_value, 0.00)
              
            } else{
              
              lower_name <- paste(PI_prefix, (1-PI)/2, sep = "_")
              
              lower_value <- as.numeric(esx_model$lower[, PI_index])
              
              result_list[[lower_name]][[ts_name]] <<- pmax(lower_value, 0.00)
              
              upper_name <- paste(PI_prefix, (1+PI)/2, sep = "_")
              
              upper_value <- as.numeric(esx_model$upper[, PI_index])
              
              result_list[[upper_name]][[ts_name]] <<- pmax(upper_value, 0.00)
              
            }
            
          }
          
          prob_fcast_time <- pt_fcast_time$time/10^9
          
          time_name <- paste(PI_prefix, "prob_fcast_time", sep = "_")
          
          result_list[[time_name]][[ts_name]] <<- prob_fcast_time
          
        }
        
      }
      
      return(2)
      
    }, finally = {
      
    })
    
    progress <- round(100*ts_index/length(ts_vec), 1)
    
    print(paste0("Finished forecasting ", ts_name, " (", progress, " %)"), quote = FALSE)
    
  }
  
  return(result_list)
  
}

# Function for reconciling point forecasts by the "optimal combination" method
reconcile_pt_fcasts <- function(pt_fcast_matrix, fcast_origin = 1941, h_out = 28, dataset_dir){
  
  groups_df <- paste(dataset_dir, "groups.csv", sep="/") %>%
    read_csv(.) %>%
    suppressMessages()
  
  sales_df <- paste(dataset_dir, "sales_all_levels.csv", sep="/") %>%
    read_csv(.) %>%
    suppressMessages()
  
  agg_lvl_df <- paste(dataset_dir, "aggregation_levels.csv", sep="/") %>%
    read_csv(.) %>%
    suppressMessages()
  
  pt_fcast_matrix <- pt_fcast_matrix %>% 
    pivot_longer(cols=!time_series, names_to="day", values_to="value") %>% 
    mutate(day=as.integer(str_remove(day, "h_"))) %>% 
    pivot_wider(id_cols="day", names_from="time_series", values_from="value") %>% 
    dplyr::select(-day)
  
  bottom_lvl_ts_names <- agg_lvl_df %>% 
    dplyr::filter(agg_lvl == 12) %>% 
    pull(time_series)
  
  bottom_lvl_ts <- sales_df[1:fcast_origin, bottom_lvl_ts_names] %>% 
    ts()
  
  row_names <- c("item", "department", "category", "store", "state", "state_category", "state_department", "store_category", "store_department", "state_item")
  
  groups_df <- groups_df %>% 
    mutate(row_names =  row_names) %>% 
    relocate(row_names) %>% 
    tibble::column_to_rownames(., var="row_names") %>% 
    as.matrix()
  
  grouped_ts <- gts(y = bottom_lvl_ts, groups = groups_df)
  
  cl <- detectCores()
  
  pt_fcasts_comb <- combinef(fcasts = pt_fcast_matrix, groups = get_groups(grouped_ts), nonnegative = TRUE, algorithm = "lu", keep = "all", parallel = TRUE, num.cores = cl) %>% 
    as_tibble() %>% 
    setNames(agg_lvl_df$time_series) %>% 
    mutate(horizon = paste0("h_", 1:h_out) ) %>% 
    pivot_longer(., cols=!horizon, names_to="time_series", values_to="forecast") %>% 
    pivot_wider(id_cols = "time_series", names_from="horizon", values_from="forecast")
  
  closeAllConnections()
  
  return(pt_fcasts_comb)
  
}

# Function for calculating point forecasting accuracy (by the WRMSSE)
calculate_WRMSSE <- function(pt_fcast_matrix, test_period=c("evaluation", "validation"), h_out = 28, dataset_dir){
  
  fcast_origin_test <- case_when(
    test_period == "evaluation" ~ 1941,
    test_period == "validation" ~ 1913)
  
  sales_long_df <- paste(dataset_dir, "sales_all_levels.csv", sep="/") %>% 
    read_csv(.) %>% 
    suppressMessages() %>% 
    mutate(day = 1:n()) %>% 
    pivot_longer(., cols=!day, names_to="time_series", values_to="sales_count")
  
  OOS_values <- sales_long_df %>% 
    dplyr::filter(day > fcast_origin_test) %>% 
    dplyr::filter(day <= fcast_origin_test + h_out)
  
  first_day_df <- sales_long_df %>% 
    group_by(time_series) %>% 
    dplyr::arrange(day) %>% 
    dplyr::summarise(first_day = min(row_number()[sales_count > 0.5])) %>% 
    ungroup()
  
  naive_errors <- sales_long_df %>% 
    left_join(first_day_df, by="time_series") %>% 
    dplyr::filter(day >= first_day) %>% 
    dplyr::filter(day <= fcast_origin_test) %>% 
    group_by(time_series) %>% 
    dplyr::summarise(sq_naive_error = mean((sales_count - lag(sales_count) )^2, na.rm=T) ) %>% 
    ungroup()
  
  # Upload weights
  
  agg_lvl_df <- paste(dataset_dir, "aggregation_levels.csv", sep="/") %>% 
    read_csv(.) %>% 
    suppressMessages()
  
  ts_names_df <- agg_lvl_df %>% 
    mutate(time_series_subm = case_when(
      str_detect(time_series, "_.*/") ~ str_remove(time_series, ".*/"),
      str_detect(time_series, "/") ~ paste0(str_remove(time_series, ".*/"), "_X"),
      time_series == "Total" ~ paste0(time_series, "_X"),
      TRUE ~ time_series ) )
  
  weights_df <- paste0("weights_", test_period, ".csv") %>% 
    paste(dataset_dir, ., sep="/") %>%
    read_csv(.) %>%
    suppressMessages()
  
  ts_weights_df <- weights_df %>% 
    mutate(time_series_subm = paste0(Agg_Level_1, "_", Agg_Level_2) ) %>%
    dplyr::select(time_series_subm, weight) %>%
    left_join(ts_names_df, by="time_series_subm") %>%
    dplyr::select(-time_series_subm)
  
  RMSSE_per_ts <- pt_fcast_matrix %>% 
    pivot_longer(cols=!time_series, names_to="horizon", values_to="forecast") %>% 
    mutate(horizon = as.integer(str_remove(horizon, "h_") ) ) %>% 
    mutate(fcast_origin = fcast_origin_test) %>% 
    mutate(day = fcast_origin + horizon) %>% 
    dplyr::select(-fcast_origin, -horizon) %>% 
    left_join(sales_long_df, by=c("day", "time_series")) %>% 
    mutate(MSE = (sales_count - forecast)^2) %>% 
    group_by(time_series) %>% 
    dplyr::summarise(MSE = mean(MSE)) %>% 
    ungroup() %>% 
    left_join(naive_errors, by="time_series") %>% 
    mutate(RMSSE = sqrt(MSE/sq_naive_error) ) %>% 
    dplyr::select(time_series, RMSSE)
  
  WRMSSE_per_agg_lvl <- RMSSE_per_ts %>% 
    left_join(ts_weights_df, by="time_series") %>% 
    mutate(WRMSSE = RMSSE*weight) %>% 
    group_by(agg_lvl) %>% 
    dplyr::summarise(WRMSSE = sum(WRMSSE)) %>% 
    ungroup()
  
  WRMSSE <- WRMSSE_per_agg_lvl %>% 
    pull(WRMSSE) %>% 
    mean()
  
  result_list <- list(RMSSE_per_ts, WRMSSE_per_agg_lvl, WRMSSE) %>% 
    setNames(c("RMSSE_per_ts", "WRMSSE_per_agg_lvl", "WRMSSE"))
  
  return(result_list)
  
}

# Function for evaluating a full submission (test and validation periods) to the M5 "Accuracy" competition
evaluate_pt_fcasts <- function(acc_subm_df, test_period=c("evaluation", "validation"), dataset_dir){
  
  fcast_origin_test <- case_when(
    test_period == "evaluation" ~ 1941,
    test_period == "validation" ~ 1913)
  
  row_names <- c("item", "department", "category", "store", "state", "state_category", "state_department", "store_category", "store_department", "state_item")
  
  groups_df <- paste(dataset_dir, "groups.csv", sep="/") %>% 
    read_csv(.) %>% 
    suppressMessages() %>% 
    mutate(row_names =  row_names) %>% 
    relocate(row_names) %>% 
    tibble::column_to_rownames(., var="row_names") %>% 
    as.matrix()
  
  bottom_lvl_ts <- acc_subm_df %>% 
    dplyr::filter(str_detect(id, test_period) ) %>%
    mutate(id = str_remove(id, paste0("_", test_period) ) ) %>%
    pivot_longer(cols=!id, names_to="day", values_to="value") %>% 
    mutate(day=as.integer(str_remove(day, "F"))) %>% 
    pivot_wider(id_cols="day", names_from="id", values_from="value") %>% 
    dplyr::select(-day)
  
  grouped_ts <- gts(y = bottom_lvl_ts, groups = groups_df)
  
  all_fcasts <- allts(grouped_ts) %>% 
    as_tibble() %>% 
    mutate(horiz = paste0("h_", 1:28) ) %>% 
    pivot_longer(cols = !horiz, names_to="time_series", values_to = "forecast") %>% 
    pivot_wider(id_cols = time_series, names_from = horiz, values_from = forecast)
  
  results_list <- calculate_WRMSSE(all_fcasts, test_period = test_period, dataset_dir = dataset_dir)
  
  return(results_list)
  
}

# Distance function for the SNAP explanatory variables
calculate_SNAP_distance <- function(SNAP_train, SNAP_test){
  
  # Calculates the dissimilarity between test and train periods,
  # according to the SNAP indicator variables
  
  h_out <- length(SNAP_test)
  
  SNAP_test_train <- rbind(SNAP_test, SNAP_train)
  
  SNAP_dist <- dist(SNAP_test_train, method = "euclidean") / sqrt(h_out)
  
  return(SNAP_dist)
  
}

# Distance function for the DOW ("days of the week") explanatory variables
calculate_DOW_distance <- function(DOW_train, DOW_test){
  
  # Calculates the dissimilarity between test and train periods,
  # according to the DOTW (days of the week) indicator variables
  
  DOW_test_train <- rbind(DOW_test, DOW_train)
  
  DOW_dist <- dist(DOW_test_train, method = "maximum")
  
  return(DOW_dist)
  
}

# Distance function for the special events' explanatory variables
calculate_event_distance <- function(event_train, event_test){
  
  # Calculates the dissimilarity between test and train periods,
  # according to the 30 events' indicator variables
  
  event_test_train <- rbind(event_test, event_train)
  
  event_dist <- dist(event_test_train, method = "canberra")
  
  return(event_dist)
  
}

# Function for selecting past train windows (for DExErS)
select_train_windows <- function(exog_vars_df, fcast_origin = 1941, h_out = 28, M = 100, N_min = 28) {
  
  # Initialize dataframe for distance measures
  
  dist_train_windows <- tibble(fcast_origin = N_min:(fcast_origin - h_out) )
  
  # Distance regarding days of the week (DOW)
  
  DOW_test <- exog_vars_df %>% 
    slice((fcast_origin + 1):(fcast_origin + h_out)) %>% 
    pull(boolsaturday)
  
  DOW_train <- exog_vars_df %>% 
    slice((N_min + 1):fcast_origin) %>% 
    pull(boolsaturday)
  
  col_name <- "d_DOW"
  
  dist_train_windows[, col_name] <- zoo::rollapply(data = DOW_train, width = h_out, FUN = calculate_DOW_distance, DOW_test = DOW_test)
  
  # Distance regarding SNAP
  
  for(state in c("CA", "TX", "WI") ){
    
    SNAP_test <- case_when(state == "CA" ~ exog_vars_df$boolsnapCA[(fcast_origin + 1):(fcast_origin + h_out)],
                           state == "TX" ~ exog_vars_df$boolsnapTX[(fcast_origin + 1):(fcast_origin + h_out)],
                           state == "WI" ~ exog_vars_df$boolsnapWI[(fcast_origin + 1):(fcast_origin + h_out)] )
    
    SNAP_train <- case_when(state == "CA" ~ exog_vars_df$boolsnapCA[(N_min+1):fcast_origin],
                            state == "TX" ~ exog_vars_df$boolsnapTX[(N_min+1):fcast_origin],
                            state == "WI" ~ exog_vars_df$boolsnapWI[(N_min+1):fcast_origin] )
    
    col_name <- paste("d_SNAP", state, sep="_")
    
    dist_train_windows[, col_name] <- zoo::rollapply(data = SNAP_train, width = h_out, FUN = calculate_SNAP_distance, SNAP_test = SNAP_test)
    
  }
  
  dist_train_windows <- dist_train_windows %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(d_SNAP = mean(c(d_SNAP_CA, d_SNAP_TX, d_SNAP_WI) ),
                  d_DOW_SNAP = mean(c(d_DOW, d_SNAP) ) )
  
  # Distance regarding events
  
  DOW_cols <- paste0("bool", str_to_lower(weekdays(Sys.Date()+0:6)))
  
  SNAP_cols <- paste0("boolsnap", c("CA", "TX", "WI") )
  
  event_cols_vec <- exog_vars_df %>%
    dplyr::select(!contains(DOW_cols)) %>% 
    dplyr::select(!contains(SNAP_cols)) %>% 
    dplyr::select(!contains("lag")) %>% 
    dplyr::select(!contains("lead")) %>% 
    colnames()
  
  for(event_col in event_cols_vec){
    
    event_test <- exog_vars_df[(fcast_origin + 1 - 3):(fcast_origin + h_out), event_col] %>% 
      pull()
    
    event_train <- exog_vars_df[(N_min + 1 - 3):fcast_origin, event_col] %>% 
      pull()
    
    event_name <- str_remove(event_col, "bool")
    
    col_name <- paste("d_event", event_name, sep = "_")
    
    dist_train_windows[, col_name] <- zoo::rollapply(data = event_train, width = h_out+3, FUN = calculate_event_distance, event_test = event_test)
    
  }
  
  # Select M closest d_DOW_SNAP and all with d_event = 0
  
  dist_train_windows <- dist_train_windows %>%
    pivot_longer(., cols = starts_with("d_event"), names_to = "event", values_to = "d_event") %>% 
    mutate(event = str_remove(event, "d_event_") ) %>% 
    mutate(d_event = ifelse(is.na(d_event), 1, d_event) ) %>% 
    group_by(across(c(-event, -d_event))) %>% 
    dplyr::summarise(d_event = min(d_event)) %>% 
    suppressMessages() %>% 
    ungroup() %>%
    dplyr::arrange(., d_DOW_SNAP, desc(fcast_origin) ) %>%  
    mutate(order_stat = 1:nrow(.) )
  
  # selected_train_origins <- dist_train_windows %>% 
  #   dplyr::filter(order_stat <= M | d_event == 0)
  
  selected_train_origins <- dist_train_windows %>% 
    dplyr::filter(order_stat <= M) %>% 
    dplyr::arrange(fcast_origin) %>% 
    pull(fcast_origin)
  
  result_list <- list(selected_train_origins, dist_train_windows) %>% 
    setNames(c("selected_train_origins", "distance_train_windows"))
  
  return(result_list)
  
}

# DExErS and "Std. OOS" (standard empirical method based on OOS errors)
make_prob_fcasts <- function(dataset_dir, pt_fcasts_dir, method=c("DExErS", "standard"), error_measure=c("signed", "percentage"), M=c(20, 40, 60, 80, 100), N_min=c(7, 14, 28, 56, 84), test_period=c("evaluation", "validation"), h_out=28, quantiles=c(0.005, 0.025, 0.165, 0.25, 0.5, 0.75, 0.835, 0.975, 0.995) ){
  
  # Files
  
  print("Reading files...")
  
  pt_fcasts_test_file <- paste0("point_forecasts_", test_period, ".csv") %>% 
    paste(pt_fcasts_dir, ., sep="/")
  
  if(method == "standard"){
    
    pt_fcasts_train_file <- paste(pt_fcasts_dir, "point_forecasts_eval_100_most_recent.csv", sep="/")
    
  } else if(method == "DExErS"){
    
    pt_fcasts_train_file <- case_when(
      test_period == "evaluation" ~ paste0("point_forecasts_eval_Nmin_", N_min, ".csv"),
      test_period == "validation" ~ paste0("point_forecasts_valid_Nmin_", N_min, ".csv") ) %>% 
      paste(pt_fcasts_dir, ., sep="/")
    
  }
  
  exec_times_file <- "point_forecast_exec_times.csv" %>% 
    paste(pt_fcasts_dir, ., sep="/")
  
  sales_file <- "sales_all_levels.csv" %>% 
    paste(dataset_dir, ., sep="/")
  
  agg_lvl_file <- "aggregation_levels.csv" %>% 
    paste(dataset_dir, ., sep="/")
  
  exog_vars_file <- "exogenous_variables.csv" %>% 
    paste(dataset_dir, ., sep="/")
  
  # Step 1: making point forecasts for the test period
  
  print("Making point forecasts (for the test period)...")
  
  fcast_origin_test <- case_when(
    test_period == "evaluation" ~ 1941,
    test_period == "validation" ~ 1913)
  
  pt_fcasts_test <- read_csv(pt_fcasts_test_file) %>% 
    suppressMessages() %>% 
    pivot_longer(cols = starts_with("h_"), names_to = "horiz", names_prefix = "h_", values_to = "pt_fcast") %>% 
    mutate(horiz = as.integer(horiz) ) %>%
    relocate(time_series, fcast_origin, horiz, pt_fcast) %>% 
    dplyr::select(time_series, fcast_origin, horiz, pt_fcast)
  
  time_step_1 <- read_csv(exec_times_file) %>% 
    suppressMessages() %>% 
    dplyr::filter(fcast_origin == fcast_origin_test) %>% 
    pull(pt_fcast_exec_time) %>%
    sum() %>% 
    as.difftime(., units = "secs")
  
  
  # Step 2: selecting train periods (function)
  
  print("Selecting train periods...")
  
  # Distance functions for comparing test and train periods
  
  calculate_SNAP_distance <- function(SNAP_train, SNAP_test){
    
    # Calculates the dissimilarity between test and train periods,
    # according to the SNAP indicator variables
    
    h_out <- length(SNAP_test)
    
    SNAP_test_train <- rbind(SNAP_test, SNAP_train)
    
    SNAP_dist <- dist(SNAP_test_train, method = "euclidean") / sqrt(h_out)
    
    return(SNAP_dist)
    
  }
  
  calculate_DOW_distance <- function(DOW_train, DOW_test){
    
    # Calculates the dissimilarity between test and train periods,
    # according to the DOTW (days of the week) indicator variables
    
    DOW_test_train <- rbind(DOW_test, DOW_train)
    
    DOW_dist <- dist(DOW_test_train, method = "maximum")
    
    return(DOW_dist)
    
  }
  
  calculate_event_distance <- function(event_train, event_test){
    
    # Calculates the dissimilarity between test and train periods,
    # according to the 30 events' indicator variables
    
    event_test_train <- rbind(event_test, event_train)
    
    event_dist <- dist(event_test_train, method = "canberra")
    
    return(event_dist)
    
  }
  
  select_train_windows <- function(exog_vars_df, fcast_origin = 1941, h_out = 28, M = 100, N_min = 28) {
    
    # Initialize dataframe for distance measures
    
    dist_train_windows <- tibble(fcast_origin = N_min:(fcast_origin - h_out) )
    
    # Distance regarding days of the week (DOW)
    
    DOW_test <- exog_vars_df %>% 
      slice((fcast_origin + 1):(fcast_origin + h_out)) %>% 
      pull(boolsaturday)
    
    DOW_train <- exog_vars_df %>% 
      slice((N_min + 1):fcast_origin) %>% 
      pull(boolsaturday)
    
    col_name <- "d_DOW"
    
    dist_train_windows[, col_name] <- zoo::rollapply(data = DOW_train, width = h_out, FUN = calculate_DOW_distance, DOW_test = DOW_test)
    
    # Distance regarding SNAP
    
    for(state in c("CA", "TX", "WI") ){
      
      SNAP_test <- case_when(state == "CA" ~ exog_vars_df$boolsnapCA[(fcast_origin + 1):(fcast_origin + h_out)],
                             state == "TX" ~ exog_vars_df$boolsnapTX[(fcast_origin + 1):(fcast_origin + h_out)],
                             state == "WI" ~ exog_vars_df$boolsnapWI[(fcast_origin + 1):(fcast_origin + h_out)] )
      
      SNAP_train <- case_when(state == "CA" ~ exog_vars_df$boolsnapCA[(N_min+1):fcast_origin],
                              state == "TX" ~ exog_vars_df$boolsnapTX[(N_min+1):fcast_origin],
                              state == "WI" ~ exog_vars_df$boolsnapWI[(N_min+1):fcast_origin] )
      
      col_name <- paste("d_SNAP", state, sep="_")
      
      dist_train_windows[, col_name] <- zoo::rollapply(data = SNAP_train, width = h_out, FUN = calculate_SNAP_distance, SNAP_test = SNAP_test)
      
    }
    
    dist_train_windows <- dist_train_windows %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(d_SNAP = mean(c(d_SNAP_CA, d_SNAP_TX, d_SNAP_WI) ),
                    d_DOW_SNAP = mean(c(d_DOW, d_SNAP) ) )
    
    # Distance regarding events
    
    DOW_cols <- paste0("bool", str_to_lower(weekdays(Sys.Date()+0:6)))
    
    SNAP_cols <- paste0("boolsnap", c("CA", "TX", "WI") )
    
    event_cols_vec <- exog_vars_df %>%
      dplyr::select(!contains(DOW_cols)) %>% 
      dplyr::select(!contains(SNAP_cols)) %>% 
      dplyr::select(!contains("lag")) %>% 
      dplyr::select(!contains("lead")) %>% 
      colnames()
    
    for(event_col in event_cols_vec){
      
      event_test <- exog_vars_df[(fcast_origin + 1 - 3):(fcast_origin + h_out), event_col] %>% 
        pull()
      
      event_train <- exog_vars_df[(N_min + 1 - 3):fcast_origin, event_col] %>% 
        pull()
      
      event_name <- str_remove(event_col, "bool")
      
      col_name <- paste("d_event", event_name, sep = "_")
      
      dist_train_windows[, col_name] <- zoo::rollapply(data = event_train, width = h_out+3, FUN = calculate_event_distance, event_test = event_test)
      
    }
    
    # Select M closest d_DOW_SNAP and all with d_event = 0
    
    dist_train_windows <- dist_train_windows %>%
      pivot_longer(., cols = starts_with("d_event"), names_to = "event", values_to = "d_event") %>% 
      mutate(event = str_remove(event, "d_event_") ) %>% 
      mutate(d_event = ifelse(is.na(d_event), 1, d_event) ) %>% 
      group_by(across(c(-event, -d_event))) %>% 
      dplyr::summarise(d_event = min(d_event)) %>% 
      suppressMessages() %>% 
      ungroup() %>%
      dplyr::arrange(., d_DOW_SNAP, desc(fcast_origin) ) %>%  
      mutate(order_stat = 1:nrow(.) )
    
    # selected_train_origins <- dist_train_windows %>% 
    #   dplyr::filter(order_stat <= M | d_event == 0)
    
    selected_train_origins <- dist_train_windows %>% 
      dplyr::filter(order_stat <= M) %>% 
      dplyr::arrange(fcast_origin) %>% 
      pull(fcast_origin)
    
    result_list <- list(selected_train_origins, dist_train_windows) %>% 
      setNames(c("selected_train_origins", "distance_train_windows"))
    
    return(result_list)
    
  }
  
  # Step 2: selecting train periods (execution)
  
  time_step_2 <- microbenchmark({
    
    if(method == "standard"){
      
      selected_origins <- tibble(selected_train_origins = seq(fcast_origin_test - M - h_out + 1, fcast_origin_test - h_out) )
      
    } else if(method == "DExErS"){
      
      exog_vars_df <- read_csv(exog_vars_file) %>%
        suppressMessages()
      
      selected_origins <- select_train_windows(exog_vars_df = exog_vars_df, fcast_origin = fcast_origin_test, h_out = h_out, M = M, N_min = N_min)
      
    }
    
  },
  
  times = 1,
  unit = "s")
  
  time_step_2 <- as.difftime(time_step_2$time/10^9, units = "secs")
  
  
  # Step 3: making point forecasts for selected train periods
  
  print("Making point forecasts (for the train periods)...")
  
  pt_fcasts_train <- read_csv(pt_fcasts_train_file) %>% 
    suppressMessages() %>%
    dplyr::filter(fcast_origin %in% selected_origins$selected_train_origins) %>% 
    pivot_longer(cols = starts_with("h_"), names_to = "horiz", names_prefix = "h_", values_to = "pt_fcast") %>% 
    mutate(horiz = as.integer(horiz) ) %>%
    relocate(time_series, fcast_origin, horiz, pt_fcast) %>% 
    dplyr::select(time_series, fcast_origin, horiz, pt_fcast)
  
  # Execution times
  
  time_step_3 <- read_csv(exec_times_file) %>% 
    suppressMessages() %>% 
    dplyr::filter(fcast_origin %in% selected_origins$selected_train_origins) %>% 
    pull(pt_fcast_exec_time) %>%
    sum() %>% 
    as.difftime(., units = "secs")
  
  
  # Step 4: calculating past forecast errors for selected train periods
  
  print("Calculating past forecast errors...")
  
  time_step_4 <- microbenchmark({
    
    obs_values <- read_csv(sales_file) %>% 
      suppressMessages() %>% 
      mutate(day = 1:n()) %>% 
      pivot_longer(., cols=!day, names_to="time_series", values_to="sales_count") %>% 
      rename(timestamp = day, obs_value = sales_count)
    
    past_fcast_errors <- pt_fcasts_train %>% 
      mutate(timestamp = fcast_origin + horiz) %>%
      left_join(y = obs_values, by = c("time_series", "timestamp") ) %>%
      mutate(fcast_error = case_when(
        error_measure == "signed" ~ obs_value - pt_fcast,
        error_measure == "percentage" ~ 1 - (pt_fcast/obs_value) ) ) %>%
      dplyr::select(time_series, fcast_origin, horiz, fcast_error) %>% 
      mutate(fcast_error = ifelse(is.nan(fcast_error), 0, fcast_error) ) },
    
    times = 1,
    unit = "s")
  
  time_step_4 <- as.difftime(time_step_4$time/10^9, units = "secs")
  
  rm(pt_fcasts_train)
  
  gc_ <- gc(verbose = FALSE)
  
  
  # Step 5: estimating error quantiles
  
  print("Estimating error quantiles...")
  
  time_step_5 <- microbenchmark({
    
    if(method == "standard"){
      
      # Substep 5.2: estimating error quantiles
      
      first_day_df <- obs_values %>% 
        group_by(time_series) %>% 
        dplyr::arrange(timestamp) %>% 
        dplyr::summarise(first_day = min(row_number()[obs_value > 0.5])) %>% 
        suppressMessages() %>% 
        ungroup()
      
      error_quantiles <- past_fcast_errors %>% 
        left_join(first_day_df, by = "time_series") %>% 
        dplyr::filter(fcast_origin - first_day + 1 >= N_min) %>%
        dplyr::select(-first_day) %>% 
        dplyr::filter(if (error_measure == "percentage") {fcast_error < 0.99999} else {TRUE}) %>% 
        group_by(time_series, horiz) %>%
        dplyr::summarise(error_quantile = quantile(fcast_error, probs = q_vec, na.rm = TRUE, type=8), prob = q_vec ) %>%
        suppressMessages() %>% 
        ungroup()
      
      rm(past_fcast_errors)
      
      gc_ <- gc(verbose=FALSE)
      
    } else if(method == "DExErS"){
      
      # Substep 5.1: calculating the dissimilarity between time series
      
      agg_lvl_df <- read_csv(agg_lvl_file) %>% 
        suppressMessages()
      
      first_day_df <- obs_values %>% 
        group_by(time_series) %>% 
        dplyr::arrange(timestamp) %>% 
        dplyr::summarise(first_day = min(row_number()[obs_value > 0.5])) %>% 
        suppressMessages() %>% 
        ungroup()
      
      dist_time_series <- obs_values %>% 
        dplyr::filter(timestamp %in% seq(fcast_origin_test - h_out + 1, fcast_origin_test) ) %>% 
        left_join(agg_lvl_df, by="time_series") %>% 
        dplyr::filter(agg_lvl %in% c(10, 11, 12) ) %>% 
        group_by(time_series) %>% 
        dplyr::summarise(ADI = sum(obs_value==0)/h_out,
                         avg_sales = sum(obs_value)/sum(obs_value!=0) ) %>% 
        suppressMessages() %>% 
        ungroup() %>% 
        left_join(first_day_df, by="time_series") %>% 
        mutate(avg_sales = ifelse(is.nan(avg_sales), 0, avg_sales) ) %>%
        mutate(avg_sales_norm = BBmisc::normalize(avg_sales, method="range") ) %>% 
        slice(rep(1:n(), each = M) ) %>% 
        mutate(fcast_origin = rep_len(selected_origins$selected_train_origins, length.out = nrow(.) ) ) %>% 
        group_by(time_series) %>% 
        dplyr::mutate(num_errors = sum(fcast_origin >= first_day + N_min - 1) ) %>% 
        ungroup() %>% 
        left_join(agg_lvl_df, by="time_series")
      
      rm(obs_values)
      
      gc_ <- gc(verbose=FALSE)
      
      time_series_comp <- dist_time_series %>% 
        dplyr::filter(num_errors >=  M) %>% 
        dplyr::select(time_series, ADI, avg_sales_norm, agg_lvl) %>% 
        distinct(time_series, .keep_all = T) %>%
        mutate(state = case_when(
          str_detect(time_series, "CA_") ~ "CA",
          str_detect(time_series, "TX_") ~ "TX",
          str_detect(time_series, "WI_") ~ "WI"
        ) ) %>% 
        mutate(store = case_when(
          agg_lvl == 12 ~ str_sub(time_series, start = -4)
        ) ) %>%
        mutate(item = case_when(
          agg_lvl == 10 ~ str_remove(time_series, ".*/"),
          agg_lvl == 11 ~ str_sub(str_remove(time_series, ".*/"), start = 4),
          agg_lvl == 12 ~ str_sub(time_series, end = -6)
        ) ) %>% 
        mutate(dept = str_sub(item, end = -5) ) %>% 
        mutate(cat = str_sub(dept, end = -3) )
      
      
      time_series_incomp <- dist_time_series %>% 
        dplyr::filter(num_errors < M) %>% 
        dplyr::select(time_series, ADI, avg_sales_norm, agg_lvl) %>% 
        mutate(state = case_when(
          str_detect(time_series, "CA_") ~ "CA",
          str_detect(time_series, "TX_") ~ "TX",
          str_detect(time_series, "WI_") ~ "WI"
        ) ) %>% 
        mutate(store = case_when(
          agg_lvl == 12 ~ str_sub(time_series, start = -4)
        ) ) %>%
        mutate(item = case_when(
          agg_lvl == 10 ~ str_remove(time_series, ".*/"),
          agg_lvl == 11 ~ str_sub(str_remove(time_series, ".*/"), start = 4),
          agg_lvl == 12 ~ str_sub(time_series, end = -6)
        ) ) %>% 
        mutate(dept = str_sub(item, end = -5) ) %>% 
        mutate(cat = str_sub(dept, end = -3) ) %>% 
        mutate(closest_ts = NA) %>% 
        distinct(time_series, .keep_all = T)
      
      num_ts_incomp <- nrow(time_series_incomp)
      
      pb <- progress::progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                                       total = num_ts_incomp,
                                       complete = "=",
                                       incomplete = "-",
                                       current = ">",
                                       clear = FALSE)
      
      for(row_index in 1:num_ts_incomp){
        
        pb$tick()
        
        time_series_incomp_ref <- time_series_incomp[row_index, ]
        
        dist_comp_incomp <- time_series_comp %>%
          mutate(compared_ts = time_series_incomp$time_series[row_index]) %>%
          left_join(time_series_incomp_ref, by=c("compared_ts"="time_series"), suffix=c("", "_ref") ) %>%
          mutate(dist_inst = sqrt((ADI - ADI_ref)^2 + (avg_sales_norm - avg_sales_norm_ref)^2) ) %>%
          mutate(agg_lvl_tier = abs(agg_lvl - agg_lvl_ref) - (agg_lvl < agg_lvl_ref)*0.5) %>%
          dplyr::slice_min(dist_inst) %>%
          dplyr::slice_min(agg_lvl_tier) %>%
          dplyr::slice_max(item == item_ref) %>%
          dplyr::slice_max(dept == dept_ref) %>%
          dplyr::slice_max(cat == cat_ref)
        
        time_series_incomp$closest_ts[row_index] <- as.character(dist_comp_incomp$time_series[1])
        
      }
      
      rm(dist_comp_incomp)
      
      gc_ <- gc(verbose=FALSE)
      
      time_series_closest <- time_series_incomp %>%
        dplyr::select(time_series, closest_ts)
      
      
      # Substep 5.2: estimating error quantiles
      
      error_quantiles <- past_fcast_errors %>% 
        left_join(first_day_df, by = "time_series") %>% 
        left_join(time_series_closest, by = "time_series") %>% 
        mutate(used_ts = if_else(fcast_origin - first_day + 1 >= N_min, time_series, closest_ts) ) %>%
        dplyr::select(-closest_ts, -first_day) %>% 
        left_join(past_fcast_errors, by = c("used_ts"="time_series", "fcast_origin", "horiz"), suffix=c("_old", "") ) %>% 
        dplyr::select(-fcast_error_old, -used_ts) %>% 
        dplyr::filter(if (error_measure == "percentage") {fcast_error < 0.99999} else {TRUE}) %>% 
        group_by(time_series, horiz) %>%
        dplyr::summarise(error_quantile = quantile(fcast_error, probs = q_vec, na.rm = TRUE, type=8), prob = q_vec ) %>%
        suppressMessages() %>% 
        ungroup()
      
      rm(past_fcast_errors)
      
      gc_ <- gc(verbose=FALSE)
      
    }
    
  },
  
  times = 1,
  unit = "s")
  
  time_step_5 <- as.difftime(time_step_5$time/10^9, units = "secs")
  
  
  # Step 6: probabilistic forecasting
  
  print("Making probabilistic forecasts...")
  
  time_step_6 <- microbenchmark({
    
    prob_fcasts <- pt_fcasts_test %>% 
      dplyr::select(-fcast_origin) %>% 
      left_join(y = error_quantiles, by = c("time_series", "horiz") ) %>% 
      mutate(prob_fcast = case_when(
        error_measure == "signed" ~ pt_fcast + error_quantile,
        error_measure == "percentage" ~ pt_fcast/(1 - error_quantile) ) ) %>%
      mutate(prob_fcast = pmax(prob_fcast, 0) ) %>% 
      dplyr::select(time_series, horiz, prob, prob_fcast)},
    
    times = 1,
    unit = "s")
  
  time_step_6 <- as.difftime(time_step_6$time/10^9, units = "secs")
  
  rm(error_quantiles)
  
  gc_ <- gc(verbose=FALSE)
  
  result_list <- prob_fcasts %>%
    pivot_wider(id_cols=c("prob","horiz"), names_from="time_series", values_from="prob_fcast") %>% 
    dplyr::select(-horiz) %>% 
    plyr::dlply(., "prob", function(x) as.list(x) ) %>% 
    lapply(., function(x) {x$prob <- NULL; x} ) %>% 
    lapply(., function(x) lapply(x, as.numeric) )
  
  result_list$time_step_1 <- time_step_1
  result_list$time_step_2 <- time_step_2
  result_list$time_step_3 <- time_step_3
  result_list$time_step_4 <- time_step_4
  result_list$time_step_5 <- time_step_5
  result_list$time_step_6 <- time_step_6
  
  return(result_list)
  
}

# Function for extracting probabilistic forecasts from results' list
extract_prob_fcasts <- function(prob_fcasts_list){
  
  q_vec <- c(0.005, 0.025, 0.165, 0.25, 0.5, 0.75, 0.835, 0.975, 0.995)
  
  prob_fcasts_df <- NULL
  
  for(quant in q_vec){
    
    if(is.null(prob_fcasts_df)){
      
      prob_fcasts_df <- tibble::enframe(prob_fcasts_list[[as.character(quant)]]) %>%
        unnest(cols = c(value)) %>% 
        mutate(horiz = rep_len(1:28, length.out = n() ) ) %>%
        mutate(prob = as.numeric(quant) )
      
    } else {
      
      prob_fcasts_new_quant <- tibble::enframe(prob_fcasts_list[[as.character(quant)]]) %>%
        unnest(cols = c(value)) %>% 
        mutate(horiz = rep_len(1:28, length.out = n() ) ) %>%
        mutate(prob = as.numeric(quant) )
      
      prob_fcasts_df <- rbind(prob_fcasts_df, prob_fcasts_new_quant)
      
    }
    
  }
  
  prob_fcasts_df <- prob_fcasts_df %>% 
    dplyr::relocate(name, prob, horiz, value) %>% 
    dplyr::rename(time_series = name,
                  prob_fcast = value)
  
  return(prob_fcasts_df)
  
}

# Function for preparing a full submission (test and validation periods) to the M5 "Uncertainty" competition
prep_prob_fcast_subm <- function(prob_fcasts_valid_df, prob_fcasts_eval_df, dataset_dir){
  
  prep_prob_fcast_subm_period <- function(test_period = c("evaluation", "validation"), prob_fcasts_df){
    
    prob_fcast_subm <- prob_fcasts_df %>%
      mutate(time_series = case_when(
        str_detect(time_series, "_.*/") ~ str_remove(time_series, ".*/"),
        str_detect(time_series, "/") ~ paste0(str_remove(time_series, ".*/"), "_X"),
        time_series == "Total" ~ paste0(time_series, "_X"),
        TRUE ~ time_series ) ) %>% 
      mutate(time_series = paste0(time_series, "_", str_pad(prob, 5, side="right", pad="0") ) ) %>% 
      mutate(time_series = paste(time_series, test_period, sep = "_") ) %>%
      dplyr::select(time_series, horiz, prob_fcast) %>% 
      mutate(horiz = paste0("F", horiz) ) %>% 
      pivot_wider(names_from = horiz, values_from = prob_fcast) %>% 
      dplyr::rename(id = time_series)
    
    return(prob_fcast_subm)
    
  }
  
  prob_fcast_subm_eval <- prep_prob_fcast_subm_period(test_period = "evaluation", prob_fcasts_df = prob_fcasts_eval_df)
  
  prob_fcast_subm_valid <- prep_prob_fcast_subm_period(test_period = "validation", prob_fcasts_df = prob_fcasts_valid_df)
  
  pt_fcast_subm_all <- prob_fcast_subm_valid %>% 
    rbind(., prob_fcast_subm_eval)
  
  prob_fcast_subm <- "sample_submission_uncertainty.csv" %>% 
    paste(dataset_dir, ., sep="/") %>% 
    read_csv(.) %>% 
    suppressMessages() %>% 
    dplyr::select(id) %>%
    left_join(., pt_fcast_subm_all, by="id")
  
  return(prob_fcast_subm)
  
}

# Function for evaluating a full submission (test and validation periods) to the M5 "Uncertainty" competition
evaluate_prob_fcasts <- function(submission_file, test_period=c("evaluation", "validation"), dataset_dir){
  
  fcast_origin_test <- case_when(
    test_period == "evaluation" ~ 1941,
    test_period == "validation" ~ 1913)
  
  agg_lvl_df <- "aggregation_levels.csv" %>% 
    paste(dataset_dir, ., sep="/") %>%
    read_csv(.) %>% 
    suppressMessages() %>% 
    mutate(time_series_subm = case_when(
      str_detect(time_series, "_.*/") ~ str_remove(time_series, ".*/"),
      str_detect(time_series, "/") ~ paste0(str_remove(time_series, ".*/"), "_X"),
      time_series == "Total" ~ paste0(time_series, "_X"),
      TRUE ~ time_series ) )
  
  obs_values <-"sales_all_levels.csv" %>% 
    paste(dataset_dir, ., sep="/") %>%
    read_csv(.) %>% 
    suppressMessages() %>% 
    mutate(day = 1:n()) %>% 
    pivot_longer(., cols=!day, names_to="time_series", values_to="sales_count") %>% 
    rename(timestamp = day, obs_value = sales_count) %>% 
    relocate(time_series, timestamp, obs_value) %>% 
    arrange(timestamp) %>% 
    arrange(factor(time_series, levels = agg_lvl_df$time_series) )
  
  weights_df <- paste0("weights_", test_period, ".csv") %>% 
    paste(dataset_dir, ., sep="/") %>%
    read_csv(.) %>%
    suppressMessages() %>% 
    mutate(time_series_subm = paste0(Agg_Level_1, "_", Agg_Level_2) ) %>%
    dplyr::select(time_series_subm, weight) %>%
    left_join(agg_lvl_df, by="time_series_subm") %>%
    dplyr::select(-time_series_subm)
  
  first_day_df <- obs_values %>% 
    group_by(time_series) %>% 
    dplyr::arrange(timestamp) %>% 
    dplyr::summarise(first_day = min(row_number()[obs_value > 0.5])) %>% 
    ungroup() %>% 
    suppressMessages()
  
  naive_errors <- obs_values %>% 
    left_join(first_day_df, by="time_series") %>% 
    dplyr::filter(timestamp >= first_day) %>% 
    dplyr::filter(timestamp <= fcast_origin_test) %>% 
    group_by(time_series) %>% 
    dplyr::summarise(naive_error = mean(abs(obs_value - lag(obs_value)), na.rm=T) ) %>% 
    ungroup() %>% 
    suppressMessages()
  
  eval_info_quantiles <- read_csv(submission_file) %>%
    suppressMessages() %>% 
    dplyr::rename(time_series = id) %>%
    dplyr::filter(str_detect(time_series, test_period) ) %>%
    pivot_longer(cols = starts_with("F"), names_to = "horiz", names_prefix = "F", values_to = "prob_fcast") %>%
    mutate(horiz = as.integer(horiz) ) %>%
    mutate(fcast_origin = fcast_origin_test) %>%
    mutate(time_series = str_remove(time_series, paste0("_", test_period) ) ) %>%
    mutate(prob = as.numeric(str_sub(time_series, start=-5) ) ) %>%
    mutate(time_series = str_sub(time_series, end=-7) ) %>%
    dplyr::rename(time_series_subm = time_series) %>%
    left_join(agg_lvl_df, by = "time_series_subm") %>%
    mutate(timestamp = fcast_origin + horiz) %>%
    dplyr::select(-time_series_subm) %>%
    left_join(y = obs_values, by = c("time_series", "timestamp") ) %>% 
    dplyr::select(time_series, agg_lvl, prob, horiz, obs_value, prob_fcast) %>% 
    relocate(time_series, agg_lvl, prob, horiz, obs_value, prob_fcast)
  
  WSPL_by_ts <- eval_info_quantiles %>%
    mutate(SPL = (prob_fcast - obs_value)*( (prob_fcast > obs_value) - prob) ) %>%
    left_join(naive_errors, by = "time_series") %>%
    mutate(SPL = SPL/naive_error) %>%
    group_by(time_series, prob) %>%
    dplyr::summarise(SPL = mean(SPL) ) %>%
    ungroup() %>% 
    left_join(weights_df, by="time_series") %>%
    mutate(WSPL = SPL*weight) %>% 
    dplyr::select(time_series, agg_lvl, prob, SPL, WSPL) %>% 
    relocate(time_series, agg_lvl, prob, SPL, WSPL) %>% 
    suppressMessages()
  
  WSPL_by_agg_level <- WSPL_by_ts %>%
    group_by(agg_lvl) %>%
    dplyr::summarise(WSPL = sum(WSPL)/9 ) %>%
    ungroup() %>%
    suppressMessages()
  
  WSPL <- WSPL_by_agg_level %>% 
    pull(WSPL) %>% 
    mean(.)
  
  WSPL_by_quantile <- WSPL_by_ts %>%
    group_by(prob) %>%
    dplyr::summarise(WSPL = sum(WSPL)/12 ) %>%
    ungroup() %>%
    suppressMessages()
  
  # mean(WSPL_by_quantile$WSPL)
  # mean(WSPL_by_agg_level$WSPL)
  
  RF_by_ts <- eval_info_quantiles %>%
    mutate(RF = obs_value < prob_fcast) %>%
    group_by(time_series, agg_lvl, prob) %>%
    dplyr::summarise(RF = mean(RF) ) %>%
    ungroup() %>%
    suppressMessages()
  
  RF_by_quantile <- RF_by_ts %>% 
    group_by(prob) %>%
    dplyr::summarise(RF = mean(RF) ) %>%
    ungroup() %>%
    suppressMessages()
  
  RF_by_agg_level <- RF_by_ts %>% 
    group_by(agg_lvl, prob) %>%
    dplyr::summarise(RF = mean(RF) ) %>%
    ungroup() %>%
    suppressMessages()
  
  PICP_by_PI <- eval_info_quantiles %>% 
    dplyr::filter(prob != 0.5) %>% 
    mutate(PI = case_when(
      prob %in% c(0.25, 0.75) ~ 50,
      prob %in% c(0.165, 0.835) ~ 67,
      prob %in% c(0.025, 0.975) ~ 95,
      prob %in% c(0.005, 0.995) ~ 99
    ) ) %>% 
    mutate(inf_sup = if_else(prob < 0.5, "Lower", "Upper") ) %>% 
    dplyr::select(-prob) %>% 
    pivot_wider(names_from = inf_sup, values_from = prob_fcast) %>% 
    mutate(PICP = (obs_value <= Upper) & (obs_value >= Lower) ) %>%
    group_by(PI) %>%
    dplyr::summarise(PICP = mean(PICP) ) %>%
    ungroup() %>%
    suppressMessages()
  
  prob_fcast_results <- list(WSPL,
                             WSPL_by_agg_level,
                             WSPL_by_quantile,
                             RF_by_agg_level,
                             RF_by_quantile,
                             PICP_by_PI)
  
  names(prob_fcast_results)<-c("WSPL", "WSPL_by_agg_level", "WSPL_by_quantile", "RF_by_agg_level", "RF_by_quantile", "PICP_by_PI")
  
  return(prob_fcast_results)
  
}
