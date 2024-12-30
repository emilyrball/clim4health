#############################################################################
# Helper Functions for Internal Checks 
#############################################################################

#' get_time_interval
#'
#' This function determines the time interval (e.g., daily, weekly, monthly, yearly)
#' of a dataset based on the differences between the first two time points.
#'
#' @param data an object of class 'data.frame' containing equally spaced
#'             time series data (daily, weekly, monthly data) for one or multiple spatial units or groups.
#' @param time A string representing the name of the column in `data` that contains time information.
#'             The column should be in "yyyy-mm-dd" format.
#' @param area (Optional) A string representing the name of the column in `data`
#'             that contains spatial or grouping information. Required if `data` contains multiple spatial units.
#' @return A character string indicating the time interval of the dataset. Possible values:
#'         "day", "week", "month", "year", or "unknown".

get_time_interval <- function(data = NULL, time = NULL, area = NULL) {
  
  # Check if 'data' and 'time' are provided
  if (is.null(data) || is.null(time)) {
    stop("'data' and 'time' must be provided.")
  }
  
  # Check if there are no duplicated dates
  # If no 'area' is provided, ensure there are no duplicate time entries
  if (is.null(area)) {
    if ((length(unique(data[[time]])) != nrow(data))) {
      stop(paste(
        "Duplicated time units detected. If the data contains multiple
        spatial units then the 'area' argument is required"
      ))
    }
  }
  
  # Sort data by 'area' and 'time'
  if(!is.null(area)) {
    data <- data[order(data[[area]], data[[time]]), ]
    date_vector <- as.Date(data[[time]])
  } else {
    data <- data[order(data[[time]]), ]
    if(is.vector(data)) {
      date_vector <- as.Date(data)
    } else {
      date_vector <- as.Date(data[[time]])
    }
  }
  
  # Check for failed date conversion
  if (any(is.na(date_vector))) {
    stop("Date conversion failed. Ensure 'time' column is in 'yyyy-mm-dd' format.")
  }
  
  # Calculate difference between the first two dates
  # This assumes the data is sorted and differences can be used to infer the interval
  date_diffs <- diff(date_vector[1:2])
  
  # Determine the time interval based on the difference
  # Recognize intervals as "day", "week", "month", "year", or "unknown"
  if (date_diffs == 1) {
    t <- "day"
  } else if (date_diffs == 7) {
    t <- "week"
  } else if (date_diffs >= 28 && date_diffs <= 31) {
    t <- "month"
  } else if (date_diffs >= 350 && date_diffs <= 370) {
    t <- "year"
  } else {
    t <- "unknown"
  }
  
  # Return the identified time interval as a string
  return(t)
}


#' check_consecutive
#'
#' This function verifies whether time values in a dataset are consecutive,
#' based on the detected time interval (e.g., daily, weekly, monthly, yearly).
#' If an `area` column is specified, the check is performed within each spatial unit.
#'
#' @param data A dataframe containing the dataset. Must include the time variable
#'             and optionally an area variable.
#' @param time A string representing the name of the column in `data` that contains time values.
#'             The column should be in "yyyy-mm-dd" format.
#' @param area (Optional) A string representing the name of the column in `data`
#'             that contains spatial or grouping information. If provided, the check will
#'             be performed separately for each spatial unit.
#'
#' @return A logical value: `TRUE` if all time values (within each spatial unit if `area` is specified)
#'         are consecutive, and `FALSE` otherwise.


check_consecutive <- function (data=NULL, time = NULL, area = NULL){
  
  # Check if 'data' and 'time' are provided
  if (is.null(data) || is.null(time)) {
    stop("'data' and 'time' must be provided.")
  }
  
  # Check for duplicate time values when 'area' is not provided
  if (is.null(area)) {
    if ((length(unique(data[[time]])) != nrow(data))) {
      stop(paste(
        "Duplicated time units detected. If the data contains multiple
        spatial units then the 'area' argument is required"
      ))
    }
  }
  
  # Subset relevant columns (area and time)
  # Use tidy evaluation to handle dynamic column selection
  data <- data %>% dplyr::select(area = {{area}}, time = {{time}})
  
  # Check consecutiveness within each 'area' if specified
  if(!is.null(area)) {
    
    # Determine the time interval (e.g., day, week, month, year)
    interval = get_time_interval(data, time = "time", area = "area")
    
    # Calculate differences and check consecutiveness for each 'area'
    is_consecutive <- data %>%
      dplyr::arrange(area, time) %>%
      dplyr::group_by(area) %>%
      dplyr::mutate(date_diffs = as.numeric(difftime(time,
                                                     dplyr::lag(time),
                                                     units = "days"))) %>% 
      # Remove NA values
      dplyr::filter(!is.na(date_diffs)) %>%
      # Create a new column `all_consecutive` summarizing whether all date differences for an 'area' are valid
      dplyr::summarize(all_consecutive = dplyr::case_when(
        interval == "day" ~ all(date_diffs == 1),
        interval == "week" ~ all(date_diffs == 7),
        interval == "month" ~ all(date_diffs >= 28 & date_diffs <= 31),
        interval == "year" ~ all(date_diffs >= 350 & date_diffs <= 370),
        TRUE ~ FALSE
      )) %>%
      dplyr::pull(all_consecutive)
    
    # Combine results across all areas
    is_consecutive <- all(is_consecutive)
    
    # Check consecutiveness when 'area' is not specified
  } else {
    
    # Determine the time interval (e.g., day, week, month, year)
    interval = get_time_interval(data = data, time = "time")
    
    # Calculate differences and check consecutiveness for the whole dataset
    is_consecutive <- data %>%
      dplyr::arrange(time) %>%
      dplyr::mutate(date_diffs = as.numeric(difftime(time,
                                                     dplyr::lag(time),
                                                     units = "days"))) %>% 
      # Remove NA values
      dplyr::filter(!is.na(date_diffs)) %>%
      # Create a new column `all_consecutive` summarizing whether all date differences are valid
      dplyr::summarize(all_consecutive = dplyr::case_when(
        interval == "day" ~ all(date_diffs == 1),
        interval == "week" ~ all(date_diffs == 7),
        interval == "month" ~ all(date_diffs >= 28 & date_diffs <= 31),
        interval == "year" ~ all(date_diffs >= 350 & date_diffs <= 370),
        TRUE ~ FALSE
      )) %>%
      dplyr::pull(all_consecutive)
    
    # Combine results
    is_consecutive <- all(is_consecutive)
  }
  
  return(is_consecutive)
}


#############################################################################
# Helper Functions For writing R-INLA formulas  
#############################################################################


#' write_re_inla
#'
#' @param id the name of the variable representing the units for which
#' the random effects should be specified.
#' @param re character, it defines the prior for the random effects.
#' Accepted arguments are "iid", "rw1", "rw2", "rw1c", "rw2c", "bym", "bym2".
#' @param rep the name of the variable for which the random effect should
#' be replicated.
#' @param graph the name of the graph with adjacency matrix in case a spatially
#' structured random effect is specified (e.g., "bym", bym2").
#'
#' @return a INLA formula for the random effect
#'

write_re_inla <- function(re = "",
                          id = "",
                          rep = NULL,
                          hyper = NULL,
                          graph = NULL) {
  
  if (re == "" || id == "") {
    stop("'re' and/or 'id' are missing")
  }
  if ((re == "bym" || re == "bym2") && is.null(graph)) {
    stop("'graph' is missing")
  } else {
    if (re == "rw1") {
      if (is.null(rep)) {
        re_inla <- paste("f(", id, ", model = 'rw1', constr = TRUE, scale.model = TRUE", if (!is.null(hyper)) paste(", hyper =", hyper), ")")
      } else {
        re_inla <- paste("f(", id, ", model = 'rw1', constr = TRUE, scale.model = TRUE, replicate =", rep, if (!is.null(hyper)) paste(", hyper =", hyper), ")")
      }
    } else if (re == "rw2") {
      if (is.null(rep)) {
        re_inla <- paste("f(", id, ", model = 'rw2', constr = TRUE, scale.model = TRUE", if (!is.null(hyper)) paste(", hyper =", hyper), ")")
      } else {
        re_inla <- paste("f(", id, ", model = 'rw2', constr = TRUE, scale.model = TRUE, replicate =", rep, if (!is.null(hyper)) paste(", hyper =", hyper), ")")
      }
    } else if (re == "rw1c") {
      if (is.null(rep)) {
        re_inla <- paste("f(", id, ", model = 'rw1', cyclic = TRUE, constr = TRUE, scale.model = TRUE", if (!is.null(hyper)) paste(", hyper =", hyper), ")")
      } else {
        re_inla <- paste("f(", id, ", model = 'rw1', cyclic = TRUE, constr = TRUE, scale.model = TRUE, replicate =", rep, if (!is.null(hyper)) paste(", hyper =", hyper), ")")
      }
    } else if (re == "rw2c") {
      if (is.null(rep)) {
        re_inla <- paste("f(", id, ", model = 'rw2', cyclic = TRUE, constr = TRUE, scale.model = TRUE", if (!is.null(hyper)) paste(", hyper =", hyper), ")")
      } else {
        re_inla <- paste("f(", id, ", model = 'rw2', cyclic = TRUE, constr = TRUE, scale.model = TRUE, replicate =", rep, if (!is.null(hyper)) paste(", hyper =", hyper), ")")
      }
    } else if (re == "iid") {
      if (is.null(rep)) {
        re_inla <- paste("f(", id, ", model = 'iid'", if (!is.null(hyper)) paste(", hyper =", hyper), ")")
      } else {
        re_inla <- paste("f(", id, ", model = 'iid', replicate =", rep, if (!is.null(hyper)) paste(", hyper =", hyper), ")")
      }
    } else if (re == "bym") {
      if (is.null(rep)) {
        re_inla <- paste("f(", id, ", model = 'bym', scale.model = TRUE, graph =", graph, if (!is.null(hyper)) paste(", hyper =", hyper), ")")
      } else {
        re_inla <- paste("f(", id, ", model = 'bym', scale.model = TRUE, graph =", graph, ", replicate =", rep, if (!is.null(hyper)) paste(", hyper =", hyper), ")")
      }
    } else if (re == "bym2") {
      if (is.null(rep)) {
        re_inla <- paste("f(", id, ", model = 'bym2', scale.model = TRUE, graph =", graph, if (!is.null(hyper)) paste(", hyper =", hyper), ")")
      } else {
        re_inla <- paste("f(", id, ", model = 'bym2', scale.model = TRUE, graph =", graph, ", replicate =", rep, if (!is.null(hyper)) paste(", hyper =", hyper), ")")
      }
    } else {
      stop("random effects must be iid, rw1, rw2, rw1c, rw2c, bym, bym2")
    }
  }
  return(re_inla)
}


#' non_linear_eff_inla
#'
#' @param predictors a vector or a matrix containing the names of the variables
#' to be expressed as non linear term in INLA.
#' @param model character, defines the model for the non linear effect ("rw1",
#'  "rw2").
#' @param method specifies the method to divide the linear covariate ("cut",
#' "quantile")
#' @param n numeric, the number of breaks to be used in the method
#' (default n=10)
#'
#' @return a vector or a matrix of predictors rexpressed as non linear terms for
#' inla models.
#'

non_linear_eff_inla <- function(predictors = NULL,
                                model = "rw2",
                                method = "cut",
                                n = 10) {
  method <- paste0("'", method, "' , n = ", n)
  
  if (is.vector(predictors)) {
    predictors_nl <- unlist(lapply(
      predictors,
      function(x) {
        paste0("f(INLA::inla.group(", x, ", method= ", method, "), model= '", model, "')")
      }
    ))
  } else if (is.matrix(predictors)) {
    predictors_nl <- apply(predictors,
                           MARGIN = c(1, 2),
                           FUN = function(x) {
                             paste0("f(INLA::inla.group(", x, ", method= ", method, "), model= '", model, "')")
                           }
    )
  }
  return(predictors_nl)
}

#############################################################################
# Helper Functions for Fitting R-INLA models  
#############################################################################

#' Fit single INLA model

#' @param forms character, a formula for INLA
#' @param family charachter, a likelihood family accepted by INLA
#' @param offset character, name of the variable representing the offset
#' @param config logical

#' @return a fitted INLA model object
#'

fit_single_model <- function(forms = NULL,
                             family = "nbinomial",
                             offset = "offset",
                             data = NULL,
                             config = FALSE,
                             nthreads = 8) {
  INLA::inla(
    stats::as.formula(forms),
    family = family,
    offset = log(offset),
    control.inla = list(strategy = "adaptive"),
    control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE, config = config),
    control.predictor = list(link = 1, compute = TRUE),
    verbose = FALSE,
    INLA::inla.setOption(num.threads = nthreads),
    control.fixed = list(correlation.matrix = FALSE),
    data = data
  )
}

#############################################################################
# Helper Functions for Extracting information from fitted R-INLA models  
#############################################################################
#' Extract the random effects variance form the INLA model

#' @param model an inla model output
#' @return model random effects variance 
#' 


extract_re_var_inla <- function(model = NULL,
                                formulas = NULL) {
  
  #Extract summary of hyperparameters 
  hyperpar <- model[["summary.hyperpar"]]
  
  # Add row names as a column for filtering
  hyperpar$Parameter <- rownames(hyperpar)
  
  # Filter rows starting with "Precision" and extract the "Parameter" and "mode" columns
  precision_modes <- hyperpar[grepl("^Precision", hyperpar$Parameter) & 
                                !grepl("INLA::inla.group", hyperpar$Parameter),
                              c("mode")]
  variance_re <-  1/precision_modes
  
  col_names<-paste0(names(formulas$re),"_var")
  n_re <- length(names(formulas$re))
  
  out <- as.data.frame(matrix(ncol = n_re,nrow = 1)) 
  names(out) <- col_names
  out[1,] <- variance_re
  
  return(out)
}

#' Extract Deviance Information Criterium (DIC) from INLA Model

#' @param model an inla model output
#' @param ci logical, if 95% confidence intervals are required 
#' @return model DIC
#' 


extract_DIC_inla <- function(model = NULL,
                             ci = TRUE,
                             p = 0.99) {
  
  # Set z-score (for now for CIs of 95%)
  z.score <- 1.96
  
  # Extract DIC value
  dic <- model[["dic"]][["dic"]]
  
  n <- length(model[["dic"]][["local.dic"]])
  
  # # Extract std. error of the point-wise DIC
  s.e <- sqrt(
    n * var(
      model[["dic"]][["local.dic"]],
      na.rm = TRUE
    )
  )
  
  if(ci){
    out <- data.frame(
      dic = dic,
      dic_lci = dic - s.e * z.score, # Lower CI from point-wise DIC
      dic_uci = dic + s.e * z.score # Upper CI from point-wise DIC
    )
  } else{
    out <- data.frame(
      dic = dic
    )
  }
  
  return(out)
}

#' Extract the Watanabe–Akaike information criterion (WAIC) from INLA Model
#' 
#' @param model an inla model output
#' @param ci logical, if confidence intervals are required 
#' @param p numeric, indicating the width of the confidence intervall
#' @return model DIC

extract_WAIC_inla <- function(model,
                              ci = TRUE,
                              p = 0.99) {
  
  # Set z-score (for now for CIs of 95% and 99%)
  z.score <- 1.96
  
  # Extract waic value
  waic <- model[["waic"]][["waic"]]
  
  n <- length(model[["waic"]][["local.waic"]])
  
  # Extract std. error of the point-wise waic
  s.e <- sqrt(
    n * var(
      model[["waic"]][["local.waic"]],
      na.rm = TRUE
    )
  )
  
  if(ci){
    out <- data.frame(
      waic = waic,
      waic_lci = waic - s.e * z.score, # Lower CI from point-wise waic
      waic_uci = waic + s.e * z.score # Upper CI from point-wise waic
    )
  } else{
    out <- data.frame(
      waic = waic
    )
  }
  
  return(out)
}


#' Extract Log Mean Score (LMS) from INLA Model

#' @param model an inla model output
#' @return model log_mean_score
#' @export
#'

extract_LMS_inla <- function(model) {
  lms <- mean(-log(model[["cpo"]][["cpo"]]))
  
  out <- data.frame(
    lms = lms
    )
  
  return(out)
}

#' Extract Mean Absolute Error (MAE) from INLA Model

#' @param model a fitted inla model
#' @param data the dataframe used to fit the model.
#' @param cases character, specifying the name of variable for cases
#' @return model MAE
#'

extract_MAE_inla <- function(model = NULL,
                             data = NULL,
                             cases = "") {
  abs_error <- abs(
    model[["summary.fitted.values"]][["0.5quant"]] - data[[cases]]
  )
  
  mae <- mean(abs_error, na.rm = TRUE)
  
  out <- data.frame(
    mae = mae
  )
  
  return(out)
  
}


#' Extract Root Mean Squared Error (RMSE) from INLA Model

#' @param model a fitted inla model
#' @param data the dataframe used to fit the model.
#' @param cases character, specifying the name of variable for cases
#' @return model RMSE
#' 

extract_RMSE_inla <- function(model = NULL,
                              data = NULL,
                              cases = "") {
  observed <- data[[cases]]
  predicted <- model[["summary.fitted.values"]][["0.5quant"]]
  sq_errors <- (observed - predicted)^2
  mean_squared_error <- mean(sq_errors)
  rmse <- sqrt(mean_squared_error)
  
  out <- data.frame(
    rmse = rmse
  )
  
  return(out)
}



#' Compute Continuous Ranked Probability Score (CRPS) from Empirical 
#' Distribution Function (EDF). It evaluates the accuracy of a predictive
#' distribution against observed values.
#'
#' @param obs A numeric vector of observed values.
#' @param pred A matrix where each row contains probabilistic predictions for 
#' the corresponding observed value in `obs`. 
#' @return A numeric vector containing the CRPS for each observed value.
#' 

crps_edf <- function(obs, pred) {
  # Check that the number of rows in `pred` matches the length of `obs`
  if (nrow(pred) != length(obs)) {
    stop("In CRPS evaluation the number of rows in `pred` must match the length of `obs`.")
  }
  
  # CRPS computation for each observation
  sapply(seq_along(obs), function(i) {
    
    # Sort predictions to compute the EDF for the current observation
    sorted_pred <- sort(pred[i, ])
    
    # Normalization constant for the EDF
    n <- length(sorted_pred)
    c_1n <- 1 / n  
    #Compute the EDF probabilities 
    edf_probs <- seq(0.5 * c_1n, 1 - 0.5 * c_1n, length.out = n)  
    
    # Compute CRPS for the current observation
    sum(2 * c_1n * ((obs[i] < sorted_pred) - edf_probs) * (sorted_pred - obs[i]))
  })
}

#' Calculate CRPS
#' @param model a fitted inla model
#' @param data the dataframe used to fit the model.
#' @param cases character, specifying the name of variable for cases
#' @param observations number of observation in the dataset
#' @return model crps
#'

extract_CRPS_inla <- function(model = NULL,
                              data = NULL,
                              cases = "") {
  # retrieve observed 
  observed <- data[[cases]]
  
  # Sample from the posterior
  s <- 1000
  xx <- INLA::inla.posterior.sample(s, model)
  
  # Extract the overdispersion parameter 
    theta <- unlist(
      lapply(
        xx,
        function(x) as.numeric(x[["hyperpar"]][
          grepl("overdispersion", names(x[["hyperpar"]]))
        ])
      )
    )
    
    # Obtain the expectation of counts per row
    latent <- lapply(
      xx,
      function(x) as.numeric(x[["latent"]][
        grepl("Predictor", rownames(x[["latent"]]))
      ])
    )
    
    # Create posterior predictive sample from a negative binomial distribution
    y_pred <- matrix(NA, length(observed), s)
    for(s.idx in 1:s) {
      y_pred[, s.idx] <- rnbinom(length(observed),
                                 mu = exp(latent[[s.idx]]), # Predicted means
                                 size = theta[s.idx]) # Overdispersion parameter
      }
  
  # Compute the overall CRPS 
  crps <- median(crps_edf(obs = observed, pred =  y_pred))
  
  out <- data.frame(
    crps = crps
  )
  
  return(crps)
}

#' out_models
#'
#' @param fitted_models an R-INLA model object 
#' @param data
#' @param formulas
#' @param crps
#' @param ci
#' @export
#' @return an object with elements:
#    -  mod.gof  : dataframe of GOF metrics (vars, dic, mae.mean, mae.median, formula)
#    -  fitted   : list of fitted values (length = number of models)
#    -  fixed    : list of fixed effects (length = number of models)
#    -  random   : list of random effects (length = number of models)


out_models <- function(
    fitted_models = NULL,
    data = NULL,
    formulas = NULL,
    crps = FALSE,
    ci = TRUE) {
  
  outcome <- formulas$outcome[[1]]
  
  mod.out <- list(
    mod.gof = dplyr::bind_cols(
      extract_re_var_inla(model = fitted_models, formulas = formulas),
      extract_DIC_inla(model = fitted_models),
      extract_WAIC_inla(model = fitted_models),
      extract_LMS_inla(model = fitted_models),
      extract_MAE_inla(model = fitted_models, data = data, cases = outcome),
      extract_RMSE_inla(model = fitted_models, data = data, cases = outcome)
    )
  )
  
  
  if (crps == TRUE) {
    
    mod.out[["mod.gof"]][["crps"]] <- extract_CRPS_inla(
      model = fitted_models,
      data = data,
      cases = outcome
      )
  }
  
  return(mod.out)
}


