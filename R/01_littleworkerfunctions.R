#-----------------------------------------------------------------------------#
#### shells to Stata to estimate lasso regression for selecting variables

countrymodel_select_stata <- function(dt,
                                      xvars,
                                      y,
                                      weights,
                                      selection = "BIC",
                                      stata_path,
                                      stata_vnum){

  dt <- as.data.table(dt)

  if(is.null(weights) == FALSE){

    dt <- na.omit(dt[,c(y, xvars, weights, "ea_id"), with = F])

    weights <- dt[, weights, with = F]

    #weights <- scale(weights)

  } else {

    dt <- na.omit(dt[,c(y, xvars, "ea_id"), with = F])

    weights <- 1

  }

  xset <- dt[, xvars, with = F]

  y <- dt[, y, with = F]

  df <- as.data.frame(dt)
  names(df) <- abbreviate(names(df), minlength = 32)

  x <- colnames(xset)

  haven::write_dta(data = df, path = "./data.dta")

  options("RStata.StataPath" = stata_path)
  options("RStata.StataVersion" = stata_vnum)

  stata_src <- c("use data.dta, replace",
                 paste0("lassowrapper ", names(y) , " ", x[1], "-",
                        x[length(x)],", weights(hhweight) force(areadummy*) select(bic, postsel) cluster(ea_id) input(data.dta) output(model.txt)"))

  RStata::stata(src = stata_src)

  var_list <- readLines("model.txt")

  ### separate the long string by space

  var_list <- strsplit(var_list, " +")[[1]]

  var_list <- var_list[!(grepl("o.areadummy", var_list))]

  var_list <- stringr::str_replace_all(var_list, "covrfrctn", "coverfraction")
  var_list <- stringr::str_replace_all(var_list, "coverfrctn", "coverfraction")

  return(var_list)

}


#-----------------------------------------------------------------------------#
### Function to realize descriptive statistics


ebp_test_means <- function(smp_data,
                           pop_data,
                           varlist,
                           smp_weights,
                           pop_weights)
{

  ### get the set of complete cases of the variables as
  ### would have been used in model estimation
  smp_df <- smp_data[complete.cases(c(varlist, smp_weights)),
                     c(varlist, smp_weights)]
  pop_df <- pop_data[complete.cases(c(varlist, pop_weights)),
                     c(varlist, pop_weights)]


  smp_means_df <- data.frame(smp_means = colMeans(smp_df[,varlist]),
                             smp_sd = apply(X = smp_df[,varlist],
                                            MARGIN = 2,
                                            FUN = sd,
                                            na.rm = TRUE),
                             variable = varlist)

  pop_means_df <- data.frame(pop_means = colMeans(pop_df[,varlist]),
                             pop_sd = apply(X = pop_df[,varlist],
                                            MARGIN = 2,
                                            FUN = sd,
                                            na.rm = TRUE),
                             variable = varlist)

  means_df <- merge(smp_means_df, pop_means_df, by = "variable")
  means_df$diff_sd <- sqrt((means_df$smp_sd)^2 + (means_df$pop_sd)^2)
  means_df$diff <- means_df$pop_means - means_df$smp_means
  means_df$zscore <- means_df$diff / means_df$diff_sd
  means_df$pvalue <- 2 * (1 - pnorm(abs(means_df$zscore)))
  return(means_df[, c("variable", "smp_means", "pop_means", "diff", "pvalue")])

}


#----------------------------------------------------------------------------#
# Modified version of the ebp_test_means() function that compares the means for a variable


compar_test_means<- function(smp_data,
                             pop_data,
                             var,
                             smp_weights,
                             pop_weights)
{

  ### get the set of complete cases of the variables as
  ### would have been used in model estimation
  smp_df <- smp_data[complete.cases(c(var, smp_weights)),
                     c(var, smp_weights)]
  pop_df <- pop_data[complete.cases(c(var, pop_weights)),
                     c(var, pop_weights)]

  smp_means_df <- data.frame(smp_means = colMeans(smp_df[var]),
                             smp_sd = apply(X = smp_df[var],
                                            MARGIN = 2,
                                            FUN = sd,
                                            na.rm = TRUE),
                             variable = var)

  pop_means_df <- data.frame(pop_means = colMeans(pop_df[var]),
                             pop_sd = apply(X = pop_df[var],
                                            MARGIN = 2,
                                            FUN = sd,
                                            na.rm = TRUE),
                             variable = var)

  means_df <- merge(smp_means_df, pop_means_df, by = "variable")
  means_df$diff_sd <- sqrt((means_df$smp_sd)^2 + (means_df$pop_sd)^2)
  means_df$diff <- means_df$pop_means - means_df$smp_means
  means_df$zscore <- means_df$diff / means_df$diff_sd
  means_df$pvalue <- 2 * (1 - pnorm(abs(means_df$zscore)))

  return(means_df[, c("variable", "smp_means", "pop_means", "diff", "pvalue")])

}

#----------------------------------------------------------------------------#
### Function to create dummy variables from categorical variables

dummify <- function(x) {
  if(is.matrix(x) || is.data.frame(x)) {
    x <- as.data.frame(x)
    y <- do.call(data.frame, lapply(x, dummify))
    return(as.matrix(y))
  }
  # x is 1-dimensional
  if(is.complex(x))
    return(as.matrix(data.frame(Re=Re(x), Im=Im(x))))
  # convert factors etc
  if(is.character(x))
    x <- factor(x)
  if(is.logical(x))
    x <- factor(x, levels=c(FALSE,TRUE))
  if(is.factor(x)) {
    # convert to dummy variables
    nx <- length(x)
    lev <- levels(x)
    y <- matrix(0L, nrow=nx, ncol=length(lev))
    colnames(y) <- lev
    y[cbind(seq_len(nx), as.integer(x))] <- 1L
    return(y)
  }
  # convert to numeric
  y <- as.numeric(x)
  if(!is.matrix(y))
    y <- matrix(y, ncol=1)
  return(y)
}


#----------------------------------------------------------------------------#
#Function to determine variables with 100% missing values
missing_var <- function(dt)
{
  df <- data.frame(dt)
  missing_count <- colSums(is.na(df))
  missing_cols <- which(missing_count == nrow(df))
  names(df[, missing_cols])
}


#----------------------------------------------------------------------------#
# Function to determine common variables between databases

cand_var <- function(pop_dt, smp_dt, oth_var) {
  pop_df <- data.frame(pop_dt)
  smp_df <- data.frame(smp_dt)
  common_vars <- intersect(names(pop_df),names(smp_df))
  res <- common_vars[!(common_vars %in% oth_var)]
  fin_res <- intersect(res[!(res %in% missing_var(smp_df))],
                       res[!(res %in% missing_var(pop_df))])
  return(fin_res)
}


#----------------------------------------------------------------------------#
# Function to plot histograms

hist_plot <- function(dt, var, weights, col)
{

  dt[[weights]] <- as.numeric(dt[[weights]])
  dt[[var]] <- as.numeric(dt[[var]])
  y <- as.data.frame(wtd.table(x = dt[[var]], weights = dt[[weights]]), col.names = c(var,"Proportion"))
  y[,2] <- prop.table(y[,2])
  hist <- ggplot(y, aes(x = !!sym(var), y=Proportion,fill="Census")) +
    geom_bar(stat = "identity") +
    xlab(paste("variable ", var, sep="")) +
    ylab("Proportion") +
    scale_fill_manual(values = col)+
    labs(fill="") +
    ggtitle(paste("Histogram of variable ", var, sep=""))

  return(hist)

}




ebp_compute_cv <- function(model,
                           calibvar = NULL,
                           boot_type = "calibrate",
                           designvar = NULL,
                           threshold = NULL,
                           B = model$call$B){



  if (is.null(model$call$weights)) {
    model$framework$smp_data$weights <- rep(1, nrow(model$framework$smp_data))
    model$call$weights <- "weights"
  }

  if (is.null(threshold)) {
    threshold <- 0.6 * median(model$framework$smp_data[[paste(model$fixed[2])]])
    message(strwrap(prefix = " ", initial = "",
                    paste0("The threshold for the HCR and the PG is
                          automatically set to 60% of the median of the
                          dependent variable and equals ", threshold)))
  }

  # Direct Estimate with calibration : Mean and CV
  if(!is.null(calibvar)) {
    calibmatrix <- povmap:::create_calibmatrix(model$framework$smp_data[[calibvar]])

    direct_calib <- povmap::direct(y = as.character(model$fixed[[2]]),
                                   smp_data = model$framework$smp_data,
                                   smp_domains = model$framework$smp_domains,
                                   weights = model$call$weights,
                                   design = designvar, threshold = threshold,
                                   var = TRUE, boot_type = boot_type,
                                   X_calib = calibmatrix, totals = NULL,
                                   na.rm = TRUE, B = B)


    direct_calib <-
      data.frame(Domain = direct_calib$ind$Domain,
                 CB_Head_Count_CV = sqrt(direct_calib$MSE$Head_Count) /
                   direct_calib$ind$Head_Count)
  }


  # HT estimator CV for direct estimate
  direct_ht <- povmap::direct(y = as.character(model$fixed[[2]]),
                              smp_data = model$framework$smp_data,
                              smp_domains = model$framework$smp_domains,
                              weights = model$call$weights,
                              threshold = threshold,
                              var = TRUE, na.rm = TRUE, HT = T)

  direct_ht <-
    data.frame(Domain = direct_ht$ind$Domain,
               Direct_Head_Count = direct_ht$ind$Head_Count,
               HT_Head_Count_CV = sqrt(direct_ht$MSE$Head_Count) /
                 direct_ht$ind$Head_Count)

  # Compute design effect controlled direct estimates and CVs. (direct CV3)
  ## first estimate naive bootstrap, than compute design effect and include psu
  ## list into the ebp data object
  model$framework$smp_data$poor <-
    as.integer(model$framework$smp_data[[as.character(model$fixed[[2]])]] <
                 threshold)
  model$framework$smp_data$weights <-
    model$framework$smp_data[[model$call$weights]]

  if(is.null(designvar)){

    ebpobj_svy <- survey::svydesign(ids = ~1, weights = ~weights, strata = NULL,
                                    survey.lonely.psu = "adjust",
                                    data = model$framework$smp_data)

  } else {

    model$framework$smp_data$designvar <- model$framework$smp_data[[designvar]]

    ebpobj_svy <- survey::svydesign(ids = ~1, weights = ~weights,
                                    strata = ~designvar,
                                    survey.lonely.psu = "adjust",
                                    data = model$framework$smp_data)

  }

  deff_adjust <- survey::svymean(x = ~poor, ebpobj_svy, na = TRUE, deff = TRUE)
  deff_adjust <- attr(deff_adjust, "deff")[1,1]

  direct_naive <- povmap::direct(y = as.character(model$fixed[[2]]),
                                 smp_data = model$framework$smp_data,
                                 smp_domains = model$framework$smp_domains,
                                 design = designvar, weights = model$call$weights,
                                 threshold = threshold, var = TRUE, B = B)

  direct_naive <-
    data.frame(Domain = direct_naive$ind$Domain,
               DesignEffect_CV = sqrt(direct_naive$MSE$Head_Count * deff_adjust) /
                 direct_naive$ind$Head_Count)

  # get values for table
  emdi_dt <- povmap::estimators(object = model, indicator = "Head_Count",
                                MSE = FALSE, CV = TRUE)
  result_dt <- emdi_dt$ind
  colnames(result_dt) <- c("Domain", "EBP_Head_Count", "EBP_Head_Count_CV")

  if (!is.null(calibvar)) {
    result_dt <- merge(result_dt, direct_calib, by = "Domain", all = TRUE)
  }
  result_dt <- merge(result_dt, direct_ht, by = "Domain", all = TRUE)
  result_dt <- merge(result_dt, direct_naive, by = "Domain", all = TRUE)

  if (is.null(calibvar)) {
    result_dt <- result_dt[,c("Domain", "Direct_Head_Count", "EBP_Head_Count",
                              "HT_Head_Count_CV", "DesignEffect_CV",
                              "EBP_Head_Count_CV")]
  } else {
    result_dt <- result_dt[,c("Domain", "Direct_Head_Count", "EBP_Head_Count",
                              "HT_Head_Count_CV", "CB_Head_Count_CV",
                              "DesignEffect_CV", "EBP_Head_Count_CV")]
  }

  return(result_dt)
}



lassoebp_vselect <- function(dt,
                             yvar,
                             candidate_vars,
                             lambda_min = 0,
                             lambda_max = 500,
                             by = 5,
                             domain,
                             scale = FALSE,
                             seed = 1909,
                             folds = 5,
                             family = poisson(link = "log"),
                             return_onlyvars = TRUE,
                             maxIter = 200,
                             epsilon = 1e-4,
                             eps.final = 1e-4){

  set.seed(seed)

  N <- dim(dt)[1]

  ind <- sample(N, N)

  lambda_vector <- seq(lambda_min, lambda_max, by = by)

  nk <- floor(N / folds)

  dev_matrix <- matrix(Inf, ncol = folds, nrow = length(lambda_vector))

  ## first fit good starting model

  bic_vector <- rep(Inf, length(lambda_vector))

  ## first, fit good starting model

  pql_formula <- as.formula(paste(yvar, 1, sep = " ~ "))

  dt$domain <- dt[[domain]]

  if (scale == TRUE){

    dt[, candidate_vars] <- scale(dt[, candidate_vars])

  }


  pql <-
    glmmPQL(fixed = pql_formula,
            random = ~1 | domain,
            family = family,
            data = dt)

  delta_start <- as.matrix(t(c(as.numeric(pql$coeff$fixed),
                               rep(0, length(candidate_vars)),
                               as.numeric(t(pql$coef$random$domain)))))

  q_start <- as.numeric(VarCorr(pql)[1, 1]) ## get variance of the random effect

  ### formula to be used by glmmLasso function
  glmlasso_formula <- as.formula(paste(paste0(yvar, " ~ "),
                                       paste(candidate_vars,
                                             collapse = " + ")))


  ## loop over the folds
  for (i in 1:folds) {

    print(paste("CV Loop ", i, sep = ""))

    if (i < folds) {

      indi <- ind[(i-1) * nk + (1 : nk)]

    } else {

      indi <- ind[((i-1) * nk + 1) : N]

    }

    train_dt <- dt[-indi,]
    test_dt <- dt[indi,]

    delta_temp <- delta_start
    q_temp <- q_start

    ## loop over lambda grid
    for(j in 1:length(lambda_vector))
    {
      #print(paste("Lambda Iteration ", j,sep=""))

      glm_model <- try(glmmLasso(glmlasso_formula,
                                 rnd = list(domain = paste(candidate_vars,
                                                           collapse = " + ")),
                                 family = family,
                                 data = train_dt,
                                 lambda = lambda_vector[j],
                                 switch.NR = FALSE,
                                 final.re = FALSE,
                                 control = list(start = delta_temp[j,],
                                                q_start = q_temp[j])),
                       silent=TRUE)

      if(!inherits(glm_model, "try-error")) {

        yhat <- predict(glm_model, test_dt)
        delta_temp <- rbind(delta_temp,
                            glm_model$Deltamatrix[glm_model$conv.step,])

        q_temp <- c(q_temp, glm_model$Q_long[[glm_model$conv.step + 1]])

        dev_matrix[j,i] <- sum(family$dev.resids(test_dt[[yvar]],
                                                 yhat,
                                                 wt = rep(1,
                                                          length(yhat))))
      }
    }
  }

  dev_vector <- apply(dev_matrix, 1, sum, na.rm = TRUE)

  optlambda_index <- which.min(dev_vector)

  ## now fit full model until optimal lambda (which is at opt4)
  for(j in 1:optlambda_index)  {
    glm_final <- glmmLasso(fix = glmlasso_formula,
                           rnd = list(domain = paste(candidate_vars,
                                                     collapse = " + ")),
                           family = family,
                           data = dt,
                           lambda = lambda_vector[j],
                           switch.NR = FALSE,
                           final.re = FALSE,
                           control = list(start = delta_start[j,],
                                          q_start = q_start[j],
                                          maxIter = maxIter,
                                          epsilon = epsilon,
                                          eps.final = eps.final))

    delta_start <- rbind(delta_start, glm_final$Deltamatrix[glm_final$conv.step,])
    q_start <- c(q_start, glm_final$Q_long[[glm_final$conv.step + 1]])
  }



  if (return_onlyvars == TRUE){

    selvars_list <- names(glm_final$coefficients[glm_final$coefficients != 0])

    selvars_list <- selvars_list[selvars_list != "(Intercept)"]

    return(selvars_list)

  }

  return(glm_final)

}

select_candidates <- function(dt,
                              y,
                              xvars,
                              threshold){


  dt <- as.data.table(dt)

  cor_matrix <- cor(dt[, c(y, xvars), with = FALSE])

  # cor_dt <- data.table(vars = names(cor_matrix[,1]),
  #                      cor = cor_matrix[,1])
  #
  # top_vars <- cor_dt[order(abs(cor_dt$cor), decreasing = TRUE),]$vars[2:N+1]
  cor_matrix <- as.data.frame(cor_matrix)

  cor_matrix$names <- rownames(cor_matrix)

  cor_matrix <- as.data.table(cor_matrix)

  cor_matrix <- melt(cor_matrix,
                     id.vars = "names",
                     measure.vars = colnames(cor_matrix)[!(colnames(cor_matrix) %in% "names")])

  cor_matrix <- cor_matrix[!grepl(".1", cor_matrix$names),]
  cor_matrix <- cor_matrix[!grepl(".1", cor_matrix$variable),]

  ycor_dt <- cor_matrix[cor_matrix$variable == y,]
  xcor_dt <- cor_matrix[cor_matrix$variable != y,]

  xcor_dt <- xcor_dt[!is.na(xcor_dt$value),]

  xcor_dt <- xcor_dt[abs(xcor_dt$value) < 1,]

  drop_vars <- unique(xcor_dt[abs(xcor_dt$value) >= threshold,]$names)

  #### select the top variables that are on the candidate_vars list
  ycor_dt <- ycor_dt[order(-ycor_dt$value),]

  top_vars <- ycor_dt$names[!grepl(y, ycor_dt$names)]

  top_vars <- top_vars[!(top_vars %in% drop_vars)]

  ycor_dt <- ycor_dt[ycor_dt$names %in% top_vars,]

  top_vars <- ycor_dt$names

  top_vars <- top_vars[!is.na(top_vars)]

  # if (top_vars %in% c("rai", "rri")){
  #
  #   top_vars <- top_vars[!(top_vars %in% "rri")]
  #
  # }

  return(top_vars)


}


### model selection by country
countrymodel_select <- function(dt, xvars, y){

  dt <- as.data.table(dt)

  dt <- dt[,which(unlist(lapply(dt, function(x)!all(is.na(x))))), with = F]

  xvars <- xvars[xvars %in% colnames(dt)]

  dt <- na.omit(dt[,c(y, xvars), with = F])

  xset <- dt[, xvars, with = F]

  dt[["y"]] <- dt[, y, with = FALSE]

  model_dt <- cbind(dt[["y"]], xset)

  lasso_model <- hdm::rlasso(V1 ~ ., data = model_dt, post = TRUE)

  lasso_model <- cbind(names(lasso_model$coefficients), as.data.table(lasso_model$coefficients))

  colnames(lasso_model) <- c("variable_name", "value")

  varsselect_list <- lasso_model$variable_name[lasso_model$value != 0]
  varsselect_list <- varsselect_list[!varsselect_list == "(Intercept)"]


  return(varsselect_list)

}


stepAIC_wrapper <- function(dt, xvars, y, weights){

  dt <- as.data.table(dt)

  dt <- dt[,which(unlist(lapply(dt, function(x)!all(is.na(x))))), with = F]

  xvars <- xvars[xvars %in% colnames(dt)]

  dt <- na.omit(dt[,c(y, xvars, weights), with = F])

  xset <- dt[, xvars, with = F]

  y <- dt[,y]

  weights <- dt[, weights, with = F]

  model_dt <- cbind(y, xset, weights)

  full_model <- lm(y ~ .,
                   data = model_dt,
                   weights = weights)

  ### use the vif method to drop multicollinear variables
  vif_model <- car::vif(full_model)


  stepwise_model <- stepAIC(full_model,
                            direction = "both",
                            trace = 0)

  return(stepwise_model)

}



create_interactions <- function(dt, interacter_var, var_list) {
  # Ensure dt is a data.table
  if (!"data.table" %in% class(dt)) {
    dt <- as.data.table(dt)
  }

  # Check if interacter_var exists in the dataset
  if (!(interacter_var %in% names(dt))) {
    stop(paste(interacter_var, "not found in dataset"))
  }

  # Check if var_list contains valid variables that exist in the dataset
  if (any(!var_list %in% names(dt))) {
    stop("Some variables in var_list are not found in the dataset.")
  }

  # Create an empty data.table to store interactions
  int_dt <- data.table(matrix(nrow = nrow(dt)))

  # Loop over var_list to create interaction terms
  for (var in var_list) {
    interaction_name <- paste0(var, "_X_", interacter_var)
    int_dt[[interaction_name]] <- dt[[var]] * dt[[interacter_var]]
  }

  return(int_dt)
}




