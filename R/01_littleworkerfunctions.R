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



### Function to realize descriptive statistics

ebp_test_means <- function(smp_data,
                           pop_data,
                           varlist,
                           smp_weights,
                           pop_weights){

  ### get the set of complete cases of the variables as
  ### would have been used in model estimation
  smp_df <- smp_data[complete.cases(c(varlist, smp_weights)),
                     c(varlist, smp_weights)]
  pop_df <- pop_data[complete.cases(c(varlist, pop_weights)),
                     c(varlist, pop_weights)]

  weighted.sd <- function(x, w){

    delta_sq <- (x - mean(x))^2 ##square deviation of the xs

    nzero_w <- (length(w[w > 0]) - 1) / length(w[w > 0])

    result <- sqrt(sum(w * (delta_sq)) / (nzero_w * sum(w)))

    return(result)
  }

  smp_means_df <- data.frame(smp_means = apply(X = smp_df[,varlist],
                                               MARGIN = 2,
                                               FUN = weighted.mean,
                                               w = smp_df[[smp_weights]]),
                             smp_sd = apply(X = smp_df[,varlist],
                                            MARGIN = 2,
                                            FUN = weighted.sd,
                                            w = smp_df[[smp_weights]]),
                             variable = varlist)

  pop_means_df <- data.frame(pop_means = apply(X = pop_df[,varlist],
                                               MARGIN = 2,
                                               FUN = weighted.mean,
                                               w = pop_df[[pop_weights]]),
                             pop_sd = apply(X = pop_df[,varlist],
                                            MARGIN = 2,
                                            FUN = weighted.sd,
                                            w = pop_df[[pop_weights]]),
                             variable = varlist)

  means_df <- merge(smp_means_df, pop_means_df, by = "variable")

  means_df$diff_sd <- sqrt((means_df$smp_sd)^2 + (means_df$pop_sd)^2)

  means_df$diff <- means_df$pop_means - means_df$smp_means

  means_df$zscore <- means_df$diff / means_df$diff_sd

  means_df$pvalue <- 2 * (1 - pnorm(abs(means_df$zscore)))

  return(means_df[, c("variable", "smp_means", "pop_means", "diff", "pvalue")])

}



### Function to create dummy variables from categorical variable with more than 2 modalities

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

