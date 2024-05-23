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



find_optlambda <- function(lambda,
                           dt,
                           cand_vars,
                           delta_start,
                           q_start) {

  tryCatch(
    {
      glm1 <- glmmLasso(
        as.formula(paste("lnrpc_tot_cons ~ ", paste(cand_vars, collapse = "+"))),
        rnd = list(targetarea_codes = ~1),
        family = gaussian(link = "identity"),
        data = na.omit(dt),
        lambda = lambda,
        switch.NR = TRUE,
        final.re = TRUE,
        control = list(start = delta_start, q_start = q_start)
      )
      return(glm1$bic)
    },
    error = function(e) Inf
  )

}







































































