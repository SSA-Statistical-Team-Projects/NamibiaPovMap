#------- Function to compare means in both databases

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

#----Function for detecting variables with only missing values

missing_var <- function(dt){

  df <- data.frame(dt)
  missing_count <- colSums(is.na(df))
  missing_cols <- which(missing_count == nrow(df))
  names(df[, missing_cols])

}


#---- Function for selecting valid variables in both database

cand_var <- function(pop_dt, smp_dt, oth_var)
                  {

  pop_df <- data.frame(pop_dt)
  smp_df <- data.frame(smp_dt)
  common_vars <- intersect(names(pop_df),names(smp_df))
  res <- common_vars[!(common_vars %in% oth_var)]
  fin_res <- intersect(res[!(res %in% missing_var(smp_df))],
                    res[!(res %in% missing_var(pop_df))])

  return(fin_res)

                }


#----Function to transform categorical variables into dummy variable


dummify <- function(x)
              {
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


#--- Function to show row of a data.frame
show_row <- function(df, row_idx)
  {
  row <- as.data.frame(df[row_idx,])
  return(row)
}





#-------Function to compare mean for a single variable
compar_test_means<- function(smp_data,
                           pop_data,
                           var,
                           smp_weights,
                           pop_weights){

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




plot_hist <- function(pop_df, var, pop_weight) {



  dt <- data.table(pop_df)
  dt[[pop_weight]] <- as.numeric(dt[[pop_weight]])
  dt[[var]] <- as.numeric(dt[[var]])

  df <- prop.table(wtd.table(x = dt[[var]], weights = dt[[pop_weight]]))

  ggplot(df, aes(x = df[[var]], fill="Census")) +
    geom_bar(stat = "identity") +
    xlab("Number of Rooms") +
    ylab("Proportion") +
    scale_fill_manual(values = "red")+
    labs(fill="") +
    ggtitle("Histogram of Number of Rooms")
}




