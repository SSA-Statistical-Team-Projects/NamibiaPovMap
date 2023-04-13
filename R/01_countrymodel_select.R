#shells to Stata to estimate lasso
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
                 paste0("lassowrapper ", colnames(y), " ", x[1], "-",
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
