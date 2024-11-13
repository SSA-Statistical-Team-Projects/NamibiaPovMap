################################################################################
################# POST ESTIMATION ANALYTICS FOR THE EBP MODEL ##################
################################################################################

pacman::p_load(data.table, dplyr, stringr, fuzzyjoin, povmap)
#------------------------------------------------------------------------------#

### read in all the data
logunit_model <- readRDS("data-clean/estimation_results/unitmodel_log.RDS")

nam_selvars_list <- readRDS("data-raw/nam_selvars_list.RDS")

census_dt <- readRDS("data-clean/census_dt.RDS")

survey_dt <- readRDS("data-clean/survey_dt.RDS")

census_dt$const_code <- as.numeric(census_dt$const_code)

survey_dt$new_const_code <- as.numeric(survey_dt$new_const_code)

# census_dt["wta_hh"] <- lapply(haven::zap_labels(census_dt["wta_hh"]) ,as.numeric)

nam_selvars_list  <- intersect(intersect(nam_selvars_list, names(survey_dt)), names(census_dt))

census_dt <- as.data.table(census_dt)
survey_dt <- as.data.table(survey_dt)

### check that the poverty rates make sense
survey_dt[, !duplicated(colnames(survey_dt)), with = F] %>%
  mutate(poor_var = ifelse(wel_PPP < 6249, 1, 0)) %>%
  summarize(weighted.mean(x = poor_var,
                          w = wta_hh * hhsize,
                          na.rm = TRUE))

#### compute the basic statistics tables

direct_dt <- povmap::direct(y = "wel_PPP",
                            smp_data = na.omit(survey_dt[, c("wel_PPP", nam_selvars_list,
                                                             "wta_hh", "hid",
                                                             "region_prev_name",
                                                             "region_prev_code",
                                                             "new_const_code"),
                                                         with = FALSE]),
                            smp_domains = "new_const_code",
                            weights = "wta_hh",
                            var = TRUE,
                            threshold = 6249)

saveRDS(direct_dt, "inst/postestimation/tables/direct_estimates.RDS")

survey_dt$const_code <- survey_dt$new_const_code

census_dt$region_prev_code <- census_dt$region_code
census_dt$new_const_code <- census_dt$const_code

logunit_model$framework$smp_data <- logunit_model$model$data



descriptives_dt <-
  ebp_reportdescriptives(model = logunit_model,
                         direct = direct_dt,
                         smp_data = as.data.frame(na.omit(survey_dt[, c("wel_PPP", nam_selvars_list,
                                                                        "wta_hh", "hid", "region_prev_code",
                                                                        "const_code", "new_const_code"),
                                                                    with = FALSE])),
                         weights = "wta_hh",
                         pop_weights = "hhsize",
                         CV_level = "region_prev_code",
                         pop_data = as.data.frame(census_dt),
                         pop_domains = "new_const_code",
                         threshold = 6249)
saveRDS(descriptives_dt, "inst/postestimation/tables/direct_estimates.RDS")


##### compare the means between survey and census prior to model estimation
checkvariables_dt <-
  ebp_test_means(varlist = nam_selvars_list,
                 pop_data = as.data.frame(census_dt),
                 smp_data = as.data.frame(survey_dt),
                 weights = "wta_hh")

write.csv(checkvariables_dt,
          "inst/postestimation/tables/compare_samplecensus_means.csv")


##### model estimations
ebp_modelresults_dt <- ebp_reportcoef_table(logunit_model, 4)

write.csv(ebp_modelresults_dt,
          "inst/postestimation/tables/ebp_regressionresults.csv")

##### include the enumeration area
logunit_model$framework$smp_data$cluster <-
  as.data.frame(na.omit(survey_dt[!is.na(new_const_code),
                                  c(nam_selvars_list,
                                    "new_const_code",
                                    "wta_hh", "cluster"),
                                  with = FALSE]))$cluster


replace_dt <- as.data.frame(na.omit(survey_dt[, c("wel_PPP",
                                                  nam_selvars_list,
                                                  "new_const_code",
                                                  "wta_hh"),
                                              with = F]))

logunit_model$framework$smp_data$wta_hh <- replace_dt$wta_hh
logunit_model$model$data$wta_hh <- replace_dt$wta_hh

#### compute different CV types
cv_dt <- ebp_compute_cv(model = logunit_model,
                        calibvar = "new_const_code",
                        designvar = "cluster",
                        threshold = 6249)

write.csv(cv_dt, "inst/postestimation/tables/targetarea_cvtable.csv")


plot(logunit_model)

#### compute and save cooks distance estimates

cooks_dt <- cooks.distance(logunit_model$model)
cooks_dt <- data.frame(index = seq_along(cooks_dt), cooks_dt)


