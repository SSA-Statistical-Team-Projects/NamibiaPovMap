#----------------------- EBP MODEL  ESTAIMATION -------------------------------#

## environment set up
remove(list = objects())

options(
  stringsAsFactors = F,
  width = 100,
  scipen = 6,
  start.time= Sys.time()
)


pacman::p_load(data.table, dplyr, stringr, fuzzyjoin, povmap, glmmLasso, lme4, caret, MASS)
#------------------------------------------------------------------------------#

nam_selvars_list <- readRDS("data-raw/nam_selvars_list.RDS")

census_dt <- readRDS("data-clean/census_dt.RDS")

survey_dt <- readRDS("data-clean/survey_dt.RDS")

census_dt$const_code <- as.numeric(census_dt$const_code)

survey_dt$new_const_code <- as.numeric(survey_dt$new_const_code)

census_dt["wta_hh"] <- lapply(haven::zap_labels(census_dt["wta_hh"]) ,as.numeric)

nam_selvars_list  <- intersect(intersect(nam_selvars_list, names(survey_dt)), names(census_dt))

census_dt <- as.data.table(census_dt)
survey_dt <- as.data.table(survey_dt)

### check that the poverty rates make sense
survey_dt[, !duplicated(colnames(survey_dt)), with = F] %>%
  mutate(poor_var = ifelse(wel_PPP < 6249, 1, 0)) %>%
  summarize(weighted.mean(x = poor_var,
                          w = wta_hh,
                          na.rm = TRUE))

# remotes::install_github("SSA-Statistical-Team-Projects/povmap",
#                         ref = "david3")
#
# log_model <- povmap::ebp(fixed = as.formula(paste("wel_PPP ~ ", paste(nam_selvars_list,
#                                                                collapse= "+"))),
#                  pop_data = as.data.frame(na.omit(census_dt[, c(nam_selvars_list,
#                                                                 "const_code","wta_hh"),
#                                                             with = F])),
#                  pop_domains = "const_code",
#                  smp_data = as.data.frame(na.omit(survey_dt[, c("wel_PPP",
#                                                                 nam_selvars_list,
#                                                                 "new_const_code",
#                                                                 "wta_hh"),
#                                                             with = F])),
#                  smp_domains = "new_const_code",
#                  transformation = "log",
#                  threshold = 6249,
#                  pop_weights = "wta_hh",
#                  weights = "wta_hh",
#                  L = 100,
#                  B = 100,
#                  cpus = 30,
#                  MSE = TRUE,
#                  na.rm = TRUE,
#                  Ydump = "//esapov/esapov/NAM/GEO/Population/povmap/unitmodel_log.csv")
# #------------------------------------------------------------------------------#
# saveRDS(log_model, "data-clean/estimation_results/unitmodel_log.RDS")
#
#
# ord_model <- povmap::ebp(fixed = as.formula(paste("wel_PPP ~ ", paste(nam_selvars_list,
#                                                                       collapse= "+"))),
#                          pop_data = as.data.frame(na.omit(census_dt[, c(nam_selvars_list,
#                                                                         "const_code","wta_hh"),
#                                                                     with = F])),
#                          pop_domains = "const_code",
#                          smp_data = as.data.frame(na.omit(survey_dt[, c("wel_PPP",
#                                                                         nam_selvars_list,
#                                                                         "new_const_code",
#                                                                         "wta_hh"),
#                                                                     with = F])),
#                          smp_domains = "new_const_code",
#                          transformation = "ordernorm",
#                          threshold = 6249,
#                          pop_weights = "wta_hh",
#                          weights = "wta_hh",
#                          L = 100,
#                          B = 100,
#                          cpus = 30,
#                          MSE = FALSE,
#                          na.rm = TRUE,
#                          Ydump = "//esapov/esapov/NAM/GEO/Population/povmap/unitmodel_ord.csv")
# #------------------------------------------------------------------------------#
# saveRDS(ord_model, "data-clean/estimation_results/unitmodel_ordernorm.RDS")
#
#
# bcx_model <- povmap::ebp(fixed = as.formula(paste("wel_PPP ~ ", paste(nam_selvars_list,
#                                                                       collapse= "+"))),
#                          pop_data = as.data.frame(na.omit(census_dt[, c(nam_selvars_list,
#                                                                         "const_code","wta_hh"),
#                                                                     with = F])),
#                          pop_domains = "const_code",
#                          smp_data = as.data.frame(na.omit(survey_dt[, c("wel_PPP",
#                                                                         nam_selvars_list,
#                                                                         "new_const_code",
#                                                                         "wta_hh"),
#                                                                     with = F])),
#                          smp_domains = "new_const_code",
#                          transformation = "box.cox",
#                          threshold = 6249,
#                          pop_weights = "wta_hh",
#                          weights = "wta_hh",
#                          weights_type = "nlme",
#                          L = 100,
#                          B = 100,
#                          cpus = 30,
#                          MSE = FALSE,
#                          na.rm = TRUE,
#                          Ydump = "//esapov/esapov/NAM/GEO/Population/povmap/unitmodel_bcx.csv")
# #------------------------------------------------------------------------------#
# saveRDS(bcx_model, "data-clean/estimation_results/unitmodel_boxcox.RDS")
#
#
# logshift_model <- povmap::ebp(fixed = as.formula(paste("wel_PPP ~ ", paste(nam_selvars_list,
#                                                                            collapse= "+"))),
#                               pop_data = as.data.frame(na.omit(census_dt[, c(nam_selvars_list,
#                                                                              "const_code","wta_hh"),
#                                                                          with = F])),
#                               pop_domains = "const_code",
#                               smp_data = as.data.frame(na.omit(survey_dt[, c("wel_PPP",
#                                                                              nam_selvars_list,
#                                                                              "new_const_code",
#                                                                              "wta_hh"),
#                                                                          with = F])),
#                               smp_domains = "new_const_code",
#                               transformation = "log.shift",
#                               threshold = 6249,
#                               pop_weights = "wta_hh",
#                               weights = "wta_hh",
#                               weights_type = "nlme",
#                               L = 100,
#                               B = 100,
#                               cpus = 30,
#                               MSE = TRUE,
#                               na.rm = TRUE,
#                               Ydump = "//esapov/esapov/NAM/GEO/Population/povmap/unitmodel_bcx.csv")
# #------------------------------------------------------------------------------#
# saveRDS(bcx_model, "data-clean/estimation_results/unitmodel_boxcox.RDS")
#
#
#
#
# ### merge to shapefile for CCDR work
#
# shp_dt <- sf::st_read("data-raw/shapefiles/ADMIN_Constituency_Boundaries_2014.shp")
#
#
#
# census_dt[, const_merge := str_replace_all(constituency_name, " ", "")]
# census_dt[, const_merge := tolower(const_merge)]
# census_dt[, const_merge := str_replace_all(const_merge, "[^[:alnum:]]", "")]
# census_dt[, const_merge := paste0(const_code, "-", const_merge)]
#
# shp_dt <-
#   shp_dt %>%
#   mutate(const_merge = str_replace_all(CONST, " ", "")) %>%
#   mutate(const_merge = tolower(const_merge)) %>%
#   mutate(const_merge = str_replace_all(const_merge, "[^[:alnum:]]", "")) %>%
#   mutate(const_merge = paste0(CONST_ID, "-", const_merge))
#
#
# fuzzyshp_dt <-
#   stringdist_join(x = unique(census_dt[, c("const_code", "const_merge")]),
#                   y = shp_dt[, c("CONST_ID", "const_merge")] %>% sf::st_drop_geometry(),
#                   by = "const_merge",
#                   mode = "left",
#                   method = "jw",
#                   max_dist = 99,
#                   distance_col = "dist")
#
# fuzzyshp_dt <- as.data.table(fuzzyshp_dt)
#
# indices_dt <- fuzzyshp_dt[, .I[which.min(dist)], by = const_merge.y]
#
# shpmatch_dt <- fuzzyshp_dt[indices_dt$V1]
#
# # shpmatch_dt <- shpmatch_dt[dist < 0.2,]
#
# ### merge the poverty rates
#
# shpmatch_dt <-
#   shpmatch_dt %>%
#   mutate(const_code = as.factor(const_code)) %>%
#   merge(nam_model$ind,
#         by.x = "const_code",
#         by.y = "Domain") %>%
#   select(CONST_ID, Head_Count) %>%
#   group_by(CONST_ID) %>%
#   summarise(Head_Count = mean(Head_Count, na.rm = TRUE))
#
# shp_dt <-
#   shp_dt %>%
#   merge(shpmatch_dt, on = "CONST_ID")
#
#
# saveRDS(shp_dt, "../MapBotsMibiaR/inst/data/nam_povertyshp.RDS")


#### try out model selection with the nlme package
train.control.CV5 <- trainControl(method = "cv",
                                  number = 5)

lambda.Range <- 10^seq(-3, 3, length = 100)


patlist <-
  paste("roof_|wall_|floor_|water14_|waterpipe_|piped_|imp_wat_",
         "toilet14_|toilet6_|fuelcook_|fuelligh_|electyp_|heatsource_",
         "rururb_|agecat_|relathh9_|radio_|television_|computer_",
         "cellphone_|landphone_|internet_|areadummy_",
         sep = "|")

candidate_vars <- colnames(survey_dt)[grepl(pattern = patlist, colnames(survey_dt))]

model_formula <- as.formula(paste("wel_PPP ~ ", paste(candidate_vars, collapse= "+")))

lasso <- train(model_formula,
               data = na.omit(survey_dt[,c("wel_PPP", candidate_vars), with = FALSE]),
               method = "glmnet",
               metric = "RMSE",
               trControl = train.control.CV5,
               lambda = lambda.Range,
               preProcess=c("center","scale"),
               tuneGrid = expand.grid(alpha = 1,
                                      lambda = lambda.Range))


#### we just showed the vanilla approach above. Lets do it with the full data

pql <- glmmPQL(wel_PPP ~ 1,
               random = as.formula(paste0("~ 1 | as.factor(new_const_code)")),
               family = "gaussian",
               data = survey_dt)

delta.start <- c(as.numeric(pql$coefficients$fixed),
                 rep(0, length(candidate_vars)),
                 as.numeric(t(pql$coefficients$random$`as.factor(new_const_code)`)))

q_start <- as.numeric(VarCorr(pql)[1,1])


### find the optimal lambda
lambda <- 10^seq(-3, 5, length = 10)

bic_vec <- rep(Inf, length(lambda))
aic_vec <- rep(Inf, length(lambda))
deviance_ma <- NULL
coeff_ma <- NULL

j <- 1

for (j in 1:length(bic_vec)){

  print(paste("Iteration", j, sep = ""))

  glm1 <- try(glmmLasso(model_formula),
              data = na.omit(survey_dt[,c("wel_PPP", candidate_vars), with = FALSE]),
              rnd = as.formula(paste0("~ 1 | as.factor(new_const_code)")),
              family = gaussian(link = "identity"),
              lambda = lambda[j],
              switch.NR = TRUE,
              final.re = TRUE,
              silent = TRUE)

  #code to make it continue anyway if an error occurs
  if(class(glm1)!="try-error")
  {

  #save BIC, AIC
  bic_vec[j] <- glm1$bic
  aic_vec[j] <- glm1$aic

  #save coefficient outputs
  coeff_ma <- cbind(coeff_ma, glm1$coefficients)

  #save error (deviance) values
  y.hat <- predict(glm1, na.omit(survey_dt[,c("wel_PPP", candidate_vars), with = FALSE]))
  devianz_ma[j]<-sum(family$dev.resids(na.omit(survey_dt[,c("wel_PPP", candidate_vars), with = FALSE])$wel_PPP,
                                       y.hat,wt=rep(1,length(y.hat))))

  }


}
























































































