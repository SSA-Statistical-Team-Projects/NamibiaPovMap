#----------------------- EBP MODEL  ESTAIMATION -------------------------------#

## environment set up
remove(list = objects())

options(
  stringsAsFactors = F,
  width = 100,
  scipen = 6,
  start.time= Sys.time()
)


pacman::p_load(data.table, dplyr, stringr, fuzzyjoin)
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

log_model <- povmap::ebp(fixed = as.formula(paste("wel_PPP ~ ", paste(nam_selvars_list,
                                                               collapse= "+"))),
                 pop_data = as.data.frame(na.omit(census_dt[, c(nam_selvars_list,
                                                                "const_code","wta_hh"),
                                                            with = F])),
                 pop_domains = "const_code",
                 smp_data = as.data.frame(na.omit(survey_dt[, c("wel_PPP",
                                                                nam_selvars_list,
                                                                "new_const_code",
                                                                "wta_hh"),
                                                            with = F])),
                 smp_domains = "new_const_code",
                 transformation = "log",
                 threshold = 6249,
                 pop_weights = "wta_hh",
                 weights = "wta_hh",
                 L = 0,
                 B = 100,
                 cpus = 30,
                 MSE = TRUE,
                 na.rm = TRUE,
                 Ydump = "//esapov/esapov/NAM/GEO/Population/povmap/unitmodel_log.csv")
#------------------------------------------------------------------------------#
saveRDS(log_model, "data-clean/estimation_results/unitmodel_log.RDS")


ord_model <- povmap::ebp(fixed = as.formula(paste("wel_PPP ~ ", paste(nam_selvars_list,
                                                                      collapse= "+"))),
                         pop_data = as.data.frame(na.omit(census_dt[, c(nam_selvars_list,
                                                                        "const_code","wta_hh"),
                                                                    with = F])),
                         pop_domains = "const_code",
                         smp_data = as.data.frame(na.omit(survey_dt[, c("wel_PPP",
                                                                        nam_selvars_list,
                                                                        "new_const_code",
                                                                        "wta_hh"),
                                                                    with = F])),
                         smp_domains = "new_const_code",
                         transformation = "ordernorm",
                         threshold = 6249,
                         pop_weights = "wta_hh",
                         weights = "wta_hh",
                         L = 100,
                         B = 100,
                         cpus = 30,
                         MSE = FALSE,
                         na.rm = TRUE,
                         Ydump = "//esapov/esapov/NAM/GEO/Population/povmap/unitmodel_ord.csv")
#------------------------------------------------------------------------------#
saveRDS(ord_model, "data-clean/estimation_results/unitmodel_ordernorm.RDS")


bcx_model <- povmap::ebp(fixed = as.formula(paste("wel_PPP ~ ", paste(nam_selvars_list,
                                                                      collapse= "+"))),
                         pop_data = as.data.frame(na.omit(census_dt[, c(nam_selvars_list,
                                                                        "const_code","wta_hh"),
                                                                    with = F])),
                         pop_domains = "const_code",
                         smp_data = as.data.frame(na.omit(survey_dt[, c("wel_PPP",
                                                                        nam_selvars_list,
                                                                        "new_const_code",
                                                                        "wta_hh"),
                                                                    with = F])),
                         smp_domains = "new_const_code",
                         transformation = "box.cox",
                         threshold = 6249,
                         pop_weights = "wta_hh",
                         weights = "wta_hh",
                         weights_type = "nlme",
                         L = 100,
                         B = 100,
                         cpus = 30,
                         MSE = FALSE,
                         na.rm = TRUE,
                         Ydump = "//esapov/esapov/NAM/GEO/Population/povmap/unitmodel_bcx.csv")
#------------------------------------------------------------------------------#
saveRDS(bcx_model, "data-clean/estimation_results/unitmodel_boxcox.RDS")


logshift_model <- povmap::ebp(fixed = as.formula(paste("wel_PPP ~ ", paste(nam_selvars_list,
                                                                           collapse= "+"))),
                              pop_data = as.data.frame(na.omit(census_dt[, c(nam_selvars_list,
                                                                             "const_code","wta_hh"),
                                                                         with = F])),
                              pop_domains = "const_code",
                              smp_data = as.data.frame(na.omit(survey_dt[, c("wel_PPP",
                                                                             nam_selvars_list,
                                                                             "new_const_code",
                                                                             "wta_hh"),
                                                                         with = F])),
                              smp_domains = "new_const_code",
                              transformation = "log.shift",
                              threshold = 6249,
                              pop_weights = "wta_hh",
                              weights = "wta_hh",
                              weights_type = "nlme",
                              L = 100,
                              B = 100,
                              cpus = 30,
                              MSE = FALSE,
                              na.rm = TRUE,
                              Ydump = "//esapov/esapov/NAM/GEO/Population/povmap/unitmodel_bcx.csv")
#------------------------------------------------------------------------------#
saveRDS(bcx_model, "data-clean/estimation_results/unitmodel_boxcox.RDS")




### merge to shapefile for CCDR work

shp_dt <- sf::st_read("data-raw/shapefiles/ADMIN_Constituency_Boundaries_2014.shp")



census_dt[, const_merge := str_replace_all(constituency_name, " ", "")]
census_dt[, const_merge := tolower(const_merge)]
census_dt[, const_merge := str_replace_all(const_merge, "[^[:alnum:]]", "")]
census_dt[, const_merge := paste0(const_code, "-", const_merge)]

shp_dt <-
  shp_dt %>%
  mutate(const_merge = str_replace_all(CONST, " ", "")) %>%
  mutate(const_merge = tolower(const_merge)) %>%
  mutate(const_merge = str_replace_all(const_merge, "[^[:alnum:]]", "")) %>%
  mutate(const_merge = paste0(CONST_ID, "-", const_merge))


fuzzyshp_dt <-
  stringdist_join(x = unique(census_dt[, c("const_code", "const_merge")]),
                  y = shp_dt[, c("CONST_ID", "const_merge")] %>% sf::st_drop_geometry(),
                  by = "const_merge",
                  mode = "left",
                  method = "jw",
                  max_dist = 99,
                  distance_col = "dist")

fuzzyshp_dt <- as.data.table(fuzzyshp_dt)

indices_dt <- fuzzyshp_dt[, .I[which.min(dist)], by = const_merge.y]

shpmatch_dt <- fuzzyshp_dt[indices_dt$V1]

# shpmatch_dt <- shpmatch_dt[dist < 0.2,]

### merge the poverty rates

shpmatch_dt <-
  shpmatch_dt %>%
  mutate(const_code = as.factor(const_code)) %>%
  merge(nam_model$ind,
        by.x = "const_code",
        by.y = "Domain") %>%
  select(CONST_ID, Head_Count) %>%
  group_by(CONST_ID) %>%
  summarise(Head_Count = mean(Head_Count, na.rm = TRUE))

shp_dt <-
  shp_dt %>%
  merge(shpmatch_dt, on = "CONST_ID")


saveRDS(shp_dt, "../MapBotsMibiaR/inst/data/nam_povertyshp.RDS")










