
#devtools::load_all()

#------------------------------------------------------------------------------#
## environment set up
#------------------------------------------------------------------------------#
remove(list = objects())
options(
  stringsAsFactors = F,
  width = 100,
  scipen = 6,
  start.time= Sys.time()
)

pacman::p_load(RStata, here, data.table, MASS, glmmLasso, nlme, tidyverse,
               lme4, cAIC4, magic, povmap, Ecfun)

#-----------------------------------------------------------------------------#

# source("R/01_littleworkerfunctions.R")
census_dt <- readRDS(here("data-clean/census_dt.RDS"))
survey_dt <- readRDS(here("data-clean/survey_dt.RDS"))
dummy_dt <- readRDS(here("data-raw/dummy_dt.RDS"))
cand_varlist <- readRDS(here("data-raw/cand_varlist.RDS"))

#------------------------------------------------------------------------------#

survey_dt$ea_id<-as.numeric(survey_dt$new_const_code)
survey_dt$hhweight <- survey_dt$wta_hh * survey_dt$hhsize
survey_dt$lwel_abs<-log(survey_dt$wel_abs)

survey_dt$rooms[is.na(survey_dt$rooms)] <- 1
#------------------------------------------------------------------------------#

# nam_selvars_list <-
#   countrymodel_select_stata(dt = na.omit(survey_dt[,c("lwel_abs", "hhweight",
#                                                       "ea_id", dummy_dt,
#                                                       cand_varlist )]),
#                             xvars = c(cand_varlist, dummy_dt),
#
#                             y = "lwel_abs",
#                             weights = "hhweight",
#                             selection = "BIC",
#                             stata_path = "C:/Program Files/Stata18/StataMP-64.exe", # change this
#                             stata_vnum = 18)

#------------------------------------------------------------------------------#

#### variable selection using the rdm rlasso functionality

survey_dt$scaled_rooms <- scales::rescale(survey_dt$rooms)
survey_dt$scaled_hhsize <- scales::rescale(survey_dt$hhsize)

census_dt$scaled_rooms <- scales::rescale(census_dt$rooms)
census_dt$scaled_hhsize <- scales::rescale(census_dt$hhsize)

cand_varlist <- c(cand_varlist, "scaled_rooms", "scaled_hhsize")

survey_dt <- as.data.table(survey_dt)
census_dt <- as.data.table(census_dt)

survey_dt <- survey_dt[,!duplicated(colnames(survey_dt)), with = FALSE]
census_dt <- census_dt[,!duplicated(colnames(census_dt)), with = FALSE]

census_dt <-
census_dt %>%
  rename(areadummy_zambezi = areadummy_caprivi)




inter_dt <- create_interactions(dt = survey_dt,
                                interacter_var = "rururb_rural",
                                var_list = c(cand_varlist, dummy_dt))

survey_dt <- cbind(survey_dt, inter_dt)

inter_dt <- create_interactions(dt = census_dt,
                                interacter_var = "rururb_rural",
                                var_list = c(cand_varlist, dummy_dt))

census_dt <- cbind(census_dt, inter_dt)

nam_selvars_list <-
  countrymodel_select(dt = survey_dt,
                      xvars = c(cand_varlist, colnames(inter_dt), dummy_dt),
                      y = "lwel_abs")

lm_obj <-
lm(formula = as.formula(paste0("lwel_abs ~ ", paste0(nam_selvars_list, collapse = " + "))),
   data = survey_dt,
   weights = survey_dt$hhweight)


### trying the stepAIC function in R
step_selvars_list <- stepAIC_wrapper(dt = survey_dt,
                                     xvars = candidate_vars,
                                     y = "lwel_abs",
                                     weights = "hhweight")

step_selvars_list <- names(step_selvars_list$coefficients)[!names(step_selvars_list$coefficients) %in% "(Intercept)"]


### variable selection with stepwise with linear mixed effects modelling

lmer_obj <- lmer(formula = as.formula(paste0(paste0("lwel_abs ~ ",
                                                    paste0(candidate_vars, collapse = " + ")),
                                             "+ (1|new_const_code)")),
                 data = survey_dt,
                 weights = survey_dt$hhweight)


condstep_selvars_list <- stepcAIC(lmer_obj,
                                  direction = "backward",
                                  returnResult = TRUE,
                                  trace = TRUE)

dt <- as.data.table(summary(condstep_selvars_list$finalModel)$coefficients)

dt[, vars := rownames(summary(condstep_selvars_list$finalModel)$coefficients)]

caic_selvars_list <- dt$vars[dt$`Pr(>|t|)` <= 0.05][!dt$vars[dt$`Pr(>|t|)` <= 0.05] %in% "(Intercept)"]



saveRDS(nam_selvars_list, here("data-raw/nam_selvars_list.RDS") )
saveRDS(caic_selvars_list, here("data-raw/nam_caicselvars_list.RDS") )
saveRDS(census_dt, "data-clean/census_final.RDS")
saveRDS(survey_dt, "data-clean/survey_final.RDS")
