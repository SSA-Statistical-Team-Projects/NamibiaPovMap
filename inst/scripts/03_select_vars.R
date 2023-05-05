
devtools::load_all()

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

pacman::p_load(RStata, here)

#-----------------------------------------------------------------------------#

source("R/01_littleworkerfunctions.R")
census_dt <- readRDS(here("data-clean/census_dt.RDS"))
survey_dt <- readRDS(here("data-clean/survey_dt.RDS"))
dummy_dt <- readRDS(here("data-raw/dummy_dt.RDS"))
cand_varlist <- readRDS(here("data-raw/cand_varlist.RDS"))

#------------------------------------------------------------------------------#

survey_dt$ea_id<-as.numeric(survey_dt$new_const_code)
survey_dt$hhweight <- survey_dt$wta_hh
survey_dt$lwel_PPP<-log(survey_dt$wel_PPP)

#------------------------------------------------------------------------------#

nam_selvars_list <-
  countrymodel_select_stata(dt = na.omit(survey_dt[,c("lwel_PPP", "hhweight",
                                                      "ea_id", dummy_dt,
                                                      cand_varlist )]),
                            xvars = c(cand_varlist, dummy_dt),

                            y = "lwel_PPP",
                            weights = "hhweight",
                            selection = "BIC",
                            stata_path = "C:/Users/wb604749/Desktop/Stata17/StataMP-64.exe", # change this
                            stata_vnum = 17)

#------------------------------------------------------------------------------#

saveRDS(nam_selvars_list, here("data-raw/nam_selvars_list.RDS") )

