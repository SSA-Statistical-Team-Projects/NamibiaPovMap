#----------------------- EBP MODEL  ESTAIMATION -------------------------------#

## environment set up
remove(list = objects())

options(
  stringsAsFactors = F,
  width = 100,
  scipen = 6,
  start.time= Sys.time()
)

devtools::install_github("SSA-Statistical-Team-Projects/EMDIplus")

pacman::p_load(emdi)
#------------------------------------------------------------------------------#

nam_selvars_list <- readRDS("data-raw/nam_selvars_list.RDS")

census_dt <- readRDS("data-clean/census_dt.RDS")

survey_dt <- readRDS("data-clean/survey_dt.RDS")

census_dt$const_code <- as.numeric(census_dt$const_code)

survey_dt$new_const_code <- as.numeric(survey_dt$new_const_code)

census_dt["wta_hh"] <- lapply(haven::zap_labels(census_dt["wta_hh"]) ,as.numeric)

nam_selvars_list  <- intersect(intersect(nam_selvars_list, names(survey_dt)), names(census_dt))


nam_model <- emdiplus::ebp(fixed = as.formula(paste("wel_PPP ~ ", paste(nam_selvars_list,
                                                               collapse= "+"))),
                 pop_data = as.data.frame(na.omit(census_dt[, c(nam_selvars_list,
                                                                "const_code","wta_hh")])),
                 pop_domains = "const_code",
                 smp_data = as.data.frame(na.omit(survey_dt[, c("wel_PPP",
                                                                nam_selvars_list,
                                                                "new_const_code",
                                                                "wta_hh")])),
                 smp_domains = "new_const_code",

                 transformation = "log",

                 threshold = 6249,

                 pop_weights = "wta_hh",

                 weights = "wta_hh",

                 # L= 100,

                 # B=100,

                 cpus = 1,

                 MSE = FALSE,

                 na.rm = TRUE)
#------------------------------------------------------------------------------#
saveRDS(nam_model, here("data-raw/nam_model.RDS") )
