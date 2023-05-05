devtools::load_all()
#renv::restore()

#------------------------------------------------------------------------------#
## environment set up
#------------------------------------------------------------------------------#
remove(list = objects())

options(
  stringsAsFactors = F, ## tell R to treat text as text, not factors
  width = 100, ## set the maximum width of outputs as 80 characters
  scipen = 6, ## discourage R from displaying numbers in scientific notation
  start.time= Sys.time()
)

pacman::p_load(here)
#------------------------------------------------------------------------------#
#                       Comparison of variables

source(here("R/01_littleworkerfunctions.R"))
census_dt <- readRDS(here("data-clean/census_dt.RDS"))
survey_dt <- readRDS(here("data-clean/survey_dt.RDS"))
varlist <- readRDS(here("data-raw/varlist.RDS"))

Table <- ebp_test_means(

               smp_data = na.omit(survey_dt[,c(varlist,"wta_hh")]),

               pop_data = na.omit(census_dt[,c(varlist,"wta_hh")]),

               varlist = varlist ,

               smp_weights = "wta_hh",

               pop_weights = "wta_hh" )

#------------------------------------------------------------------------------#

write.csv(Table,here("inst/Report/Descriptive_Statitics.csv"), row.names = FALSE)

cand_varlist <- subset(Table, !is.na(pvalue)& pvalue>0.5 )$variable

saveRDS(cand_varlist, here("data-raw/cand_varlist.RDS") )
