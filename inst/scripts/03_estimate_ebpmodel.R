#----------------------- EBP MODEL  ESTAIMATION -------------------------------#

## environment set up
remove(list = objects()) ## clear all objects in R workspace

options(
  stringsAsFactors = F, ## tell R to treat text as text, not factors
  width = 100, ## set the maximum width of outputs as 80 characters
  scipen = 6, ## discourage R from displaying numbers in scientific notation
  start.time= Sys.time()
)


pacman::p_load(emdi)

nam_selvars_list <- readRDS("data-raw/nam_selvars_list.RDS")

census_dt <- readRDS("data-raw/census_dt.RDS")
survey_dt <- readRDS("data-raw/survey_dt.RDS")

census_dt$const_code <- as.numeric(census_dt$const_code)
survey_dt$new_const_code <- as.numeric(survey_dt$new_const_code)
census_dt["wta_hh"] <- lapply(haven::zap_labels(census_dt["wta_hh"]) ,as.numeric)
nam_selvars_list  <- intersect(intersect(nam_selvars_list, names(survey_dt)), names(census_dt))

### remove NA column names
survey_dt <- survey_dt[,!is.na(colnames(survey_dt))]

nam_model <- ebp(fixed = as.formula(paste("lwel_PPP ~ ", paste(nam_selvars_list,
                                                               collapse= "+"))),
                 pop_data = as.data.frame(na.omit(census_dt[, c(nam_selvars_list,
                                                                "const_code")])),
                 pop_domains = "const_code",
                 smp_data = as.data.frame(na.omit(survey_dt[, c("wel_PPP",
                                                                nam_selvars_list,
                                                                "new_const_code",
                                                                "hhweight")])),
                 smp_domains = "new_const_code",
                 transformation = "log",
                 threshold = 6249,
                 weights = "hhweight",
                 cpus = 1,
                 MSE = FALSE,
                 na.rm = TRUE)


shapefile <- st_read(here("shapefiles/gadm41_NAM_2.shp"))
plot(shapefile, max.plot=1)
