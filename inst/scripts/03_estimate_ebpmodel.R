#----------------------- EBP MODEL  ESTAIMATION -------------------------------#
library(emdi)
library(sf)
library(ggplot2)
library(rgdal)
library(sp)
library(here)

nam_selvars_list <- readRDS(here("data-raw/nam_selvars_list.RDS"))
census_dt <- readRDS(here("data-raw/census_dt.RDS"))
survey_dt <- readRDS(here("data-raw/survey_dt.RDS"))

census_dt$const_code <- as.numeric(census_dt$const_code)
survey_dt$new_const_code <- as.numeric(survey_dt$new_const_code)
census_dt["wta_hh"] <- lapply(haven::zap_labels(census_dt["wta_hh"]) ,as.numeric)
nam_selvars_list  <- intersect(intersect(nam_selvars_list, names(survey_dt)), names(census_dt))

nam_model <- ebp(fixed =as.formula(paste("lwel_PPP ~ ", paste(nam_selvars_list, collapse= "+"))),

               pop_data = as.data.frame(na.omit(census_dt[, c(nam_selvars_list,
                                                                                          "const_code"
                                                                                    )])),
               pop_domains = "const_code",

               smp_data = as.data.frame(na.omit(survey_dt[, c("wel_PPP",
                                            nam_selvars_list,
                                            "new_const_code", "hhweight")])),
               smp_domains = "new_const_code",

                # L = 100,

                # B = 100,

               transformation = "log",

               threshold = log(6249),

               weights = "hhweight",

               cpus = 1,

               MSE = FALSE,

               na.rm = TRUE)


shapefile <- st_read(here("shapefiles/gadm41_NAM_2.shp"))
plot(shapefile, max.plot=1)
