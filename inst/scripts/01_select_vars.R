#-----------------------------------------------------
# selecting variables : lasso regression

library(data.table)
library(knitr)
library(RStata)
library(stats)
library(haven)
load("~/World Bank/GitHub_Projects/NamibiaPovMap/inst/data/cencus_dt.RData")
load("~/World Bank/GitHub_Projects/NamibiaPovMap/inst/data/survey_dt.RData")

survey_dt$ea_id <- survey_dt$cluster
survey_dt$lwel_PPP <- log(survey_dt$wel_PPP)


candvar_list<-c("rururb" ,"rooms" , "wall" , "roof",
             "floor", "fuelcook", "fuelligh", "heatsource", "water14", "toilet14",
              "relathh9", "agecat","marital5", "radio",
             "television", "computer", "cellphone" , "landphone" ,"internet",
             "literacy", "everattd", "educat5"  )

nam_selvars_list <-
  countrymodel_select_stata(dt = survey_dt[,c("lwel_PPP", "wta_hh",
                                                                 "ea_id",
                                                                 candvar_list)],
                            xvars = candvar_list,
                            y = "lwel_PPP",
                            weights = "wta_hh",
                            selection = "BIC",
                            stata_path = "C:/Program Files/Stata17/StataMP-64.exe",
                            stata_vnum = 17)




