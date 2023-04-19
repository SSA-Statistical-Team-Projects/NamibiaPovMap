#-----------------------------------------------------
# selecting variables : lasso regression

library(data.table)
library(knitr)
library(RStata)
library(stats)
library(haven)
library(pacman)
library(here)
load(here("inst/data/cencus_dt.RData"))
load(here("inst/data/survey_dt.RData"))


candvar_list<-c("rururb" ,"wel_PPP",  "wall", "floor" ,"water14" ,
                "agecat", "relathh9", "marital5", "radio", "television" , "computer", "cellphone" , "landphone" ,
                "internet", "waterpipe", "piped", "cluster")

candvar_list <- c("rururb", "wall", "floor", "agecat", "hhsize")

survey_dt[,candvar_list] <- lapply(survey_dt[,candvar_list] ,as.numeric)
survey_dt$ea_id<-survey_dt$cluster
survey_dt$lwel_PPP<-log(survey_dt$wel_PPP)

dummy_dt <- dummify(survey_dt$region_name)

colnames(dummy_dt)[grepl("!karas", colnames(dummy_dt))] <- "karas"

colnames(dummy_dt) <- gsub(" ", "", colnames(dummy_dt))

colnames(dummy_dt) <- paste0("areadummy_", colnames(dummy_dt))

survey_dt$hhweight <- survey_dt$wta_hh

survey_dt <- cbind(survey_dt, dummy_dt)




#file.create("Rstata.log")

nam_selvars_list <-
  countrymodel_select_stata(dt = na.omit(survey_dt[,c("lwel_PPP", "hhweight",
                                                      "ea_id", colnames(dummy_dt),
                                                      candvar_list)]),
                            xvars = c(candvar_list, colnames(dummy_dt)),
                            y = "lwel_PPP",
                            weights = "hhweight",
                            selection = "BIC",
                            stata_path = "C:/Program Files/Stata17/StataMP-64",
                            stata_vnum = 17)




