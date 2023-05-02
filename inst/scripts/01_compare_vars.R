library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(weights)
library(descr)
library(knitr)
library(data.table)
library(stats)
library(pacman)
library(here)
library(haven)
library(stats)
library(openxlsx)

pacman::p_load(c("ggplot2", "gridExtra", "dplyr", "tidyr", "weights",
                 "descr", "knitr", "data.table", "stats", "here"))

load(here("inst/data/cencus_dt.RData"))
load(here("inst/data/survey_dt.RData"))


#------------------------------------------------------------------------------#
#Some changes to the data
#------------------------------------------------------------------------------#

census_dt$constituency_name <-
  ifelse(census_dt$constituency_name %in%
           c(" eengodi", " okankolo", " guinas"," tsumeb"),
         " nehale lyampingana",
         census_dt$constituency_name)

census_dt$const_code <-  ifelse(census_dt$const_code %in% c("1201", "1202",
                                                            "1203","1210"),
                                "1203",
                                census_dt$const_code)

survey_dt$contituency_name <-
  ifelse(survey_dt$contituency_name %in%
           c(" eengodi"," okankolo", " guinas"," tsumeb"),
         " nehale lyampingana",
         survey_dt$contituency_name)


survey_dt$new_const_code <- NA
for (i in 1:nrow(survey_dt)) {
  match_row <- which(census_dt$constituency_name == survey_dt$contituency_name[i] |
                       census_dt$constituency_name == survey_dt$contituency_prev_name[i])
        if (length(match_row) > 0) {
          survey_dt$new_const_code[i] <- census_dt$const_code[match_row[1]]
        }
      }

survey_dt$internet <- recode(survey_dt$internet, "Subscribed in the house"="Yes",
                             " Accessible outside the house" ="Yes" ,
                             "Either"="Yes",
                             "No internet"= "No")

survey_dt$wta_hh <-as.numeric(survey_dt$wta_hh)
census_dt["wta_hh"] <- lapply(haven::zap_labels(census_dt["wta_hh"]) ,as.numeric)

ID <- c("hid","region_code", "region_name", "const_code","wta_hh", "wta_pop" )

first_cand_varlist <- cand_var(pop_dt = census_dt, smp_dt = survey_dt, oth_var=ID)


# For the census
dt1 <- data.frame(dummify(census_dt[first_cand_varlist]))

colnames(dt1) <- gsub("\\.", "_", colnames(dt1))

names(dt1)[grep("waterpipe_Yes__unstated_whether_in_or_outside_premise",
                # this name is too long for a variable name in STATA
                names(dt1))]<- "waterpipe_Yes_outside_premise"

census_dt <- cbind(census_dt, dt1)
dummy_dt1 <- dummify(census_dt$region_name)
colnames(dummy_dt1)[grepl("!karas", colnames(dummy_dt1))] <- "karas"
colnames(dummy_dt1) <- gsub(" ", "", colnames(dummy_dt1))
colnames(dummy_dt1) <- paste0("areadummy_", colnames(dummy_dt1))
census_dt <- cbind(census_dt, dummy_dt1)


# For the survey
dt2 <- data.frame(dummify(survey_dt[first_cand_varlist]))

colnames(dt2) <- gsub("\\.", "_", colnames(dt1))

names(dt2)[grep("waterpipe_Yes__unstated_whether_in_or_outside_premise",
                # this name is too long for a variable name in STATA
                names(dt2))]<- "waterpipe_Yes_outside_premise"

survey_dt <- cbind(survey_dt, dt2)
dummy_dt2 <- dummify(survey_dt$region_name)
colnames(dummy_dt2)[grepl("!karas", colnames(dummy_dt2))] <- "karas"
colnames(dummy_dt2) <- gsub(" ", "", colnames(dummy_dt2))
colnames(dummy_dt2) <- paste0("areadummy_", colnames(dummy_dt2))
survey_dt <- cbind(survey_dt, dummy_dt2)


varlist=intersect(names(dt1), names(dt2))

#------------------------------------------------------------------------------#

dummy_dt  <- colnames(dummy_dt2)
saveRDS(dummy_dt, here("data-raw/dummy_dt.RDS") )
saveRDS(survey_dt, here("data-raw/survey_dt.RDS") )
saveRDS(census_dt, here("data-raw/census_dt.RDS") )

#------------------------------------------------------------------------------#
#                       Comparison of variables


Table <- ebp_test_means(smp_data =na.omit(survey_dt[,c(varlist,"wta_hh")]),

               pop_data = na.omit(census_dt[,c(varlist,"wta_hh")]),

               varlist = varlist ,

               smp_weights ="wta_hh",

               pop_weights = "wta_hh" )

#------------------------------------------------------------------------------#
write.csv(Table,here("inst/Report/Descriptive_Statitics.csv"), row.names = FALSE)


cand_varlist <- subset(Table, !is.na(pvalue)& pvalue>0.5 )$variable

saveRDS(cand_varlist, here("data-raw/cand_varlist.RDS") )
