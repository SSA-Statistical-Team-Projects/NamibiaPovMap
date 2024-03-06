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


#------------------------------------------------------------------------------#

pacman::p_load(dplyr, here, descr, data.table, tidyr, stats,  haven)


# source(here("R/01_littleworkerfunctions.R"))
load(here("inst/data/cencus_dt.RData"))
load(here("inst/data/survey_dt.RData"))

#------------------------------------------------------------------------------#
#Some changes to the data
#------------------------------------------------------------------------------#

## Grouping some constituencies in the census and generating a new constituency


census_dt$constituency_name <-   ifelse(
                  census_dt$constituency_name %in%  c(" eengodi", " okankolo",
                                                        " guinas"," tsumeb"),
                                                    " nehale lyampingana",
                                                census_dt$constituency_name)


census_dt$const_code <-  ifelse(
                    census_dt$const_code %in% c("1201", "1202",
                                                "1203","1210"),
                                              "1203",
                                              census_dt$const_code)


survey_dt$contituency_name <-   ifelse(
                    survey_dt$contituency_name %in% c(" eengodi"," okankolo",
                                                      " guinas"," tsumeb"),
                                             " nehale lyampingana",
                                             survey_dt$contituency_name)


survey_dt$region_name <-  ifelse(
                   survey_dt$region_name %in% c("kavango east", "kavango west"),
                                                    "kavango",
                                                    survey_dt$region_name)


survey_dt$internet <- recode(survey_dt$internet,
                             "Subscribed in the house"="Yes",
                             " Accessible outside the house" ="Yes" ,
                             "Either"="Yes",
                             "No internet"= "No")



survey_dt  <- rename(survey_dt, constituency_name = contituency_name,
                     constituency_prev_name = contituency_prev_name)


#------------------------------------------------------------------------------#

# harmonization of constituencies names and constituencies code between  survey
# and census

#------------------------------------------------------------------------------#

survey_dt$new_const_code <- NA

for (i in 1:nrow(survey_dt)) {

  match_row <- which(

    census_dt$constituency_name == survey_dt$constituency_name[i] |

      census_dt$constituency_name == survey_dt$constituency_prev_name[i])

  if (length(match_row) > 0) {

    survey_dt$new_const_code[i] <- census_dt$const_code[match_row[1]]
  }

}

rm(i, match_row)


#------------------------------------------------------------------------------#
survey_dt$wta_hh <-as.numeric(survey_dt$wta_hh)

census_dt["wta_hh"] <- lapply(haven::zap_labels(census_dt["wta_hh"]) ,as.numeric)

ID <- c("hid","region_code", "region_name", "const_code",
        "wta_hh", "wta_pop", "constituency_name" )

first_cand_varlist <- cand_var(pop_dt = census_dt,
                               smp_dt = survey_dt,
                               oth_var= ID)


#------------------------------------------------------------------------------#
# For the census

dt1 <- data.frame(dummify(census_dt[first_cand_varlist]))

colnames(dt1) <- gsub("\\.", "_", colnames(dt1))

names(dt1)[grep("waterpipe_Yes__unstated_whether_in_or_outside_premise",

                           names(dt1))]<- "waterpipe_Yes_outside_premise"

census_dt <- cbind(census_dt, dt1)

dummy_dt1 <- dummify(census_dt$region_name)

colnames(dummy_dt1)[grepl("!karas", colnames(dummy_dt1))] <- "karas"

colnames(dummy_dt1) <- gsub(" ", "", colnames(dummy_dt1))

colnames(dummy_dt1) <- paste0("areadummy_", colnames(dummy_dt1))

census_dt <- cbind(census_dt, dummy_dt1)

#------------------------------------------------------------------------------#
## For the survey

dt2 <- data.frame(dummify(survey_dt[first_cand_varlist]))


colnames(dt2) <- gsub("\\.", "_", colnames(dt2))

names(dt2)[grep("waterpipe_Yes__unstated_whether_in_or_outside_premise",

                names(dt2))]<- "waterpipe_Yes_outside_premise"

survey_dt <- cbind(survey_dt, dt2)

dummy_dt2 <- dummify(survey_dt$region_name)

colnames(dummy_dt2)[grepl("!karas", colnames(dummy_dt2))] <- "karas"

colnames(dummy_dt2) <- gsub(" ", "", colnames(dummy_dt2))

colnames(dummy_dt2) <- paste0("areadummy_", colnames(dummy_dt2))

survey_dt <- cbind(survey_dt, dummy_dt2)

#------------------------------------------------------------------------------#

varlist=intersect(names(dt1), names(dt2))

saveRDS(varlist, here("data-raw/varlist.RDS") )

dummy_dt  <- colnames(dummy_dt2)

saveRDS(dummy_dt, here("data-raw/dummy_dt.RDS") )

saveRDS(survey_dt, here("data-clean/survey_dt.RDS") )

saveRDS(census_dt, here("data-clean/census_dt.RDS") )
