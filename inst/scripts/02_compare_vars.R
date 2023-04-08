#-----------------------------------------------------
library(data.table)
library(haven)

load("~/World Bank/GitHub_Projects/NamibiaPovMap/inst/data/cencus_dt.RData")
load("~/World Bank/GitHub_Projects/NamibiaPovMap/inst/data/survey_dt.RData")
survey_dt$ea_id<-survey_dt$cluster
census_dt$ea_id<-census_dt$const_code
survey_dt$lwel_PPP=log(survey_dt$wel_PPP)


candvar_list<-c("rururb"  ,"rooms" ,
                "radio",  "television", "computer", "cellphone" , "landphone" ,"internet",
                "literacy", "everattd" )



ebp_test_means(smp_data =as.data.frame(na.omit(survey_dt[,c("lwel_PPP",candvar_list,"ea_id","wta_hh")])),
               pop_data = as.data.frame(na.omit(census_dt[,c(candvar_list, "ea_id","wta_hh")])),
               varlist = candvar_list, smp_weights ="wta_hh",pop_weights = "wta_hh" )
