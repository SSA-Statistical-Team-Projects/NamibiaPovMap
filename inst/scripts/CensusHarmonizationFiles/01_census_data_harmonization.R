## Meta-Information ######------######------######------
## Author: Hardika Dayalani (hardika.dayalani@gmail.com)
## Creation: Namibia Small Area Poverty Estimates 
## Description: Clean Census 2011 data for harmonization

## Environment Setup ######------######------######------

rm(list =  ls())

## Set Working Directory
getwd()
setwd("C:\Users\wb604749\OneDrive - WBG\Documents\World Bank\NamibiaPovMap\data-raw")

## Load Libraries 
library(haven)
library(data.table)

## Geographic Data ######------######------######------

## Import Constituency Data
const_code_df <-  read.csv("Namibia_Constituency_Code.txt", header = FALSE)

## Clean Constituency Data
names(const_code_df) <-  c("const_code", "region", "constituency")
const_code_df$const_code <-  sprintf("%04.f", const_code_df$const_code)
const_code_df$region_code <-  substr(const_code_df$const_code, 1, 2)

## Region Code .
region_code_df <-  const_code_df[, c("region", "region_code")]
region_code_df <-  unique(region_code_df)

## Housing Data ######------######------######------

## Import Data
house_df <-  read_dta("Housing_2011.dta")
names(house_df) <-  tolower(names(house_df))

## Subset relevant variables
temp <-  c("region", "constituency", "hh_type", "ea_code", "du_number",
           "hh_number", "h1", "h4", "h5", "h6", "h7", "h8a", 
           "h8b", "h8c", "h9", "h10", "h14")

house_df <-  house_df[, temp] 
## Dropped 11 variables from 28 to 17

## Subset to conventional households
##house_df <-  house_df[house_df$hh_type == 100, ]



## region names
house_df$region_code <-  sprintf("%02.f", house_df$region)
temp <-  match(house_df$region_code, region_code_df$region_code)
house_df$region_name <-  region_code_df$region[temp]

## constituency
house_df$const_code <-  paste0(house_df$region_code, sprintf("%02.f", house_df$constituency))
temp <-  match(house_df$const_code, const_code_df$const_code)
house_df$constituency_name <-  const_code_df$constituency[temp]


house_df <- house_df[!is.na(house_df$hh_number),]
## Unique Household ID
house_df$hid <-  paste0(house_df$const_code, 
                      sprintf("%04.f", house_df$ea_code),
                      sprintf("%04.f", house_df$du_number),
                      sprintf("%04.f", house_df$hh_number))


## Household Weight
house_df$wta_hh = 1

## dweltyp (Type of Dwelling)
## H1. Housing Type
house_df$dweltyp <-  NA
house_df$dweltyp[house_df$h1 == 1] = "1"
house_df$dweltyp[house_df$h1 == 2] = "6"
house_df$dweltyp[house_df$h1 %in% c(3, 4)] = "3"
house_df$dweltyp[house_df$h1 == 5] = "7"
house_df$dweltyp[house_df$h1 %in% c(6, 7, 10)] = "9"
house_df$dweltyp[house_df$h1 %in% c(8, 9)] = "8"

house_df$dweltyp <-  factor(house_df$dweltyp, 
                          levels =  1:9, 
                          labels =  c("1" = "Detached house", 
                                     "2" = "Multi-family house",
                                     "3" = "Separate apartment",
                                     "4" = "Communal apartment", 
                                     "5" = "Room in a larger dwelling",
                                     "6" = "Several buildings connected",
                                     "7" = "Several separate buildings",
                                     "8" = "Improvised housing unit",
                                     "9" = "Other"))

## rooms (Number of habitable rooms)
## H4. Sleeping Rooms
house_df$rooms <-  house_df$h4

## roof (Main material used for roof)
## H6. Roof
house_df$roof <-  NA
house_df$roof[house_df$h6 == 1] = "12"
house_df$roof[house_df$h6 == 2] = "9"
house_df$roof[house_df$h6 == 3] = "7"
house_df$roof[house_df$h6 == 4] = "11"
house_df$roof[house_df$h6 == 5] = "1"
house_df$roof[house_df$h6 %in% c(6, 7, 8, 10)] = "15"
house_df$roof[house_df$h6 == 8] = "3"

house_df$roof <-  factor(house_df$roof, 
                       levels =  1:15, 
                       labels =  c("1" = "Natural - Thatch/palm leaf", 
                                  "2" = "Natural - Sod",
                                  "3" = "Natural - Other", 
                                  "4" = "Rudimentary - Rustic mat",
                                  "5" = "Rudimentary - Palm/bamboo",
                                  "6" = "Rudimentary - Wood planks",
                                  "7" = "Rudimentary - Other", 
                                  "8" = "Finished - Roofing",
                                  "9" = "Finished - Asbestos", 
                                  "10" = "Finished - Tile",
                                  "11" = "Finished - Concrete", 
                                  "12" = "Finished - Metal tile", 
                                  "13" = "Finished - Roofing shingles",
                                  "14" = "Finished - Other",
                                  "15" = "Other - Specific")) 

## wall (Main material used for external walls)
## H5. Outer Walls
house_df$wall <-  NA
house_df$wall[house_df$h5 == 1] = "13"
house_df$wall[house_df$h5 %in% c(2, 8, 9)] = "19"
house_df$wall[house_df$h5 == 3] = "3"
house_df$wall[house_df$h5 == 4] = "18"
house_df$wall[house_df$h5 == 5] = "17"
house_df$wall[house_df$h5 %in% c(6, 7)] = "10"

house_df$wall <-  factor(house_df$wall, 
                       levels =  1:19,
                       labels =  c("1" = "Natural - Cane/palm/trunks",
                                  "2" = "Natural - Dirt",
                                  "3" = "Natural - Other", 
                                  "4" = "Rudimentary - Bamboo with mud",
                                  "5" = "Rudimentary - Stone with mud",
                                  "6" = "Rudimentary - Uncovered adobe",
                                  "7" = "Rudimentary - Plywood",
                                  "8" = "Rudimentary - Cardboard",
                                  "9" = "Rudimentary - Reused wood",
                                  "10" = "Rudimentary - Other", 
                                  "11" = "Finished - Woven Bamboo", 
                                  "12" = "Finished - Stone with lime/cement",
                                  "13" = "Finished - Cement blocks", 
                                  "14" = "Finished - Covered adobe",
                                  "15" = "Finished - Wood planks/shingles",
                                  "16" = "Finished - Plaster wire",
                                  "17" = "Finished - GRC/Gypsum/Asbestos", 
                                  "18" = "Finished - Other", 
                                  "19" = "Other")) 

## floor (Main material used for floor)
## H7. Floor
house_df$floor <-  NA
house_df$floor[house_df$h7 == 1] = "1"
house_df$floor[house_df$h7 %in% c(2, 5)] = "11"
house_df$floor[house_df$h7 == 3] = "3"
house_df$floor[house_df$h7 == 4] = "4"
house_df$floor[house_df$h7 == 6] = "10"
house_df$floor[house_df$h7 == 7] = "14"

house_df$floor <-  factor(house_df$floor,
                        levels =  1:14,
                        labels =  c("1" = "Natural - Earth/sand", 
                                   "2" = "Natural - Dung",
                                   "3" = "Natural - Other", 
                                   "4" = "Rudimentary - Wood planks", 
                                   "5" = "Rudimentary - Palm/bamboo",
                                   "6" = "Rudimentary - Other",
                                   "7" = "Finished - Parquet or polished wood",
                                   "8" = "Finished - Vinyl or asphalt strips",
                                   "9" = "Finished - Ceramic/marble/granite",
                                   "10" = "Finished - Floor tiles/teraso", 
                                   "11" = "Finished - Cement/red bricks", 
                                   "12" = "Finished - Carpet",
                                   "13" = "Finished - Other",
                                   "14" = "Other - Specific")) 

## water14 (Sources of drinking water (14 categories))
## H9. Drinking Water Source
house_df$water14 <-  NA
house_df$water14[house_df$h9 == 1] = "1"
house_df$water14[house_df$h9 == 2] = "2"
house_df$water14[house_df$h9 == 3] = "3"
house_df$water14[house_df$h9 %in% c(4, 5)] = "4"
house_df$water14[house_df$h9 %in% c(6, 7)] = "13"
house_df$water14[house_df$h9 == 8] = "5"
house_df$water14[house_df$h9 == 9] = "10"
house_df$water14[house_df$h9 == 10] = "14"


## water8. Main source of drinking water (8 categories)
## water14. Main source of drinking water (14 categories)
house_df$water8 <-  NA
house_df$water8[house_df$water14 == 1] = "1"
house_df$water8[house_df$water14 %in% c(2, 3)] = "2"
house_df$water8[house_df$water14 %in% c(4, 5, 6)] = "3"
house_df$water8[house_df$water14 %in% c(9, 10)] = "4"
house_df$water8[house_df$water14 ==13] = "5"
house_df$water8[house_df$water14 == 8] = "6"
house_df$water8[house_df$water14 %in% c(11, 12)] = "7"
house_df$water8[house_df$water14 == 14] = "8"

house_df$water8 <- factor(house_df$water8,
                          levels =  1:8, 
                          labels =  c("1" = "Piped water (own tap)",
                                     "2" = "Public tap or standpipe", 
                                     "3" = "Protected well",
                                     "4" = "Unprotected well",
                                     "5" = "Surface water",
                                     "6" = "Rainwater",
                                     "7" = "Tanker-truck, vendor",
                                     "8" = "Other"))

##water8. Will be delete beacause of 57.93% of missing data



## waterpipe. Household has piped water
## water14. Main source of drinking water (14 categories)
house_df$waterpipe <-  "0"
house_df$waterpipe[house_df$water14 %in% c(1, 2) ]= "1"
house_df$waterpipe[house_df$water14 ==3] = "2"
house_df$waterpipe <- ifelse(is.na(house_df$water14), NA, house_df$waterpipe)

house_df$waterpipe <- factor(house_df$waterpipe,
                             levels =  0:3, 
                             labels =  c("0" = "No",
                                        "1" = "Yes, in premise", 
                                        "2" = "Yes, but Not in premise",
                                        "3" = "Yes, unstated whether in or outside premise"))


## piped. Access to piped water
## water14. Main source of drinking water (14 categories)
house_df$piped <- "0"
house_df$piped[house_df$water14 %in% c(1, 2, 3)] = "1"
house_df$piped[!(house_df$water14 %in% c(1, 2, 3))] = "0"
house_df$piped <- ifelse(is.na(house_df$water14), NA, house_df$piped)

house_df$piped <- factor(house_df$piped,
                         levels =  0:1, 
                         labels =  c("0" = "No",
                                    "1" = "Yes"))


## piped_to_prem. Access to piped water on premises
## water14. Main source of drinking water (14 categories)
house_df$piped_to_prem <- "0"
house_df$piped_to_prem [house_df$water14 %in% c(1, 2)] = "1"
house_df$piped_to_prem [!(house_df$water14 %in% c(1, 2))] = "0"
house_df$piped_to_prem <- ifelse(is.na(house_df$water14), NA, house_df$piped_to_prem)

house_df$piped_to_prem  <- factor(house_df$piped_to_prem ,
                         levels =  0:1, 
                         labels =  c("0" = "No",
                                    "1" = "Yes"))


## imp_wat_rec. Household has improved water sources
## water14. Main source of drinking water (14 categories)
house_df$imp_wat_rec <- "0"
house_df$imp_wat_rec [house_df$water14 %in% c(1:6,8)] = "1"
house_df$imp_wat_rec [!(house_df$water14 %in% c(1:6,8))] = "0"
house_df$imp_wat_rec <- ifelse(is.na(house_df$water14), NA, house_df$imp_wat_rec)

house_df$imp_wat_rec  <- factor(house_df$imp_wat_rec ,
                                  levels =  0:1, 
                                  labels =  c("0" = "No",
                                             "1" = "Yes"))


house_df$water14 <-  factor(house_df$water14,
                          levels =  1:14, 
                          labels =  c("1" = "Piped water into dwelling",
                                     "2" = "Piped water to yard/plot", 
                                     "3" = "Public tap or standpipe",
                                     "4" = "Tube well or borehole",
                                     "5" = "Protected dug well",
                                     "6" = "Protected spring",
                                     "7" = "Bottled water",
                                     "8" = "Rainwater",
                                     "9" = "Unprotected spring",
                                     "10" = "Unprotected dug well",
                                     "11" = "Cart with small tank/drum",
                                     "12" = "Tanker-truck",
                                     "13" = "Surface water",
                                     "14" = "Other"))


## toilet14 (Main sanitation facility (14 categories))
## H10. Toilet Facility
house_df$toilet14 <-  NA
house_df$toilet14[house_df$h10 %in% c(1, 2)] = "2"
house_df$toilet14[house_df$h10 %in% c(3, 4)] = "3"
house_df$toilet14[house_df$h10 == 5] = "5"
house_df$toilet14[house_df$h10 == 6] = "6"
house_df$toilet14[house_df$h10 == 7] = "10"
house_df$toilet14[house_df$h10 == 8] = "11"
house_df$toilet14[house_df$h10 == 9] = "13"
house_df$toilet14[house_df$h10 == 10] = "14"



## toilet6. Household has improved water sources
## toilet14 (Main sanitation facility (14 categories))
house_df$toilet6 <-  NA
house_df$toilet6[house_df$toilet14 %in% c(1,2,3)] = "1"
house_df$toilet6[house_df$toilet14 ==5] = "2"
house_df$toilet6[house_df$toilet14 ==7] = "3"
house_df$toilet6[house_df$toilet14 ==6] = "4"
house_df$toilet6[house_df$toilet14 ==13] = "5"
house_df$toilet6 [!(house_df$toilet14 %in% c(1:3,5:7,13))] = "9"
house_df$toilet6 <- ifelse(is.na(house_df$toilet14), NA, house_df$toilet6)



## open_def. Access to any sanitation facility
## toilet14 (Main sanitation facility (14 categories))
house_df$open_def <-  NA
house_df$open_def <- ifelse(house_df$toilet14==13 | house_df$toilet14==14, "1", "0")
house_df$open_def <- ifelse(is.na(house_df$toilet14), NA, house_df$open_def)

house_df$open_def <- factor(house_df$open_def,
                                            levels =  0:1, 
                                            labels =  c("0" = "No",
                                                       "1" = "Yes"))


## toiletshared (Is toilet facility shared with other households?)
house_df$toiletshared <-  NA
house_df$toiletshared[house_df$h10 %in% c(2, 4)] <-  1
house_df$toiletshared[house_df$h10 %in% c(1, 3, 5:8)] <-  0
##toiletshared. Will be delete beacause of 49.26% of missing data


## imp_san_rec. access to improved sanitation
house_df$imp_san_rec <- "0"
house_df$imp_san_rec [house_df$toilet6 %in% c(1:4)] = "1"
house_df$imp_san_rec [!(house_df$toilet6 %in% c(1:4))] = "0"
house_df$imp_san_rec <- ifelse(is.na(house_df$toilet6), NA, house_df$imp_san_rec)
house_df$imp_san_rec <- ifelse(house_df$toiletshared == 1, "0", house_df$imp_san_rec)
##imp_san_rec. Will be delete beacause of 49.26% of missing data


house_df$imp_san_rec  <- factor(house_df$imp_san_rec ,
                                levels =  0:1, 
                                labels =  c("0" = "No",
                                           "1" = "Yes"))


house_df$toilet6  <- factor(house_df$toilet6 ,
                            levels =  c(1:5,9), 
                            labels =  c("1" = "Flush toilet",
                                       "2" = "Ventilated Improved Pit (VIP) latrine", 
                                       "3" = "Composting toilet",
                                       "4" = "Pit latrine with slab",
                                       "5" = "No facility",
                                       "9" = "Other"))


house_df$toilet14 <-  factor(house_df$toilet14, 
                           levels =  1:14,
                           labels =  c("1" = "A flush toilet",
                                      "2" = "A piped sewer system",
                                      "3" = "A septic tank",
                                      "4" = "Pit latrine",
                                      "5" = "Ventilated improved pit latrine (VIP)",
                                      "6" = "Pit latrine with slab",
                                      "7" = "Composting toilet",
                                      "8" = "Special case",
                                      "9" = "A flush/pour flush to elsewhere",
                                      "10" = "A pit latrine without slab",
                                      "11" = "Bucket",
                                      "12" = "Hanging toilet or hanging latrine",
                                      "13" = "No facilities or bush or field",
                                      "14" = "Other"))

house_df$toiletshared  <- factor(house_df$toiletshared ,
                               levels =  0:1, 
                               labels =  c("0" = "No",
                                          "1" = "Yes"))

## fuelcook (Main cooking fuel)
## H8a: Cooking
house_df$fuelcook <-  NA
house_df$fuelcook[house_df$h8a %in% c(1, 2)] = "4"
house_df$fuelcook[house_df$h8a == 3] = "5"
house_df$fuelcook[house_df$h8a == 4] = "2"
house_df$fuelcook[house_df$h8a == 5] = "1"
house_df$fuelcook[house_df$h8a == 6] = "3"
house_df$fuelcook[house_df$h8a %in% c(8, 9, 11)] = "9"



## fuelligh (Main source of lighting) 
## H8b: Lighting
house_df$fuelligh <-  NA
house_df$fuelligh[house_df$h8b %in% c(1, 2)] = "1"
house_df$fuelligh[house_df$h8b == 3] = "4"
house_df$fuelligh[house_df$h8b == 4] = "2"
house_df$fuelligh[house_df$h8b %in% c(5, 6, 8, 9, 11)] = "9"
house_df$fuelligh[house_df$h8b == 7] = "3"
house_df$fuelligh[house_df$h8b == 11] = "10"


## electyp. Source of energy
house_df$electyp <- NA
house_df$electyp[house_df$fuelcook == 4|house_df$fuelligh == 1] = "1"
house_df$electyp[(house_df$fuelcook == 5|house_df$fuelligh == 4)&(is.na(house_df$electyp))] = "2"
house_df$electyp[(house_df$fuelcook == 2|house_df$fuelligh %in% c(2,3))&(is.na(house_df$electyp))] = "3"
house_df$electyp[(house_df$fuelcook %in% c(1,3,9)|house_df$fuelligh == 9)&(is.na(house_df$electyp))] = "4"
house_df$electyp[house_df$fuelcook == 10 & house_df$fuelligh == 10] = "10"


house_df$fuelcook <-  factor(house_df$fuelcook, 
                           levels =  c(1:5, 9, 10),
                           labels =  c("1" = "Firewood",
                                      "2" = "Kerosene",
                                      "3" = "Charcoal",
                                      "4" = "Electricity",
                                      "5" = "Gas",
                                      "9" = "Other",
                                      "10" = "None"))

house_df$fuelligh <-  factor(house_df$fuelligh, 
                           levels =  c(1:4, 9, 10),
                           labels =  c("1" = "Electricity",
                                      "2" = "Kerosene",
                                      "3" = "Candles",
                                      "4" = "Gas",
                                      "9" = "Other",
                                      "10" = "None"))


house_df$electyp <-  factor(house_df$electyp, 
                           levels =  c(1:4, 10),
                           labels =  c("1" = "Electricity",
                                      "2" = "Gas",
                                      "3" = "Lamp",
                                      "4" = "Others",
                                      "10" = "None"))



## heatsource (Main source of heating)
## H8c: Heating
house_df$heatsource <-  NA
house_df$heatsource[house_df$h8c %in% c(1, 2)] = "4"
house_df$heatsource[house_df$h8c == 3] = "5"
house_df$heatsource[house_df$h8c == 5] = "1"
house_df$heatsource[house_df$h8c == 6] = "3" 
house_df$heatsource[house_df$h8c %in% c(8, 9, 11)] = "9"
house_df$heatsource[house_df$h8c == 10] = "10" 

house_df$heatsource <-  factor(house_df$heatsource, 
                             levels =  c(1:6, 9, 10),
                             labels =  c("1" = "Firewood",
                                        "2" = "Kerosene",
                                        "3" = "Charcoal",
                                        "4" = "Electricity",
                                        "5" = "Gas",
                                        "6" = "Central", 
                                        "9" = "Other",
                                        "10" = "No heating"))

## Subset to Harmonized Variables 
temp <-  c("region_code", "region_name", "const_code", "constituency_name", "hid",
        "dweltyp", "rooms", "roof", "wall", "floor", "water14", "waterpipe", "piped", "piped_to_prem", "imp_wat_rec" , 
         "toilet14", "toilet6", "open_def", "fuelcook", "fuelligh", "electyp", "heatsource")

house_df <-  house_df[, temp] 

## Person Data ######------######------######------

## Import Data
per_df <-  read_dta("Persons_2011.dta")
names(house_df) <-  tolower(names(house_df))
names(per_df) <-  tolower(names(per_df))
per_df <-  setDT(per_df)

## Subset to conventional households
##per_df <-  per_df[per_df$hh_type == 100, ]
## Dropped 53547 observations 

## Subset to relevant variables 
temp <-  c("region", "constituency", "ea_code", "du_number", "hh_number", "urban_rural", "b3", "b4", "b5",
         "b8", "b19b", "b19c", "b19d", "b19e", "b19f", "b19i", "d1a", "d2", "d3",
         "e1", "e2", "e3", "e4")

per_df <-  per_df[, .SD, .SDcols =  temp]
## Dropped 30 variables 

per_df <- per_df[!is.na(per_df$hh_number),]
## Unique Household ID
per_df$hid <-  paste0(sprintf("%02.f", per_df$region),
                    sprintf("%02.f", per_df$constituency),
                    sprintf("%04.f", per_df$ea_code),
                    sprintf("%04.f", per_df$du_number),
                    sprintf("%04.f", per_df$hh_number))

## Weights
per_df$wta_pop = 1

## urban_rural
per_df$rururb <-  NA
per_df$rururb[per_df$urban_rural == 1] = "1"
per_df$rururb[per_df$urban_rural == 2] = "0"
per_df$rururb <-  factor(per_df$rururb, labels =  c("0" = "rural",
                                                     "1" = "urban"))


## agecat (Age Group)
## B5: Age
temp_df <-  data.frame(code =  as.character(c(0:15)), 
                     label =  c("0",
                               "01-04",
                               "05-09",
                               "10-14",
                               "15-19",
                               "20-24",
                               "25-29",
                               "30-34",
                               "35-39",
                               "40-44",
                               "45-49",
                               "50-54",
                               "55-59",
                               "60-64",
                               "65-74",
                               "75+"))
per_df$b5 <-  as.character(per_df$b5)
temp <-  match(per_df$b5, temp_df$code)
per_df$agecat <-  temp_df$label[temp]

## B4: Sex ## c(`0.Female` =  0, `1. Male` =  1)
per_df$sex <-  per_df$b4 - 1
per_df$sex <-  factor(per_df$sex, labels =  c("0" = "Female",
                                           "1" = "Male"))

## relathh9 (Relationship to head of household harmonized across all regions)
## B3. Relationship
per_df$relathh9 <-  NA
per_df$relathh9[per_df$b3 == 1] = "1"
per_df$relathh9[per_df$b3 == 2] = "2"
per_df$relathh9[per_df$b3 == 3] = "3"
per_df$relathh9[per_df$b3 == 4] = "6"
per_df$relathh9[per_df$b3 == 5] = "5"
per_df$relathh9[per_df$b3 == 6] = "4"
per_df$relathh9[per_df$b3 == 7] = "7"
per_df$relathh9[per_df$b3 == 8] = "8"
per_df$relathh9[per_df$b3 == 9] = "9"



## relathh6. Relationship to household head (6 categories)
per_df$relathh6 <- NA
per_df$relathh6 [per_df$relathh9==1 ] <-  1
per_df$relathh6 [per_df$relathh9==2 ] <-  2
per_df$relathh6 [per_df$relathh9==3 ] <-  3
per_df$relathh6 [per_df$relathh9==4 ] <-  4
per_df$relathh6 [per_df$relathh9 %in% c(5:7)] <-  5
per_df$relathh6 [per_df$relathh9 %in% c(8:9)] <-  6



per_df$relathh9 <-  factor(per_df$relathh9, 
                         levels =  1:9,
                         labels =  c("1" = "Head",
                                    "2" = "Spouse",
                                    "3" = "Child",
                                    "4" = "Parents/parents-in-law",
                                    "5" = "Grandchild",
                                    "6" = "Son-in-law/daughter-in-law",
                                    "7" = "Other relative",
                                    "8" = "Domestic help/boarder",
                                    "9" = "Non-relative"))


per_df$relathh6 <-  factor(per_df$relathh6, 
                         levels =  1:6,
                         labels =  c("1" = "Head",
                                    "2" = "Spouse",
                                    "3" = "Child",
                                    "4" = "Parents",
                                    "5" = "Other relative",
                                    "6" = "Non-relative"))






## marital5 (Marital status)
## B8. Marital Status
per_df$marital5 <-  NA
per_df$marital5[per_df$b8 == 1] = "2"
per_df$marital5[per_df$b8 %in% c(2, 3)] = "1"
per_df$marital5[per_df$b8 == 4] = "3"
per_df$marital5[per_df$b8 %in% c(5, 7)] = "4"
per_df$marital5[per_df$b8 == 6] = "5"

per_df$marital5 <-  factor(per_df$marital5, 
                         levels =  1:5,
                         labels =  c("1" = "Married",
                                    "2" = "Never married", 
                                    "3" = "Living together",
                                    "4" = "Divorced/Separated",
                                    "5" = "Widowed"))

## literacy (Individual can read and write)
## D1. Literacy
per_df$literacy <- ifelse(per_df$d1a == 0, 1, 0)
per_df$literacy <-  factor(per_df$literacy, labels =  c("1" = "yes",
                                                     "0" = "No"))

## everattd (Ever attended school)
## D2. Ever Attended School
per_df$everattd <-  NA
per_df$everattd[per_df$d2 == 1] = "0"
per_df$everattd[per_df$d2 %in% 2:5] = "1"

per_df$everattd <-  factor(per_df$everattd, labels =  c("1" = "yes",
                                                     "0" = "No"))

## educat5 (Highest level of education completed (5 categories))
## D3. Highest Grade Completed
per_df$educat5 <-  per_df$d3 + 1
per_df$educat5[per_df$educat5 > 5] <-  NA

per_df$educat5 <-  factor(per_df$educat5, labels =  c("1" = "No education",
                                                   "2" = "Primary incomplete",
                                                   "3" = "Primary complete but Secondary incomplete",
                                                   "4" = "Secondary complete",
                                                   "5" = "Tertiary (completed or incomplete)"))

## lstatus (Labor status (7-day ref period))
## E1. Work
per_df$lstatus <-  NA
per_df$lstatus[per_df$e1==1 |per_df$e1==2] <-  "1"
per_df$lstatus[per_df$e1==3] <-  "2"
per_df$lstatus[per_df$e1==4 |per_df$e1==5|per_df$e1==6|per_df$e1==7|per_df$e1==8|per_df$e1==9|per_df$e1==10|per_df$e1==11|per_df$e1==12|per_df$e1==13] <-  "3"
per_df$lstatus <-  factor(per_df$lstatus, labels =  c("1" = "Employed",
                                              "2" = "Unemployed",
                                              "3" = "Not in labor force"))

## industrycat10_year (1 digit industry classification, primary job (12-mon ref period))
## E3. Industry
per_df$industrycat10_year <-  NA
per_df$industrycat10_year[per_df$e3 == 1] = "1"
per_df$industrycat10_year[per_df$e3 == 2] = "2"
per_df$industrycat10_year[per_df$e3 == 3] = "3"
per_df$industrycat10_year[per_df$e3 %in% c(4, 5)] = "4"
per_df$industrycat10_year[per_df$e3 == 6] = "5"
per_df$industrycat10_year[per_df$e3 == 7] = "6"
per_df$industrycat10_year[per_df$e3 %in% c(8, 10)] = "7"
per_df$industrycat10_year[per_df$e3 %in% c(9, 16:21)] = "10"
per_df$industrycat10_year[per_df$e3 %in% c(11:14)] = "8"
per_df$industrycat10_year[per_df$e3 == 15] = "9"
##industrycat10_year Will be delete beacause of 45.87% of missing data

##industrycat4_year  ( 4-category industry classification, secondary job (12-mon ref period))
## industrycat10_year (1 digit industry classification, primary job (12-mon ref period))
per_df$industrycat4_year <-  NA
per_df$industrycat4_year[per_df$industrycat10_year == 1] = "1"
per_df$industrycat4_year[per_df$industrycat10_year %in% c(2,3,5)] = "2"
per_df$industrycat4_year[per_df$industrycat10_year %in% c(6,7,8,9,10)] = "3"
per_df$industrycat4_year[per_df$industrycat10_year %in% c(4)] = "4"
##industrycat4_year Will be delete beacause of 45.87% of missing data

per_df$industrycat10_year <-  factor(per_df$industrycat10_year,
                                   levels =  1:10,
                                   labels =  c("1" = "Agriculture, Hunting, Fishing, etc.",
                                              "2" = "Mining",
                                              "3" = "Manufacturing",
                                              "4" = "Public Utility Services",
                                              "5" = "Construction",
                                              "6" = "Commerce",
                                              "7" = "Transport and Communications",
                                              "8" = "Financial and Business Services",
                                              "9" = "Public Administration",
                                              "10" = "Others Services, Unspecified"))

per_df$industrycat4_year <-  factor(per_df$industrycat4_year,
                                   levels =  1:4,
                                   labels =  c("1" = "Agriculture",
                                              "2" = "Industry",
                                              "3" = "Services",
                                              "4" = "Other"))


## occup_year (1 digit occupational classification, primary job)
## E2. Occupation 
per_df$occup_year <-  per_df$e2 - 1
per_df$occup_year[per_df$occup_year == 0] <-  10
per_df$occup_year[per_df$occup_year == 98] <-  99
##occup_year Will be delete beacause of 45.56% of missing data

per_df$occup_year <-  factor(per_df$occup_year, 
                           levels =  c(1:10, 99),
                           labels =  c("1" = "Managers",
                                      "2" = "Professionals",
                                      "3" = "Technicians and associate professionals",
                                      "4" = "Clerical support workers",
                                      "5" = "Service and sales workers",
                                      "6" = "Skilled agricultural, forestry and fishery workers",
                                      "7" = "Craft and related trades workers",
                                      "8" = "Plant and machine operators, and assemblers",
                                      "9" = "Elementary occupations",
                                      "10" = "Armed forces occupations",
                                      "99" = "Other/unspecified")) 

## B19B. ICT: Radio
per_df$radio <-  per_df$b19b
per_df$radio[per_df$radio == 2] <-  0

temp_df <-  per_df[, sum(radio), by =  hid]
temp_df$V1[temp_df$V1 > 0] <-  1
temp <-  match(per_df$hid, temp_df$hid)
per_df$radio <-  temp_df$V1[temp]
per_df$radio <- factor(per_df$radio,
                            levels <-  0:1, 
                            labels <-  c("0" = "No",
                                         "1" = "Yes"))


## B19C. ICT: TV
per_df$television <-  per_df$b19c
per_df$television[per_df$television == 2] <-  0

temp_df <-  per_df[, sum(television), by =  hid]
temp_df$V1[temp_df$V1 > 0] <-  1
temp <-  match(per_df$hid, temp_df$hid)
per_df$television <-  temp_df$V1[temp]
per_df$television <- factor(per_df$television,
                       levels <-  0:1, 
                       labels <-  c("0" = "No",
                                    "1" = "Yes"))


## B19D. ICT: Computer
per_df$computer <-  per_df$b19d
per_df$computer[per_df$computer == 2] <-  0

temp_df <-  per_df[, sum(computer), by =  hid]
temp_df$V1[temp_df$V1 > 0] <-  1
temp <-  match(per_df$hid, temp_df$hid)
per_df$computer <-  temp_df$V1[temp]
per_df$computer <- factor(per_df$computer,
                       levels <-  0:1, 
                       labels <-  c("0" = "No",
                                    "1" = "Yes"))


## B19E. ICT: Cell Phone
per_df$cellphone <-  per_df$b19e
per_df$cellphone[per_df$cellphone == 2] <-  0

temp_df <-  per_df[, sum(cellphone), by =  hid]
temp_df$V1[temp_df$V1 > 0] <-  1
temp <-  match(per_df$hid, temp_df$hid)
per_df$cellphone <-  temp_df$V1[temp]

per_df$cellphone <- factor(per_df$cellphone,
                       levels <-  0:1, 
                       labels <-  c("0" = "No",
                                    "1" = "Yes"))


## B19F. ICT: Telephone (Fixed)
per_df$landphone <-  per_df$b19f
per_df$landphone[per_df$landphone == 2] <-  0

temp_df <-  per_df[, sum(landphone), by =  hid]
temp_df$V1[temp_df$V1 > 0] <-  1
temp <-  match(per_df$hid, temp_df$hid)
per_df$landphone <-  temp_df$V1[temp]

per_df$landphone <- factor(per_df$landphone,
                       levels <-  0:1, 
                       labels <-  c("0" = "No",
                                    "1" = "Yes"))


## B19I. ICT: Internet
per_df$internet <-  per_df$b19i
per_df$internet[per_df$internet == 2] <-  0

temp_df <-  per_df[, sum(internet), by =  hid]
temp_df$V1[temp_df$V1 > 0] <-  1
temp <-  match(per_df$hid, temp_df$hid)
per_df$internet <-  temp_df$V1[temp]

per_df$internet <- factor(per_df$internet,
                       levels <-  0:1, 
                       labels <-  c("0" = "No",
                                    "1" = "Yes"))


## Subset to Harmonized Variables 
temp <-  c("hid", "rururb", "agecat", "sex", "relathh9", "relathh6", "marital5", "literacy", "everattd",
         "educat5", "lstatus", "radio", "television", "computer", "cellphone", "landphone", "internet")

per_df <-  per_df[, .SD, .SDcols =  temp] 

## Merge Housing & Person Data ######------######------######------

## Calculate Household Size
temp_df <-  per_df[, .N, by =  hid]

## Add Household Size
temp <-  match(house_df$hid, temp_df$hid)
house_df$hhsize <-  temp_df$N[temp]

## Subset to Head of household
per_df <-  per_df[per_df$relathh9  == "Head", ]

## Merge data
house_df <-  merge(house_df, per_df, by = "hid", all =  TRUE)

## Save Data
##write.csv(house_df, "namibia_census_2011_harmonized.csv", row.names =  FALSE)


#library(foreign)
#house_df=write.dta(house_df, "house_df.dta")
#per_df=write.dta(per_df, "per_df.dta")
