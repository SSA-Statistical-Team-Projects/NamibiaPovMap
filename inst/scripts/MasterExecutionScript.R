#-----------------------------------------------------------------------------#
#                    A SCRIPT TO RUN THE ENTIRE REPOSITIORY                   #
#-----------------------------------------------------------------------------#
remove(list = objects())
devtools::load_all()
pacman::p_load(here)

source(here("inst/scripts/01_prepare_datasets.R"))
source(here("inst/scripts/02_compare_vars.R"))
source(here("inst/scripts/03_select_vars.R"))
source(here("inst/scripts/04_estimate_ebpmodel.R"))
