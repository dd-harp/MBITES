###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Simulations of Peri-domestic breeding resource-scapes
#     Script for submitting series of jobs in parallel
#     MBITES Team
#     May 2018
#
###############################################################################

rm(list=ls());gc()
library(here)
library(MBITES)

seeds <- 1:26

# set up simulation initial conditions
for(i in seq_along(seeds)){

  cat(" --- setting up simulation initial conditions ",i," of 26 --- \n")


  ###############################################################################
  # load landscape sets
  ###############################################################################

  landscapes <- readRDS(file = paste0(here("data/landscape/"),"periDomesticLandscapes.rds"))
  humans <- readRDS(file = paste0(here("data/landscape/"),"periDomesticHumans.rds"))
  mosquitoes <- readRDS(file = paste0(here("data/landscape/"),"periDomesticMosquitoes.rds"))

  peridomestic_data <- vector(mode="list",length=length(landscapes))
  directory <- here("output/peridom")

  for(i in 1:length(peridomestic_data)){
    peridomestic_data[[i]]$id <- i
    peridomestic_data[[i]]$seed <- i
    peridomestic_data[[i]]$directory <- directory
    peridomestic_data[[i]]$landscape <- landscapes[[i]]$sites
    peridomestic_data[[i]]$humans <- as.data.frame(humans[[i]])
    peridomestic_data[[i]]$mosquitoes <- as.data.frame(mosquitoes[[i]])
  }

}

rm(landscapes,humans,mosquitoes);gc()

# run simulations
for(i in 1:seq_along(peridomestic_data)){

  ###############################################################################
  # set seed
  ###############################################################################

  seed <- peridomestic_data[[i]]$seed
  set.seed(seed)
  cat(" --- setting up simulation initial conditions ",i," of 26 --- \n")


  ###############################################################################
  # start simulation
  ###############################################################################

  PATHOGEN_Setup(pathogen_model = "null")

  # we want detailed output of blood hosts from the mosquito
  trackBloodHost()
  trackOviposition()

  # set parameters
  MBITES:::Parameters$set_parameters(Bs_surv = 0.95,Os_surv = 0.95,B_surv = 0.99,O_surv = 0.99,
                                     Bs_succeed = 0.99,Os_succeed = 0.99,B_succeed = 0.95,O_succeed = 0.99,
                                     S_u = 0,disperse = 0.2)

  # initialize a tile
  Tile_Initialize(peridomestic_data[[i]]$landscape)
  Human_NULL_Initialize(peridomestic_data[[i]]$humans)
  MBITES_Initialize(peridomestic_data[[i]]$mosquitoes)

  # run simulation
  set_output(directory = peridomestic_data[[i]]$directory,runID = peridomestic_data[[i]]$id)

  simulation(tMax = 5*365,pretty = TRUE)
  hardreset()

}
