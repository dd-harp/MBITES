# Simulation where people have a Plasmodium-equivalent standin pathogen.
#
rm(list = ls());gc()
library(here)

library(MBITES)
library(futile.logger)

MBITES::local_logging("trace", "mbites.log")
set.seed(91324724)

###############################################################################
# Make landscape initialization object
###############################################################################

# landscapes
landscape <- vector(mode = "list",length = 3)

# site characteristics
for(i in 1:3){
  landscape[[i]]$id = i
  landscape[[i]]$xy = c(1,1)
  landscape[[i]]$type = 1L
  landscape[[i]]$tileID = 1L
  landscape[[i]]$move = rep(0.5,2)
  landscape[[i]]$move_id = (1:3)[-i]
  landscape[[i]]$haz = 0.005
  # null resources
  landscape[[i]]$feed = NULL
  landscape[[i]]$aqua = NULL
}

# site 1 has both resources
landscape[[1]]$feed[[1]] = list(w=1,enterP=1,zoo_id=-1,zoo_w=0.1)
landscape[[1]]$aqua[[1]] = list(w=1,lambda=1)

# site 2 has only blood feeding resource
landscape[[2]]$feed[[1]] = list(w=1,enterP=1,zoo_id=-1,zoo_w=0.1)

# site 3 has only aquatic habitat resource
landscape[[3]]$aqua[[1]] = list(w=1,lambda=1)


###############################################################################
# Make human initialization object
###############################################################################

nHumans = 10L
if (nHumans > 9L) {
  initial_pr <- 0.4
  pr_vals <- rbinom(nHumans, 1L, initial_pr)
} else {  # randomizing for too few could mean no pathogens.
  pr_vals <- rep(c(1L, 0L), nHumans)[1:nHumans]
}

humans = data.frame(
  tileID = rep(1,nHumans),
  # make sure the humans are at the sites with blood feeding resources
  siteID = 1:2,
  feedingID = rep(1,nHumans),
  w = rep(0.9,nHumans),
  pr = pr_vals
)


###############################################################################
# Make mosquito initialization object
###############################################################################

nMosquitos = 50

mosquitos = data.frame(
  tileID = rep(1,nMosquitos),
  # make sure the mosquitos emerge from aquatic habitats
  siteID =rep(c(1,3),length.out=nMosquitos),
  female = rep(T,nMosquitos)
)


###############################################################################
# Run MBITES
###############################################################################

directory <- here("output/sei/")
if(!dir.exists(directory)){
  dir.create(directory,recursive = T)
}

cat("initializing MBITES functional forms for MBDETES approximation\n")

# time to event parameters
MBITES::MBITES_Setup_Timing(timing_model = 2,
                            rate_b = 1/(3/24),tmin_b = 0,
                            rate_bs = 1/(6/24),tmin_bs = 0,
                            rate_o = 1/(3/24),tmin_o = 0,
                            rate_os = 1/(6/24),tmin_os = 0,
                            # minimum waiting time needed for ppr to avoid numerical difficulties
                            ppr_model = 2,rate_ppr = 1/(18/24),tmin_ppr = 1e-2)

MBITES:::Globals$set_SETUP("timing")

# bloodmeal: turn off overfeeding
Mosquito_Female$set(which = "public",name = "Overfeeding",
                    value = MBITES:::mbites_OverfeedingNull, overwrite = TRUE
)
MBITES:::Globals$set_SETUP("bloodmeal")

# Oogenesis options
# egg batch size proportional to blood meal
Mosquito_Female$set(which = "public",name = "rBatchSize",
                    value = MBITES:::mbites_rBatchSizeBms, overwrite = TRUE
)

# main oogenesis model
Mosquito_Female$set(which = "public",name = "Oogenesis",
                    value = MBITES:::mbites_oogenesisMBDETES, overwrite = TRUE
)

# refeeding probability is function of blood meal size
Mosquito_Female$set(which = "public",name = "checkRefeed",
                    value = MBITES:::mbites_checkRefeedMBDETES, overwrite = TRUE
)

Mosquito_Female$set(which = "public",name = "pReFeed",
                    value = MBITES:::mbites_pReFeed_bm, overwrite = TRUE
)
MBITES:::Globals$set_SETUP("oogenesis")

# turn sugar off
Mosquito_Female$set(which = "public",name = "queueSugarBout",
                    value = MBITES:::mbites_queueSugarBout_null, overwrite = TRUE
)
MBITES:::Globals$set_SETUP("energetics")

# oviposition uses emerge
Mosquito_Female$set(which = "public",name = "layEggs",
                    value = MBITES:::mbites_layEggs_Emerge, overwrite = TRUE
)
MBITES:::Globals$set_SETUP("oviposition")

# survival: turn off wing tattering and senescence
Mosquito$set(which = "public",name = "WingTattering",
             value = MBITES:::mbites_WingTattering_null, overwrite = TRUE
)

Mosquito$set(which = "public",name = "Senescence",
             value = MBITES:::mbites_Senescence_null, overwrite = TRUE
)
MBITES:::Globals$set_SETUP("survival")

## End of Setup for MBDETES



PATHOGEN_Setup(pathogen_model = "SEI")

# we want detailed output of blood hosts from the mosquito
trackBloodHost()
trackOviposition()


# a good parameter set
MBITES:::Parameters$set_parameters(Bs_surv = 0.95,Os_surv = 0.95,B_surv = 0.99,O_surv = 0.99,
                                   Bs_succeed = 0.99,Os_succeed = 0.99,B_succeed = 0.95,O_succeed = 0.99,
                                   S_u = 0,disperse = 0.2)


# initialize a tile
Tile_Initialize(landscape)
Human_NULL_Initialize(humans)
MBITES_Initialize(mosquitos)

human_hash <- MBITES:::Globals$get_tile(1)$get_humans()
stopifnot(human_hash$size() == nHumans)

p_ancestor <- SEI_Pathogen$new()
initial_pathogen_id <- p_ancestor$get_id()
for (hidx in which(humans$pr == 1L)) {
  h <- human_hash$get(hidx)
  p <- SEI_Pathogen$new(initial_pathogen_id)
  p$initial_age(sample(1L:200L, 1))  # give them random previous infection dates
  h$add_pathogen(p)
}

# run simulation
set_output(directory = directory,runID = 1)

duration_days <- 3  # 365 * 5
simday <- function() simulation(tMax = duration_days, pretty = TRUE, cleanup = FALSE)
simday()

# This gets its list of bite times by asking each pathogen on a human
# when it was attached to that human.
bites_infectious_to_humans <- MBITES:::Globals$get_tile(1)$human_bites()

now <- MBITES:::Globals$get_tNow()
sample_infectious_bites_from_human <- data.frame(
  human = c(1, 2, 3),
  mosquito = MBITES:::Globals$get_tile(1)$get_mosquitoes()$ls()[1:3],
  parent_pathogen = c(30, 31, 32),
  infection_time = c(now - 2, now - 3, now - 1)
)

# This retroactively infects mosquitoes, according to a list of bites.
register_infected_mosquitoes <- function(df) {
  for (icnt in 1:nrow(df)) {
    pathogen = SEI_Pathogen$new(
      parentID = df[icnt, "parent_pathogen"], # parent ID of pathogen
      mosquito_id = df[icnt, "mosquito"]
    )
    pathogen$human2mosquito()
    pathogen$time_created <- df[icnt, "infection_time"]
    mosquito_to_infect <- MBITES:::Globals$get_tile(1)$get_mosquito(
      df[icnt, "mosquito"])
    mosquito_to_infect$add_pathogen(pathogen)
  }
}

register_infected_mosquitoes(sample_infectious_bites_from_human)
