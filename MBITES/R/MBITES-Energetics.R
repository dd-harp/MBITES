###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES-Energetics (blood and sugar energetics)
#     MBITES Team
#     March 2018
#
###############################################################################

#' MBITES: Energetics
#'
#' @section Blood and Sugar Energetics:
#'
#' Energetics handles mosquito energetics derived from blood and sugar meals. The main methods implemented are:
#'  * \code{\link{mbites_BloodEnergetics}} which is called upon taking a successful blood meal
#'  * \code{\link{mbites_sugarMeal}} which is called upon taking a successful sugar meal
#'  * \code{\link{mbites_energetics}} which is called during \code{\link{mbites_updateState}} and calculates energy expenditure from flight and potentially queues a sugar feeding bout.
#'
#' @name MBITES-Energetics
NULL
#> NULL


###############################################################################
# Flight Energetics (called from updateState in MBITES-Bout.R)
###############################################################################

#' MBITES: Energetics
#'
#' Calculate energy burned from flight with \code{\link{mbites_flightBurnEnergy}} and potentially queue
#' a sugar bout with \code{\link{mbites_queueSugarBout}}.
#'  * This method is bound to \code{Mosquito$energetics}
#'
mbites_energetics <- function(){
  self$flightBurnEnergy()
  self$queueSugarBout()
}

#' MBITES: Energy burn from flight
#'
#' Calculate energy in the mosquito's fuel tank after a flight.
#'  * This method is bound to \code{Mosquito$flightBurnEnergy()}
#'
mbites_flightBurnEnergy <- function(){
  private$energy = max(0,private$energy - MBITES:::Parameters$get_S_u())
}

#' MBITES: Queue a sugar bout
#'
#' If the mosquito queues a sugar bout (with probability \code{\link{mbites_pSugarBout}}), set \code{starved} flag true.
#'  * This method is bound to \code{Mosquito$queueSugarBout}
#'
mbites_queueSugarBout <- function(){
  if(runif(1) < self$pSugarBout()){
    private$starved = TRUE
  }
}

#' MBITES: Null sugar bout
#'
#' Null version of \code{\link{mbites_queueSugarBout}} if sugar feeding behavior is to be turned off.
#'  * This method is bound to \code{Mosquito$queueSugarBout}
#'
mbites_queueSugarBout_null <- function(){
  # dont do anything
}

#' MBITES: Probability of a sugar bout
#'
#' Calculate the probability to queue a sugar bout as a function of the mosquito's current energy levels.
#' The probability is given by \eqn{\frac{2+S_{sb}}{1+S_{sb}} - \frac{e^{S_{sa} \times energy}}{S_{sb}+e^{S_{sa}\times energy}}}
#'  * This method is bound to \code{Mosquito$pSugarBout}
#'
mbites_pSugarBout <- function(){
  omega = MBITES:::Parameters$get_omega()
  S_sb = MBITES:::Parameters$get_S_sb()
  S_sa = MBITES:::Parameters$get_S_sa()
  S_w = MBITES:::Parameters$get_S_w()
  S_p = MBITES:::Parameters$get_S_p()
  exp(S_sa)/(S_sb*(1+(omega*S_w)^S_p)+exp(S_sa)) - exp(S_sa*private$energy)/(S_sb*(1+(omega*S_w)^S_p) + exp(S_sa*private$energy))
}

# mbites_pSugarBout <- function(){
#   S_sb = MBITES:::Parameters$get_S_sb()
#   S_sa = MBITES:::Parameters$get_S_sa()
#   (2+S_sb)/(1+S_sb)-exp(S_sa*private$energy)/(S_sb+exp(S_sa*private$energy))
# }

Mosquito$set(which = "public",name = "energetics",
    value = mbites_energetics, overwrite = TRUE
)

Mosquito$set(which = "public",name = "flightBurnEnergy",
    value = mbites_flightBurnEnergy, overwrite = TRUE
)

Mosquito$set(which = "public",name = "pSugarBout",
    value = mbites_pSugarBout, overwrite = TRUE
)


###############################################################################
# Blood Energetics
###############################################################################

#' MBITES: Blood Energetics
#'
#' Add energy derived from blood to the mosquito's fuel tank. This method is called from \code{\link{mbites_BloodMeal}}.
#'  * This method is bound to \code{MosquitoFemale$BloodEnergetics}
#'
mbites_BloodEnergetics <- function(){ # called from MBITES-Bloodmeal.R
  # overfeeding could have killed us
  if(private$alive){
    topUp = self$energyFromBlood() # energy derived from blood meal is function of meal size
    private$energy = min(1, private$energy + topUp)    
    if(!private$mature){
      private$energyPreG = private$energyPreG - topUp
      if(private$energyPreG <= 0 & private$mated){
        private$mature = TRUE
      }
    }
  }
}

#' MBITES: Energy derived from blood
#'
#' Energy derived from the blood meal is a linear function of blood meal size with half-maximum parameter \code{energyFromBlood_b}.
#'  * This method is bound to \code{Mosquito_Female$energyFromBlood}.
#'
mbites_energyFromBlood <- function(){
  private$bmSize * MBITES:::Parameters$get_energyFromBlood_b()
}

# set methods
Mosquito$set(which = "public",name = "BloodEnergetics",
    value = mbites_BloodEnergetics, overwrite = TRUE
)

Mosquito$set(which = "public",name = "energyFromBlood",
    value = mbites_energyFromBlood, overwrite = TRUE
)


###############################################################################
# Sugar Energetics
###############################################################################

#' MBITES: Choose Sugar Source
#'
#' During a sugar bout the mosquito samples from available sugar feeding resources
#' at the \code{\link{Site}} it is at.
#'  * this method is bound to \code{Mosquito$chooseSugarSource}
#'
mbites_chooseSugarSource <- function(){
  # when atsb, etc exist, do the checks here
  private$sugar_resource = private$site$sample_sugar() # sample resources
  private$sugarID = 1L # normal sugar source

}

#' MBITES: Sugar Energetics
#'
#' Add energy derived from sugar to the mosquito's fuel tank, and if the mosquito is immature, subtract energy
#' derived from this sugar meal from the pre-gonotrophic energy requirement. If the mosquito is mated
#' and has passed the energy requirement, set \code{mature = TRUE}.
#'  * This method is bound to \code{Mosquito_Female$sugarMeal}
#'
mbites_sugarMeal <- function(){ # called from MBITES-Bouts.R, boutS
  private$energy = 1 # always top up to full
  private$starved = FALSE # satiated
  if(!private$mature){
    private$energyPreG = private$energyPreG - MBITES:::Parameters$get_preGsugar()
    if(private$energyPreG <= 0 & private$mated){
      private$mature = TRUE
    }
  }
}

# set methods
Mosquito$set(which = "public",name = "chooseSugarSource",
    value = mbites_chooseSugarSource, overwrite = TRUE
)

Mosquito_Female$set(which = "public",name = "sugarMeal",
    value = mbites_sugarMeal, overwrite = TRUE
)
