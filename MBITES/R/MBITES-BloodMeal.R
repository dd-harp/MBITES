###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES-Blood Meal
#     MBITES Team
#     March 2018
#
###############################################################################

#' MBITES: Blood Meal
#'
#' @section The Blood Meal Event:
#'
#' During a human host encounter (\code{\link{mbites_humanEncounter}}) or a non-human host encounter
#' (\code{\link{mbites_zooEncounter}}), if the mosquito survives to blood feed, it takes a blood meal.
#'
#' The blood meal sets the \code{bloodfed} flag to true, which resets \code{boutFail} flag to 0 at the end of
#' \code{\link{mbites_attempt_B}}.
#'
#' The blood meal consists of:
#'  1. Sampling a blood meal size (\code{\link{mbites_rBloodMealSize}})
#'  2. Checking for mortality from overfeeding (\code{\link{mbites_Overfeeding}})
#'  3. Calculating energetics/maturation from blood (\code{\link{mbites_bloodEnergetics}})
#'  4. Simulating oogenesis/egg production (\code{\link{oogenesis}})
#'
#' @include MBITES-Mosquito.R
#' @name MBITES-BloodMeal
NULL
#> NULL

###############################################################################
# Bloodmeal (note: refeeding is now in MBITES-Oogenesis)
###############################################################################

#' MBITES: The Blood Meal Event
#'
#' Handle bloodmeal size (see \code{\link{mbites_rBloodMealSize}}), overfeeding (see \code{\link{mbites_pOverFeed}}),
#' and egg batch size and maturation time. Note that refeeding behavior is calculated during resting bout, depending
#' on the selection of oogenesis model.
#'  * This method is bound to \code{Mosquito_Female$BloodMeal}.
#'
mbites_BloodMeal <- function(){

  private$bmSize = self$rBloodMealSize()
  private$bloodfed = TRUE

  # overfeeding mortality and blood-derived energetics
  self$Overfeeding() # MBITES-BloodMeal.R
  self$BloodEnergetics() # MBITES-Energetics.R

  # # post-prandial rest (digestion)
  # self$PPRFlight() # moved to restingSpot: MBITES-BloodMeal.R
  # self$Oogenesis() # moved to updateState: MBITES-Oogenesis.R
}

#' MBITES: Draw Bloodmeal Size
#'
#' Draw a bloodmeal size from Beta(bm.a,bm.b)
#'  * This method is bound to \code{Mosquito_Female$rBloodMealSize}.
#'
mbites_rBloodMealSize <- function(){
  rbeta(n=1,MBITES:::Parameters$get_bm_a(),MBITES:::Parameters$get_bm_b())
}

# set methods
Mosquito_Female$set(which = "public",name = "BloodMeal",
    value = mbites_BloodMeal, overwrite = TRUE
)

Mosquito_Female$set(which = "public",name = "rBloodMealSize",
    value = mbites_rBloodMealSize, overwrite = TRUE
)


###############################################################################
# Post-prandial Resting Flight Mortality
###############################################################################

#' MBITES: Probability to Survive the Post Prandial Flight
#'
#' Survival probability as a function of being laden with blood during the post-prandial flight \eqn{ \frac{e^{S.a\times energy}}{S.b+e^{S.a\times energy}} }
#'  * This method is bound to \code{Mosquito_Female$pPPRFlight}.
#'
mbites_pPPRFlight <- function(){
  PPR_a = MBITES:::Parameters$get_PPR_a()
  PPR_b = MBITES:::Parameters$get_PPR_b()
  exp(PPR_a*private$bmSize)/(PPR_b + exp(PPR_a*private$bmSize))
}

#' MBITES: Post Prandial Flight and Rest
#'
#' If the mosquito did not die from overfeeding, update time according to the time needed by PPR (see \code{\link{set_ttEvent_ppr_MBITES_Parameters}}).
#' Mosquito dies as a result of the post-prandial flight with probability given by \code{\link{mbites_pPPRFlight}}.
#'  * This method is bound to \code{Mosquito_Female$PPRFlight}.
#'
mbites_PPRFlight <- function(){
  if(private$alive){
    private$tNow = MBITES:::Parameters$ttEvent$ppr(private$tNow)
    self$trackRest()
    if(runif(1) < 1-self$pPPRFlight()){
      private$alive = FALSE
      private$cod = "PPRFlight"
    }
  }
}

Mosquito_Female$set(which = "public",name = "pPPRFlight",
    value = mbites_pPPRFlight, overwrite = TRUE
)

Mosquito_Female$set(which = "public",name = "PPRFlight",
    value = mbites_PPRFlight, overwrite = TRUE
)


###############################################################################
# Overfeeding
###############################################################################

#' MBITES: Overfeeding Mortality
#'
#' Calculate overfeeding mortality (see \code{\link{mbites_pOverFeed}}).
#'  * This method is bound to \code{MosquitoFemale$Overfeeding}.
#'
mbites_Overfeeding <- function(){
  # Overfeeding mortality
  if(runif(1) < self$pOverFeed()){
    private$alive = FALSE
    private$cod = "Overfeeding"
  }
}

#' MBITES: Null Overfeeding
#'
#' If overfeeding mortality turned off, do nothing
#'  * This method is bound to \code{MosquitoFemale$Overfeeding}.
#'
mbites_OverfeedingNull <- function(){
  # null overfeeding
}

#' MBITES: Probaility of Overfeeding due to Bloodmeal Size
#'
#' Probability of death due to overfeeding from bloodmeal size given by \eqn{\frac{e^{of.a\times bmSize}}{of.b+e^{of.a\times bmSize}}}
#'  * This method is bound to \code{MosquitoFemale$pOverFeed}.
#'
mbites_pOverFeed <- function(){
  of_a = MBITES:::Parameters$get_of_a()
  of_b = MBITES:::Parameters$get_of_b()
  exp(of_a*private$bmSize)/(of_b + exp(of_a*private$bmSize))
}
