###############################################################################
#         __                    __
#        / /   ____ _____  ____/ /_____________ _____  ___
#       / /   / __ `/ __ \/ __  / ___/ ___/ __ `/ __ \/ _ \
#      / /___/ /_/ / / / / /_/ (__  ) /__/ /_/ / /_/ /  __/
#     /_____/\__,_/_/ /_/\__,_/____/\___/\__,_/ .___/\___/
#                                            /_/
#     Landscape-Resource-Aquatic Habitat-Aquatic Population (Emerge)
#     MBITES Team
#     February 2018
#
###############################################################################


#' Aquatic Ecology: Emerge Model Class
#'
#' Class that implements the 'Emerge' model of aquatic ecology, inheriting the
#' interface of \code{\link{Aqua_Resource}}.
#'
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**: * lambda: either numeric or vector of at least
#'   length 365 * site: a reference to a \code{\link{Site}} object
#'
#' @section **Methods**: * one_day: function with void return that runs one day
#'   of the specific aquatic population simulation implementation * get_imago:
#'   function that returns an ImagoQ (a \code{list} object) for imagos (adult
#'   mosquitoes) ready to emerge on that day
#'
#' @section **Fields**: * lambda: numeric vector * constant: logical
#'
#' @export
Aqua_Resource_Emerge <-
  R6::R6Class(
    classname = "Aqua_Resource_Emerge",
    portable = TRUE,
    cloneable = FALSE,
    lock_class = FALSE,
    lock_objects = FALSE,
    inherit = MBITES:::Aqua_Resource,

    public = list(
      initialize = function(w, site, lambda) {
        # futile.logger::flog.trace("Aqua_Resource_Emerge being born at: self %s
        # , private %s",pryr::address(self),pryr::address(private))

        if (length(lambda) < 365 & length(lambda) > 1) {
          stop(
            paste(
              "length of provided lambda vector: ",
              length(lambda),
              ", but require vector either >= 365 days or",
              "1 day (constant emergence)"
            )
          )
        }

        super$initialize(w, site) # construct base-class parts

        if (length(lambda) == 1) {
          private$constant = TRUE
          private$lambda = lambda
        } else {
          private$constant = FALSE
          private$lambda = lambda
        }

      },

      finalize = function() {
        # futile.logger::flog.trace("Aqua_Resource_Emerge being killed at: self
        # %s , private %s",pryr::address(self),pryr::address(private))
      }

    ),

    private = list(lambda            = numeric(1),
                   # boolean indicating constant emergence or lambda vector
                   constant          = logical(1)
                  )
)


###############################################################################
# one day of aquatic population dynamics
###############################################################################

#' Aquatic Ecology: Emerge - Daily Emergence
#'
#' Add a cohort of emerging imagos based on current value of \eqn{\lambda} by
#' calling \code{add2Q} function of the imago closure object, see
#' \code{\link{make_ImagoQ}}.
#'
#' * This method is bound to \code{Aqua_Resource_Emerge$one_day}
#'
one_day_Emerge <- function(){

  tNow = MBITES:::Globals$get_tNow()

  # sample number of emerging mosquitoes
  if(private$constant){
    lambda_t = rpois(n = 1, lambda = private$lambda)
  } else {
    lambda_now = private$lambda[floor(tNow)%%365+1]
    lambda_t = rpois(n = 1, lambda = lambda_now)
  }
  eggs <- self$EggQ$popQ(tNow)
  logtrace(paste("eggs_emerge", sum(eggs)))

  # if emerging mosquitoes, send them to the population
  if(lambda_t>0){
    self$ImagoQ$add2Q(lambda_t,tNow,T)
  }

}

Aqua_Resource_Emerge$set(which = "public",name = "one_day",
          value = one_day_Emerge, overwrite = TRUE
)


###############################################################################
# get emerging imagos
###############################################################################

#' Aquatic Ecology: Emerge - Emerging Imagos
#'
#' Take emering imagos (adult mosquitoes) and assign them to the mosquito
#' population in this \code{\link{Tile}}.
#'
#'  * This method is bound to \code{Aqua_Resource_Emerge$push_imago}
#'
push_imago_Emerge <- function(){

  tNow = MBITES:::Globals$get_tNow() # time now
  tile_id = private$SiteP$get_tileID() # integer id of my tile
  tile = MBITES:::Globals$get_tile(tile_id) # tile reference

  imagos = self$ImagoQ$popQ(time_e=tNow) # emerging imagos

  # if there is are cohort(s) of imagos ready to go
  if(!is.na(imagos$female[1]) & (imagos$imagos>0)){

    # iterate over those cohorts (i)
    for(i in 1:length(imagos$imagos)){
      # cohort (i) is female
      if(imagos$female[i]){
        for(j in 1:imagos$imagos[i]){
          mosy = Mosquito_Female$new(tNow,private$SiteP,tile_id)
          tile$get_mosquitoes()$assign(key=mosy$get_id(),value=mosy)
        }
      # cohort (i) is male
      } else {
        for(j in 1:imagos$imagos[i]){
          mosy = Mosquito_Male$new(tNow,private$SiteP,tile_id)
          tile$get_mosquitoes()$assign(key=mosy$get_id(),value=mosy)
        }
      }
    } # end loop over cohorts i

  } # end conditional
} # end get_imago


Aqua_Resource_Emerge$set(which = "public",name = "push_imago",
          value = push_imago_Emerge, overwrite = TRUE
)
