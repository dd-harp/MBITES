###############################################################################
#         ____  ___  ________  ______  _____________   __
#        / __ \/   |/_  __/ / / / __ \/ ____/ ____/ | / /
#       / /_/ / /| | / / / /_/ / / / / / __/ __/ /  |/ /
#      / ____/ ___ |/ / / __  / /_/ / /_/ / /___/ /|  /
#     /_/   /_/  |_/_/ /_/ /_/\____/\____/_____/_/ |_/
#
#     PATHOGEN-Generic Class
#     MBITES Team
#     March 2018
#
###############################################################################

#' PATHOGEN: Generic Models
#'
#' All pathogen objects need to be able to communicate with a pathogen pedigree,
#' which links all clonal variants together through a \code{id} and
#' \code{parentID} field. Pathogen models link human and mosquito contacts which
#' occur during feeding, see \code{\link{MBITES-HostEncounter}}
#' for more details on the "host encounter" process.
#'
#' All pathogen modules need to implement the following methods in humans:
#'  * add_pathogen(pathogen): during successful mosquito -> human transmission
#'
#' All pathogen modules need to implement the following methods in mosquitoes:
#'  * pathogenDynamics
#'  * probeHost
#'  * feedHost
#'
#' All pathogen modules need to implement the following methods in pathogens:
#'  * oneDay_human
#'
#' @name PathogenGeneric
NULL
#> NULL


#' Pathogen: Generic Class
#' @export
Generic_Pathogen <- R6::R6Class(
  classname = "Generic_Pathogen",
  portable = TRUE,
  cloneable = TRUE,
  lock_class = FALSE,
  lock_objects = FALSE,

  public = list(
    #' @description
    #' Constructor for Generic_Pathogen
    #' @param parentID The parent pathogen in lineage.
    initialize = function(parentID = NULL) {
      private$id = MBITES:::Pedigree$get_pathogen_id()
      if (is.null(parentID)) {
        private$parentID = 0L
      }

      # futile.logger::flog.trace("Generic_Pathogen being born at self: %s , private: %s",pryr::address(self),pryr::address(private))
    },

    #' @description
    #' Destructor
    finalize = function() {
      # futile.logger::flog.trace("Generic_Pathogen being killed at self: %s , private: %s",pryr::address(self),pryr::address(private))
    },

    #' @description
    #' Simulate one day in the life.
    oneDay_human = function() {
      stop("oneDay_human should never be called from abstract base class 'Generic_Pathogen'!")
    },

    #' @description
    #' Simulate interaction with mosquito.
    oneBout_mosquito = function() {
      stop(
        "oneBout_mosquito should never be called from abstract base class 'Generic_Pathogen'!"
      )
    },

    #' @description
    #' Get the pathogen ID.
    get_id = function(){
      return(private$id)
    },

    #' @description
    #' Get ID of the parent pathogen in lineage.
    get_parentID = function(){
      return(private$parentID)
    }
  ),

  private = list(
    id = integer(1), # pathogen id
    parentID = integer(1) # parent id
  )
)
