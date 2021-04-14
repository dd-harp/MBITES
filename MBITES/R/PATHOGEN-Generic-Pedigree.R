###############################################################################
#         ____  ___  ________  ______  _____________   __
#        / __ \/   |/_  __/ / / / __ \/ ____/ ____/ | / /
#       / /_/ / /| | / / / /_/ / / / / / __/ __/ /  |/ /
#      / ____/ ___ |/ / / __  / /_/ / /_/ / /___/ /|  /
#     /_/   /_/  |_/_/ /_/ /_/\____/\____/_____/_/ |_/
#
#     PATHOGEN-Generic Pedigree
#     MBITES Team
#     March 2018
#
###############################################################################

#' PATHOGEN Generic Pedigree
#'
#' This class is a singleton object in the \code{MBITES} package namespace that
#' provides a unique integer ID for each pathogen and holds a map from
#' pathogen ID to a list describing each pathogen transmission event.
#'
#' It can be accessed by the global \code{MBITES:::Pedigree}.
#'
Generic_Pedigree <- R6::R6Class(
  classname = "Generic_Pedigree",
  portable = TRUE,
  cloneable = FALSE,
  lock_class = FALSE,
  lock_objects = FALSE,
  inherit = HashMap,

  public = list(
    #' @description
    #' Nothing during initialization.
    initialize = function() {
      # futile.logger::flog.trace("Generic_Pedigree being born at self: %s , private: %s",pryr::address(self),pryr::address(private))
    },

    #' @description
    #' Get a unique ID for a new pathogen instance.
    get_pathogen_id = function(){
      private$pathogen_id = private$pathogen_id + 1L
      return(private$pathogen_id)
    },

    #' @description
    #' Nothing during destruction
    finalize = function() {
      # futile.logger::flog.trace("Generic_Pedigree being killed at self: %s , private: %s",pryr::address(self),pryr::address(private))
    }

  ),

  private = list(pathogen_id = 0L) # end private members
)


# Create a global object in the MBITES namespace.
Pedigree <- Generic_Pedigree$new()
