###############################################################################
#         __  __
#        / / / /_  ______ ___  ____ _____
#       / /_/ / / / / __ `__ \/ __ `/ __ \
#      / __  / /_/ / / / / / / /_/ / / / /
#     /_/ /_/\__,_/_/ /_/ /_/\__,_/_/ /_/
#
#     NULL Human-Class
#     MBITES Team
#     February 2018
#
###############################################################################

#' HUMAN: Null Human
#'
#' The null human class (see \code{\link{Human_NULL}}) is a simple logging class to record biting events
#' when human dynamics are not of interest. It can only be used with the \code{\link{PathogenNull}} model.
#'
#'
#' @name HUMAN-Null
NULL
#> NULL

###############################################################################
# Class definition
###############################################################################

#' NULL Human Class
#'
#' A \code{Human_NULL} can be used as a drop-in replacement when human dynamics do not need
#' to be simulated explicitly.
#'
#'
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * id: integer id
#'  * w: numeric biting weight
#'  * feedingID: id of the \code{\link{Feeding_Resource}} where my biting weight will be added
#'  * siteID: id of the \code{\link{Site}} where my feeding site resides
#'  * tileID: id of the \code{\link{Tile}} where I reside
#'
#' @section **Methods**:
#'  * method: i'm a method!
#'
#' @section **Fields**:
#'  * id: integer id
#'  * w: numeric biting weight
#'  * UNBITTEN: logical flag
#'  * mosquito_id: integer vector of mosquitoes that have bitten me
#'  * mosquito_t: numeric vector of times i was bitten
#'  * bloodFeed: \code{FALSE} corresponds to probing-only events, \code{TRUE} corresponds to combined probe-bloodfeed events.
#'
#' @export
Human_NULL <- R6::R6Class(
  classname = "Human_NULL",
  portable = TRUE,
  cloneable = FALSE,
  lock_class = FALSE,
  lock_objects = FALSE,

  public = list(
    #' @description
    #' Construct a human
    #' @param id Integer identifier of human.
    #' @param w biting weight of human.
    #' @param feedingID where it is a feeding site
    #' @param siteID site for human
    #' @param tileID The tile that has that site.
    initialize = function(id, w, feedingID, siteID, tileID) {
      # futile.logger::flog.trace("Human_NULL %i being born at self: %s , private: %s",id,pryr::address(self),pryr::address(private))

      # basic parameters
      private$id = id
      private$w = w

      # location fields
      private$feedingID = feedingID
      private$siteID = siteID
      private$tileID = tileID

    },

    #' @description
    #' Add a pathogen to this human's pathogens.
    #' @param pathogen The pathogen to add to the list.
    add_pathogen = function(pathogen) {
      private$pathogens <- c(private$pathogens, pathogen)
      logtrace(paste("add_pathogen human", self$get_id()))
      invisible(self)
    },

    #' @description
    #' Get a pathogen of a certain type.
    #' @param kind The class of the pathogen as a string.
    #' Will return the first pathogen of this kind.
    on_pathogens = function(fun) {
      lapply(private$pathogens, FUN=fun)
    },

    #' @description
    #' Decide which pathogens from the host will transmit to the mosquito.
    #' @param mosquito The mosquito feeding on this host.
    #' @param mPathogens The mosquito pathogens that it already has.
    #' Returns a list of new pathogens to add to the mosquito.
    #' This list may contain NULL values when no pathogen is added.
    feedHost = function(mosquito, mPathogens) {
      pathogen_cnt <- length(private$pathogens)
      mosquito_pathogens <- lapply(private$pathogens, FUN=function(hPathogen) {
        hPathogen$feedHost(self, mosquito, mPathogens)
      })
      self$pushFeed()
      mpath_cnt <- sum(vapply(mosquito_pathogens,
                              function(x) !is.null(x), logical(1)))
      logtrace(paste("feedhost m", mosquito$get_id(), "h", self$get_id(),
                     "hpathogens", pathogen_cnt, "toadd", mpath_cnt))
      mosquito_pathogens
    },

    #' @description
    #' Get this Human's ID
    get_id = function() { private$id },

    #' @description
    #' Take a pathogen from this human's list of pathogens.
    #' @param pathogen The pathogen to remove.
    remove_pathogen = function(pathogen) {
      to_remove <- integer(0)
      for (ridx in 1:length(private$pathogens)) {
        if (identical(pathogen, private$pathogens[[ridx]])) {
          to_remove <- c(ridx, to_remove)
        }
      }
      private$pathogens <- private$pathogens[-to_remove]
      logtrace(paste("h", self$get_id(), "remove",
                     paste(to_remove, collapse=", "),
                     length(private$pathogens)))
      invisible(self)
    },

    #' This host was probed by a particular mosquito.
    mosquitoProbe = function(mosquito_id) {
      private$probedBy <- append(private$probedBy, mosquito_id)
      tNow <- MBITES:::Globals$get_tNow()
      private$probedWhen <- append(private$probedWhen, tNow)
    },

    infectious_bites = function() {
      bite_times <- numeric(0)
      for (p in private$pathogens) {
        when <- p$infectious_bite_time()
        if (!is.null(when)) {
          bite_times <- append(bite_times, when)
        }
      }
      report <- list(
        bite_times = bite_times,
        probed = data.frame(
          mosquito_id = private$probedBy,
          when = private$probedWhen
        )
      )
      private$probedBy <- integer(0)
      private$probedWhen <- numeric(0)
      report
    },

    #' @description
    #' Destructor
    finalize = function() {
      # futile.logger::flog.trace("Human_NULL %i being killed at self: %s , private: %s",private$id,pryr::address(self),pryr::address(private))
    }

  ),

  # private members
  private = list(
    # local fields
    id                  = integer(1),
    # my id
    w                   = numeric(1),
    # my biting weight

    # location fields
    feedingID           = integer(1),
    siteID              = integer(1),
    tileID              = integer(1),

    # biting dynamics
    UNBITTEN            = TRUE,
    # have i been bitten yet?
    mosquito_id         = integer(1),
    # vector of mosquitoes that have bitten me
    mosquito_t          = numeric(1),
    # vector of times i was bitten
    bloodFeed           = logical(1),
    # F for probing-only events, T for blood feeding events

    # A human can have multiple pathogens
    pathogens           = list(),

    probedBy            = integer(0),
    probedWhen          = numeric(0)
  )
)


###############################################################################
# export history and remove self.
###############################################################################

#' export history and remove self.
exit_Human_NULL <- function(){
  cat(jsonlite::toJSON(x = list(
          id = private$id,
          tile = private$tileID,
          site = private$siteID,
          bite_id = private$mosquito_id,
          bite_times = private$mosquito_t,
          bloodfeed_bool = private$bloodFeed
      ), pretty = MBITES:::Globals$pretty),",\n",sep="",file=MBITES:::Globals$get_human_out())
  # remove the human from the Hash map.
  MBITES:::Globals$get_tile(private$tileID)$get_humans()$rm(private$id)
}

Human_NULL$set(which = "public",name = "exit",
    value = exit_Human_NULL, overwrite = TRUE
)


###############################################################################
# Interface with Tile-Simulation
###############################################################################

#' Null Human: Daily Event Queue Simulation
#'
#' For the null human model, the daily time step executes a daily time step
#' for each pathogen it has.
#'
oneDay_EventQ_Human_NULL <- function(){
  for (pathogen in private$pathogens) {
    pathogen$oneDay_human(self)
  }
}

#' Null Human: Daily Activity Space Simulation
#'
#' Always add 100% of my risk to my home site.
#'
oneDay_ActivitySpace_Human_NULL <- function(){
  # add all my risk to my home site
  MBITES:::Globals$get_tile(private$tileID)$get_sites()$get(
    private$siteID)$get_feed(private$feedingID)$RiskQ$add2Q(
      private$id,private$w,1)
}

# set methods
Human_NULL$set(which = "public",name = "oneDay_EventQ",
    value = oneDay_EventQ_Human_NULL, overwrite = TRUE
)

Human_NULL$set(which = "public",name = "oneDay_ActivitySpace",
    value = oneDay_ActivitySpace_Human_NULL, overwrite = TRUE
)


###############################################################################
# Interface with Pathogen-NULL
###############################################################################

#' Null Human: Push Host Probing Event to History
#'
#' If using the null human and pathogen model this function logs host probing
#' events on this \code{\link{Human_NULL}}.
#'
#'  * This method is bound to \code{Human_NULL$pushProbe}
#'
#' @param m_id id of the \code{\link{Mosquito_Female}}
#' @param t time of the probing event
#'
pushProbe_Human_NULL <- function(m_id,t){
  if(private$UNBITTEN){
    private$mosquito_id = m_id
    private$mosquito_t = t
    private$bloodFeed = FALSE
    private$UNBITTEN = FALSE
  } else {
    private$mosquito_id = append(private$mosquito_id,m_id)
    private$mosquito_t = append(private$mosquito_t,t)
    private$bloodFeed = append(private$bloodFeed,FALSE)
  }
}

#' Null Human: Push Host Blood feeding Event to History
#'
#' If using the null human and pathogen model this function logs host blood feeding events on this \code{\link{Human_NULL}}.
#'  * This method is bound to \code{Human_NULL$pushFeed}
pushFeed_Human_NULL <- function(){
  private$bloodFeed[length(private$bloodFeed)] = TRUE
}

# set methods
Human_NULL$set(which = "public",name = "pushProbe",
    value = pushProbe_Human_NULL, overwrite = TRUE
)

Human_NULL$set(which = "public",name = "pushFeed",
    value = pushFeed_Human_NULL, overwrite = TRUE
)
