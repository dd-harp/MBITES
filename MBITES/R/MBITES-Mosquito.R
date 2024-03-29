###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES-Mosquito
#     MBITES Team
#     February 2018
#
###############################################################################


###############################################################################
# Abstract Base Mosquito
###############################################################################

#' MBITES: Mosquito Class
#'
#' All mosquitoes inherit from the \code{Mosquito} abstract base class object.
#'
#' @export
Mosquito <- R6::R6Class(
  classname = "Mosquito",
  portable = TRUE,
  cloneable = FALSE,
  lock_class = FALSE,
  lock_objects = FALSE,

  public = list(
    initialize = function(bDay, state, site, tileID) {
      # set up parameters
      private$id = MBITES:::Globals$get_mosquito_id()

      private$alive = TRUE

      private$site = site
      private$tileID = tileID

      private$bDay = bDay
      private$tNow = bDay
      private$tNext = bDay

      private$search = TRUE
      private$state = state
      private$starved = FALSE

      # set up history
      private$timeHist[1] = bDay
      private$siteHist[1] = site$get_id()
      private$stateHist[1] = state
      private$searchHist[1] = TRUE

      # logging # futile.logger::flog.trace("Mosquito %s being born at: self %s
      # , private %s",private$id,pryr::address(self),pryr::address(private))
    },

    #' @description
    #' Get the integer ID of the mosquito.
    get_id = function(){
      return(private$id)
    },

    #' @description
    #' Get a pathogen of a certain type.
    #' @param kind The class of the pathogen as a string.
    #' Will return the first pathogen of this kind.
    on_pathogens = function(fun) {
      lapply(private$pathogens, FUN=fun)
    },

    finalize = function() {
      # # futile.logger::flog.trace("Mosquito %s being killed at: self %s ,
      # private %s",private$id,pryr::address(self),pryr::address(private))
    }

  ),

  private = list(
    # basic parameters
    id             = integer(1),
    # character id
    alive          = logical(1),
    # am i alive?

    # location
    tileID         = integer(1),
    # id of the tile i am in
    site           = NULL,
    # reference to my current site
    rspot          = "v",
    # my current resting spot

    # resources
    sugar_resource      = NULL,
    # reference to my current sugar resource
    mating_resource       = NULL,
    # reference to my current mating swarm resource
    pathogens = list(),

    # timing
    bDay          = numeric(1),
    # the day i emerged
    tNext         = numeric(1),
    # time of my next launch
    tNow          = numeric(1),
    # time of my current launch

    # behavioral state parameters
    search         = logical(1),
    # next launch is for search or attempt bout?
    searchNow      = logical(1),
    # is my current bout a search bout?
    state          = character(1),
    # my current behavioral state
    starved        = FALSE,
    # am i starved for sugar?
    boutFail       = 0L,
    # counter

    # energetics
    energy         = 1,
    # my current energy
    mature         = FALSE,
    # am i mature?

    # survival (mosquitoes start out at full life)
    damage_physical = 0,
    # physical damage
    damage_chemical = 0,
    # chemical damage

    # resource ids
    sugarID        = integer(1),
    # id of my sugar  source
    mateID         = integer(1),
    # id of my mate

    # history
    nEvent         = 1L,
    # number of bouts + emergence (birthday) (increment at the beginning of the
    # trackHistory function)
    timeHist       = numeric(30),
    # history of event times (t)
    siteHist       = integer(30),
    # history of sites visited (s)
    searchHist     = logical(30),
    # history of searching?
    stateHist      = character(30),
    # history of behavioral states (b)
    cod            = character(1) # for mosquito autopsies
  )
)


###############################################################################
# Female Mosquito
###############################################################################

#' MBITES: Female Mosquito Class
#'
#' Female mosquitoes inherit from the \code{\link{Mosquito}} abstract base class
#' object.
#'
#' @export
Mosquito_Female <- R6::R6Class(
  classname = "Mosquito_Female",
  portable = TRUE,
  cloneable = FALSE,
  lock_class = FALSE,
  lock_objects = FALSE,
  inherit = MBITES:::Mosquito,

  public = list(
    initialize = function(bDay, site, tileID) {
      super$initialize(bDay,
                       MBITES:::Parameters$get_defaultState_F(),
                       site,
                       tileID) # construct the base-class parts

      private$energyPreG = MBITES:::Parameters$get_energyPreG()

      # only search if i need to (or if dispersion makes me go anyway)
      private$search = FALSE
      switch(
        private$state,
        B = {
          self$BloodFeedingSearchCheck()
        },
        O = {
          self$OvipositSearchCheck()
        },
        M = {
          self$MatingSearchCheck()
        },
        S = {
          self$SugarSearchCheck()
        },
        {
          stop("behavioral state should be one of B, O, M, S: ",
               private$state, "\n")
        }
      )
    },

    #' @description
    #' Probe the host and possibly infect that host with pathogens.
    probeHost = function() {
      host <- MBITES:::Globals$get_tile(private$tileID)$get_human(private$hostID)
      if (is.null(host)) {
        stop(paste("the host is null", private$hostID))
      }
      logtrace(paste("mosquito_probe_host", self$get_id(),
                     length(private$pathogens), host$get_id()))
      for (pathogen in private$pathogens) {
        pathogen$probeHost(self, host)
      }
      host$mosquitoProbe(self$get_id())
    },

    #' @description
    #' Feed on the host and possibly acquire a pathogen from that host.
    feedHost = function() {
      host <- MBITES:::Globals$get_tile(private$tileID)$get_human(private$hostID)
      to_add <- host$feedHost(self, private$pathogens)
      logtrace(paste("mosquito_feed_host", self$get_id(),
               class(to_add), paste(to_add, collapse=",")))
      for (mPathogen in to_add) {
        if (!is.null(mPathogen)) {
          self$add_pathogen(mPathogen)
        }
      }
      logtrace(paste("mosquito_feed_host", self$get_id(),
                     length(private$pathogens)))
    },

    add_pathogen = function(mPathogen) {
      private$pathogens[[length(private$pathogens) + 1]] <- mPathogen
    },

    # pathogenDynamics
    pathogenDynamics = function() {
      # futile.logger::flog.warn("default 'pathogenDynamics' being called for
      # mosquito: ",private$id)
      for (pathogen in private$pathogens) {
        pathogen$oneBout(private$tNext)
      }
    }

  ),

  private = list(
    # resources
    aqua_resource        = NULL,
    # reference to my current aquatic habitat resource
    feeding_resource     = NULL,
    # reference to my current blood feeding resource

    # behavioral state parameters
    mated          = FALSE,
    # have i mated yet?
    gravid         = FALSE,
    # am i gravid to oviposit?

    # energetics
    energyPreG    = numeric(1),
    # pre-gonotrophic energy requirement

    # bloodfeeding and oogenesis
    bloodfed       = FALSE,
    # have i fed on blood this bout?
    batch          = 0,
    # size of my egg batch
    eggT           = 2e16,
    # time my egg batch is ready
    eggP           = numeric(1),
    bmSize         = 0,
    # size of my blood meal

    # host ids
    hostID         = integer(1) # id of my blood host

  )
)


###############################################################################
# Male Mosquito
###############################################################################

#' MBITES: Male Mosquito Class
#'
#' Male mosquitoes inherit from the \code{\link{Mosquito}} abstract base class
#' object.
#'
#' @export
Mosquito_Male <- R6::R6Class(
  classname = "Mosquito_Male",
  portable = TRUE,
  cloneable = FALSE,
  lock_class = FALSE,
  lock_objects = FALSE,
  inherit = MBITES:::Mosquito,

  public = list(
    initialize = function(bDay, site, tileID) {
      super$initialize(bDay,
                       MBITES:::Parameters$get_defaultState_M(),
                       site,
                       tileID) # construct the base-class parts
    }

  ),

  private = list()
)
