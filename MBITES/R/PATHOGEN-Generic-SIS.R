###############################################################################
#         ____  ___  ________  ______  _____________   __
#        / __ \/   |/_  __/ / / / __ \/ ____/ ____/ | / /
#       / /_/ / /| | / / / /_/ / / / / / __/ __/ /  |/ /
#      / ____/ ___ |/ / / __  / /_/ / /_/ / /___/ /|  /
#     /_/   /_/  |_/_/ /_/ /_/\____/\____/_____/_/ |_/
#
#     PATHOGEN-Generic SIS Model
#     MBITES Team
#     March 2018
#
###############################################################################


#' Generic SEI: Pathogen Class
#'
#' This is a generic SEI pathogen class without superinfection. Incubation times
#' can be set to 0 to recover SI behavior. Pathogen classes do not need explicit
#' "recovery" dynamics as clearing them from a queue or vector in the host is
#' akin to killing this genetic clone and resetting the host to a "susceptible"
#' state. Technically the pathogen only exists in incubating or infectious
#' states, but we keep the "SEI" designation for comprehension.
#'
#' @export
SEI_Pathogen <- R6::R6Class(
  classname = "SEI_Pathogen",
  portable = TRUE,
  cloneable = TRUE,
  lock_class = FALSE,
  lock_objects = FALSE,
  inherit = Generic_Pathogen,

  public = list(
    #' @description
    #' Create a new SEI_Pathogen object.
    #' @param parentID The ID for the parent pathogen in lineage.
    initialize = function(parentID = NULL, host_id = NULL, mosquito_id = NULL) {
      super$initialize(parentID) # initialize base parts
      private$human_id <- host_id
      private$mosquito_id <- mosquito_id

      private$b <- PathogenParameters$get_b()
      private$c <- PathogenParameters$get_c()

      # futile.logger::flog.trace("SEI_Pathogen being born at self: %s , private: %s",pryr::address(self),pryr::address(private))
    },

    #' @description
    #' Destroy an SEI_Pathogen object.
    finalize = function() {
      super$finalize() # destruct base parts

      # futile.logger::flog.trace("SEI_Pathogen being killed at self: %s , private: %s",pryr::address(self),pryr::address(private))
    },

    initial_age = function(days_previous) {
      private$incubating <- as.integer(days_previous)
    },

    #' @description
    #' During a bout, a pathogen is responsible for getting closer to infectious
    #' Pathogen method: overwrites oneDay_mosquito in the generic class.
    oneBout = function(tNext){
      if (is.null(private$time_created)) {
        private$time_created <- MBITES:::Globals$get_tNow()
      }
      # if not infectious advance the incubation period to the time of next launch
      if(!private$infectious){
        private$incubating = private$incubating + tNext
        if(!private$infectious && private$incubating >= private$incubation_m){
          private$infectious = TRUE
          private$time_became_infectious = MBITES:::Globals$get_tNow()
        }
      }
    },

    #' @description
    #' Generic SEI: Push a Pathogen to the Pedigree
    #'
    #' During a transmission event, record the transfer of the pathogen
    #' from human to mosquito or vice versa.
    #'
    #' @param hID id of the human involved in this transmission event
    #' @param mID id of the mosquito involved in this transmission event
    #' @param tEvent time of the event
    #' @param event "M2H" or "H2M" for mosquito to human and human to mosquito transmission, respectively
    #'
    push2pedigree = function(hID,mID,tEvent,event){
      new = list(id=private$id,parentID=private$parentID,hID=hID,tEvent=tEvent,event=event)
      MBITES:::Pedigree$assign(key=private$id,value=new)
    },

    #' @description
    #' Conditional transfer of pathogen from mosquito to human during probing.
    #' @param mosquito Reference to the mosquito object.
    #' @param human Reference to the human object.
    probeHost = function(mosquito, host){
      # No multiple infection, so check the host for same pathogen type.
      is_SEI <- function(p) { "SEI_Pathogen" %in% class(p) }
      sei_list <- host$on_pathogens(is_SEI)
      has_SEI <- any(as.logical(sei_list))

      if(!has_SEI && self$m2h_transmission()){
        # based on pf dynamics; recombination occurs in the mosquito, therefore a simple clone
        # of the object is all that's needed for the human.
        pathogen = SEI_Pathogen$new(
          parentID = self$get_id(),
          host_id = host$get_id()
          )
        pathogen$mosquito2human()
        host$add_pathogen(pathogen)
        m_id <- mosquito$get_id()
        host$pushProbe(m_id=m_id,t=private$tNow)
        logtrace(paste("m2h", m_id, private$human_id))
      } else {
        logtrace(paste("probe_no_transmit", mosquito$get_id(), host$get_id()))
      }
    },

    #' @description
    #' Decide whether this pathogen is infectious. Returns a boolean.
    m2h_transmission = function(){
      # haven't passed incubation period
      if(!private$infectious){
        return(FALSE)
        # have passed incubation; check for successful transmission
      } else {
        if(runif(1) < private$b){
          return(TRUE)
        } else {
          return(FALSE)
        }
      }
    },

    #' @description
    #' Tell this pathogen that it is living inside a human.
    mosquito2human = function(){
      private$infectious = FALSE
      private$incubating = 0L
      private$incubation_h = MBITES:::PathogenParameters$get_human_incubation()
    },

    #' @description
    #' A day in the life of a pathogen when it's in a human.
    #' @param human Reference to the human who has this pathogen.
    #' Overwrites function in the generic pathogen class.
    oneDay_human = function(human){
      malaria_clearance <- 200  # days
      if (is.null(private$time_created)) {
        private$time_created <- MBITES:::Globals$get_tNow()
      }
      # if not infectious advance the incubation period by one day
      private$incubating = private$incubating + 1L
      if(private$incubating > malaria_clearance){
        logtrace(paste("human_clear_pathogen", private$incubating))
        human$remove_pathogen(self)
        self$exit()
      } else if (!private$infectious &&
                 private$incubating >= private$incubation_h) {
        private$infectious = TRUE
        private$time_became_infectious = MBITES:::Globals$get_tNow()
        logtrace(paste("human_pathogen_infectious", private$incubating))
      } else {
        logtrace(paste("human_pathogen_age", private$incubating))
      }
    },

    #' The day on which this infectious bite happened.
    #' This may be null if the first `oneDay()` hasn't been called.
    infectious_bite_time = function() {
      private$time_created
    },

    #' @description
    #' Determine whether a human can transmit this pathogen to a mosquito.
    #' Returns a bool.
    h2m_transmission = function(){
      # havent passed incubation period
      if(!private$infectious){
        return(FALSE)
        # have passed incubation; check for successful transmission
      } else {
        if(runif(1) < private$c){
          return(TRUE)
        } else {
          return(FALSE)
        }
      }
    },

    #' @description
    #' Decide whether this pathogen will enter the given mosquito.
    #' @param host The human host.
    #' @param mosquito The mosquito R6 object.
    #' @param mPathogens The pathogens already in the mosquito.
    feedHost = function(host, mosquito, mPathogens){
      # no superinfection, so only do this if i don't have any pathogens in me
      is_SEI <- function(p) { "SEI_Pathogen" %in% class(p) }
      sei_list <- mosquito$on_pathogens(is_SEI)
      has_SEI <- any(as.logical(sei_list))

      mPathogen <- NULL
      if(!has_SEI && self$h2m_transmission()){
        # generate a new pathogen and push it to the pedigree (recombination
        # occurs in mosquito)
        mPathogen <- SEI_Pathogen$new(
          parentID = self$get_id(),
          mosquito_id = mosquito$get_id()
          )
        mPathogen$human2mosquito()
        mPathogen$push2pedigree(
          hID=host$get_id(),mPathogen$get_id(),tEvent=private$tNow,event="H2M"
        )
        logtrace(paste("h2m", "yes_transmit", host$get_id(), self$get_id()))
      } else {
        logtrace(paste("h2m", "no_transmit", host$get_id(), self$get_id()))
      }
      mPathogen
    },

    #' @description
    #' Initialize this for new creation in a mosquito.
    human2mosquito = function(){
      private$incubation_m = MBITES:::PathogenParameters$get_mosquito_incubation()
    },

    exit = function() {
      cat(
        jsonlite::toJSON(
          x = list(
            id = private$id,
            parent_id = private$parentID,
            human_id = private$human_id,
            mosquito_id = private$mosquito_id,
            birth = private$time_created,
            time_infectious = private$time_became_infectious,
            death = MBITES:::Globals$get_tNow()
          ),
          pretty = MBITES:::Globals$pretty
        ),
        ",\n",
        sep = "",
        file = MBITES:::Globals$get_pathogen_out()
      )
    }

  ),

  private = list(
    infectious = FALSE,
    # state: SI?
    incubating = 0,
    # how long i have been incubating
    incubation_h = integer(1),
    # incubation in humans
    incubation_m = integer(1),
    # incubation in mosquitoes (EIP)

    mosquito_id = NULL,
    human_id = NULL,
    time_created = NULL,
    time_became_infectious = NULL,
    # transmission efficiency
    b = numeric(1),
    # mosy -> human
    c = numeric(1) # human -> mosy
  )
)


###############################################################################
# Push Pathogen to Pedigree
###############################################################################

# If using the SEI generic model; anyone with parentID = 0 needs to look up the
# ancestor pathogen.
ancestor_SEI <- function(){
  ancestor = list(id=0L,parentID=NaN,hID=-1L,tEvent=0,event="ancestor")
  MBITES:::Pedigree$assign(key=0L,value=new)
}


PathogenSEI_SETUP <- function(){
  flog.debug("Setting up SEI pathogen hooks on Mosquito.")
}
