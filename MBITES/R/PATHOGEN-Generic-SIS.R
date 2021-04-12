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
    initialize = function(parentID = NULL) {
      super$initialize(parentID) # initialize base parts

      private$b = PathogenParameters$get_b()
      private$c = PathogenParameters$get_c()

      # futile.logger::flog.trace("SEI_Pathogen being born at self: %s , private: %s",pryr::address(self),pryr::address(private))
    },

    #' @description
    #' Destroy an SEI_Pathogen object.
    finalize = function() {
      super$finalize() # destruct base parts

      # futile.logger::flog.trace("SEI_Pathogen being killed at self: %s , private: %s",pryr::address(self),pryr::address(private))
    },

    # pathogen method: overwrite oneDay_mosquito
    oneBout <- function(){
      # if not infectious advance the incubation period to the time of next launch
      if(!infectious){
        private$incubating = private$incubating + private$tNext
        if(private$incubating >= private$incubation_m){
          private$infectious = TRUE
        }
      }
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

#' Generic SEI: Push a Pathogen to the Pedigree
#'
#' During a transmission event, push a pathogen to the pedigree.
#'
#' @param pathogen object of class \code{\link{SEI_Pathogen}}
#' @param hID id of the human involved in this transmission event
#' @param mID id of the mosquito involved in this transmission event
#' @param tEvent time of the event
#' @param event "M2H" or "H2M" for mosquito to human and human to mosquito transmission, respectively
#'
push2pedigree_SEI <- function(hID,mID,tEvent,event){
  new = list(id=private$id,parentID=private$parentID,hID=hID,tEvent=tEvent,event=event)
  MBITES:::Pedigree$assign(key=private$id,value=new)
}


###############################################################################
# Host Probing (probeHost): Mosquito -> Human Transmission
#   and other human dynamics
###############################################################################

# mosquito method
probeHost_SEI <- function(){
  if(private$pathogen$m2h_transmission()){
    # based on pf dynamics; recombination occurs in the mosquito, therefore a simple clone
    # of the object is all that's needed for the human.
    pathogen = pathogen$clone()
    pathogen$mosquito2human()
    MBITES:::Globals$get_tile(private$tileID)$get_human(private$hostID)$add_pathogen(pathogen)
  }
}

# pathogen method
m2h_transmission_SEI <- function(){
  # havent passed incubation period
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
}

# pathogen method
mosquito2human_SEI <- function(){
  private$infectious = FALSE
  private$incubating = 0L
  private$incubation_h = MBITES:::PathogenParameters$get_human_incubation()
}

# pathogen method
oneDay_human_SEI <- function(){
  # if not infectious advance the incubation period by one day
  if(!infectious){
    private$incubating = private$incubating + 1L
    if(private$incubating >= private$incubation_h){
      private$infectious = TRUE
    }
  }
}

# human method
add_pathogen_SEI <- function(pathogen){
  # no superinfection
  if(is.null(private$pathogen)){
    private$pathogen = pathogen
  }
}

###############################################################################
# Host Feeding (feedHost): Human -> Mosquito Transmission
#   and other mosquito dynamics
###############################################################################

# mosquito method
bloodFeed_SEI <- function(){
  # no superinfection, so only do this if i don't have any pathogens in me
  if(is.null(private$pathogen)){
    hPathogen = MBITES:::Globals$get_tile(private$tileID)$get_human(private$hostID)$get_pathogen()
    # only if the human had a pathogen
    if(!is.null(hPathogen)){
      # if it was actually infectious
      if(hPathogen$h2m_transmission()){
        # generate a new pathogen and push it to the pedigree (recombination occurs in mosquito)
        mPathogen = SEI_Pathogen$new(parentID = hPathogen$get_id())
        mPathogen$human2mosquito()
        private$pathogen = mPathogen
        private$pathogen$push2pedigree(hID=private$hostID,mID$private$id,tEvent=private$tNow,event="H2M")
      }
    }
  }
}

# pathogen method
h2m_transmission_SEI <- function(){
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
}

# pathogen method
human2mosquito_SEI <- function(){
  private$incubation_m = MBITES:::PathogenParameters$get_mosquito_incubation()
}

# mosquito method: update dynamics after the bout.
pathogenDynamics_SEI <- function(){
  if(!is.null(private$pathogen)){
    private$pathogen$oneBout()
  }
}


Pathogen_SEI_SETUP <- function(){
  Mosquito_Female$set(which = "public",name = "probeHost",
                      value = probeHost_SEI, overwrite = TRUE
  )

  Mosquito_Female$set(which = "public",name = "feedHost",
                      value = bloodFeed_SEI, overwrite = TRUE
  )

  Mosquito_Female$set(which = "public",name = "pathogenDynamics",
                      value = pathogenDynamics_SEI, overwrite = TRUE
  )
}
