#' An aquatic resource with daily survival for larvae.
#'
#' This has eggs, larvae, and imagos.
Aqua_Resource_Daily <- R6::R6Class(
  classname = "Aqua_Resource_Daily",
  portable = TRUE,
  cloneable = FALSE,
  lock_class = FALSE,
  lock_objects = FALSE,
  inherit = MBITES:::Aqua_Resource,

  public = list(
    #' @description
    #' Construct a new daily aqua resource.
    #' @param w A weight for this resource relative to other sites.
    #' @param site The site that contains this resource.
    initialize = function(w, site) {
      logtrace("Aqua_Resource_Daily")
      survival <- 0.9
      larvae_days <- 3
      self$larvae <- build_larvae(survival, larvae_days)
      super$initialize(w, site)
    },

    one_day = function() {
      tNow = MBITES:::Globals$get_tNow()
      logtrace(paste("Aqua_Resource_Daily", tNow))

      eggs <- self$EggQ$popQ(tNow)
      eggs_total <- sum(eggs)
      if (eggs_total > 0) {
        logtrace(paste("larvae_emerge", eggs_total))
        self$larvae$push(eggs_total)
      }
      larvae_total <- self$larvae$oneDay()
      # if emerging mosquitoes, send them to the population
      if (larvae_total > 0) {
        logtrace(paste("imagos_emerge", larvae_total))
        self$ImagoQ$add2Q(larvae_total, tNow, TRUE)
      }  # else no eggs to add, so continue.
    },

    push_imago = function() {
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
    },

    larvae = NULL
  )
)


#' Build a list of larvae.
#' @param p Daily survival.
#' @param m Number of days to maturation.
#' These larvae survive at a rate $p$ per day, and they stay larvae until
#' $m$ days have passed.
build_larvae <- function(p, m) {
  larvae <- list()

  list(
    #' Add eggs to become larvae.
    #' @param eggs A number of eggs to add.
    push = function(eggs) {
      larvae[[length(larvae) + 1]] <<- list(n = eggs, m = m)
    },

    count = function() {
      sum(vapply(larvae, function(l) l$n, numeric(1)))
    },

    oneDay = function() {
      imago_cnt <- 0L
      removal <- integer(0)
      if (length(larvae) > 0) {
        for (i in 1:length(larvae)) {
          larvae[[i]]$n <<- larvae[[i]]$n * p
          larvae[[i]]$m <<- larvae[[i]]$m - 1
          if (larvae[[i]]$m == 0) {
            removal <- append(removal, i)
            imago_cnt <- imago_cnt + rpois(1, larvae[[i]]$n)
          }
        }
      }
      larvae[removal] <<- NULL
      imago_cnt
    }
  )
}
