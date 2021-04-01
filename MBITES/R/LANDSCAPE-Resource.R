###############################################################################
#         __                    __
#        / /   ____ _____  ____/ /_____________ _____  ___
#       / /   / __ `/ __ \/ __  / ___/ ___/ __ `/ __ \/ _ \
#      / /___/ /_/ / / / / /_/ (__  ) /__/ /_/ / /_/ /  __/
#     /_____/\__,_/_/ /_/\__,_/____/\___/\__,_/ .___/\___/
#                                            /_/
#     Landscape-Resource
#     MBITES Team
#     February 2018
#
###############################################################################


#' Landscape Resource manages weight of resources within a site.
#'
#' @description
#' All resources inherit from the \code{Resource} abstract base class object.
#'
#' @export
Resource <- R6::R6Class(
  classname = "Resource",
  portable = TRUE,
  cloneable = FALSE,
  lock_class = FALSE,
  lock_objects = FALSE,

  public = list(

   #' @description
   #' Create a resource.
   #' @param w relative weight of this resource among all resources at a site.
   #' @param site reference to the site that has this resource.
   #' @return a new resource object.
   initialize = function(w,site){
     # futile.logger::flog.trace("Resource being born at: self %s , private %s",pryr::address(self),pryr::address(private))

     private$w = w
     private$SiteP = site

   },

   #' @description
   #' Release memory associated with this resource.
   finalize = function(){
     # futile.logger::flog.trace("Resource being killed at: self %s , private %s",pryr::address(self),pryr::address(private))
   },

   #' @description
   #' Clear state of a resource to prepare for a new simulation.
   reset = function(){},

   #' @description
   #' Get weight of this resource among resources at site.
   get_w = function() {
     return(private$w)
   },

   #' @description
   #' Get the site that contains this resource.
   get_site = function() {
     return(private$SiteP)
   }
  ),

  private = list(

    w             = numeric(1),  # weight of this resource
    SiteP         = NULL # reference to enclosing Site

  )

)
