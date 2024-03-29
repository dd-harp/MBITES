###############################################################################
#         __  __
#        / / / /_  ______ ___  ____ _____
#       / /_/ / / / / __ `__ \/ __ `/ __ \
#      / __  / /_/ / / / / / / /_/ / / / /
#     /_/ /_/\__,_/_/ /_/ /_/\__,_/_/ /_/
#
#     Human-Initialize Human Populations
#     MBITES Team
#     March 2018
#
###############################################################################


###############################################################################
# Initialize human populations
###############################################################################

#' Initialize Null Human Populations
#'
#' Initialize \code{\link{Human_NULL}} populations
#'
#' @param human_init a \code{\link{data.frame}} object where each row corresponds to a human
#'
#' @details each row must have the following named columns:
#' \itemize{
#'   \item tileID: the tile the human resides in (right now should be all the same)
#'   \item siteID: the \code{\link[MBITES]{Site}} that the human resides in (must be a site that has a \code{\link[MBITES]{Feeding_Resource}})
#'   \item feedingID: the specific \code{\link[MBITES]{Feeding_Resource}} that this person resides at
#'   \item w: this person's relative biting weight, must be a number in [0,inf)
#' }
#'
#'
#' @export
Human_NULL_Initialize <- function(human_init){

  n = nrow(human_init)
  pb = txtProgressBar(min = 0, max = n, initial = 0)
  time = MBITES:::Globals$get_tNow()

  for(i in 1:n){
    tileID = human_init[i,"tileID"]
    siteID = human_init[i,"siteID"]
    feedingID = human_init[i,"feedingID"]
    w = human_init[i,"w"]
    id = MBITES:::Globals$get_human_id()
    human = Human_NULL$new(id,w,feedingID,siteID,tileID)
    MBITES:::Globals$get_tile(tileID)$get_humans()$assign(key=id,value=human)

    setTxtProgressBar(pb,i)
  }
}
