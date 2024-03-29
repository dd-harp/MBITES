###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Process Bionomics for peri-domestic experiments
#     MBITES Team
#     October 2018
#
###############################################################################


###############################################################################
# initialization bits
###############################################################################

rm(list=ls());gc()
library(here)

# set up
script_dir <- here("scripts/")
out_dir <- here("output/peridom/")
analysis_dir <- here("output/peridom_analyzed/")
lscape_dir <- here("data/pointsets/")

library(Rcpp)
library(lokern)
library(MBITES)
library(jsonlite)

Rcpp::sourceCpp(paste0(script_dir,"bionomics.cpp"))

# compare floats
fequal <- function(x,y,tol=.Machine$double.eps^0.5){
  abs(x-y) < tol
}

# take a vector of distances and make ECDF,PMF,smoothed CDF, smoothed PDF
# tol: tolerance when computing PMF
smooth_kernels <- function(distances, tol = .Machine$double.eps^0.75){

  d_ecdf <- stats::ecdf(distances)
  d_knots <- stats::knots(d_ecdf)

  d_pmf <- vapply(d_knots, function(x,tol){
    d_ecdf(x+.Machine$double.eps^0.75) - d_ecdf(x-.Machine$double.eps^0.75)
  }, numeric(1), tol = tol)

  d_cdf <- lokern::glkerns(d_knots,d_ecdf(d_knots),deriv = 0,korder = 4)
  d_pdf <- lokern::glkerns(d_knots,d_ecdf(d_knots),deriv = 1,korder = 3)

  return(list(
    ecdf=d_ecdf,
    knots=d_knots,
    pmf=d_pmf,
    cdf=d_cdf,
    pdf=d_pdf
  ))
}

# plots a smooth kernel (output from 'smooth_kernels' in an aesthetically pleasing way)
# does not change par() at all; reccomend to call par(mar = c(4.5, 4.5, 2.5, 4.5)) before
plot_smooth_kernels <- function(kernel, name){

  maxx <- max(kernel$knots)+(0.001*max(kernel$knots))
  k_mean <- weighted.mean(x = kernel$knots,w = kernel$pmf) # mean from PMF

  # PDF
  pdf_trunc <- ifelse(kernel$pdf$est < 0,0,kernel$pdf$est)
  plot(c(0,kernel$knots),
       c(0,pdf_trunc),
       type="n",xlab="Distance",ylab="Density")
  polygon(x = c(0,kernel$knots,maxx),
          y = c(0,pdf_trunc,0),border = NA,col = adjustcolor("mediumblue",alpha.f = 0.6))
  mtext(paste0(name," (mean: ",round(k_mean,3),")"),side = 3,line = 0.5,cex=1.25)
  par(new = TRUE)

  # CDF
  cdf_trunc <- ifelse(kernel$cdf$est < 0,0,kernel$cdf$est)
  plot(kernel$knots,
       cdf_trunc,
       type="n", xaxt = "n", yaxt = "n",ylab = "", xlab = "")
  lines(x = kernel$knots,
          y = cdf_trunc,
          col = adjustcolor("firebrick3",1),lwd=2)
  axis(side=4, at = pretty(range(cdf_trunc)))
  mtext("Cumulative Probability", side = 4, line = 2.25)

}

# calculate QSD (quasi-stationary distribution) for transition matrix P over transient set
# see eqn 3: Darroch, J. N.; Seneta, E. (1965). "On Quasi-Stationary Distributions in Absorbing Discrete-Time Finite Markov Chains". Journal of Applied Probability. 2 (1): 88–100. doi:10.2307/3211876
qsd_dtmc <- function(M,abs = c("D")){
  P <- M[-which(rownames(M) %in% abs),-which(colnames(M) %in% abs)]
  pi <- c(F=1,B=0,R=0,L=0,O=0)
  e <- rep(1,5)
  num <- (pi %*% solve(diag(nrow(P)) - P))
  num / as.numeric((num %*% e))
}


###############################################################################
# process run(s)
###############################################################################

n <- 26
sumstat <- vector("list",n)

verbose <- TRUE # if TRUE print out progress  bars in the fn's; o/w just have a global bar
if(!verbose){
  pb <- txtProgressBar(1,n)
}

for(i in 1:n){

  # read in data
  mosquitos <- fromJSON(paste0(out_dir,"/mosquito_F_",i,".json"), flatten = TRUE)
  mosquitos <- mosquitos[-which(sapply(mosquitos$id,is.null)),]
  humans <- fromJSON(paste0(out_dir,"/human_",i,".json"), flatten = TRUE)
  humans <- humans[-which(sapply(humans$id,is.null)),]
  dist <- as.matrix(read.csv(paste0(lscape_dir,"dist_",i,".csv"), header = FALSE))

  # summary statistics

  # state transitions
  sumstat[[i]]$transitions <- Bionomics_StateTransitionCpp(mosquitos,verbose = verbose)

  # lifespan
  sumstat[[i]]$lifespan <- Bionomics_lifespanCpp(mosquitos,verbose = verbose)

  # number of blood hosts
  sumstat[[i]]$blood_hosts <- Bionomics_humanBloodHostCpp(mosquitos,who = "human",verbose = verbose)

  # feeding interval & resting interval
  sumstat[[i]]$blood_interval <- Bionomics_bloodIntervalsCpp(mosquitos,who = "human",verbose = verbose)
  sumstat[[i]]$blood_interval <- unlist(sumstat[[i]]$blood_interval)
  sumstat[[i]]$rest_interval <- Bionomics_restIntervalsCpp(mosquitos,verbose = verbose)
  sumstat[[i]]$rest_interval <- unlist(sumstat[[i]]$rest_interval)

  # hbr (human biting proportion)
  sumstat[[i]]$human_biterate <- Bionomics_humanBitingProportionCpp(mosquitos,verbose = verbose)

  # blood feeding rate
  sumstat[[i]]$blood_rate <- Bionomics_bloodfeedingRateCpp(mosquitos,verbose = verbose)
  sumstat[[i]]$blood_rate <- unlist(sumstat[[i]]$blood_rate)

  # lifetime egg production
  sumstat[[i]]$life_egg <- Bionomics_lifetimeOvipositionCpp(mosquitos,dist,verbose = verbose)

  # oviposition interval
  sumstat[[i]]$egg_interval <- Bionomics_ovipositionIntervalCpp(mosquitos,verbose = verbose)

  # oviposition rate
  sumstat[[i]]$egg_rate <- Bionomics_ovipositionRateCpp(mosquitos,verbose = verbose)

  # vectorial capacity
  sumstat[[i]]$VC <- Bionomics_vectorialCapacityCpp(mosquitos,dist,nrow(humans),EIP = 10,unique = F,verbose = verbose)

  # vectorial capacity (unique secondary hosts)
  sumstat[[i]]$VC_unique <- Bionomics_vectorialCapacityCpp(mosquitos,dist,nrow(humans),EIP = 10,unique = T,verbose = verbose)

  # dispersion of vc and eggs
  sumstat[[i]]$vc_dispersion <- smooth_kernels(distances = sumstat[[i]]$VC$dispersion)
  sumstat[[i]]$vc_dispersion_unique <- smooth_kernels(distances = sumstat[[i]]$VC_unique$dispersion)
  sumstat[[i]]$egg_dispersion <- smooth_kernels(distances = sumstat[[i]]$life_egg$dispersion)

  # cumulative and absolute dispersal of mosquitos
  sumstat[[i]]$disperse_cum <- Bionomics_cumulativeDisperseCpp(mosquitos = mosquitos,dist = dist,verbose = verbose)
  sumstat[[i]]$disperse_abs <- Bionomics_absoluteDisperseCpp(mosquitos = mosquitos,dist = dist,verbose = verbose)

  # smoothed mosquito dispersion
  sumstat[[i]]$disperse_cum_smooth <- smooth_kernels(distances = sumstat[[i]]$disperse_cum)
  sumstat[[i]]$disperse_abs_smooth <- smooth_kernels(distances = sumstat[[i]]$disperse_abs)

  # remove data
  rm(mosquitos,humans,dist);gc()

  if(!verbose){
    setTxtProgressBar(pb,i)
  } else {
    cat("completed run: ",i," of ",n,"\n")
  }
}

saveRDS(object = sumstat,file = paste0(analysis_dir,"summary.rds"),compress = TRUE)


###############################################################################
# process EIRs
###############################################################################

n <- 26
EIRs <- vector("list",n)

verbose <- TRUE # if TRUE print out progress  bars in the fn's; o/w just have a global bar
if(!verbose){s
  pb <- txtProgressBar(1,n)
}

for(i in 1:n){

  # read in data
  humans <- fromJSON(paste0(out_dir,"/human_",i,".json"), flatten = TRUE)
  humans <- humans[-which(sapply(humans$id,is.null)),]


  EIRs[[i]] <- Bionomics_EIR(humans = humans,tStart = 0,tEnd = (365*5),verbose = verbose)

  # remove data
  rm(humans);gc()

  if(!verbose){
    setTxtProgressBar(pb,i)
  } else {
    cat("completed run: ",i," of ",n,"\n")
  }
}

saveRDS(object = EIRs,file = paste0(analysis_dir,"EIR.rds"),compress = TRUE)
