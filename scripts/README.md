# scripts

What each file does:
  * bionomics.cpp: fast functions in C++ to calculate summary statistics of interest from simulation output
  * bionomics.R: the interface, point it at necessary files and calculate those statistics/bionomics!
  * model-comparison: compare MBDETES and MBITES on a simple landscape, this loads in pre-simulated MBITES stuff and then runs MBDETES with matching parameters
  * trivial-simulation.R: run MBITES on a very simple landscape (used for model comparison)
  * landscape-kernels.R: takes the raw point process output and makes distance matrix/movement matrices for each (run this before the landscape-kernels.R)
  * landscape-kernels.R: takes distance matrix/movement matrices and returns processed movement kernels (empirical and smoothed)
