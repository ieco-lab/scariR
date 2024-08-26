# Title: "initialize package using renv"
# Author: Samuel M. Owens
# date: 08-26-2024
# Contact: sam.owens@temple.edu

# In this file, I will initialize the dependency packages for slfSpread and initialize the renv package to keep my package versions consistent.

# Package dependencies----------------------------------------------------------

# Run this chunk to see the dependencies for the slfSpread package.

# install.packages("renv")
library(renv)
library(here)

# discover what packages are used in this project
packages <- renv::dependencies() %>%
  # select unique package names
  dplyr::summarize(package = sort(unique(Package))) %>%
  as.matrix() %>%
  as.character()

# if scrubr, webshot or geodata show up on this list, ignore these as they are no longer needed.

# renv initialization-----------------------------------------------------------

# Only run this section if you need to

# renv initialization-----------------------------------------------------------

# I created this section to initialze the renv package for the first time.
# DO NOT run this section if you have downloaded this package from my github repo.

# initialization of project renv
renv::init()

# record latest package versions in lockfile
renv::snapshot()







