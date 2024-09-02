# Title: "initialize package using renv"
# Author: Samuel M. Owens
# date: 08-26-2024
# Contact: sam.owens@temple.edu

# In this file, I will initialize the dependency packages for slfSpread and initialize the renv package to keep my package versions consistent.

# Package dependencies----------------------------------------------------------

# Run this chunk to see the dependencies for the slfSpread package.

library(tidyverse)
library(renv)
library(here)

# discover what packages are used in this project
packages <- renv::dependencies() %>%
  # select unique package names
  dplyr::summarize(package = sort(unique(Package))) %>%
  as.matrix() %>%
  as.character()

# install correct package versions for project----------------------------------

# you should run this to ensure the correct package versions are used while you are working within this project
# renv::restore()










# renv initialization-----------------------------------------------------------

# I created this section to initialize the renv package for the first time.
# DO NOT run this section if you have downloaded this package as an outside user

# initialization of project renv
# only run once
# renv::init()

# update packages
# only run as needed
# renv::update()

# record latest package versions in lockfile
# renv::snapshot()


# renv restore to previous package versions-------------------------------------

# only use if the current package version need to be reverted
renv::history()
renv::revert(project = "slfSpread", commit = "")





