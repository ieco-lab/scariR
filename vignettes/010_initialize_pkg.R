# Title: "initialize package using renv"
# Author: Samuel M. Owens
# date: 08-26-2024
# Contact: sam.owens@temple.edu

# In this file, I will list the dependency packages for slfSpread and initialize the `renv` package to keep my other package versions consistent.
# End users should only run sections 1 and 2. Section 3 is for troubleshooting common issues.
# section 4 should only be used for the first-time initialization of the package.

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

# first, check the status of your current package versions vs those in renv
renv::status()

# you should run this to ensure the correct package versions are used while you are working within this project. This aligns the sys library with the lockfile.
renv::restore()
# only use this version if you want to edit your system library to match the renv library
renv::restore(clean = TRUE)







# troubleshooting---------------------------------------------------------------

# the first thing you should try is updating your R and Rtools version.. This messed up my renv for quite awhile
# NOTE TO SELF: DONT FORGET to update R_LIBS_USER PATH system variable in windows if R was updated by a major version (eg 4.4 -> 4.5)
## might be the solution if packages are behaving strangely

# hydrate() corrected an issue where renv could not discover any local packages in the system library
renv::hydrate()

# install specific packages (usually needed for packages not on the CRAN)
# you may need to enter your github credentials into the .Renviron or log into github for this to work
# first, create a github PAT on website
usethis::create_github_token()
# enter created PAT (personal access token) into the .Renviron
## be sure to use defaults and allow download and upload of packages
gitcreds::gitcreds_set()
# now install packages not on the CRAN
devtools::install_github("densitymodelling/dsmextra") # dsmextra
devtools::install_github("ieco-lab/lydemapr") # lydemapr
devtools::install_github("jasonleebrown/humboldt") # humboldt
devtools::install_github("ropensci/rnaturalearthhires")# rnaturalearthhires

# renv restore to previous package versions
# only use if the current package version need to be reverted to a previous state
renv::history()
renv::revert(project = "slfSpread", commit = "")

# repair common issues
renv::repair()

# update renv to latest version in this project (but this will differ from the source download of this project folder)
renv::upgrade()

# run this to rebuild and reinstall all packages in the library- this starts them from a clean slate
renv::rebuild()



# one-time renv initialization--------------------------------------------------

# I created this section to initialize the renv package for the first time ONLY.
# DO NOT run this section if you have downloaded this package as an outside user.

# initialization of project renv
# only run once
renv::init()

# record newest package versions in lockfile
renv::snapshot()

# update packages
# only run as needed if a package breaks or something
renv::update()
# alternatively, install the latest CRAN versions:
renv::install()
