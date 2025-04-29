rm(list=ls())  # Clears environment
gc()  # Runs garbage collection

#-------------------------------------------------------------
# If installing r4ss from github 
#-------------------------------------------------------------

# devtools::install_github("https://github.com/r4ss/r4ss.git")

#-------------------------------------------------------------
# Libraries to load
#-------------------------------------------------------------

load.lib <- c("tidyverse", "r4ss", "here")

# If library is not installed, intall library and dependancies
install.lib <- load.lib[!load.lib %in% installed.packages()]

for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)

#-------------------------------------------------------------
# Set relative path
#-------------------------------------------------------------

wd <- here()

#-------------------------------------------------------------
# If loading functions, source them
#-------------------------------------------------------------

# files <- list.files(here(wd, "", "functions"), full.names = TRUE)
# lapply(files, source)

#-------------------------------------------------------------------------
# r4ss options
#-------------------------------------------------------------------------

skip_finished <- FALSE
launch_html <- TRUE

#-------------------------------------------------------------------------
# To get and install ss3 executable files
#-------------------------------------------------------------------------

#' Wrapper for r4ss::get_ss3_exe to check for, download, and return name of SS3 exe file
#' @param dir directory to install SS3 in
#' @param ... Other arguments to `r4ss::get_ss3_exe`
#'
#' @return Character string with name of downloaded SS3 exe (without extension)
set_ss3_exe <- function(dir, ...) {
  
  # Get and set filename for SS3 exe
  ss3_exe <- c("ss", "ss3")
  ss3_check <- vapply(ss3_exe, \(x) file.exists(file.path(dir, paste0(x, ".exe"))), logical(1))
  if (!any(ss3_check)) r4ss::get_ss3_exe(dir, ...)
  ss3_exe <- ss3_exe[which(vapply(ss3_exe, \(x) file.exists(file.path(dir, paste0(x, ".exe"))), logical(1)))]
  return(ss3_exe)
  
}

#-------------------------------------------------------------------------
# Whether to re-run previously fitted models
#-------------------------------------------------------------------------

rerun <- FALSE

#-------------------------------------------------------------------------
# 2010 Const Growth Base Model
#-------------------------------------------------------------------------

## Read in the base file
ResBaseMod2025dir <- here("models","ResBaseMod2025")

ss3_exe <- set_ss3_exe(ResBaseMod2025dir, version = "v3.30.23")

# run model
# With estimation and hessian it takes ~12 minutes
r4ss::run(
  dir = ResBaseMod2025dir,
  exe = ss3_exe,
  #extras = "-nohess",
  show_in_console = TRUE,
  skipfinished = !rerun
)


# Get r4ss output
replist <- SS_output(
  dir = ResBaseMod2025dir,
  verbose = TRUE,
  printstats = TRUE,
  covar = TRUE
)

# Plots the results (store in the 'R_Plots' sub-directory)
SS_plots(replist, dir = ResBaseMod2025dir, printfolder = "R_Plots")

# -------------------------------------------------------------------------