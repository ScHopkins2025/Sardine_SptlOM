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
# 2025 Const Growth Base Model
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

#-------------------------------------------------------------------------
# Rerun values based on new files params
# -------------------------------------------------------------------------

# step 1: Create outputs by copying and modifying base model

updateParms_dir <- here(wd, "models", "UpdateBaseModParms", "ChangeToInit_new")

if (!dir.exists(updateParms_dir)) {
  
  dir.create(updateParms_dir, recursive = TRUE)
  
  r4ss::copy_SS_outputs(ResBaseMod2025dir, updateParms_dir)
  
}

SS_readdat(here(wd, "models", "UpdateBaseModParms", "ChangeToInit_new", "data.ss"))
