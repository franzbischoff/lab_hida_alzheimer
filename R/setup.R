pkgs <- c("Hmisc")
not_inst <- !(pkgs %in% row.names(installed.packages()))

if (any(not_inst)) {
  cat("The following packages must be installed:\n")
  cat(paste(pkgs[not_inst], collapse = ", "))
  a <- readline(prompt="Do you want to install them now? (Y/N) ")

  if(tolower(a) == "y") {
    install.packages(pkgs[not_inst])
  } else {
    stop("Cannot continue.")
  }
}

if (!requireNamespace("ADNIMERGE")) {
  cat("ADNIMERGE package is not installed.\n")
  cat("Please install it manually.")
}

######## General Information ----
##
## The data is in 'long' format, where each line is a 'visit', and the participant
## may appear several times.
##
## RID: Participant roster ID
## EXAMDATE: date of the exam; When not available: 'extract EXAMDATE from REGISTRY table using RID and VISCODE'
## USERDATE: date of data entry (not the actual exam date)
##
## Missings: -1 is a confirmed missing. -4 is a passive missing.
#
# NEUROPATH_04_12_18_DICT <- read_csv("data/NEUROPATH_04_12_18_DICT.csv",
#   col_types = cols(ID = col_skip()), guess_max = 10000
# )
#
# DATADIC <- read_csv("data/DATADIC.csv", guess_max = 10000)
# BLCHANGE <- read_csv("data/BLCHANGE.csv", guess_max = 10000)
# ADSXLIST <- read_csv("data/ADSXLIST.csv", guess_max = 10000)
#
#
# ######## Description DXSUM_PDXCONV_ADNIALL ----
# ##
# ## Diagnosis by visit
# ## ADNI1 and ADNIGO2 use different variables for diagnosis
# ## ADNI1: DXCURREN, DXCONV, DXREV, DXCONTYP
# ## ADNIGO2: DXCHANGE
#
# DXSUM_PDXCONV_ADNIALL <- read_csv("data/DXSUM_PDXCONV_ADNIALL.csv",
#                                   col_types = cols(DIAGNOSIS = col_number()),
#                                   guess_max = 10000)
#
# ######## Description NEUROPATH_04_12_18 ----
# ##
# ## Diagnosis by visit
# ## ADNI1 and ADNIGO2 use different variables for diagnosis
# ## ADNI1: DXCURREN, DXCONV, DXREV, DXCONTYP
# ## ADNIGO2: DXCHANGE
#
# NEUROPATH_04_12_18 <- read_csv("data/NEUROPATH_04_12_18.csv", guess_max = 10000)
#
# DXCODES <- DXSUM_PDXCONV_ADNIALL %>% select(
#   Phase:RID, matches("CODE"), USERDATE,
#   EXAMDATE, DXCHANGE, DXCURREN, DIAGNOSIS
# )
