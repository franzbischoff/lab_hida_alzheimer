library(readr)
library(dplyr)
NEUROPATH_04_12_18_DICT <- read_csv("data/NEUROPATH_04_12_18_DICT.csv",
  col_types = cols(ID = col_skip()), guess_max = 10000
)
NEUROPATH_04_12_18 <- read_csv("data/NEUROPATH_04_12_18.csv", guess_max = 10000)
DXSUM_PDXCONV_ADNIALL <- read_csv("data/DXSUM_PDXCONV_ADNIALL.csv",
                                  col_types = cols(DIAGNOSIS = col_number()),
                                  guess_max = 10000)
DATADIC <- read_csv("data/DATADIC.csv", guess_max = 10000)
BLCHANGE <- read_csv("data/BLCHANGE.csv", guess_max = 10000)
ADSXLIST <- read_csv("data/ADSXLIST.csv", guess_max = 10000)


DXCODES <- DXSUM_PDXCONV_ADNIALL %>% select(
  Phase:RID, matches("CODE"), USERDATE,
  EXAMDATE, DXCHANGE, DXCURREN, DIAGNOSIS
)
