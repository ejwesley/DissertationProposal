library(tidyverse)
library(brms)
library(here)

summer2010 <- read_csv(here("data", "Summer2010.csv"),
                       col_types = cols(GEOID10 = col_factor(),
                                        Date = col_date(format = ""),
                                        n = col_integer(),
                                        severity = col_factor(),
                                        median = col_double()))
