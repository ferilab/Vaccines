# The US contagious diseases data is saved in R format

# The data were collected, organized, and distributed by the Tycho
# Project: http://www.tycho.pitt.edu/ and include weekly reported counts for 
# seven diseases from 1928 to 2011, from all the US states.
# The data in yearly totals format can be taken from the dslabs package.

# Data for Hepatitis A is extracted from the whole data set.
# Years with less than 30 reported weeks are removed from the data.

library(tidyverse)
library(dslabs)

data(us_contagious_diseases)
the_case <- "Hepatitis A"

HepatitisA_dat <- us_contagious_diseases %>%
  filter(weeks_reporting >= 30) %>%   
  filter(disease == the_case) %>%  
  mutate(disease_rate = count / population * 10^4 * 52 / weeks_reporting) %>%  ## calc. rate per ten tousands 
  mutate(state = reorder(state, disease_rate))
save(HepatitisA_dat, file = 'rdas/HepatitisA_dat.rda')

