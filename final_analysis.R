# "Modal American" Analysis
# This is the code behind Planet Money Episode 936: The Modal American
# https://www.npr.org/2019/08/28/755191639/episode-936-the-modal-american
# Analysis by Ben Casselman, Kenny Malone, Liza Yeager and Darian Woods.
# Comments/questions/corrections to ben.casselman@nytimes.com

library(tidyverse)

# DOWNLOAD AND PARSE THE DATA

# Data used in the episode is from the 2013-17 5-year American Community Survey,
# via the IPUMS project at the University of Minnesota: https://usa.ipums.org/usa/
# We downloaded 62 variables, but our final analysis used only the following:
# Technical: PERWT
# Demographic: AGE, SEX, RACE, HISPAN, MARST
# Economic: HHINCOME, WKSWORK2, UHRSWORK
# Geographic: STATEFIP, PUMA

# For supplementary analysis not discussed in the episode,
# we aslso looked at two other variables (OCC and NCHILD) and also ran
# the analysis for 1980 (using data from the 1980 decennial census, also via IPUMS).

# Select the relevant variables at IPUMS.org and download as a .csv file.
# These files are quite large (for both years combined, in excess of 4.5gb), so
# they may be too large to handle in memory.
# The following is my method for dealing with this, using the "chunkwise" package
# -- there are certainly other methods that would work.

library(chunked)
library(dbplyr)

filename <- "" # Inser name of zipped .csv file from IPUMS
R.utils::gunzip(paste0(filename, ".gz"))
raw_data <- read_csv_chunkwise(file = filename) # This doesn't actually read the full file, just sets it up

con <- src_sqlite("acs", create = TRUE) # Set up SQLite db.
insert_chunkwise_into(raw_data, con, "full_data", temporary = FALSE) # This will read the file by chunks into the db.

# COLLECT THE RELEVANT VARIABLES

# Note: This file will still be quite large, on the order of 2.5gb.
acs_17 <- src_sqlite("acs", create = FALSE) %>% 
  tbl("full_data") %>% 
  filter(YEAR == 2017) %>% # Only necessary if you've downloaded multiple years.
  select(PERWT, AGE, SEX, HHINCOME, WKSWORK2, UHRSWORK, 
         EDUCD, RACE, HISPAN, MARST, STATEFIP, PUMA,
         OCC, NCHILD) %>% 
  collect() # This will collect all the data into memory. 

# Because this file is large, I highly recommend creating a sample for any
# exploratory analysis. You can run the final code on the full sample to confirm
# the results are the same:

data_slice <- acs_17 %>% 
  sample_n(100000)

# However, I will show this using the full data since that's what we did in our
# final analysis on the show.

# ASSIGN THE BUCKETS

# We used eight variables in our final analysis: Sex, age, race/ethnicity, marital status, education, household income, employment status, urbanicity.
# All except for sex required some modification or grouping from the underlying variables.
# This code assigns the relevant buckets for all variables *except* urbanicity.
# For more discussion of how we settled on these variables, see the show page: https://www.npr.org/2019/08/28/755191639/episode-936-the-modal-american

acs_17 <- acs_17 %>% 
  mutate(age2 = cut(AGE, 
                    breaks = c(-Inf, 20, 36, 52, 71, Inf), 
                    labels = c("Gen Z", "Millennial", "Gen X", "Boomers", "Silent")),
                race2 = case_when(HISPAN == 0 & RACE == 1 ~ "White",
                         TRUE ~ "Non-white"),
         marital_status = case_when(MARST %in% c(1,2) ~ "Married",
                                    MARST %in% 3:5 ~ "Divorced/separated/widowed",
                                    MARST == 6 ~ "Never married"),
         college = case_when(EDUCD < 100 ~ "non-college",
                             EDUCD %in% 100:116 ~ "college"),
         inc_bucket = cut(HHINCOME, 
                          breaks = c(-Inf, 30000, 75000, 165000, 9999998, 9999999),
                          labels = c("low_income", "middle_income", "upper_middle_income", "high_income", NA))
         )

# Urbanicity is more complicated.
# First need to calculate the density of each PUMA. Run `density.R` 
# in a separate file in this repo, then join resulting
# dataframe with our data.

# (Alternatively, use the csv in this repo:)
# puma_density <- read_csv("puma_density.csv")

acs_17 <- acs_17 %>% 
  mutate(puma = paste0(sprintf("%02d", STATEFIP), sprintf("%05d", as.numeric(PUMA)))) %>% 
  left_join(puma_density, by = "puma")

# There are lots of ways we could decide how to assign urban categories based on
# density. But we'll keep it simple: Trulia ran a survey several years ago that
# asked people whether they lived in a urban, suburban or rural area. We'll use the
# proportions found there, and assign people accordingly (e.g. the  21% of the
# population living in the least dense places are assigned 'rural' status) :
urban_cuts <- working %>% filter(AGE >= 18) %$% 
  weighted.quantile(logged_density, PERWT, probs = c(0, .21, .74, 1))

acs_17 <- acs_17 %>% 
  mutate(urbanicity = cut(logged_density, breaks = urban_cuts, 
                      labels = c("rural", "suburban", "urban"), include.lowest = T)) 

# RUN THE ANALYSIS

# Simple function to bucket according to whatever set of variables we want.
# Particularly useful during the exploratory phase.

# Function for performing analysis:
grouper <- function(data, ...) {
  data %>% 
    group_by(...) %>% 
    summarize(total = sum(PERWT)) %>% 
    arrange(desc(total))
}

prelim_results <- acs_17 %>% 
  grouper(SEX, age2, ftyr, inc_bucket, college, race2, marital_status, groups)

View(prelim_results)

# This will show the top results as all children (hi, Jacob's kids!).
# Feel free to explore, but for our purposes we'll drop them.

rm(prelim_results)

# For final results, we'll eliminate minors.

final_results <- acs_17 %>% 
  filter(AGE >= 18) %>% 
  grouper(SEX, age2, ftyr, inc_bucket, college, race2, marital_status, groups)

View(final_results)

# Can write these results to csv:
write.csv(final_results, file = "final_results.csv")

# FURTHER EXPLORATION

# We might want to explore our "modal group" more -- for example
# to find out what is typical *within* that group.
# A few examples we looked at shown here:

modal_group <- acs_17 %>% 
  filter(SEX == "Male",
         AGE %in% 37:52,
         ftyr == "full-time year-round worker",
         HHINCOME %in% 75001:165000,
         college == "non-college",
         race_group == "White",
         marital_status == "Married",
         urbanicity == "suburban")

# What are the most common occupations?
modal_group %>% 
  grouper(OCC) %>% 
  mutate(share = total/sum(total))

# What about grouping these occupations together into some broad categories?
modal_group <- modal_group %>% 
  mutate(occ_group = case_when(OCC %in% 1:3655 ~ "white-collar professional",
                      OCC %in% 3700:5940 ~ "service sector",
                      OCC %in% 6005:9920 ~ "blue-collar"))

modal_group %>% 
  grouper(occ_group) %>% 
  mutate(share = total/sum(total))

# How many of this group have children at home?
modal_group %>% 
  grouper(NCHILD) %>% 
  mutate(share = total/sum(total))

# Our modal group doesn't have a bachelor's degree. 
# But let's look at their education status in more detail:
modal_group %>% 
  mutate(educ_var = case_when(EDUCD <= 61 ~ "less than HS",
                              EDUCD %in% 62:64 ~ "HS only",
                              EDUCD %in% 65:99 ~ "some college",
                              EDUCD %in% 100:101 ~ "BA",
                              EDUCD %in% 102:116 ~ "grad degree")) %>% 
  grouper(educ_varp) %>% 
  mutate(share = total/sum(total))