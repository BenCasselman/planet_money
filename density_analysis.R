# Urbanicity
# Use tract-weighted density by PUMA.
# Need tract-level population or households, area, plus tract-to-puma crosswalk.


# We need number of households in each census tract and the land area of each tract.
# These files are published tables from the Census Bureau.
# Area is from table GCT-PH1 from the 2010 decennial census, SF1.
# Housohlds are from 2017 5-year ACS (same as main data), tabe S1101.
# Download and read in each file:
tract_areas <- read_csv(unz("<LOCATION OF ZIP FILE>", "DEC_10_SF1_GCTPH1.CY07_with_ann.csv"))
tract_hholds <- read_csv(unz("<LOCATION OF ZIP FILE>", "ACS_17_5YR_S1101_with_ann.csv"))

# Join files and calculate density of each tract:
tract_density <- tract_hholds %>% 
  select(GEO.id, tract_name = `GEO.display-label`, pop = HC01_EST_VC02) %>% 
  left_join(tract_areas %>% select(geo = `GCT_STUB.target-geo-id`, area = SUBHD0303),
            by = c("GEO.id" = "geo")) %>% 
  mutate(density = pop/area)

rm(tract_areas, tract_hholds)

# We have to translate tracts to PUMAs using the 
# MABLE Geocorr engine from the Missouri Census Data Center:
# http://mcdc.missouri.edu/applications/geocorr2014.html
# Download to relationship file as a .csv and read in:

tract_to_puma <- read_csv("geocorr2014.csv", skip = 2, 
                          col_names = c("county", "tract", "state", "puma12", "stab", "cntyname", "PUMAname", "pop10", "afact"))

tract_to_puma <- tract_to_puma %>% 
  mutate(tract_id = paste0(county, substr(tract, 1, 4), substr(tract, 6, 7)),
         puma = paste0(state, puma12))

tract_density <- tract_density %>% 
  mutate(tract_id = substr(GEO.id, 10, 20)) %>% 
  left_join(tract_to_puma %>% select(tract_id, state, puma, PUMAname), by = "tract_id")

# Collapse to PUMA level
# We're weighting the density of each tract by the number of households living there.
# We'll do both a simple mean and also a logged density:
puma_density <- tract_density %>% 
  filter(area > 0,
         pop > 0) %>% 
  group_by(puma, PUMAname) %>% 
  summarize(simple_density = sum(density*pop)/sum(pop),
            logged_density = exp(sum(log(density)*pop)/sum(pop)),
            hholds = sum(pop)) %>% 
  arrange(desc(simple_density))

# Writing this to csv for github upload
write.csv(puma_density, file = "puma_density.csv")