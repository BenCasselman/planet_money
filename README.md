# Planet Money "Modal American"
Data and code behind Planet Money "Modal American" [episode](https://www.npr.org/2019/08/28/755191639/episode-936-the-modal-american), which aired Aug. 28, 2019.
Analysis by Ben Casselman, Kenny Malone, Liza Yeager and Darian Woods.
Comments/questions/corrections to [Ben Casselman](mailto:ben.casselman@nytimes.com).

Data: American Community Survey, five-year public-use microdata (2013-17), via [IPUMS USA](www.ipums.org), University of Minnesota.

There are four files in this repo:
1. `final_analysis.R` contains the main code for parsing the data and replicating the analysis used on the episode.
2. `density_analysis.R` contains supplementary code for calculating the tract-weighted population densities of each Public Use Microdata Area (PUMA).
3. `puma_density.csv` contains the output of `density_analysis.R` for those who don't want to replicate that section of the analysis. The records are ranked in order of descending (logged) densities.
4. `final_results.csv` contains the results of the analysis, as used in the episode. (This lists all 3,433 buckets, ranked by descending order of the population in each bucket.)