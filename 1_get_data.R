# -------------------
# Functionality:
#
# 1) Download data using the stat19 package
#
# Instructions for use:
# -> Install R packages as necessary
# -> Set working directory
#
# Code referenced from:
#  - https://cran.r-project.org/web/packages/stats19/vignettes/stats19.html (accessed 23/03/2025)
#  - https://cran.r-project.org/web/packages/stats19/stats19.pdf (accessed 23/03/2025)
# -------------------

# Load packages (install if necessary) 
# install.packages("stats19")
# install.packages("sf")
library(stats19)
library(sf)

# Set working directory
dir <- "C:/Users/Boomerang/OneDrive - University College London/Documents/CEGE0042 - Spatial-Temporal Data Analysis (STDM)/Coursework/final/"
setwd(dir)

# --------------------------------------
# 1) Download data 
# --------------------------------------

# download 2020->2021 data
collisions_2020 = get_stats19(year = 2020, type = "collision")
collisions_2021 = get_stats19(year = 2021, type = "collision")
collisions_2022 = get_stats19(year = 2022, type = "collision")

nrow(collisions_2020) # 91199
nrow(collisions_2021) # 101087
nrow(collisions_2022) # 106004

# join datasets
collisions = rbind(collisions_2020, collisions_2021, collisions_2022)
nrow(collisions) # 298290
names(collisions)
# write.csv(collisions,"./collisions.csv", row.names = TRUE)

# Convert STATS19 data into spatial (Sf) object
collisions_sf = format_sf(collisions)

# Load Boundary file for London (Greater London Authority)
# https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london (accessed 22/03/2025)
gla <- read_sf(dsn = "./data/reference/gla_boundary/", layer = "London_GLA_Boundary")
plot(gla) # 67653 obs.

# Clip the collision data to within the GLA polygon
gla_collisions_sf = st_intersection(collisions_sf,gla)

# Plot to visually check that the shape of the points is that of the GLA
plot(gla_collisions_sf[c("number_of_vehicles")])

# Create decimal time (used for ESTDA)
gla_collisions_sf$hour_decimal = sapply(strsplit(gla_collisions_sf$time,":"),
                                        function(x) {
                                          x <- as.numeric(x)
                                          (x[1] + (x[2]/60))
                                        })

# extract the month from the 'date' column
gla_collisions_sf$month <- format(as.Date(gla_collisions_sf$date, format="%d/%m/%Y"),"%Y-%m")
gla_collisions_sf[c("date", "month")]

# Save to CSV
st_write(gla_collisions_sf, "gla_collisions_sf.csv", layer_options = "GEOMETRY=AS_XY")
