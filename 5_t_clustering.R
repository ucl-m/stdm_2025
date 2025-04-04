# -------------------
# Prerequisites:
#    - Functionality in '1_get_data.R' must be run in order to download and format data

# Functionality:
#
# (1) Time series clustering based on paper https://discovery.ucl.ac.uk/id/eprint/10190951/1/submission_85.pdf (accessed 28/03/2025)
#    - Uses '3.clustering.ipynb' for clustering 
#
# Instructions for use:
# -> Install R packages as necessary
# -> Set working directory
# -> Set conda env path
# -------------------

# Load packages (install if necessary) 
# install.packages("sf")
# install.packages("reticulate")
library("sf")
library(reticulate)

# USER SET VARIABLES:
w_dir <- "C:/Users/Boomerang/OneDrive - University College London/Documents/CEGE0042 - Spatial-Temporal Data Analysis (STDM)/Coursework/final/"
conda_env <- "c:/Users/Boomerang/AppData/Local/ESRI/conda/envs/p312" # can be obtained by running 'conda env list' at the command prompt)

# Set working directory
setwd(w_dir)

# Load data (created by 1_get_data.R)
data = read.csv("gla_collisions_sf.csv")

# Convert to simple feature data frame
sdata = st_as_sf(data, coords=c("X","Y"), crs=27700)

# For these experiments, we only need the date, time and geometry
sdata <- subset(sdata, select = c("date", "time") )
sdata$date <- as.Date(sdata$date, "%Y/%m/%d")

# --------------------------------------
# 1) Time series clustering: R for data eng. / export to Python for clustering
# --------------------------------------

# *** FROM PAPER ***
# 3.1 Footfall calculation
# Stay detection: We identify stays where individuals remain stationary for more than a specified
# duration (in this study, we set the threshold at 5 minutes). (NOT REQUIRED)
# *** FROM PAPER ***

# *** FROM PAPER ***
# Hourly footfall calculation: 
# Stays are joined to high street boundaries 
# *** FROM PAPER ***
# (high street boundaries equivalent = boroughs, wards too small)
boroughs <- read_sf(dsn = "./data/reference/borough_boundary", layer = "Borough_boundary")

st_crs(boroughs)

# Assign a borough to each point
sdata_borough = st_join(
  sdata,
  boroughs,
  join = st_intersects,
  left = TRUE
)

str(sdata_borough)

# Split data into 3 months for comparison 
feb_2020 = subset(sdata_borough, date >= "2020-02-01" & date <= "2020-02-29") # NOTE: Leap year!
feb_2020 # 1858

feb_2021 = subset(sdata_borough, date >= "2021-02-01" & date <= "2021-02-28")
feb_2021 # 1184

feb_2022 = subset(sdata_borough, date >= "2022-02-01" & date <= "2022-02-28")
feb_2022 # 1664

# *** FROM PAPER ***
# ...and footfall is calculated on an hourly basis. 
# *** FROM PAPER ***

# Aggregate by hour
feb_2020$hour <- substr(feb_2020$time,1,2)
feb_2021$hour <- substr(feb_2021$time,1,2)
feb_2022$hour <- substr(feb_2022$time,1,2)

feb_2020_hour <- aggregate(feb_2020[c("time")],
                           by = list(feb_2020$POLYGON_ID, feb_2020$hour),
                           FUN = length) # 630 obs.

feb_2021_hour <- aggregate(feb_2021[c("time")],
                           by = list(feb_2021$POLYGON_ID, feb_2021$hour),
                           FUN = length) # 492 obs.

feb_2022_hour <- aggregate(feb_2022[c("time")],
                           by = list(feb_2022$POLYGON_ID, feb_2022$hour),
                           FUN = length) # 588 obs.


feb_2020_hour <- setNames(feb_2020_hour, c("poly_id", "hour", "count", "geometry"))
feb_2021_hour <- setNames(feb_2021_hour, c("poly_id", "hour", "count", "geometry"))
feb_2022_hour <- setNames(feb_2022_hour, c("poly_id", "hour", "count", "geometry"))

# Geometry no longer needed (for time series clustering)
feb_2020_hour_df <- st_drop_geometry(feb_2020_hour)
feb_2021_hour_df <- st_drop_geometry(feb_2021_hour)
feb_2022_hour_df <- st_drop_geometry(feb_2022_hour)

class(feb_2020_hour_df)
str(feb_2020_hour_df)

# The one-month hourly footfall is then averaged and aggregated into 24 hours (of a day)
days_feb_2020 = 29
days_feb_2021 = 28
days_feb_2022 = 28

feb_2020_hour_df$mean_count <- feb_2020_hour_df$count / days_feb_2020
feb_2021_hour_df$mean_count <- feb_2021_hour_df$count / days_feb_2021
feb_2022_hour_df$mean_count <- feb_2022_hour_df$count / days_feb_2022

# Add missing hour values in Python (where no accidents occurred).
#   NOTE: this calls out to python library utils.py
#   -> TODO: Rewrite Python script in R
#
# ** Backup method ** -> if calling Python from R fails, export to CSV and run through 'annex_a.ipynb' to add missing zero collision hours
#   write.csv(feb_2020_hour_df, "./feb_2020_hour_df.csv", row.names = FALSE)
#   feb_2020_complete = read.csv("feb_2020_complete.csv")

# Reference conda env
use_condaenv(conda_env)

# Used to reload the Python module (if modified after importing here)
# importlib <- import("importlib")
# importlib$reload(utils)

# Import Python module with functionality
utils <- import("utils")

# Get zero accident hours
feb_2020_missing = utils$get_missing_hours(feb_2020_hour_df)
feb_2021_missing = utils$get_missing_hours(feb_2021_hour_df)
feb_2022_missing = utils$get_missing_hours(feb_2022_hour_df)

# Combine to form complete time series for Feb
c_feb_2020 <- rbind(feb_2020_hour_df, feb_2020_missing) # CHECKSUM: 33 (boroughs) x 24 (hours) = 792 records
c_feb_2021 <- rbind(feb_2021_hour_df, feb_2021_missing) # CHECKSUM: 33 (boroughs) x 24 (hours) = 792 records
c_feb_2022 <- rbind(feb_2022_hour_df, feb_2022_missing) # CHECKSUM: 33 (boroughs) x 24 (hours) = 792 records

# Save to CSV for clustering in Python notebook
write.csv(c_feb_2020, "./c_feb_2020.csv")
write.csv(c_feb_2021, "./c_feb_2021.csv")
write.csv(c_feb_2022, "./c_feb_2022.csv")

# ********** Continued in 5_t_clustering.ipynb **********
