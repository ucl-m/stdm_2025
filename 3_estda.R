# -------------------
# Prerequisites:
#    - Functionality in '1_get_data.R' must be run in order to download and format data

# Functionality:
#
# (2) Exploratory spatio-temporal data analysis
#
# Instructions for use:
# -> Install R packages as necessary
# -> Set working directory
# -------------------

library(ggplot2)
library(gridExtra)

# USER SET VARIABLES:
w_dir <- "C:/Users/Boomerang/OneDrive - University College London/Documents/CEGE0042 - Spatial-Temporal Data Analysis (STDM)/Coursework/final/"

# Set working directory
setwd(w_dir)

# Load data
data = read.csv("gla_collisions_sf.csv")

lapply(data, class)
data$date <- as.Date(data$date, "%Y/%m/%d")
lapply(data, class)

# ******************************* Temporal Autocorrelation *******************************

# **********
# ** Prep **
# **********

# Analysis of collisions per day
daily_count = setNames(aggregate(data[c("accident_severity")],
                        by = list(data$date),
                        FUN = length),c("date", "collision_count"))


p1 <- ggplot(daily_count, aes(x=date, y=collision_count)) + geom_line()
grid.arrange(p1, nrow=1)

acf(daily_count$collision_count, lag.max=1096)


# Analysis of collisions per hour, sample weekly window 1 (term-time)
w_sample_1 = subset(data, date >= "2020-03-30" & date <= "2020-04-05")

w_sample_1$hour = substr(w_sample_1$time,1,2)

w_sample_1_agg = setNames(aggregate(w_sample_1[c("accident_severity")],
                                 by = list(w_sample_1$date, w_sample_1$hour),
                                 FUN = length),c("date", "hour", "collision_count"))

w_sample_1_agg$date_hour = as.integer(paste(substr(w_sample_1_agg$date,1,4),substr(w_sample_1_agg$date,6,7),substr(w_sample_1_agg$date,9,10),w_sample_1_agg$hour, sep=""))

# Write to CSV to manually add missing hour values where no accidents occurred, sort ascending for ACF. TODO: In code add hours
# write.csv(w_sample_1_agg,"./w_sample_1_agg.csv", row.names = FALSE)
w_sample_1_agg = read.csv("w_sample_1_agg.csv")


# Analysis of collisions per hour, sample weekly window 2 (Easter holidays)
w_sample_2 = subset(data, date >= "2020-04-06" & date <= "2020-04-12")

w_sample_2$hour = substr(w_sample_2$time,1,2)

w_sample_2_agg = setNames(aggregate(w_sample_2[c("accident_severity")],
                                    by = list(w_sample_2$date, w_sample_2$hour),
                                    FUN = length),c("date", "hour", "collision_count"))

w_sample_2_agg$date_hour = as.integer(paste(substr(w_sample_2_agg$date,1,4),substr(w_sample_2_agg$date,6,7),substr(w_sample_2_agg$date,9,10),w_sample_2_agg$hour, sep=""))

# Write to CSV to manually add missing hour values where no accidents occurred TODO: In code add hours
# write.csv(w_sample_2_agg,"./w_sample_2_agg.csv", row.names = FALSE)
w_sample_2_agg = read.csv("w_sample_2_agg.csv")


# *****************
# ** Time series **
# *****************
w_sample_1_agg$date_hour_dt = as.POSIXct(as.character(w_sample_1_agg$date_hour), format="%Y%m%d%H")
w_sample_2_agg$date_hour_dt = as.POSIXct(as.character(w_sample_2_agg$date_hour), format="%Y%m%d%H")

p1 <- ggplot(w_sample_1_agg, aes(x=date_hour_dt, y=collision_count)) + ylim(0,12.5) + geom_line()
p2 <- ggplot(w_sample_2_agg, aes(x=date_hour_dt, y=collision_count)) + ylim(0,12.5) + geom_line()
grid.arrange(p1, p2, nrow=1)


# *********
# ** ACF **
# *********
acf(w_sample_1_agg$collision_count, lag.max=168)
acf(w_sample_2_agg$collision_count, lag.max=168)

# **********
# ** PACF **
# **********
pacf(w_sample_1_agg$collision_count, lag.max=168)
pacf(w_sample_2_agg$collision_count, lag.max=168)


# ******************************* Spatial Autocorrelation *******************************
library(spdep)
library(tmap)
# library(ggplot2)
#library(sp)
library(sf)
library(knitr)

sdata = st_as_sf(data, coords=c("X","Y"), crs=27700)

# CEGE0097 Data.zip\Data\Boundaries\London\wards
load("./data/reference/LondonWards")
# load("./data/tutorial/housepricesshp")
m25 = st_read(dsn="./data/processed/m25/", layer="m25")

londonWards = st_transform(LondonWards, crs = 27700)
# st_write(londonWards, "LondonWards.shp")

class(data)
st_crs(londonWards)
st_crs(sdata)

count_collisions <- setNames(aggregate(sdata[c("accident_severity")], londonWards, length), c("collision_count", "geometry"))

count_collisions$ward <- londonWards$NAME

plot(count_collisions)

tmap_mode("view")

tm_shape(count_collisions) + tm_polygons(col="collision_count", palette="YlOrRd", style="jenks", legend.show = TRUE) +
  tm_shape(m25) + tm_lines(col="black", legend.show = TRUE)

# Moran's I
W <- nb2listw(poly2nb(londonWards))
W

kable(listw2mat(W))

cc = setNames(count_collisions$collision_count, count_collisions$ward)

head(cc)

moran(cc, W, n=length(W$neighbours), S0=Szero(W))

moran.test(cc, W) # Test under randomisation

moran.mc(x=cc, listw=W, nsim=9999)

# Local Moran’s I
lm <- localmoran(x=cc, listw=W)
lm

count_collisions$Ii <- lm[,"Ii"]

count_collisions$Iip_unadjusted <- lm[,"Pr(z != E(Ii))"]
count_collisions$Ii_un_sig <- "nonsignificant"
count_collisions$Ii_un_sig[which(count_collisions$Iip_unadjusted < 0.05)] <- "significant"

# p-values (unadjusted)
tm_shape(count_collisions) + tm_polygons(col="Ii_un_sig", palette="-RdBu")

#  statistically significant wards (unadjusted p-value)
tm_shape(count_collisions) + tm_polygons(col="Ii", palette="-RdBu", style="quantile")

# Bonferroni adjustment
count_collisions$Iip_adjusted <- p.adjust(count_collisions$Iip_unadjusted, method="bonferroni")
count_collisions$Ii_ad_sig <- "nonsignificant"
count_collisions$Ii_ad_sig[which(count_collisions$Iip_adjusted < 0.05)] <- "significant"
tm_shape(count_collisions) + tm_polygons(col="Ii_ad_sig", palette="-RdBu")

# Moran Clusters
# moranCluster <- function(shape, W, var, alpha=0.05, p.adjust.method="bonferroni")
moranCluster <- function(shape, W, var, alpha=0.05, p.adjust.method="none")
{
  # Code adapted from https://rpubs.com/Hailstone/346625
  Ii <- localmoran(shape[[var]], W)
  shape$Ii <- Ii[,"Ii"]
  Iip <- p.adjust(Ii[,"Pr(z != E(Ii))"], method=p.adjust.method)
  shape$Iip <- Iip
  shape$sig <- shape$Iip<alpha
  # Scale the data to obtain low and high values
  shape$scaled <- scale(shape[[var]]) # high low values at location i
  shape$lag_scaled <- lag.listw(W, shape$scaled) # high low values at neighbours j
  shape$lag_cat <- factor(ifelse(shape$scaled>0 & shape$lag_scaled>0, "HH",
                                 ifelse(shape$scaled>0 & shape$lag_scaled<0, "HL",
                                        ifelse(shape$scaled<0 & shape$lag_scaled<0, "LL",
                                               ifelse(shape$scaled<0 & shape$lag_scaled<0, "LH", "Equivalent")))))
  shape$sig_cluster <- as.character(shape$lag_cat)
  shape$sig_cluster[!shape$sig] <- "Non-sig"
  shape$sig_cluster <- as.factor(shape$sig_cluster)
  results <- data.frame(Ii=shape$Ii, pvalue=shape$Iip, type=shape$lag_cat, sig=shape$sig_cluster)
  
  return(list(results=results))
}

clusters <- moranCluster(count_collisions, W=W, var="collision_count")$results


# ******************************* Spatial-temporal Autocorrelation *******************************
# Needs some work to do the following, before it can be used:
#   1) Add in zero values for months and places that didn't have an accident
#   2) Transpose into format required for the 'stacf' function from starima_package.R

wards_geo = londonWards[c("NAME","geometry")]
head(wards_geo)

sdata_ward = st_join(
  sdata,
  wards_geo,
  join = st_intersects,
  left = TRUE
)

sdata_agg_st = aggregate(sdata_ward[c("accident_severity")],
                                    by = list(sdata_ward$month, sdata_ward$NAME.y),
                                    FUN = length)


# ******************************* Complete Spatial Randomness *******************************
library(spatstat)
library(ggplot2)
library(knitr)


# Test for Complete Spatial Randomness (CSR)
ppp1 <- as.ppp(st_geometry(sdata))

window(ppp1)

new_win = owin(xrange=c(520000,540000),yrange=c(170000,190000))

Window(ppp1) <- new_win

colQC <- quadratcount(ppp1, nx=100, ny=100)
colQC

# Version 1
plot(ppp1)
axis(1)
axis(2)
plot(colQC, add = TRUE, cex = 2)

n <- ppp1$n
q <- 100 # Number of quadrats
lambda <- n/q # Calculate the intensity as number of points over number of quadrats

chi1 <- sum(((colQC-lambda)^2)/lambda)

pchisq(chi1 , df=99, lower.tail=FALSE)
pchisq

# Version 2 
lambda <- n/q # Calculate the intensity as number of points over number of quadrats
colfreq <- data.frame(k=0:(max(colQC)), frequency=tabulate(colQC+1)) #  Note the +1 is used because tabulate only works on positive integers.
colfreq

probs=sapply(0:60, function(x) {(((lambda*1)^x)/factorial(x)) * exp(-lambda*1)}) # Calculate the PMF for k=0:17
exfreqs <- probs*q # Multiply the probability by the number of quadrats to obtain expected number of quadrats containing each value of k.

colfreq$probs <- probs
colfreq$exfreqs <- exfreqs

kable(colfreq, caption="Observed and expected frequencies, 100*100 quadrat counts, London Collisions data", booktabs=T)

chi2 <- sum((colfreq$frequency-colfreq$exfreqs)^2/colfreq$exfreqs)
chi2

# ******************************* Kernel Density Estimation *******************************
ppp2 <- as.ppp(st_geometry(sdata))
plot(density(ppp2))

# Method #2
install.packages("rspat")
library(rspat)

st_coordinates(sdata)
xy <- st_coordinates(sdata)

mc <- apply(xy, 2, mean)
sd <- sqrt(sum((xy[,1] - mc[1])^2 + (xy[,2] - mc[2])^2) / nrow(xy))


expanse("city")
