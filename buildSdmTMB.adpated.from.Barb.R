################################################################################################
# Build new adult sardine SDM using environmental predictors from the 3km WC15 ROMS
# Vars are SST, surface chl, upper 50m zooplankton biomass, SSB 
# Chl didn't improve AUC, dropped for now
################################################################################################

library(sdmTMB)
library(sp)
library(ggforce) # enables plot_anisotropy
library(pROC)
# remotes::install_github("pbs-assess/sdmTMBextra", dependencies = TRUE)
library(sdmTMBextra) # for adding barrier mesh
library(dplyr)
library(sf)
library(geodata) # To get coastlines
library(ggplot2)
library(visreg)

# Load data
dat <- readRDS("./stephanie/sardineTrainingData.rds")

# Project data
dat2 <- dat
coordinates(dat2) <- c("lon", "lat")
proj4string(dat2) <- CRS("+proj=longlat +datum=WGS84")
datP <- spTransform(dat2, CRS("+proj=utm +zone=10 ellps=WGS84")) 
# Convert back to df
datP <- as.data.frame(datP)
datP$X <- datP$coords.x1 / 1000 # Changes from UTM m to UTM km, based on warning from 1st attempt to make mesh
datP$Y <- datP$coords.x2 / 1000

# Test/train split: 16 years for training, 6 years for out-of-model testing
yrs <- data.frame("yr" = unique(datP$yr))
set.seed(2)
index <- sample(1:nrow(yrs), round(0.75 * nrow(yrs))) 
trainYrs <- yrs[index,]
testYrs <- yrs[-index,]
train <- subset(datP, yr %in% trainYrs) 
test <- subset(datP, yr %in% testYrs) 
table(train$yr) 
table(test$yr)

# Next construct the mesh: see https://pbs-assess.github.io/sdmTMB/articles/basic-intro.html:
# "cutoff is in the units of X and Y (km here), represents minimum distance between knots before a new mesh vertex is added). 
# I tried a bunch of cutoff values, and chose one that 1) had good out-of-sample predictability, and 2) wasn't too "blocky"
# Values between ~ 50 and 200 were fairly reasonable
meshTrain <- make_mesh(train, xy_cols = c("X", "Y"), cutoff = 75)  
# Add a barrier mesh: https://pbs-assess.github.io/sdmTMB/reference/add_barrier_mesh.html
# First use geodata::gadm (replaces raster::getData) to get a better coastline 
us <- gadm(country = c('United States'), level = 0, path = "./caClimate/data") 
mx <- gadm(country = c('Mexico'), level = 0, path = "./caClimate/data")
# Join and trim
usmx <- rbind(us, mx)
bbox <- st_bbox(c(xmin = -130, xmax = -114, ymin = 30, ymax = 49))
usmxCrop <- sf::st_as_sf(usmx) %>% st_crop(bbox)
# plot(usmxCrop)
# Needs to be same projection as mesh
usmxProj <- st_transform(usmxCrop, crs = "+proj=utm +zone=10 ellps=WGS84") 
# plot(usmxProj) # Looks ok?
# Add barrier
mesh2 <- add_barrier_mesh(meshTrain, usmxProj, proj_scaling = 1000)
plot(mesh2)

# Example plot code from ?add_barrier_mesh
mesh_df_water <- mesh2$mesh_sf[mesh2$normal_triangles, ]
mesh_df_land <- mesh2$mesh_sf[mesh2$barrier_triangles, ]
ggplot(usmxProj) +
  geom_sf() +
  geom_sf(data = mesh_df_water, size = 1, colour = "blue") +
  geom_sf(data = mesh_df_land, size = 1, colour = "green")
## Important: cannot use anisotropy with barrier mesh (see warning when run sdmTMB)

#################################################################################################
# Now build the models: I built one with spatiotemporal effects, and one with none
set.seed(1)
fit0 <- sdmTMB(sardPA ~ s(sst) + s(zoo50) + s(ssb, k = 3), 
               data = train, spatial = "off", 
               family = binomial(link = "logit"), anisotropy = FALSE)
set.seed(1)
fit1 <- sdmTMB(sardPA ~ s(sst) + s(zoo50) + s(ssb, k = 3), 
               data = train, mesh = mesh2, time = "mo", spatiotemporal = "ar1", 
               family = binomial(link = "logit"), anisotropy = FALSE)

# Check model skill, fit, and convergence
sanity(fit0) # ln_smooth_sigma warning ~ means that you have presences and absences in the same polygon
sanity(fit1) # (Which I'm ok with in this context: otherwise you have ~ 1 polygon per trawl)

# Predict (gives a whole new df, unlike GAM)
pTrain0 <- predict(fit0, newdata = train, type = "response")
pTest0 <- predict(fit0, newdata = test, type = "response")
pTrain1 <- predict(fit1, newdata = train, type = "response")
pTest1 <- predict(fit1, newdata = test, type = "response")
# Overall AUCs for test and train 
auc(pTrain0$sardPA, pTrain0$est, direction = "<", quiet = TRUE) # 0.7105
auc(pTest0$sardPA, pTest0$est, direction = "<", quiet = TRUE) # 0.6957
auc(pTrain1$sardPA, pTrain1$est, direction = "<", quiet = TRUE) # 0.8189 ST
auc(pTest1$sardPA, pTest1$est, direction = "<", quiet = TRUE) # 0.7349 ST

# Partial plots. Could make nicer later
# SST
sstPartial0 <- visreg(fit0, xvar = "sst", gg = TRUE)
sstPartial0 
sstPartial1 <- visreg(fit1, xvar = "sst", gg = TRUE)
sstPartial1