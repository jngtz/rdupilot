# This script explores how to geotag with image offsets using
# an ardupilot log and a CHDK KAP log

#------------------------Import ArduPilot Log------------------
# Exported from Mission Planner
setwd("data-raw")

plane.log <- read.apm.log("arduplane_example.log")

#------------------------Prepare CHDK KAP Log------------------

# Import CHDK KAP log file
kap <- read.kap.log("KAP.log")

# Determine the range of  photos in the KAP log to be geo-located
good.start <- "IMG_2084.JPG"
good.end <- "IMG_2255.JPG"

# Apply selection
kap <- kap[which(kap$img.name == good.start):which(kap$img.name == good.end),]

# I'm not sure why there is approx. 0.01 diff
# in time during conversion
# It should be OK, our observed GSM UTC time from
# a phone app is only to the 100th of a millisecond..maybe not ok..


#-----------Geotagging KAP log from APM LOG file----------------

# Create a field that adjust the time with the offset between
# the GPS time and the KAP time - we calculated this from taking
# picture of an accurate UTC clock and comparing to KAP log.

shutterlag_s <- 0.64 # for Canon s110 (full autofocus) !Important for accurate georef

offset_s <- 3667.262 - shutterlag_s
leap_s <- 0 # Default 17 - GPS leap seconds - not sure if we need...

#Select which GPS message data from the APM to use GPS (default) or GPS2
gps.msg <- "GPS"

# Calculate reference height above datum
#   This is where velocity is low...
#   Can also just use the Leica GNSS obs for the start position if
#   recorded in the field. OR extract this value from another high
#   resolution digital elevation model.

refHeight <- 357.6527 #WGS 84 <- measured  with Leica GNSS at target were Plane was armed

# Create an adjusted height above the datum by add position where
# the plane was armed + the relative height estimation (baro + acc)


test <- geotag.images(plane.log, kap_log = kap, shutterlag = 0.64,
                      gps_offset = 3667.262, ref_height = 357.6527)

gps <- plane.log[[gps.msg]]
gps$timeUTC <- gps.time.to.utc(gps$GWk[1], gps$GMS, hrsDiff = 2)

gps$adjHeight_m <- refHeight + gps$RAlt

#Subtract offset
gps$adjPOSIXct <- gps$timeUTC  - offset_s - leap_s

strip.time <- strftime(gps$adjPOSIXct, format="%H:%M:%OS3", tz = "UTC")
gps$adjTime_s <- unlist(lapply(strip.time, time.to.seconds))

# For error estimates........................
# Calculate altitude difference (m)
gps$altDiffNeg_m <- NA #if tag diff positive
gps$altDiffPos_m <- NA #if tag dff negative
gps$negSpdDiff_ms <- NA
gps$posSpdDiff_ms <- NA

for(i in 2:nrow(gps)){
  gps$altDiffNeg_m[i] <- gps$RAlt[i] - gps$RAlt[i-1]
  gps$negSpdDiff_ms[i] <- gps$Spd[i] - gps$Spd[i-1]
}

for(i in 1:nrow(gps)-1){
  gps$altDiffPos_m[i] <- gps$RAlt[i+1] - gps$RAlt[i]
  gps$posSpdDiff_ms[i] <- gps$Spd[i+1] - gps$Spd[i]
}

# Calculate change in alt/second - currently at 5Hz (0.2s)
# T = 1/f, T = 1/5Hz T = 0.2s e.g... T = period

gps_period <- round(gps$adjTime_s[2] - gps$adjTime_s[1], digits = 2)

gps$deltaAltPos <- gps$altDiffPos_m / gps_period
gps$deltaAltNeg <- gps$altDiffNeg_m / gps_period

# Calculate change in speed - currently at 5Hz (0.2s)
gps$deltaSpdPos <- gps$posSpdDiff_ms / gps_period
gps$deltaSpdNeg <- gps$negSpdDiff_ms / gps_period

# ...........................................

# Load attitude log
att <- plane.log[['ATT']]

gps$yaw <- NA
gps$pitch <- NA
gps$roll <- NA
gps$attTimeDiff_s <- NA
gps$attTimeUS <-NA

for(i in 1:nrow(gps)){
  mtchTime <- which.min(abs(att$TimeUS - gps$TimeUS[i])) #matched record

  gps$yaw[i] <- att$Yaw[mtchTime]
  gps$pitch[i] <- att$Pitch[mtchTime]
  gps$roll[i] <- att$Roll[mtchTime]
  gps$attTimeUS[i] <- att$TimeUS[mtchTime]

  # Difference in time (s) between GPS TimeUS and ATT matched TimeUS
  gps$attTimeDiff_s[i] <- (att$TimeUS[mtchTime] - gps$TimeUS[i])/10^6 #From microseconds
}

kap$gpsTimeUS <- NA #TimeUS is in microseconds (10^6 seconds)
kap$latitude <- NA
kap$longitude <- NA
kap$gpsHeight_m <- NA
kap$distImage_m <- NA
kap$tagDiff_s <- NA
kap$yaw <- NA
kap$pitch <- NA
kap$roll <- NA
kap$adjHeight_m <- NA
kap$gpsGndSpd_ms <- NA
kap$gpsGndCrs <- NA
kap$deltaAltPos <- NA
kap$deltaAltNeg <- NA
kap$deltaSpdPos <- NA
kap$deltaSpdNeg <- NA
kap$gpsRAlt_m <- NA

for(i in 1:nrow(kap)){
  mtch <- which.min(abs(gps$adjTime_s - kap$time_s[i])) # matched record

  kap$gpsTimeUS[i] <- gps$TimeUS[mtch]
  kap$latitude[i] <- gps$Lat[mtch]
  kap$longitude[i] <- gps$Lng[mtch]
  kap$gpsGndCrs[i] <- gps$GCrs[mtch]

  kap$yaw[i] <- gps$yaw[mtch]
  kap$pitch[i] <- gps$pitch[mtch]
  kap$roll[i] <- gps$roll[mtch]

  kap$gpsGndSpd_ms[i] <- gps$Spd[mtch]

  kap$deltaSpdPos[i] <- gps$deltaSpdPos[mtch]
  kap$deltaSpdNeg[i] <- gps$deltaSpdNeg[mtch]

  kap$deltaAltPos[i] <- gps$deltaAltPos[mtch]
  kap$deltaAltNeg[i] <- gps$deltaAltNeg[mtch]

  # Difference in time (s) between adjusted GPS time and the KAP time
  kap$tagDiff_s[i] <- gps$adjTime_s[mtch] - kap$time_s[i]

  # GPS observed height above datum (m)
  kap$gpsHeight_m[i] <- gps$Alt[mtch]
  kap$gpsRAlt_m[i] <- gps$RAlt[mtch]

  # Adjusted Height using a reference height from GPS
  kap$adjHeight_m[i] <- gps$adjHeight_m[mtch]

  # Distance (m) from previous photo position
  if( i == 1 ){
    kap$distImage_m = 0
  } else {
    kap$distImage_m[i] <- geodesic.distance(kap$longitude[i-1], kap$latitude[i-1],
                                            kap$longitude[i], kap$latitude[i])
  }
}

kap$estTimeUS <- kap$gpsTimeUS - (kap$tagDiff_s * 10^6)

kap$estPitch <- NA
kap$estYaw <- NA
kap$estRoll <- NA

for(i in 1:nrow(kap)){
  mtch <- which.min(abs(att$TimeUS - kap$estTimeUS[i])) #matched record

  kap$estPitch[i] <- att$Pitch[mtch]
  kap$estYaw[i] <- att$Yaw[mtch]
  kap$estRoll[i] <- att$Roll[mtch]
}



#------------------------Positional difference est. from taging-----------------

# Max. distoff error would be
# max(kap$gpsGndSpd_ms) * 0.1 #b/c GPS records at 5Hz (0.2s)

# Estimate the distance the position of the camera may be off
kap$tagDiff_m <- kap$gpsGndSpd_ms * kap$tagDiff_s
hist(kap$tagDiff_m )

# Error in the vector compents x,y using GPS Ground Course for angle
# Resolving 3D vector in Cartesian components

#Vh <- kap$gpsGndSpd_ms # horizontal speed of the the aircraft

#Vx <- Vh*sin((kap$gpsGndCrs)*pi/180)
#Vy <- Vh*cos((kap$gpsGndCrs)*pi/180)


#Vz <- Vh*tan(kap$pitch*pi/180)

kap$xTagDiff_m <- NA # Vx * kap$tagDiff_s
kap$yTagDiff_m <- NA # Vy * kap$tagDiff_s
kap$zTagDiff_m <- NA # kap$deltaAlt * kap$tagDiff_s

Vh <- NA

for(i in 1:nrow(kap)){

  if (kap$tagDiff_s[i] > 0){
    kap$zTagDiff_m[i] <- kap$deltaAltPos[i] * kap$tagDiff_s[i]

    Vh <- kap$gpsGndSpd_ms[i] + kap$deltaSpdPos[i] * kap$tagDiff_s[i]

    kap$xTagDiff_m[i] <- Vh*sin((kap$gpsGndCrs[i])*pi/180) * kap$tagDiff_s[i]
    kap$yTagDiff_m[i] <- Vh*cos((kap$gpsGndCrs[i])*pi/180) * kap$tagDiff_s[i]

  } else if (kap$tagDiff_s[i] < 0){
    kap$zTagDiff_m[i] <- kap$deltaAltNeg[i] * kap$tagDiff_s[i]

    Vh <- kap$gpsGndSpd_ms[i] + kap$deltaSpdNeg[i] * kap$tagDiff_s[i]

    kap$xTagDiff_m[i] <- Vh*sin((kap$gpsGndCrs[i])*pi/180) * kap$tagDiff_s[i]
    kap$yTagDiff_m[i] <- Vh*cos((kap$gpsGndCrs[i])*pi/180) * kap$tagDiff_s[i]

  } else {
    kap$zTagDiff_m[i] <- 0
    kap$xTagDiff_m[i] <- 0
    kap$yTagDiff_m[i] <- 0
  }

  #print(paste(Vh, "-", kap$gpsGndSpd_ms[i]))

}


#------------------------Estimate image position using timing difference-----------------

# Create a SpatialPointsDataFrame object so we can transform to UTM.
# The UTM projection will allow us to attempt to correct the position
# by the estimated xyz errors calculated.

library(sp)
library(rgdal)

xy <- kap[c("longitude", "latitude")]
coordinates(xy) <- c("longitude", "latitude")
proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example

# Transform coordinates to utm 32N
res <- spTransform(xy, CRS("+proj=utm +zone=32 +ellps=WGS84 +units=m +no_defs"))
newCoord <- coordinates(res)

kap$Easting <- newCoord[,1]
kap$Northing <- newCoord[,2]

# Apply the estimated corrections
kap$adjEasting <- kap$Easting - kap$xTagDiff_m # subtracting b/c diffTime = gpstime - kaptime
kap$adjNorthing <- kap$Northing - kap$yTagDiff_m

# Transform coordinates back to WGS84
en <- kap[c("adjEasting","adjNorthing")]
coordinates(en) <- c("adjEasting", "adjNorthing")
proj4string(en) <- CRS("+proj=utm +zone=32 +ellps=WGS84 +units=m +no_defs")

trfm <- spTransform(en, CRS("+proj=longlat +datum=WGS84"))
newWGS84 <- coordinates(trfm)

kap$estLongitude <- newWGS84[,1]
kap$estLatitude <- newWGS84[,2]
kap$estGPSheight_m <- kap$gpsHeight_m + kap$zTagDiff_m
kap$estAdjHeight_m <- kap$adjHeight_m  + kap$zTagDiff_m


# Note: These corrections (adjustments) deal only with
#       the time difference between GPS measurements
#       corresonding to the GPStime a photo was matched to.
#       It does not correct for any error related to the
#       original time offset between the time a photo is
#       taken and the GPS time.


# Compare performance to positions determined from SFM ---------------------------
## Read PhotoScan positions
sfmpos <- read.csv("camera_pos_photoscan.txt", header=TRUE, skip = 1 )
sfmpos <- sfmpos[!is.na(sfmpos$Y.North),]
sfmpos <- sfmpos[!is.na(sfmpos$X_est),]
#xy.sfm <- sfmpos[c("X.East", "Y.North")]
xy.sfm <- sfmpos[c("X_est", "Y_est")]

#need to match to same number images used... to calc diff.
sfm.res <- read.csv("camera_pos_photoscan.txt", header=TRUE, skip = 1 )
sfm.res <- sfm.res[!is.na(sfm.res$Y.North),]

# to utm coord

sfm.xy <- sfm.res[c("X_est", "Y_est")]
sfm.xy <- sfm.xy[!is.na(sfm.xy$X_est),]
coordinates(sfm.xy) <- c("X_est", "Y_est")
proj4string(sfm.xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example

# Transform coordinates to utm 32N
sfm.tx <- spTransform(sfm.xy, CRS("+proj=utm +zone=32 +ellps=WGS84 +units=m +no_defs"))
sfm.utm <- as.data.frame(coordinates(sfm.tx))

sfm.tmp <- sfm.res[!is.na(sfm.res$Y_est),]
kap.flt <- kap[!is.na(sfm.res$X_est),]

georef.errs <- data.frame(
  x_err = kap.flt$Easting - sfm.utm$X_est,
  y_err = kap.flt$Northin - sfm.utm$Y_est,
  z_err = kap.flt$gpsHeight_m - sfm.tmp$Z_est,
  xy_err = sqrt((kap.flt$Easting - sfm.utm$X_est)^2 + (kap.flt$Northin - sfm.utm$Y_est)^2),
   ## Need to transform to UTM project to determine in m...
  xadj_err = kap.flt$adjEasting - sfm.utm$X_est,
  yadj_err = kap.flt$adjNorthing - sfm.utm$Y_est,
  zadj_err = kap.flt$adjHeight_m - sfm.tmp$Z_est,
  xyadj_err = sqrt((kap.flt$adjEasting - sfm.utm$X_est)^2 + (kap.flt$adjNorthing - sfm.utm$Y_est)^2)
)

bxplt.err <- rbind(data.frame(error_m = georef.errs$xy_err, method = "nearest"),
                   data.frame(error_m = georef.errs$xyadj_err, method = "adjusted"))
summary(round(georef.errs, digits = 2))

boxplot(error_m ~ method, data = bxplt.err, main = "xy error")

hist(round(georef.errs$zadj_err, digits = 2))

#------------------------Plot results-----------------
library(leaflet)

# a character of the time diff for a popup msg
kap$tagOff <- as.character(round(kap$tagDiff_s, digits = 3))


map <- leaflet(kap) %>%
  # Base groups
  #addTiles(group = "OSM (default)") %>%
  addProviderTiles("Esri.WorldImagery") %>%
  # Overlay groups
  addCircles(data = gps, lng = ~Lng, lat = ~Lat,
             color = "yellow", group = "GNSS", radius = 0.3) %>%
  addCircles(~longitude, ~latitude, color = "blue",
             group = "Nearest", radius = 0.7,
             popup = ~img.name) %>%
  addCircles(data = kap, lng = ~estLongitude, lat = ~estLatitude,
             color = "red", group = "Estimated", radius = 0.7,
             popup = ~tagOff) %>%
  addCircles(data = sfmpos, lng = ~X_est, lat = ~Y_est,
             color = "green", group = "Estimated", radius = 0.7,
             popup = ~X.Label) %>%

  # Layers control
  addLayersControl(
    #baseGroups = c("OSM (default)", "ESRI Imagery"),
    overlayGroups = c("Nearest", "Estimated", "SFM", "GNSS"),
    options = layersControlOptions(collapsed = FALSE)
  )
map


#------------------------Output Table with Geotag Info-----------------
# Write a csv file

flds <- c("date", # Date image was taken (according to Camera)
            "time", # Time image was taken (according to Camera)
            "img.name", # Image file name
            "Tv", "Av", "Sv", # Photo shutter speed, aperature setting, and ISO sensitivy
            "longitude", "latitude", "gpsHeight_m", # Nearest GNSS position to image time
            "gpsRAlt_m", # Nearest relative altitude measure from IMU (baro + acc)
            "gpsGndSpd_ms", "gpsGndCrs", # Nearest GNSS grond speed and ground course
            "yaw", "pitch", "roll", # Relative to nearest GNSS record - !Could also correct with tagging difference
            "adjHeight_m", # Relative Alt (baro + acc) adjusted to a survey height of where aircraft is armed
            "tagDiff_s", # Time difference between nearest GNSS position and when image was taken
            "tagDiff_m", # Distance difference between nearest GNSS position and when image was taken
            "estLongitude", "estLatitude", # Estimated corrections of image GNSS horizontal positions
            "estGPSheight_m", "estAdjHeight_m", # Estimated corrections of image GNSS vertical positions
            "xTagDiff_m", "yTagDiff_m", "zTagDiff_m") # Est. positional error btw nearest GNSS record and image time

export <- kap[, flds]
flnm <- paste(gps.msg, "_", kap$date[1], "_Rgeotag.csv", sep = "")

write.csv(export, file = flnm, quote = FALSE, row.names = FALSE)
