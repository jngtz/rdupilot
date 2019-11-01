# Functions related to geotagging images from APM log files and RTKLIB pos files.

# Author: Jason Goetz - jason.goetz@uni-jena.de



#----------------------Great-circle distance calculation-----------------------

#' Calculate the great-circle distance
#'
#' \code{geodesic.distance} calculates the great-circle distance between two points
#'
#' @param lon1 Longititude of point 1
#' @param lat1 Latitude ot point 1
#' @param lon2 Longititude of point 2
#' @param lat2 Latitude ot point 2
#' @return Distance in meters
#' @examples
#' # Distance from Toronto to Ottawa in m
#' dist_otttor <- geodesic.distance(-75.695000, 45.424721, -79.347015, 43.651070)
#' # in km
#' dist_ottor / 1000

geodesic.distance <- function(lon1, lat1, lon2, lat2) {
  #Haversine formula
  #https://www.r-bloggers.com/great-circle-distance-calculations-in-r/
  # Convert from degrees to radians
  lat1 <- lat1*pi/180
  lat2 <- lat2*pi/180
  lon1 <- lon1*pi/180
  lon2 <- lon2*pi/180

  R <- 6371 # Earth mean radius [km]
  delta.lon <- (lon2 - lon1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.lon/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d*1000) # Distance in m
}

dist.coords <- function(lat1, lon1, lat2, lon2){
  #http://www.novatel.com/support/known-solutions/finding-the-distance-in-meters-between-2-decimal-degrees-coordinates/
  lat1 <- lat1*pi/180
  lat2 <- lat2*pi/180
  lon1 <- lon1*pi/180
  lon2 <- lon2*pi/180

  equatRadius <- 6378200
  polarRadius <- 6356750

  diffLat <- abs(lat1-lat2)/360 * polarRadius * 2 * pi
  diffLon <- abs(lon1-lon2)/360 * equatRadius * 2 * pi * cos(lat1+lat2/2)
  distance <- sqrt(diffLat^2 - diffLon^2)
  return(distance) # Distance in m
}



#----------------------Convert 24h time to seconds----------------------

#' Convert time in 24h to seconds
#'
#' \code{time.to.seconds} converts time from a 24h to seconds
#'
#' @param x time string (00:00:00.000)
#' @return Time in seconds
#' @examples
#' time.to.seconds("10:44:13.133")

time.to.seconds <- function(x){
  # Converts 24 time (00:00:00.000) to time in seconds
  # options(digits = 15) # print all digits...
  timeString <- x
  timeExtract <- as.numeric(c( substr(timeString , 1,2),  # Hours
                               substr(timeString , 4,5), # Minutes
                               substr(timeString ,  7, 1000000L))) # Seconds
  time_s <- timeExtract[1]*3600 + 60*timeExtract[2] + timeExtract[3]
  return(time_s)
}



#-------------------Import KAP UAV Camera log file-----------------------

#' Reads KAP UAV Exposure Control Script Logged Data
#'
#' \code{read.kap.log} imports the logged camera data from the KAP UAV Exposure
#'  Control Script.
#'
#' @param file a KAP.log file
#' @return A dataframe containing the date, time, image name, shutter speed (Tv),
#'  aperature setting (Av), ISO sensitivity (Sv), and time convered to seconds.


read.kap.log <- function(file){

  # Imports logged camera data from the KAP UAV Exposure Control Script.
  # Only records where the camera was triggered are imported

  # http://chdk.wikia.com/wiki/KAP_UAV_Exposure_Control_Script

  # Args:
  #   file: a KAP.log file

  # Returns:
  #   A dataframe containing the date, time, image name, shutter speed (Tv),
  #   aperature setting (Av), ISO sensitivity (Sv), and time convered to seconds

  d <- read.table(file, header = FALSE,
                  sep= " ",
                  blank.lines.skip = FALSE,
                  stringsAsFactors = FALSE)

  # Extract the rows corresponding to the triggered camera time
  d.triggered <- d[which(d$V3 != "" & d$V3 != "NDF:NDout"),]
  # Extract rows related to camera settings
  d.settings <- d[which(d$V3 != "" & d$V3 != "NDF:NDout") + 2,]

  # Build dataframe with triggering info and actual Tv, Av and Sv settings
  kap.log <- data.frame(  date = d.triggered$V1,
                          time = d.triggered$V2,
                          img.sequence = as.integer(gsub(")", "", d.triggered$V3)),
                          img.name = d.triggered$V4 ,
                          Tv = d.settings$V8,
                          Av = d.settings$V9,
                          Sv = d.settings$V10,
                          stringsAsFactors = FALSE)
  # Calculate time in seconds
  kap.log$time_s <- unlist(lapply(kap.log$time, time.to.seconds))

  kap.log$date <- as.Date(kap.log$date, format = "%Y%B%d")

  # Create a record with a POSIXct date and time class  <- add to read.KAPlog
  dt <- paste(kap.log$date, kap.log$time, sep = " ")
  kap.log$POSIXct <- as.POSIXct(dt,  tz = 'UTC')


  ## Maybe should change this to calculate time in seconds from the POSIXct to be consistent
  # with the GPS converstion that happens also....

  return(kap.log)
}


#' Select Sequence of Photos from Kap.log
#'
#' \code{select.photos} selects a sequence of photos from the imported KAP.log
#'
#' @param kapLog the dataframe of the importated KAP.log file
#' @param start the name (image.name) of the first file in a sequence
#' @param end the name (image.name) of the last file in a sequence
#' @return A dataframe containing the KAP log info for a sequence of photos

select.photos <- function(kapLog, start, end){
  # Select a sequence of photos from the KAP log uploaded in R

  # Args:
  #   kapLog: the dataframe of the importated KAP.log file
  #   start:  the name (image.name) of the first file in a sequence
  #   end:    the name (image.name) of the last file in a sequence

  # Returns:
  #   A dataframe containing the KAP log info for a sequence of photos

  row.start <- which(kapLog$img.name == start)[1]
  row.end <- which(kapLog$img.name == end)[1]
  photos.df <- kapLog[row.start:row.end,]
  return(photos.df)
}


#------------------------Some stats stuff------------------------

getmode <- function(v) {
  # Calculate the mode value
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



#------------------------Import RTKLIB pos file ------------------

#' Imports a processed RTKLIB file
#'
#' \code{read.rtklib.pos} imports a processed RTKLIB file http://www.rtklib.com
#'
#' @param file an RTKLIB.pos file
#' @param skip how many lines to skip header info - should probably refine this
#' @return A dataframe with GNSS positional information from the RTKLIB pos file
#'

read.rtklib.pos <- function(file, skip = 26){

  # the skip of lines is not always consistent, should re-work this...

  df <- read.table(file, header = FALSE,
                   #file should record time in WW GPSS
                   skip = skip, #no. lines to skip - can vary
                   sep= "",
                   blank.lines.skip = FALSE,
                   stringsAsFactors = FALSE)

  names(df) <- c(	 "GWk", "GMS", "latitude_d", "longitude_d", "height_m", "Q", "ns",
                   "sdn_m", "sde_m", "sdu_m", "sdne_m", "sdeu_m", "sdun_m",
                   "age_s", "ratio")
  df$GMS <- df$GMS * 1000
  return(df)
}


#-----------Geotagging KAP log from APM LOG file----------------

geotag.images <- function(apm_log, gps_msg="GPS", kap_log, shutterlag=0,
                          gps_offset=0, ref_height = NA, leap_s = 0){

  kap <- kap_log
  shutterlag_s <- shutterlag
  offset_s <- gps_offset
  refHeight <- ref_height

  gps <- apm_log[[gps_msg]]
  gps$timeUTC <- gps.time.to.utc(gps$GWk[1], gps$GMS, hrsDiff = 2)

  if(is.na(ref_height)){
    gps$adjHeight_m <- NA
  } else {
    gps$adjHeight_m <- refHeight + gps$RAlt
  }

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

  return(kap)
}








