# Import ArduPilot (APM) DataFlash log

# Author: Jason Goetz - jason.goetz@uni-jena.de

# Required Data: An APM DataFlash log file converted from .bin to .log
# Can be converted/exported using either Mission Planner or APM Planner
# ground control software.

# See http://ardupilot.org/copter/docs/common-downloading-and-analyzing-data-logs-in-mission-planner.html

# Other useful links
# Message layout
# https://github.com/ArduPilot/ardupilot/blob/master/libraries/DataFlash/LogStructure.h
# Message inputs
# https://github.com/ArduPilot/ardupilot/blob/master/libraries/DataFlash/LogFile.cpp



#------------------- Roughly define varible classes----------------

#' Define class
#'
#' Determines if vectors is numeric or integer to minimize memory
#'
#' @param x A vector containing a message parameter
#' @return The vector with the class defined as either integer or numeric
#' @examples
#' x <- c(1,2,4,5)
#' str(x)
#' x <- class.define(x)
#' str(x)

class.define <- function(x){

  # Assigns a class to APM message parameters to reduce file size
  #  e.g. for the GPS message, the GWK (GPS weeks) parameter
  #  will be assigned as an integer

  if (grepl(".",  x[1] , fixed = TRUE)) {
		x <- as.numeric(x)

		# Note all digits are there, they are just only printed on screen
		# to r default - e.g. options(digits=9)

	} else if (grepl("+[[:digit:]]+", x[1])) {
		x <- as.integer(x)
	}
	return(x)
}

#' Apply define class to dataframes in a list

#'
#' @param x A list of dataframes
#' @return The list of dataframes with defined integer/numeric classes

class.define.list <- function(x){

  # Enables the class.define() to be applied to a list of dataframes

	for(MSG in names(x)[-which(names(x) == 'FMT')]){
		for(VAR in names(x[[MSG]])[!is.na(names(x[[MSG]]))]){ # To ignore NA headings
			x[[MSG]][VAR] <- class.define(x[[MSG]][,VAR])
		}
	}
	return(x)
}



#------------------- Convert GPS time in milliseconds to UTC------------

#' Convert GPS time to UTC time
#'
#' \code{GPStimeToUTC} converts GPS time in weeks and milliseconds to UTC time
#'
#' @param GPSweek GPS week number - ardupilot log GPS message parameter (GWk)
#' @param GPSms GPS time in milliseconds - ardupilot log GPS message parameter (GMS) -
#' @param hrsDiff May set time difference in hours to UTC time of your location
#' @param leapsec Time correction between GPS and UTC time in seconds
#' @return Time in UTC time unless included hours difference argument
#' @examples
#' gps_week <- 1923
#' gps_seconds <- 381765
#' gps_milliseconds <- gps_seconds * 1000
#' utc_time <- GPStimeToUTC(gps_week, gps_milliseconds)
#' utc_time

# Add option to make subtracting leap seconds optional...
gps.time.to.utc <- function(GPSweek, GPSms, hrsDiff = 0, leapsec = 17){

  gps.epoch <- as.POSIXct("1980-01-06 00:00:00.000",  tz = 'UTC')
	timestamp <- gps.epoch + GPSweek *7*24*60*60 + GPSms/1000 - leapsec

	return(timestamp + hrsDiff*60*60)

	# Note: timestamp for GPS seems to be off by 0.02 seconds and
	# GPS2 0.001 seconds compared to what is seen in Mission Planner

}



#------------------- Import APM log file-------------------------

#' Import ardupilot dataflash log
#'
#' @param file an ardupilot dataflash log (.log)
#' @return The ardupilot messages are organized into a list of dataframes
#'   containing columns for the message parameters
#'

read.apm.log <- function(file){

  # Imports an APM dataflash log (.log)

  # Args:
  #   file: an APM dataflash .log file

  # Returns:
  #   The APM messages are organized into a list of dataframes
  #   containing columns for the message parameters

	d <- read.table(file, header = FALSE, sep = ",",
					blank.lines.skip = FALSE,
					stringsAsFactors = FALSE,
					strip.white = TRUE)

	# Create a factor class for column 1 for message types

	d$V1<- as.factor(d$V1)

	# Extract column names 'FMT'

	fmt <- d[d$V1== 'FMT',]
	fmt$V4 <- as.factor(fmt$V4)

	# Fill each message list slot with a dataframe of associated parameters

	d.l <- list()

	for(MSG.TYP in levels(d$V1)){
		# Each object is named after its message (e.g 'FMT' or 'GPS')
		# Extract dataframe fcorresponding to message
		msg.df <- d[d$V1 == MSG.TYP,]
		# Remove empty columns and message dataframe object to list
		if(MSG.TYP %in% levels(fmt$V4)){ #Skip unneeded messages
			# that are not in fmt
			d.l[[MSG.TYP]] <- msg.df[, colSums(msg.df != "") != 0]
			# Get column names from FMT
			fmt.nms <- fmt[fmt$V4 == MSG.TYP,] # call message type
			fmt.nms <- fmt.nms[, colSums(fmt.nms != "") != 0] # remove empty cols
			fmt.nms <- as.character(fmt.nms[5:length(fmt.nms)]) # select only necessary cols
			fmt.nms[1] <- 'Message'
			names(d.l[[MSG.TYP]]) <- fmt.nms
			rm(fmt.nms, msg.df)
		}
	}

	rm(MSG.TYP, fmt)

	return(class.define.list(d.l))
}


