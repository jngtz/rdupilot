
setwd("data-raw")

kap.log <- read.kap.log("KAP.log")

# Determine the range of  photos in the KAP log to be geo-located
# These are the photo's that have taken a photo of time of corrected
# GSM time on an iphone  using the Emerald Time app
# https://itunes.apple.com/us/app/emerald-time/id290384375?mt=8


# Apply selection

kap.log <- select.photos(kap.log, "IMG_2349.JPG", "IMG_2367.JPG")




## Observed time of in the Time app on phone (seen in photos taken)
obs.time <- c(
"08:45:02.500",
"08:45:04.900",
"08:45:07.300",
"08:45:09.800",
"08:45:12.200",
"08:45:14.700",
"08:45:17.000",
"08:45:19.500",
"08:45:21.900",
"08:45:24.300",
"08:45:26.700",
"08:45:29.300",
"08:45:31.700",
"08:45:34.000",
"08:45:36.500",
"08:45:38.800",
"08:45:41.300",
"08:45:43.700",
"08:45:46.100"
)

## Convert observed time to seconds
obs.seconds <- unlist(lapply(obs.time, time.to.seconds)) + 3600*2 #Add time diff

## Camera KAP time in seconds
kap.seconds <- kap.log$time_s

diff.seconds <- obs.seconds - kap.seconds
mean(diff.seconds)
median(diff.seconds)
max(diff.seconds) - min(diff.seconds)


getmode(diff.seconds)


