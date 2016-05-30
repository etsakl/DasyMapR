library(sp)

setClass("trip", representation("SpatialPointsDataFrame", TOR.columns = "character"), validity <- function(object) {
    if (length(object@TOR.columns) != 2) 
        stop("Time/id column names must be of length 2")
    if (!all(object@TOR.columns %in% names(object@data))) 
        stop("Time/id columns must be present in attribute table")
    TRUE
})

trip.default <- function(obj, TORnames) {
    if (!is(obj, "SpatialPointsDataFrame")) 
        stop("trip only supports SpatialPointsDataFrame")
    if (is.numeric(TORnames)) 
        TORnames <- names(obj)[TORnames]
    new("trip", obj, TOR.columns = TORnames)
}

if (!isGeneric("trip")) setGeneric("trip", function(obj, TORnames) standardGeneric("trip"))

setMethod("trip", signature(obj = "SpatialPointsDataFrame", TORnames = "ANY"), trip.default)

# example
turtle <- read.csv(system.file("external/seamap105_mod.csv", package = "sp"))
timestamp <- as.POSIXlt(strptime(as.character(turtle$obs_date), "%m/%d/%Y %H:%M:%S"), "GMT")
turtle <- data.frame(turtle, timestamp = timestamp)
turtle$lon <- ifelse(turtle$lon < 0, turtle$lon + 360, turtle$lon)
turtle <- turtle[order(turtle$timestamp), ]
coordinates(turtle) <- c("lon", "lat")
proj4string(turtle) <- CRS("+proj=longlat +ellps=WGS84")
turtle$id <- c(rep(1, 200), rep(2, nrow(coordinates(turtle)) - 200))
turtle_trip <- trip(turtle, c("timestamp", "id")) 
