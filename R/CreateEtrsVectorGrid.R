#' CreateEtrsVectorGrid
#' @description  Implements a geographical grid system as vector data (polygons) based on ETRS89-LAEA
#' @param x a Spatial object or an object with a bbox(object) matrix
#' my Deafult method
#' @param cell.size a numeric one of the resolutions of the grid  that is 0.5m, 1m, 2.5m, 5m, 10m, 25m, 50m, 100m, 250m, 500m, 1Km, 2.5Km, 5Km, 10Km, 25Km, 50Km, 100Km.
#' @param save.flag if you want to save the results to a file in the disk turn
#' it to TRUE
#'
#' @return An object of  SpatialPolygonsDataFrame containing an attribute coding system follows the recommendations from the European Environmental Agency [EEA 2008]
#' @examples
#' xc <- round(runif(10, min = 4321000, max = 4323000), 2)
#' yc <- round(runif(10, min = 3210000, max = 3212000), 2)
#' xy <- cbind(xc, yc)
#' xy.sp <- SpatialPoints(xy, proj4string = CRS("+init=epsg:3035"))
#' df<-as.data.frame(rep(1,length(xy.sp)))
#' colnames(df)<-"VALUE"
#' xy.sp<-SpatialPointsDataFrame(xy.sp,df)
#' cell.size <- 1000
#' aGrid <- CreateEtrsVectorGrid(x = xy.sp, cell.size = cell.size)
#' # ... aggregate some data into it
#' agg<-aggregate(xy.sp,aGrid,sum)
#' X11(width = 10,10)
#' split.screen(figs=c(1,2))
#' screen(1)
#' plot(aGrid)
#' plot(xy.sp,add=TRUE,pch=21,bg=rgb(1,0,0,.1),cex=xy.sp[["VALUE"]])
#' screen(2)
#' plot(aGrid)
#' plot(gCentroid(agg,byid=TRUE),add=TRUE,pch=21,bg=rgb(1,0,0,.1),cex=agg[["VALUE"]])
#' @export CreateEtrsVectorGrid

CreateEtrsVectorGrid <- function(x, cell.size, save.flag = FALSE) {
  # Implements a geographical grid system as vector data (polygons) based on ETRS89-LAEA Args: x: a spatial objects from class
  # 'Spatial' [package 'sp'] or ANY obj that is an array with at leats 2 columns
  # cell.size: one of the resolutions of the grid
  # that is 0.5m, 1m, 2.5m, 5m, 10m, 25m, 50m, 100m, 250m, 500m, 1Km, 2.5Km, 5Km, 10Km, 25Km, 50Km, 100Km.  Returns: An object of
  # SpatialPolygonsDataFrame containing an attribute coding system follows he recommendations from the European Environmental
  # Agency [EEA 2008]
  require(sp)
  require(rgdal)
  # Only spatial objects have CRS
  stopifnot(is(x, "Spatial"))
  # Which must be ETRS89-LAEA
  if (!identical(x@proj4string, CRS("+init=epsg:3035"))) {
    warning(
      "Your Spatial object must be projected to ETRS89-LAEA. try converting ..... and try again"
    )
    x <- EtrsTransform(x)
  }

  # EEA 2008 compatible (10^n / 2^m) n=c(0:5) and m=c(c0:2)
  primary.level <- c(0:5)
  quadtree.level <- c(0:2)
  quadtree.level.row <- rep(quadtree.level, length(primary.level))
  primary.level.row <-
    rep(primary.level, each = length(quadtree.level))
  value.resolution <-
    sort((10 ^ primary.level.row) / (2 ^ quadtree.level.row))
  # geoagraphical grid of 0.25m, 05m not recomented from EEA so are excluded
  value.resolution <-
    value.resolution[!value.resolution %in% value.resolution[1:2]]

  # check if cell size is EEA valid
  if (!is.element(cell.size, value.resolution)) {
    warnings("This is not valid cell size. continuing with 10km cell size")
    cell.size <- 10000
  }

  bb <- bbox(x)


  # Creating the grid name as it is sugested for EEA
  grid.name <-
    paste("ETRS89.LAEA.", as(
      ifelse(cell.size >= 1000, cell.size / 1000, cell.size), "character"
    ), as(ifelse(cell.size >=
                   1000, "km", "m"), "character"), sep = "")



  # Calculate the limits of the grid
  bb.outer.limits <-
    cbind((floor(bb[, 1] / cell.size)) * cell.size, (ceiling(bb[, 2] / cell.size)) * cell.size)


  # Calculates eastings
  eastings <-
    seq(from = bb.outer.limits[1, 1], to = bb.outer.limits[1, 2], by = cell.size)


  # Calculates northings
  northings <-
    seq(from = bb.outer.limits[2, 1], to = bb.outer.limits[2, 2], by = cell.size)
  eastings.northings <- expand.grid(eastings, northings)
  # A cell code identifier for ETRS



  # Calculate the limits of the grid
  bb.outer.limits <-
    cbind((floor(bb[, 1] / cell.size)) * cell.size, (ceiling(bb[, 2] / cell.size)) * cell.size)

  # Calculates eastings
  eastings <-
    seq(from = bb.outer.limits[1, 1], to = bb.outer.limits[1, 2], by = cell.size)

  # Calculates northings
  northings <-
    seq(from = bb.outer.limits[2, 1], to = bb.outer.limits[2, 2], by = cell.size)
  eastings.northings <- expand.grid(eastings, northings)

  # A cell code identifier for ETRS Define the cell size prefix
  prefix <-
    paste("", as(
      ifelse(cell.size >= 1000, cell.size / 1000, cell.size), "character"
    ), as(ifelse(cell.size >= 1000, "km",
                 "m"), "character"), sep = "")
  # Identify the number of zeros to remove from Eastings and Northings Values
  nz <-
    as.numeric(unlist(strsplit(as.character(
      format(cell.size, scientific = TRUE)
    ), split = ".e\\+"))[2])
  # Define string value based on Easting
  eaststr <-
    paste("E", as.character(eastings.northings[, 1] / 10 ^ nz), sep = "")
  # Define string value based on Northings
  northststr <-
    paste("N", as.character(eastings.northings[, 2] / 10 ^ nz), sep = "")
  # Concatenate strings into cell code
  etrs.cell.code <- paste(prefix, eaststr, northststr, sep = "")
  etrs.table.cell.codes <-
    as.data.frame(
      cbind(
        CELLCODE = etrs.cell.code, EASTOFORIGIN = eastings.northings[, 1], NORTHOFORIGIN = eastings.northings[,
                                                                                                              2]
      )
    )
  etrs.table.cell.codes
  cell.centers <- eastings.northings + c(cell.size / 2, cell.size / 2)
  cell.centers <-
    cell.centers[-which(cell.centers[, 1] > bb.outer.limits[1, 2]),]
  cell.centers <-
    cell.centers[-which(cell.centers[, 2] > bb.outer.limits[2, 2]),]



  if (length(cell.centers[, 1] > 2)) {
    # Implement the grid with coords
    the.pnts.grid <-
      SpatialPixels(SpatialPoints(cell.centers, proj4string = CRS("+init=epsg:3035")))
    the.grid.sp <- as(the.pnts.grid, "SpatialPolygons")

  } else {
    # Implement the grid with gridTopology

    cells.dim <- c(1, 1)


    cell.center = c(min(cell.centers[, 1]), min(cell.centers[, 2]))


    the.grid.topology <-
      GridTopology(
        cellcentre.offset = cell.center, cellsize = c(cell.size, cell.size), cells.dim = cells.dim
      )
    the.grid <-
      SpatialGrid(the.grid.topology, proj4string = CRS("+init=epsg:3035"))
    # and then to SpatialPolygons
    the.grid.sp <- as(the.grid, "SpatialPolygons")
  }


  # Save the grid in the disk named according EEA sugestions
  if (save.flag == TRUE){

     f<-paste0(getwd(),"/",as.character(grid.name))

 save(the.grid.sp, file = as.character(f))}

  the.grid.sp
}
