#' EtrsPoints class extends \code{"ETRS"} parrent class
#' @description Represents a set of lower corner grid points of the geographical grid ETRS89-LAEA
#' @slot SpatialPointsDataFrame : an object of SPatialPointsDataFrame
#' @slot etrs.cell.codes.columns : a character object that keeps the colnames of the dataframe
#' @importClassesFrom sp SpatialPointsDataFrame
#' @import methods
#' @export EtrsPoints
#' @examples
#' # Test for EtrsPoint
#' bb <- bbox(cbind(c(4321000, 4323000), c(3210000,3212000)))
#' cell.size = 1000
#' # Calculate the limits of the grid
#' bb.outer.limits <-
#'cbind((floor(bb[, 1] / cell.size)) * cell.size, (ceiling(bb[, 2] / cell.size))*cell.size)
# Calculates eastings
#' eastings <- seq(from = bb.outer.limits[1, 1], to = bb.outer.limits[1, 2], by = cell.size)
#' #Calculates northings
#' northings <- seq(from = bb.outer.limits[2, 1], to = bb.outer.limits[2, 2], by = cell.size)
#' eastings.northings <- as.matrix(expand.grid(eastings, northings))
#' low.corner.cell.etrs.points<-etrsPoints(eastings.northings = eastings.northings,cell.size=10000)
#' plot(low.corner.cell.etrs.points)
#' text(eastings.northings[,1]+200,eastings.northings[,2],low.corner.cell.etrs.points[["NORTHOFORIGIN"]])
#' text(eastings.northings[,1],eastings.northings[,2]+200,low.corner.cell.etrs.points[["EASTOFORIGIN"]],srt=90)
#' title("Lower Cell Corner Etrs Points using ETRS-LAEA")
#' @include Class-ETRS.R
EtrsPoints <- setClass(
  "EtrsPoints",
  representation("SpatialPointsDataFrame"),

  validity  <- function(obj) {
    #Is it projected on ETRS89-LAEA (INSPIRE-Requirement 3) ?
    #if (!identical(object@proj4string,  CRS("+init=epsg:3035")))
    cat("~~~  EtrsPoints: inspector ~~~\n")
    if (!CheckEtrsValidity(obj))
      stop("Points must be projected to ETRS89-LAEA")
    #Is it a grid?
    stopifnot((gridded(obj) = TRUE))
    # Is it a lower corner of a valid grid (INSPIRE -Requirment 4)?
    bb <- bbox(obj)
    grid.topology <- slot(obj,"grid")
    cell.size <- slot(grid.topology,"cellsize")
    bb.outer.limits <-
      cbind((floor(bb[,1] / cell.size)) * cell.size,
            (ceiling(bb[,2] / cell.size)) * cell.size)
    if (!all(bb == bb.outer.limits))
      stop("(EofOrigin,NofOrigin shall coincide with grid points at Grid_ETRS89-LAEA")
    if (!EtrsCheckCodeColumns(obj))
      stop("Not Valid ETRS codes")
  },
  contains = "ETRS"
)


# Contsructor -------------------------------------------------------------

#' \code{"etrsPoints"} is the \code{"EtrsPoints"} object constructor
#'
#' @param eastings.northings : a matrix of coordinates
#' @param cell.size : a numeric object represnts the cell size of the grid
#'
#' @return An EtrsPoints object
#' @export etrsPoints
#'
#' @examples
#' # Test for EtrsPoint
#' bb <- bbox(cbind(c(4321000, 4323000), c(3210000,3212000)))
#' cell.size = 1000
#' # Calculate the limits of the grid
#' bb.outer.limits <- cbind((floor(bb[, 1]/cell.size))*cell.size,(ceiling(bb[,2]/cell.size))*cell.size)
# Calculates eastings
#' eastings <- seq(from = bb.outer.limits[1, 1], to = bb.outer.limits[1, 2], by = cell.size)
#' #Calculates northings
#' northings <- seq(from = bb.outer.limits[2, 1], to = bb.outer.limits[2, 2], by = cell.size)
#' eastings.northings <- as.matrix(expand.grid(eastings, northings))
#' low.corner.cell.etrs.points<-etrsPoints(eastings.northings = eastings.northings,cell.size=1000)
#' plot(low.corner.cell.etrs.points)
#' text(eastings.northings[,1]+200,eastings.northings[,2],low.corner.cell.etrs.points[["NORTHOFORIGIN"]])
#' text(eastings.northings[,1],eastings.northings[,2]+200,low.corner.cell.etrs.points[["EASTOFORIGIN"]],srt=90)
#' title("Lower Cell Corner Etrs Points using ETRS-LAEA")
etrsPoints.default <-
  function(eastings.northings = "matrix", cell.size = "numeric") {
    etrs.table.codes <-
      EtrsTableCodes(eastings.northings,cell.size)

    eastings.northings.sp <-
      SpatialPoints(eastings.northings,proj4string = CRS("+init=epsg:3035"))

    obj <-
      SpatialPointsDataFrame(eastings.northings.sp, data = etrs.table.codes)

    new("EtrsPoints",obj)

  }

# Generic -----------------------------------------------------------------

if (!isGeneric("etrsPoints")) {
  setGeneric(
    name = "etrsPoints", def = function(eastings.northings,cell.size)
      standardGeneric("etrsPoints")
  )
}else{
  stop("You have to remove the etrsPoints generic before you displace it")
}

# Method ------------------------------------------------------------------

#' \code{"etrsPoints"} is the \code{"EtrsPoints"} object constructor
#'
#' @param eastings.northings : a matrix of coordinates
#' @param cell.size : a numeric object represnts the cell size of the grid
#'
#' @return An EtrsPoints object
#' @export etrsPoints
#'
#' @examples
#' # Test for EtrsPoint
#' bb <- bbox(cbind(c(4321000, 4323000), c(3210000,3212000)))
#' cell.size = 1000
#' # Calculate the limits of the grid
#' bb.outer.limits <- cbind((floor(bb[, 1]/cell.size))*cell.size,(ceiling(bb[,2]/cell.size))*cell.size)
# Calculates eastings
#' eastings <- seq(from = bb.outer.limits[1, 1], to = bb.outer.limits[1, 2], by = cell.size)
#' #Calculates northings
#' northings <- seq(from = bb.outer.limits[2, 1], to = bb.outer.limits[2, 2], by = cell.size)
#' eastings.northings <- as.matrix(expand.grid(eastings, northings))
#' low.corner.cell.etrs.points<-etrsPoints(eastings.northings = eastings.northings,cell.size=1000)
#' plot(low.corner.cell.etrs.points)
#' text(eastings.northings[,1]+200,eastings.northings[,2],low.corner.cell.etrs.points[["NORTHOFORIGIN"]])
#' text(eastings.northings[,1],eastings.northings[,2]+200,low.corner.cell.etrs.points[["EASTOFORIGIN"]],srt=90)
#' title("Lower Cell Corner Etrs Points using ETRS-LAEA")
setMethod(
  "etrsPoints",signature = signature(eastings.northings = "matrix",cell.size =
                                       "numeric"),etrsPoints.default
)
