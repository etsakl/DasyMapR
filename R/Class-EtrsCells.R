#' The etrsCells class
#'
#' Produce a SpatialPolygons object of EtrsGridCells given a data.frame
#' of CELLCODE NORTHINGS EASTINGS
#'
#' @slot SpatialPolygons Object
#'
#' @export EtrsCells
#' @examples
#' bb <- bbox(cbind(c(4321000, 4323000), c(3210000,3212000)))
#' cell.size = 1000
#' # Calculate the limits of the grid
#' bb.outer.limits <-
#'  cbind((floor(bb[, 1] / cell.size)) * cell.size, (ceiling(bb[, 2] / cell.size)) * cell.size)
#' # Calculates eastings
#' eastings <-
#'  seq(from = bb.outer.limits[1, 1], to = bb.outer.limits[1, 2], by = cell.size)
#' # Calculates northings
#' northings <-
#'  seq(from = bb.outer.limits[2, 1], to = bb.outer.limits[2, 2], by = cell.size)
#' eastings.northings <- as.matrix(expand.grid(eastings, northings))
#' low.corner.cell.etrs.points<-etrsPoints(eastings.northings = eastings.northings,cell.size =1000)
#' df <-EtrsTableCodes(eastings.northings,1000)
#' sp.cells <- etrsCells(df,1000)
#' summary(sp.cells)
#'
#' @include Class-ETRS.R
#'
EtrsCells<-setClass(
  "EtrsCells",representation =   representation("SpatialPolygons") ,

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

#' The Contsructor of the EtrsCells object
#'
#' @param etrs.table.codes a data frame CELLCODE,EASTOFORIGIN,NORTHOFORIGIN
#' @param cell.size numeric : one of the resolutions of the grid  that is 0.5m, 1m, 2.5m, 5m, 10m, 25m,
#' 50m, 100m, 250m, 500m, 1Km, 2.5Km, 5Km, 10Km, 25Km, 50Km, 100Km
#'
#' @return an EtrsCells object
#' @export etrsCells
#' @examples
#' bb <- bbox(cbind(c(4321000, 4323000), c(3210000,3212000)))
#' cell.size = 1000
#' # Calculate the limits of the grid
#' bb.outer.limits <-
#'  cbind((floor(bb[, 1] / cell.size)) * cell.size, (ceiling(bb[, 2] / cell.size)) * cell.size)
#' # Calculates eastings
#' eastings <-
#'  seq(from = bb.outer.limits[1, 1], to = bb.outer.limits[1, 2], by = cell.size)
#' # Calculates northings
#' northings <-
#'  seq(from = bb.outer.limits[2, 1], to = bb.outer.limits[2, 2], by = cell.size)
#' eastings.northings <- as.matrix(expand.grid(eastings, northings))
#' low.corner.cell.etrs.points<-etrsPoints(eastings.northings = eastings.northings,cell.size =1000)
#' df <-EtrsTableCodes(eastings.northings,1000)
#' sp.cells <- etrsCells(df,1000)
#' summary(sp.cells)
etrsCells.default <-
  function(etrs.table.codes = "data.frame",cell.size = "numeric") {

    en <-
      cbind(x = as.numeric(as.character(etrs.table.codes$EASTOFORIGIN)), # FAQ factors to numeric
            y = as.numeric(as.character(etrs.table.codes$NORTHOFORIGIN)))
    eec = as.character(etrs.table.codes$CELLCODE)

# A function for list of polygon for the lower corner point
    srs <-
      function(x,y,eec)
        Polygons(list(Polygon(cbind(
          c(x,x + cell.size,x + cell.size,x,x),c(y,y,y + cell.size,y + cell.size,y)
        ))),eec)
# SpatialPoloygons using the mapply results
    obj <-
      SpatialPolygons(mapply(srs,en[,1],en[,2],eec),proj4string = CRS("+init=epsg:3035"))

    new("EtrsCells",obj)
  }

# Generic -----------------------------------------------------------------

if (!isGeneric("etrsCells")) {
  setGeneric(
    name = "etrsCells", def = function(etrs.table.codes,cell.size)
      standardGeneric("etrsCells")
  )
}else{
  stop("You have to remove the etrsPoints generic before you displace it")
}

# Method ------------------------------------------------------------------

#' The Contsructor of the EtrsCells object
#'
#' @param etrs.table.codes a data frame CELLCODE,EASTOFORIGIN,NORTHOFORIGIN
#' @param cell.size  a numeric  0.5m, 1m, 2.5m, 5m, 10m, 25m, 50m, 100m, 250m,
#' 500m, 1Km, 2.5Km, 5Km, 10Km, 25Km, 50Km, 100Km
#'
#' @return an EtrsCells object
#' @export EtrsCells
#'
#' @examples
#' bb <- bbox(cbind(c(4321000, 4323000), c(3210000,3212000)))
#' cell.size = 1000
#' # Calculate the limits of the grid
#' bb.outer.limits <-
#'  cbind((floor(bb[, 1] / cell.size)) * cell.size, (ceiling(bb[, 2] / cell.size)) * cell.size)
#' # Calculates eastings
#' eastings <-
#'  seq(from = bb.outer.limits[1, 1], to = bb.outer.limits[1, 2], by = cell.size)
#' # Calculates northings
#' northings <-
#'  seq(from = bb.outer.limits[2, 1], to = bb.outer.limits[2, 2], by = cell.size)
#' eastings.northings <- as.matrix(expand.grid(eastings, northings))
#' low.corner.cell.etrs.points<-etrsPoints(eastings.northings = eastings.northings,cell.size =1000)
#' df <-EtrsTableCodes(eastings.northings,1000)
#' sp.df <- etrsCells(df,1000)
#' summary(sp.df)
setMethod(
  "etrsCells",signature(etrs.table.codes = "data.frame",cell.size = "numeric"),etrsCells.default
)


