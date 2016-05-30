#' Create the EtrsGrid Class
#'
#' Represents A Geographical grid on ERTS89-LAEA
#' Set the name of the class
#'
#' @slot  SpatialPolygonsDataFrame. An object that the Grid net should cover completely grid
#' @slot the.grid.name character. The grid is designated as Grid_ETRS89-LAEA. For identification of an individual resolution level the cell size in metres is appended to the name. EXAMPLE LAEA_100K. The grid at a resolution level of 100km is designated as Grid_ETRS89-
#' @param cell.size a numeric one of the resolutions of the grid  that is 0.5m, 1m, 2.5m, 5m, 10m, 25m, 50m, 100m, 250m, 500m, 1Km, 2.5Km, 5Km, 10Km, 25Km, 50Km, 100Km.
#'
#' @return An object of the class EtrsGrid
#' @export
#'
#' @examples
#' x <- round(runif(1, min = 4320200, max = 4322000), 1)
#' y <- round(runif(1, min = 3210000, max = 3212000), 1)
#' xy <- cbind(x, y)
#' size = round(runif(1,min = 1500,max = 2000),0)
#' pl1 <-
#'   Polygons(list(Polygon(cbind(
#'     c(x,x + size,x + size,x,x),c(y,y,y + size,y + size,y)
#'   ))),round(runif(1,min = 1,max = 100),0))
#' x <- x + size
#' pl2 <-
#'   Polygons(list(Polygon(cbind(
#'     c(x,x + size,x + size,x,x),c(y,y,y + 2 * size,y + 2 * size,y)
#'   ))),round(runif(1,min = 1,max = 100),0))
#' x<-x-size
#' y<-y+size
#' pl3 <-
#'   Polygons(list(Polygon(cbind(
#'     c(x,x + size,x + size,x,x),c(y,y,y +  size,y +  size,y)
#'   ))),round(runif(1,min = 1,max = 100),0))
#' sps <-
#'   SpatialPolygons(list(pl1,pl2,pl3),proj4string = CRS("+init=epsg:3035"))
#' df <-
#'   data.frame(val=c("R5","R40","R80"),row.names = sapply(slot(sps, "polygons"), function(x)
#'     slot(x, "ID")))
#' sps<-SpatialPolygonsDataFrame(sps,data = df)
#' the.etrs.grid<-etrsGrid(sps,1000)
#' slotNames(the.etrs.grid)
#' the.etrs.grid@the.grid.name
#' head(the.etrs.grid@data,3)
#' plot(the.etrs.grid)
#' title(paste("the etrs grid",the.etrs.grid@the.grid.name))
#'
#' @include Class-ETRS.R
EtrsGrid <- setClass(
  Class = "EtrsGrid",
  representation(
    "SpatialPolygonsDataFrame", the.grid.name = "character",cell.size = "numeric"
  ),

  prototype(grid.name = NA_character_),

  #   validity( function(object) {  #some test
  #
  #     return(TRUE)
  #   })
  contains = "ETRS"
)

# funcion etrsGrid  -------------------------------------------------------


#' the etrsGrid default method
#'
#' @param obj SpatialPolygonsDataFrame. An object that the Grid net should cover completely grid
#' @param cell.size ell.size a numeric one of the resolutions of the grid  that is 0.5m, 1m, 2.5m, 5m, 10m, 25m, 50m, 100m, 250m, 500m, 1Km, 2.5Km, 5Km, 10Km, 25Km, 50Km, 100Km.
#'
#' @return An object of the class EtrsGrid
#' @export
#'
#' @examples
#' x <- round(runif(1, min = 4320200, max = 4322000), 1)
#' y <- round(runif(1, min = 3210000, max = 3212000), 1)
#' xy <- cbind(x, y)
#' size = round(runif(1,min = 1500,max = 2000),0)
#' pl1 <-
#'   Polygons(list(Polygon(cbind(
#'     c(x,x + size,x + size,x,x),c(y,y,y + size,y + size,y)
#'   ))),round(runif(1,min = 1,max = 100),0))
#' x <- x + size
#' pl2 <-
#'   Polygons(list(Polygon(cbind(
#'     c(x,x + size,x + size,x,x),c(y,y,y + 2 * size,y + 2 * size,y)
#'   ))),round(runif(1,min = 1,max = 100),0))
#' x<-x-size
#' y<-y+size
#' pl3 <-
#'   Polygons(list(Polygon(cbind(
#'     c(x,x + size,x + size,x,x),c(y,y,y +  size,y +  size,y)
#'   ))),round(runif(1,min = 1,max = 100),0))
#' sps <-
#'   SpatialPolygons(list(pl1,pl2,pl3),proj4string = CRS("+init=epsg:3035"))
#' df <-
#'   data.frame(val=c("R5","R40","R80"),row.names = sapply(slot(sps, "polygons"), function(x)
#'     slot(x, "ID")))
#' sps<-SpatialPolygonsDataFrame(sps,data = df)
#' the.etrs.grid<-etrsGrid(sps,1000)
#' slotNames(the.etrs.grid)
#' the.etrs.grid@the.grid.name
#' head(the.etrs.grid@data,3)
#' plot(the.etrs.grid)
#' title(paste("the etrs grid",the.etrs.grid@the.grid.name))
etrsGrid.default <-
  function(obj = "SpatialPolygonsDataFrame", cell.size = "numeric") {
    CheckEtrsValidity(obj)
    CheckEtrsResolution(cell.size)

    # Creating the grid name as it is sugested for EEA
    grid.name <-
      paste("ETRS89.LAEA.", as(
        ifelse(cell.size >= 1000, cell.size / 1000, cell.size), "character"
      ), as(ifelse(cell.size >= 1000, "km", "m"), "character"), sep = "")

    bb <- bbox(obj)
    # Calculate the limits of the grid
    bb.outer.limits <-
      cbind((floor(bb[, 1] / cell.size)) * cell.size, (ceiling(bb[, 2] / cell.size)) * cell.size)

    # Calculates eastings
    eastings <-
      seq(from = bb.outer.limits[1, 1], to = bb.outer.limits[1, 2], by = cell.size)

    # Calculates northings
    northings <-
      seq(from = bb.outer.limits[2, 1], to = bb.outer.limits[2, 2], by = cell.size)

    eastings.northings <-
      as.matrix(expand.grid(eastings, northings))

    etrs.table.codes <- EtrsTableCodes(eastings.northings,cell.size)

    grid.cells.spdf <-
      SpatialPolygonsDataFrame(etrsCells(etrs.table.codes,cell.size),data = etrs.table.codes)
    new("EtrsGrid",grid.cells.spdf, the.grid.name = grid.name,cell.size = cell.size)

  }

# Generic etrsGrid  -------------------------------------------------------

if (!isGeneric("etrsGrid")) {
  setGeneric(
    name = "etrsGrid",def = function(obj, cell.size)
      standardGeneric("etrsGrid")
  )
}else{
  stop("You have to remove the etrsGrid generic before you displace it")
}

# Method etrsGrid ---------------------------------------------------------
#' the etrsGrid default method
#'
#' @param obj SpatialPolygonsDataFrame.
#' @param cell.size numeric. @param cell.size a numeric one of the resolutions of the grid  that is 0.5m, 1m, 2.5m, 5m, 10m, 25m, 50m, 100m, 250m, 500m, 1Km, 2.5Km, 5Km, 10Km, 25Km, 50Km, 100Km.
#'
#' @return EtrsGrid class object
#' @export
#'
#' @examples
#' #' x <- round(runif(1, min = 4320200, max = 4322000), 1)
#' y <- round(runif(1, min = 3210000, max = 3212000), 1)
#' xy <- cbind(x, y)
#' size = round(runif(1,min = 1500,max = 2000),0)
#' pl1 <-
#'   Polygons(list(Polygon(cbind(
#'     c(x,x + size,x + size,x,x),c(y,y,y + size,y + size,y)
#'   ))),round(runif(1,min = 1,max = 100),0))
#' x <- x + size
#' pl2 <-
#'   Polygons(list(Polygon(cbind(
#'     c(x,x + size,x + size,x,x),c(y,y,y + 2 * size,y + 2 * size,y)
#'   ))),round(runif(1,min = 1,max = 100),0))
#' x<-x-size
#' y<-y+size
#' pl3 <-
#'   Polygons(list(Polygon(cbind(
#'     c(x,x + size,x + size,x,x),c(y,y,y +  size,y +  size,y)
#'   ))),round(runif(1,min = 1,max = 100),0))
#' sps <-
#'   SpatialPolygons(list(pl1,pl2,pl3),proj4string = CRS("+init=epsg:3035"))
#' df <-
#'   data.frame(val=c("R5","R40","R80"),row.names = sapply(slot(sps, "polygons"), function(x)
#'     slot(x, "ID")))
#' sps<-SpatialPolygonsDataFrame(sps,data = df)
#' the.etrs.grid<-etrsGrid(sps,1000)
#' slotNames(the.etrs.grid)
#' the.etrs.grid@the.grid.name
#' head(the.etrs.grid@data,3)
#' plot(the.etrs.grid)
#' title(paste("the etrs grid",the.etrs.grid@the.grid.name))
setMethod(
  f = "etrsGrid",signature = signature(obj = "SpatialPolygonsDataFrame"
                                       ,cell.size = "numeric"),etrsGrid.default
)

# etrsGrid2Spdf ---------------------------------------------------------

#' Converts an etrsgrid obj to standard SpatiaPolygonsDataFrame
#' @description It is usefull for rGeos that doesn't support coercion. ALso to be used for coercion
#' @param the.etrs.grid
#'
#' @return SpatialPolygonsDataFrame object
#' @export
#'
#' @examples
#'
#'
etrsGrid2Spdf <- function(the.etrs.grid = "etrsGrid") {
  sP <-
    SpatialPolygons(Srl = the.etrs.grid@polygons,proj4string = the.etrs.grid@proj4string)
  the.grid <-
    SpatialPolygonsDataFrame(sP,data = the.etrs.grid@data)
  the.grid
}


#' Produce a etrs aggregating point data
#'
#' @param obj a SpatialPointsDataFrame object
#' @param point.value.col The value that is going to be aggregated
#' @param cell.size the resolution of the grid
#' @param mean.flag False for \code{sum()} the value TRUE \code{mean()} the value
#'
#' @return SpatialPointsDataFrame
#' @export etrsPoint2Grid
#'
#' @examples
#' xc <- round(runif(10, min = 4321000, max = 4323000), 2)
#' yc <- round(runif(10, min = 3210000, max = 3212000), 2)
#' xy <- cbind(xc, yc)
#' xy.sp <- SpatialPoints(xy, proj4string = CRS("+init=epsg:3035"))
#' df<-as.data.frame(rep(1,length(xy.sp)))
#' colnames(df)<-"VALUE"
#' xy.sp<-SpatialPointsDataFrame(xy.sp,df)
#' aGrid <- etrsPoint2Grid(obj = xy.sp, point.value.col=1,cell.size = 1000,mean.flag =FALSE)
#' X11(width = 10,10)
#' split.screen(figs=c(1,2))
#' screen(1)
#' plot(aGrid)
#' title("Some Points ...")
#' plot(xy.sp,add=TRUE,pch=21,bg=rgb(1,0,0,.1),cex=xy.sp[["VALUE"]])
#' screen(2)
#'  plot(aGrid)
#' plot(gCentroid(etrsGrid2Spdf(aGrid),byid=TRUE),add=TRUE,pch=21,bg=rgb(1,0,0,.1),cex=aGrid[["VALUE"]])
#' title(paste("aggregated in the",aGrid@the.grid.name))
etrsPoint2Grid <-
  function(obj = "SpatialPointsDataFrame", point.value.col = "numeric",cell.size = "numeric",mean.flag =
             FALSE) {
    CheckEtrsValidity(obj)
    CheckEtrsResolution(cell.size)

    # Creating the grid name as it is sugested for EEA
    grid.name <-
      paste("ETRS89.LAEA.", as(
        ifelse(cell.size >= 1000, cell.size / 1000, cell.size), "character"
      ), as(ifelse(cell.size >= 1000, "km", "m"), "character"), sep = "")

    bb <- bbox(obj)
    # Calculate the limits of the grid
    bb.outer.limits <-
      cbind((floor(bb[, 1] / cell.size)) * cell.size, (ceiling(bb[, 2] / cell.size)) * cell.size)

    # Calculates eastings
    eastings <-
      seq(from = bb.outer.limits[1, 1], to = bb.outer.limits[1, 2], by = cell.size)

    # Calculates northings
    northings <-
      seq(from = bb.outer.limits[2, 1], to = bb.outer.limits[2, 2], by = cell.size)

    eastings.northings <-
      as.matrix(expand.grid(eastings, northings))

    etrs.table.codes <- EtrsTableCodes(eastings.northings,cell.size)

    grid.cells.spdf <-
      SpatialPolygonsDataFrame(etrsCells(etrs.table.codes,cell.size),data = etrs.table.codes)

    if (mean.flag) {
      agg.point.surface <-
        aggregate(obj[point.value.col],grid.cells.spdf,mean)
    }
    else{
      agg.point.surface <-
        aggregate(obj[point.value.col],grid.cells.spdf,sum)
    }

    agg.point.surface <-
      agg.point.surface[which(!is.na(agg.point.surface[[point.value.col]])),]

    new("EtrsGrid",agg.point.surface, the.grid.name = grid.name,cell.size = cell.size)

  }
