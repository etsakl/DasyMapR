#' The EtrsAncillarySurface class
#'
#' @description
#' The EtrsAncillarySurface class holds the basic information for ancillary surface represanation in ETRS-LAEA grid form. Land-use or land-cover are the most frequently used ancillary datasets, but any dataset that has classes of relatively  density could be used here.
#' @slot  SpatialPolygonsDataFrame. the input surface plus new data column WCELLWEIGHT
#' @slot over.method.type character. PropCal only. An ancillary Surface must have a CELLVALUE
#' @slot cell.size numeric. Indexing the column of data frame that contains the value of #'interest
#' @slot CELLVALUE character. The size of the cell (the new map unit)
#'
#' @return An EtrsAncillarySurface Object
#' @export EtrsAncillarySurface
#' @examples
#' # test EtrsAncillarySurface -------------------------------------------------------
#' x <- round(runif(1, min = 4320200, max = 4322000), 1)
#' y <- round(runif(1, min = 3210000, max = 3212000), 1)
#' xy <- cbind(x, y)
#' size = round(runif(1,min = 500,max = 1000),0)
#' pl1 <-
#'   Polygons(list(Polygon(cbind(
#'     c(x,x + size,x + size,x,x),c(y,y,y + size,y + size,y)
#'   ))),round(runif(1,min = 1,max = 100),0))
#' x <- x + size
#'
#' pl2 <-
#'   Polygons(list(Polygon(cbind(
#'     c(x,x + size,x + size,x,x),c(y,y,y + size,y + size,y)
#'   ))),round(runif(1,min = 1,max = 100),0))
#' ancS <-SpatialPolygons(list(pl1,pl2),proj4string = CRS("+init=epsg:3035"))
#'
#' df <-data.frame(RelDens=c(0.15,0.85),bin=c(1,1),row.names = sapply(slot(ancS, "polygons"), function(x)slot(x, "ID")))
#' ancS<-SpatialPolygonsDataFrame(ancS,data = df)
#' ancS.grided<-etrsAncillarySurface(input.surface = ancS,over.method.type = "PropCal",surface.value.col = 1,cell.size = 500,binary = FALSE)
#' plot(ancS.grided,border=4,lwd=2)
#' plot(ancS,add=TRUE,col=rgb(0,.25,.3,.1))
#' text(coordinates(ancS)[,1],coordinates(ancS)[,2],paste("reDens=",ancS[[1]]))
#' text(coordinates(ancS.grided)[,1],coordinates(ancS.grided)[,2],paste("CELLVALUE=",ancS.grided[[4]]))

#' @include Class-ETRS.R

EtrsAncillarySurface <-
  setClass(
    Class = "EtrsAncillarySurface",representation = representation("EtrsSurface"),

    validity <-
      function(obj) {

          if (!(over.method.type %in% "PropCal"))
            stop("Only valid over.method.type. is 'PropCal' for ancyllary Surface")
      }
  )


# etrsAncillarySurface.default -----------------------------------------------
#' etrsAncillarySurface
#' @description the default method that creates an EtrsAncillary Surface Object
#' @param input.surface A SpatialPolygonsDataFrame object that represents a ancillary dataset to be used to redistribute VALUE. The output  from this tool can be used in as an input to \code{EtrsDasymetricSurface()} method Land-use or land-cover are the most frequently used ancillary datasets, but any dataset that has classes of relatively homogenous  density  relativy with the VALUE could be used here.
#' @param over.method.type "PropCal" in proportional calculation the cell takes a calculated value depending on the values of the units falling inside and their share within the cell. This method seems very appropriate for countable variables
#' @param surface.value.col the number of colum that keeps  the relative density of a cell with land-cover some type
#' @param cell.size: one of the resolutions of the grid  that is 0.5m, 1m, 2.5m, 5m, 10m, 25m, 50m, 100m, 250m, 500m, 1Km, 2.5Km, 5Km, 10Km, 25Km, 50Km, 100Km
#'
#' @return an EtrsAncillarySurface Object
#' @examples
#' #' # test EtrsAncillarySurface -------------------------------------------------------
#' x <- round(runif(1, min = 4320200, max = 4322000), 1)
#' y <- round(runif(1, min = 3210000, max = 3212000), 1)
#' xy <- cbind(x, y)
#' size = round(runif(1,min = 500,max = 1000),0)
#' pl1 <-
#'   Polygons(list(Polygon(cbind(
#'     c(x,x + size,x + size,x,x),c(y,y,y + size,y + size,y)
#'   ))),round(runif(1,min = 1,max = 100),0))
#' x <- x + size
#'
#' pl2 <-
#'   Polygons(list(Polygon(cbind(
#'     c(x,x + size,x + size,x,x),c(y,y,y + size,y + size,y)
#'   ))),round(runif(1,min = 1,max = 100),0))
#' ancS <-SpatialPolygons(list(pl1,pl2),proj4string = CRS("+init=epsg:3035"))
#'
#' df <-data.frame(RelDens=c(0.15,0.85),bin=c(1,1),row.names = sapply(slot(ancS, "polygons"), function(x)slot(x, "ID")))
#' ancS<-SpatialPolygonsDataFrame(ancS,data = df)
#' ancS.grided<-etrsAncillarySurface(input.surface = ancS,over.method.type = "PropCal",surface.value.col = 1,cell.size = 500,binary = FALSE)
#' plot(ancS.grided,border=4,lwd=2)
#' plot(ancS,add=TRUE,col=rgb(0,.25,.3,.1))
#' text(coordinates(ancS)[,1],coordinates(ancS)[,2],paste("reDens=",ancS[[1]]))
#' text(coordinates(ancS.grided)[,1],coordinates(ancS.grided)[,2],paste("CELLVALUE=",ancS.grided[[4]]))

#' @export
etrsAncillarySurface.default <-
  function(input.surface,over.method.type, surface.value.col, cell.size,binary) {

    mc <- match.call()
    mc[[1]] <- quote(etrsSurface)
    tmp <- eval(mc)

    colnames(tmp@data)[4] <- "WCELLWEIGHT"

    if(binary == TRUE)
      tmp@data[,4] <- round(tmp@data$WCELLWEIGHT,0L) #CHNGED 7-5 AS BUG tmp@data[4]
   tmp
     }

# generic -----------------------------------------------------------------

if (!isGeneric("etrsAncillarySurface")) {
  setGeneric(
    name = "etrsAncillarySurface",
    def = function(input.surface,over.method.type,surface.value.col,cell.size,binary)
      standardGeneric("etrsAncillarySurface")
  )
}else{
  stop("You have to remove the etrsAncillarySurface generic before you displace it")
}
#' etrsAncillarySurface
#' @description  the default method that creates an EtrsAncillary Surface Object
#' @param input.surface A SpatialPolygonsDataFrame object that represents a ancillary dataset to be used to redistribute VALUE. The output  from this tool can be used in as an input to \code{EtrsDasymetricSurface()} method Land-use or land-cover are the most frequently used ancillary datasets, but any dataset that has classes of relatively homogenous  density  relativy with the VALUE could be used here.
#' @param over.method.type PropCal Proportional calculation: the cell takes a calculated value depending on the values of the units falling inside and their share within the cell. This method seems very appropriate for countable variables
#' @param surface.value.col Index number of colum that keeps  the relative density of a cell with land-cover some type
#' @param cell.size: one of the resolutions of the grid  that is 0.5m, 1m, 2.5m, 5m, 10m, 25m, 50m, 100m, 250m, 500m, 1Km, 2.5Km, 5Km, 10Km, 25Km, 50Km, 100Km
#'
#' @return an EtrsAncillarySurface Object
#' @export etrsAncillarySurface
#'
#' @examples
#' # test EtrsAncillarySurface -------------------------------------------------------
#' x <- round(runif(1, min = 4320200, max = 4322000), 1)
#' y <- round(runif(1, min = 3210000, max = 3212000), 1)
#' xy <- cbind(x, y)
#' size = round(runif(1,min = 500,max = 1000),0)
#' pl1 <-
#'   Polygons(list(Polygon(cbind(
#'     c(x,x + size,x + size,x,x),c(y,y,y + size,y + size,y)
#'   ))),round(runif(1,min = 1,max = 100),0))
#' x <- x + size
#'
#' pl2 <-
#'   Polygons(list(Polygon(cbind(
#'     c(x,x + size,x + size,x,x),c(y,y,y + size,y + size,y)
#'   ))),round(runif(1,min = 1,max = 100),0))
#' ancS <-SpatialPolygons(list(pl1,pl2),proj4string = CRS("+init=epsg:3035"))
#'
#' df <-data.frame(RelDens=c(0.15,0.85),bin=c(1,1),row.names = sapply(slot(ancS, "polygons"), function(x)slot(x, "ID")))
#' ancS<-SpatialPolygonsDataFrame(ancS,data = df)
#' ancS.grided<-etrsAncillarySurface(input.surface = ancS,over.method.type = "PropCal",surface.value.col = 1,cell.size = 500,binary = FALSE)
#' plot(ancS.grided,border=4,lwd=2)
#' plot(ancS,add=TRUE,col=rgb(0,.25,.3,.1))
#' text(coordinates(ancS)[,1],coordinates(ancS)[,2],paste("reDens=",ancS[[1]]))
#' text(coordinates(ancS.grided)[,1],coordinates(ancS.grided)[,2],paste("CELLVALUE=",ancS.grided[[4]]))


setMethod(
  f = "etrsAncillarySurface",signature(
    input.surface = "SpatialPolygonsDataFrame",over.method.type = "character", surface.value.col ="numeric", cell.size = "numeric",binary = "logical"
  ),
  etrsAncillarySurface.default
)
