#' The EtrsSourceSurface class
#'
#' @description
#' The EtrsSourceSurface class holds the basic information for Source surface represanation in ETRS-LAEA grid form
#' @slot  SpatialPolygonsDataFrame. the input surface plus new data columns
#' @slot over.method.type character. MaxArea for categorical data PropCal for numeric #'values
#' @slot cell.size numeric. Indexing the column of data frame that contains the value of #'interest
#' @slot CELLVALUE character. The size of the cell (the new map unit)
#'
#' @return An EtrsSourceSurface Object
#' @export EtrsSourceSurface
#' @examples
#' # test Source Surface -------------------------------------------------------
#'
#'   x <- round(runif(1, min = 4320200, max = 4322000), 1)
#'   y <- round(runif(1, min = 3210000, max = 3212000), 1)
#'   xy <- cbind(x, y)
#'   size = round(runif(1,min = 1500,max = 2000),0)
#'   pl1 <-
#'     Polygons(list(Polygon(cbind(
#'       c(x,x + size,x + size,x,x),c(y,y,y + size,y + size,y)
#'     ))),round(runif(1,min = 1,max = 100),0))
#'   x <- x + size
#'
#'   pl2 <-
#'     Polygons(list(Polygon(cbind(
#'       c(x,x + size,x + size,x,x),c(y,y,y + 2 * size,y + 2 * size,y)
#'     ))),round(runif(1,min = 1,max = 100),0))
#'   sps <-
#'     SpatialPolygons(list(pl1,pl2),proj4string = CRS("+init=epsg:3035"))
#'
#'   df <-
#'     data.frame(AREA = sapply(slot(sps, "polygons"), function(x)
#'       slot(x, "area")),VALUE=c(10,20),row.names = sapply(slot(sps, "polygons"), function(x)
#'       slot(x, "ID")))
#'   sps<-SpatialPolygonsDataFrame(sps,data = df)
#' sps.source<-etrsSourceSurface(input.surface = sps,over.method.type = "PropCal",surface.value.col = 2,cell.size = 500)
#'
#'plot(sps,border=2,lwd=3,col=rgb(.4,sps@data$VALUE/100,0,.25))
#'text(coordinates(sps)[,1],coordinates(sps)[,2],paste("VALUE=",sps@data$VALUE,sep=" "),cex=1)
#'text(coordinates(sps)[,1],coordinates(sps)[,2]+100,paste("ID=",row.names(sps@data),sep=" "),cex=1.2)
#'plot(sps.source,add=TRUE,lty=3,border=4)
#'text(coordinates(sps.source)[,1],coordinates(sps.source)[,2],sps.source@data$CELLVALUE,col=4)
#' @include Class-ETRS.R

EtrsSourceSurface <-
  setClass(Class = "EtrsSourceSurface",representation = representation("EtrsSurface"),

           validity <-
             function(obj) {

             })


# etrsSourceSurface.default -----------------------------------------------
etrsSourceSurface.default <-
  function(input.surface,over.method.type,surface.value.col,cell.size) {
    mc <- match.call()
    mc[[1]] <- quote(etrsSurface)
    ss<-eval(mc)
new("EtrsSourceSurface",
    ss)

  }

# etrsSourceSurfacePar -----------------------------------------------
etrsSourceSurfacePar <-
  function(input.surface,over.method.type,surface.value.col,cell.size) {
    mc <- match.call()
    mc[[1]] <- quote(etrsSurfacePar)
    eval(mc)
    new("EtrsSourceSurface",
        ss)

  }


# generic -----------------------------------------------------------------

if (!isGeneric("etrsSourceSurface")) {
  setGeneric(
    name = "etrsSourceSurface",
    def = function(input.surface,over.method.type,surface.value.col,cell.size)
      standardGeneric("etrsSourceSurface")
  )
}else{
  stop("You have to remove the etrsSourceSurface generic before you displace it")
}

if (!isGeneric("etrsSourceSurfacePar")) {
  setGeneric(
    name = "etrsSourceSurfacePar",
    def = function(input.surface,over.method.type,surface.value.col,cell.size)
      standardGeneric("etrsSourceSurfacePar")
  )
}else{
  stop("You have to remove the etrsSourceSurfacePar generic before you displace it")
}
# etrsSourceSurface  ---------------------------------------------
#' The constructor for an EtrsSourceSurface object
#'
#' @description Creates an EtrsSourceSurface class that holds the basic information for Source surface represanation in ETRS-LAEA grid form
#'
#' @param input.surface SpatialPolygonsDataFrame.
#' @param over.method.type ANY.
#' @param surface.value.col ANY.
#' @param cell.size numeric : cell.size numeric :  one of the resolutions of the grid  that is 0.5m, 1m, 2.5m, 5m, 10m, 25m, 50m, 100m, 250m, 500m, 1Km, 2.5Km, 5Km, 10Km, 25Km, 50Km, 100Km
#' @return an EtrsSourceSurface object
#' @export etrsSourceSurface
#' @examples
#'   x <- round(runif(1, min = 4320200, max = 4322000), 1)
#'   y <- round(runif(1, min = 3210000, max = 3212000), 1)
#'   xy <- cbind(x, y)
#'   size = round(runif(1,min = 1500,max = 2000),0)
#'   pl1 <-
#'     Polygons(list(Polygon(cbind(
#'       c(x,x + size,x + size,x,x),c(y,y,y + size,y + size,y)
#'     ))),round(runif(1,min = 1,max = 100),0))
#'   x <- x + size
#'
#'   pl2 <-
#'     Polygons(list(Polygon(cbind(
#'       c(x,x + size,x + size,x,x),c(y,y,y + 2 * size,y + 2 * size,y)
#'     ))),round(runif(1,min = 1,max = 100),0))
#'   sps <-
#'     SpatialPolygons(list(pl1,pl2),proj4string = CRS("+init=epsg:3035"))
#'
#'   df <-
#'     data.frame(AREA = sapply(slot(sps, "polygons"), function(x)
#'       slot(x, "area")),VALUE=c(10,20),row.names = sapply(slot(sps, "polygons"), function(x)
#'       slot(x, "ID")))
#'   sps<-SpatialPolygonsDataFrame(sps,data = df)
#' sps.source<-etrsSourceSurface(input.surface = sps,over.method.type = "PropCal",surface.value.col = 2,cell.size = 500)
#'
#'plot(sps,border=2,lwd=3,col=rgb(.4,sps@data$VALUE/100,0,.25))
#'text(coordinates(sps)[,1],coordinates(sps)[,2],paste("VALUE=",sps@data$VALUE,sep=" "),cex=1)
#'text(coordinates(sps)[,1],coordinates(sps)[,2]+100,paste("ID=",row.names(sps@data),sep=" "),cex=1.2)
#'plot(sps.source,add=TRUE,lty=3,border=4)
#'text(coordinates(sps.source)[,1],coordinates(sps.source)[,2],sps.source@data$CELLVALUE,col=4)
setMethod(
  f = "etrsSourceSurface",signature(
    input.surface = "SpatialPolygonsDataFrame",over.method.type = "ANY", surface.value.col =
      "ANY", cell.size = "numeric"
  ),
  etrsSourceSurface.default
)




# ActuallVal2Density ------------------------------------------------------


#' Converts actuall value of a Surface to Value Density (in km^2 by default)
#'
#' @param input.surface A SpatialPolygonsDataFrame object
#' @param surface.value.col Index of VALUE column
#' @param area.uint usually to km^2
#'
#' @return the original  SpatialPolygonsDataFrame object but VALUE is now Density
#'
#' @export ActuallVal2Density
#'
#' @examples
#' # test  ActuallVal2Density -------------------------------------------------------
#' # Make a spatial polygon
#' S<-readWKT("POLYGON((0 0,2000 0,2000 2000,0 2000,0 0))",id = "S",p4s = CRS("+init=epsg:3035"))
#' df<-cbind(AREA=gArea(S),VALUE=1000)
#' row.names(df)<-sapply(slot(S,"polygons"),function(x) slot(x,"ID"))
#' df<-as.data.frame(df)
#' S<-SpatialPolygonsDataFrame(S,data = df,match.ID = TRUE)
#' # Covert actuall value to density
#' S_D<-ActuallVal2Density(input.surface = S,surface.value.col = 2,area.unit = 1e+06)
#' # plot
#' X11(width=12,height = 12)
#' split.screen(figs = c(2,1))
#' screen(1)
#' plot(S)
#' title("the input surface")
#' text(coordinates(S)[,1],coordinates(S)[,2],paste("Actuall Value =",S[["VALUE"]]))
#' screen(2)
#' plot(S_D)
#' title("the input surface changed the attr VALUE")
#' text(coordinates(S_D)[,1],coordinates(S_D)[,2],paste("Density =", S_D[["VALUE"]]))
ActuallVal2Density<-  function(input.surface = "SpatialPolygonsDataFrame",surface.value.col =
              "numeric",area.unit = 1e+06)
  {
    input.surface[["AREA"]] <-
      (sapply(slot(input.surface,"polygons"),function(x)
        slot(x,"area")))/area.unit
    input.surface[["VALUE"]] <-
       round(input.surface[[surface.value.col]]/input.surface[["AREA"]],4)

    input.surface
  }

