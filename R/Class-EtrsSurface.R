# EtrsSurfaceClass --------------------------------------------------------
# Documentation -----------------------------------------------------------
#' The EtrsSurface class
#'
#' The etrs surface class holds the basic information for surface represanation in ETRS-LAEA grid from
#' @slot  SpatialPolygonsDataFrame. the input surface plus new data columns
#' @slot over.method.type character. MaxArea for categorical data PropCal for numeric #'values
#' @slot cell.size numeric. Indexing the column of data frame that contains the value of #'interest
#' @slot CELLVALUE character. The size of the cell (the new map unit)
#'
#' @return An EtrsSurface Object
#' @export EtrsSurface
#' @examples
#' testpropcal <- function() {
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
#'   SpatialPolygons(list(pl1,pl2,pl3),proj4string = CRS("+init=epsg:2100"))
#' df <-
#'   data.frame(val=c("R5","R40","R80"),row.names = sapply(slot(sps, "polygons"), function(x)
#'     slot(x, "ID")))
#' sps<-SpatialPolygonsDataFrame(sps,data = df)
#' }
#' Source.Surface <- testpropcal()
#'
#' # Uses the default etrsSurface method
#'Source.Surface.MaxArea <-
#' etrsSurface(
#'   input.surface = Source.Surface, over.method.type = "MaxArea", cell.size = 1000
#' )
#' summary(Source.Surface.MaxArea)
#'
#' @seealso etrsSurface.default, etrsMaxArea, etrsPropCal
#' @include Class-ETRS.R
#'
#CODE --------------------------------------------------------------------
EtrsSurface <- setClass(
  Class = "EtrsSurface",representation = representation(
    "SpatialPolygonsDataFrame", over.method.type = "character",cell.size = "numeric",CELLVALUE = "character"
  ),
  validity <- (function(obj) {
    if (CheckEtrsValidity(obj))
      return(TRUE)
    if (!over.method.type %in% c("MaxArea","PropCal","PropWeighted"))
      stop("Not valid over.method.type. MaxArea | PropCal | PropWeighted is valid")

    if (over.method.type %in% "PropCal" &
        !surface.value %in% rownames(obj@data))
      stop("You have to determine the surface value column for the over method")

    return(TRUE)
  }),

  contains = "ETRS"
)


#etrsTransform -----------------------------------------------------------
# Documentation -----------------------------------------------------------
# CODE --------------------------------------------------------------------
#' Transforms from the current CRS to ETRS-LAEA
#'
#' @param obj
#'
#' @return an EtrsSurface in ETRS CRS
#' @export EtrsTransform
EtrsTransform <- function(obj) {
  obj <- spTransform(obj,CRS("+init=epsg:3035"))
  return(obj)
}
# etrsCellCenter ----------------------------------------------------------
# Documentation -----------------------------------------------------------
#
#' The polygon in which the center of the cell yields the attribute to assign to the cell.
#'
#' @param the.etrs.grid an EtrsGrid
#'
#' @return Returns centroids of an etrsgrid
#' @export etrsCellCenter
#'
#' @examples
# CODE  -----------------------------------------------------------
etrsCellCenter <-
  function(the.etrs.grid = "EtrsGrid") {
    cell.size = the.etrs.grid@cell.size

    the.grid <- etrsGrid2Spdf(the.etrs.grid)

    pnts <- gCentroid(the.etrs.grid,byid = TRUE)
  }


# 1. Maximum area criteria: the cell takes the value of the unit which covers most of the cell area. It should be a good option for uncountable variables

# 1. Κριτήριο μέγιστης επιφάνειας: το κελί παίρνει την τιμή του χαρακτηριστικού που καταλαμβάνει το μεγαλύτερο μέρος της επιφανείας του. Είναι καλή επίλογή για μη απαρηθμίσiμες μεταβλητές


# etrsMaxArea ---------------------------------------------------------------
# Documentation -----------------------------------------------------------
#' Computes a single figure by each reference grid cell using Maximum Area as integration methods
#' @description  The Maximum area criteria: the cell takes the value of the unit which covers most of the cell area. It should be a good option for uncountable variables
#'
#' @param the.etrs.grid An object of the class EtrsGrid
#' @param the.surface an object of class EtrsSurfaface
#'
#' @return An EtrsSurface
#' @export etrsMaxArea
#' @examples
#' x <- round(runif(1, min = 4321000, max = 4322000), 1)
#' y <- round(runif(1, min = 3211000, max = 3212000), 1)
#' xy <- cbind(x, y)
#' size = round(runif(1,min = 1500,max = 1500),0)
#' pl1 <-Polygons(list(Polygon(cbind(c(x,x + size,x + size,x,x),c(y,y,y + size,y + size,y)   ))),round(runif(1,min = 1,max = 100),0))
#' x <- x + size
#' pl2 <- Polygons(list(Polygon(cbind(c(x,x + size,x + size,x,x),c(y,y,y + 2 * size,y + 2 * size,y)))),round(runif(1,min = 1,max = 100),0))
#' x<-x-size
#' y<-y+size
#' pl3 <-Polygons(list(Polygon(cbind(c(x,x + size,x + size,x,x),c(y,y,y +  size,y +  size,y)   ))),round(runif(1,min = 1,max = 100),0))
#' sps <- SpatialPolygons(list(pl1,pl2,pl3),proj4string = CRS("+init=epsg:2100"))
#' df <-data.frame(val=c("R5","R40","R80"),row.names = sapply(slot(sps, "polygons"), function(x) slot(x, "ID")))
#' Source.Surface <-SpatialPolygonsDataFrame(sps,data = df)
#'
#' # Uses the default etrsSurface method
#' Source.Surface.MaxArea <- etrsSurface(input.surface = Source.Surface, over.method.type = "MaxArea", cell.size = 1000)
#' Source.Surface.MaxArea <-
#' etrsSurface(input.surface = Source.Surface, over.method.type = "MaxArea", cell.size = 1000)
#' Source.Surface<-EtrsTransform(Source.Surface)
#' plot(Source.Surface)
#' plot(Source.Surface.MaxArea,lty = 3,lwd = 1.2,border = 3,add=TRUE)
#' x.y.s.s <- coordinates(EtrsTransform(Source.Surface))
#' x.y.max <- coordinates(Source.Surface.MaxArea)
#' text(x.y.s.s[,1],x.y.s.s[,2],Source.Surface@data$val,col = 4,cex = 1.5)
#' text(x.y.s.s[,1],x.y.s.s[,2] - 100,paste("Feature=",rownames(Source.Surface@data)),col = 4,cex = 1.2)
#' text(x.y.max[,1],x.y.max[,2],Source.Surface.MaxArea@data$FEATURE,col=3)
#' title("The 3 regions in Etrs Grid using Max Area Intergration")
#'
#'
# CODE --------------------------------------------------------------------
etrsMaxArea <-
  function(the.etrs.grid = "EtrsGrid", the.surface = "EtrsSurface") {
    cell.size = the.etrs.grid@cell.size
    the.grid <- etrsGrid2Spdf(the.etrs.grid)

    # Dimensionally Extended Nine-Intersection Model (DE-9IM)
    the.grid <- the.grid[the.surface,]

    cell.names <-
      names(which(gArea(
        gIntersection(
          the.grid,the.surface,byid = TRUE,drop_lower_td = TRUE
        ),byid = TRUE
      ) >= (cell.size ^ 2) / 2))
    if (length(cell.names) == 0)
      stop("Max area method has no results for the specific cell size")
    cell.names <-
      cbind(
        CELLCODE = sapply(strsplit(cell.names,split = " "), function(x)
          x[1]),FEATURE = sapply(strsplit(cell.names,split = " "), function(x)
            x[2])
      )

    rownames(cell.names) <- cell.names[,1]
    #cell.names <- cell.names[,2]
    cell.names
    featured <-
      the.etrs.grid[which(the.etrs.grid@data$CELLCODE %in% cell.names[,1]),]
    featured[["FEATURE"]] <- cell.names[,"FEATURE"]
    featured

  }



# PropCal -----------------------------------------------------------------
# Documentation -----------------------------------------------------------
#Intersection (method 2 countable)
#
# 2. Proportional calculation: the cell takes a calculated value depending on the values
#of the units falling inside and their share within the cell. This method seems very
# appropriate for countable variables.
# Cell value = Σ ( V i * Share i ) V i = Value of unit i, Share i = Share of unit i within the cell
#' Computes a single figure by each reference grid cell using Proportional calculation as integration methods
#'
#' @description Proportional calculation: the cell takes a calculated value depending on the values of the units falling inside and their share within the cell. This method seems very appropriate for countable variables.
#' Cell value = Σ ( V i * Share i ) V i = Value of unit i, Share i = Share of unit i within the cell
#'
#' @param the.etrs.grid An object of the class EtrsGrid
#' @param the.surface An object of class EtrsSurfaface
#'
#' @return An EtrsSurface
#' @export etrsPropValue
#' @examples
#'
#' x <- round(runif(1, min = 4321000, max = 4322000), 1)
#' y <- round(runif(1, min = 3211000, max = 3212000), 1)
#' xy <- cbind(x, y)
#' size = round(runif(1,min = 1500,max = 1500),0)
#' pl1 <-Polygons(list(Polygon(cbind(c(x,x + size,x + size,x,x),c(y,y,y + size,y + size,y)   ))),round(runif(1,min = 1,max = 100),0))
#' x <- x + size
#' pl2 <- Polygons(list(Polygon(cbind(c(x,x + size,x + size,x,x),c(y,y,y + 2 * size,y + 2 * size,y)))),round(runif(1,min = 1,max = 100),0))
#' x<-x-size
#' y<-y+size
#' pl3 <-Polygons(list(Polygon(cbind(c(x,x + size,x + size,x,x),c(y,y,y +  size,y +  size,y)   ))),round(runif(1,min = 1,max = 100),0))
#' sps <- SpatialPolygons(list(pl1,pl2,pl3),proj4string = CRS("+init=epsg:2100"))
#' df <-data.frame(val=c("R5","R40","R80"),row.names = sapply(slot(sps, "polygons"), function(x) slot(x, "ID")),VALUE=c(20,10,50))
#' Source.Surface <-SpatialPolygonsDataFrame(sps,data = df)
#' Source.Surface.propcal<-etrsSurface(input.surface = Source.Surface,over.method.type = "PropCal",surface.value.col = 2,cell.size = 1000)
#' plot(Source.Surface.propcal,lty=3,lwd=1.2,border=3)
#' plot(EtrsTransform(Source.Surface),add=TRUE)
#' x.y.s.s <-coordinates(EtrsTransform(Source.Surface))
#' x.y.propcal <-coordinates(Source.Surface.propcal)
#' text(x.y.s.s[,1],x.y.s.s[,2],paste("ID=",Source.Surface@data$val),col=4,cex = 1.5)
#' text(x.y.s.s[,1],x.y.s.s[,2]-100,paste("Feature=",rownames(Source.Surface@data)),col=4,cex = 1.2)
#' text(x.y.s.s[,1],x.y.s.s[,2]-200,paste("VALUE =",Source.Surface@data$VALUE),col=4,cex = 1.5)
#' text(x.y.propcal[,1],x.y.propcal[,2]+100,Source.Surface.propcal@data$CELLVALUE,col=3)

# CODE --------------------------------------------------------------------
etrsPropValue <-
  function(the.etrs.grid = "EtrsGrid", the.surface = "SpatialPoygonsDataFrame", surface.value.col =
             "numeric") {
    cell.size = the.etrs.grid@cell.size
    the.grid <- etrsGrid2Spdf(the.etrs.grid)

    # Dimensionally Extended Nine-Intersection Model (DE-9IM)
    the.grid <- the.grid[the.surface,]



    cell.names <-
      sapply(slot(
        gIntersection(
          the.grid,the.surface,byid = TRUE,drop_lower_td = TRUE
        ),"polygons"
      ),function(x)
        slot(x,"ID"))

    cell.share <-
      round(sapply(slot(
        gIntersection(
          the.grid,the.surface,byid = TRUE,drop_lower_td = TRUE
        ),"polygons"
      ),function(x)
        slot(x,"area")) / (cell.size ^ 2),4)

    cell.area <-
      round(sapply(slot(
        gIntersection(
          the.grid,the.surface,byid = TRUE,drop_lower_td = TRUE
        ),"polygons"
      ),function(x)
        slot(x,"area")),4)

    cell.names <-
      cbind(
        CELLCODE = sapply(strsplit(cell.names,split = " "), function(x)
          x[1]),FEATURE = as.character(sapply(strsplit(cell.names,split = " "), function(x)
            x[2])) #added as.character()
      )

    rownames(cell.names) <- cell.names[,"CELLCODE"]

    cell.names <-
      cbind(
        cell.names,AREA = cell.area,CELLSHARE = cell.share,SURVALUE = the.surface@data[surface.value.col][match(cell.names[,"FEATURE"],as.character(rownames(the.surface@data))),] #CHANGED as.character(...)
      ) # add the columname

    cell.names <-
      cbind(cell.names,CELLVALUE = as.numeric(as.character(cell.names[,"CELLSHARE"])) *
              as.numeric(as.character(cell.names[,"SURVALUE"])))



    cell.values <-
      as.matrix(tapply(cell.names[,"CELLVALUE"],cell.names[,"CELLCODE"],function(x)
        sum(as.numeric(as.character(
          x
        )))))
    colnames(cell.values) <- "SUMsΗAREi*Vi"

    surface.detailed.table <-
      merge(cell.names,cell.values,by = 0,all = TRUE)


    saveRDS(object = surface.detailed.table, file = ".surface.detailed.table.rds")

    featured <-
      the.etrs.grid[which(!is.na(match(
        row.names(the.etrs.grid),row.names(cell.values)
      ))),]


    featured@data$CELLVALUE <-
      cell.values[match(row.names(featured@data),rownames(cell.values)),]

    featured

  }

# Etrs (source or Ancillary) surface creation -----------------------------

# Documentation -----------------------------------------------------------

#' Etrs (source or Ancillary) surface creation
#'
#'Depending on each type of indicator or variable to be integrated within the reference grid, a different type of integration should be decided and tested. Besides the method finally chosen to integrate, it is important to highlight that indicator figures given by area unit, e.g. by square kilometre, should be converted considering that each cell has a total area of 1 km 2.
#'
#' @param over.method.type "MaxArea" The Maximum area criteria: the cell takes the value of the unit which covers most of the cell area. It should be a good option for uncountable variables
#' "PropCal" in proportional calculation the cell takes a calculated value depending on the values of the units falling inside and their share within the cell. This method seems very appropriate for countable variables
#' @param surface.value.col the number of colum that keeps  the relative density of a cell with land-cover some type
#' @param cell.size: one of the resolutions of the grid  that is 0.5m, 1m, 2.5m, 5m, 10m, 25m, 50m, 100m, 250m, 500m, 1Km, 2.5Km, 5Km, 10Km, 25Km, 50Km, 100Km
#' @param ...
#'
#' @return an EtrsSurface
#' @export etrsSurface.default
#' @examples
#'
#' testpropcal <- function() {
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
#'   SpatialPolygons(list(pl1,pl2,pl3),proj4string = CRS("+init=epsg:2100"))
#' df <-
#'   data.frame(val=c("R5","R40","R80"),row.names = sapply(slot(sps, "polygons"), function(x)
#'     slot(x, "ID")))
#' sps<-SpatialPolygonsDataFrame(sps,data = df)
#' }
#' Source.Surface <- testpropcal()
#'
#' # Uses the default etrsSurface method
#'Source.Surface.MaxArea <-
#' etrsSurface(input.surface = Source.Surface, over.method.type = "MaxArea", cell.size = 1000)
#' summary(Source.Surface.MaxArea)
#' @seealso  etrsMaxArea, etrsPropValue
#'
etrsSurface.default <-
  function(input.surface = "SpatialPolygonsDataFrame", over.method.type =
             "character", surface.value.col = "numeric", cell.size = "numeric",...) {
    #  tryCatch({
    #       CheckEtrsValidity(input.surface)
    #     },error =  function(err){input.surface<- EtrsTransform(input.surface)
    #       print("changed to ETRS-LEAE if necessary")
    #       return(input.surface)})
    if (!identical(input.surface@proj4string,  CRS("+init=epsg:3035"))) {
      warning("Input surface isn't projected to ETRS-LAEA. Transforming .... ")
      input.surface <- EtrsTransform(input.surface)
    }

    the.etrs.grid <-
      etrsGrid.default(obj = input.surface,cell.size = cell.size)

    if (identical(x = over.method.type,y = "MaxArea")) {
      source.surface <-
        etrsMaxArea(the.etrs.grid = the.etrs.grid,the.surface = input.surface)
      CELLVALUE <- "FEATURE"
    }else{
      if (identical(x = over.method.type,y = "PropCal")) {
        source.surface <-
          etrsPropValue(
            the.etrs.grid = the.etrs.grid,
            the.surface = input.surface,surface.value.col = surface.value.col
          )
        CELLVALUE <- "CELLVALUE"

      }else{
        stop("Provided method not Valid. Valid methods are 'MaxArea' or 'PropCal'")
      }
    }

    new(
      "EtrsSurface",
      source.surface,
      over.method.type = over.method.type,
      cell.size = cell.size,
      CELLVALUE = CELLVALUE
    )
  }


if (!isGeneric("etrsSurface")) {
  setGeneric(
    name = "etrsSurface",
    def = function(input.surface,over.method.type,surface.value.col,cell.size,...)
      standardGeneric("etrsSurface")
  )
}else{
  stop("You have to remove the etrsSurface generic before you displace it")
}

# Method etrsSurface ---------------------------------------------------------
# Documentation -----------------------------------------------------------
#' Etrs (source or Ancillary) surface creation
#'
#'Depending on each type of indicator or variable to be integrated within the reference grid, a different type of integration should be decided and tested. Besides the method finally chosen to integrate, it is important to highlight that indicator figures given by area unit, e.g. by square kilometre, should be converted considering that each cell has a total area of 1 km 2.
#'
#'
#' @param over.method.type "PropCal" in proportional calculation the cell takes a calculated value depending on the values of the units falling inside and their share within the cell. This method seems very appropriate for countable variables
#' @param surface.value.col the number of colum that keeps  the relative density of a cell with land-cover some type
#' @param cell.size: one of the resolutions of the grid  that is 0.5m, 1m, 2.5m, 5m, 10m, 25m, 50m, 100m, 250m, 500m, 1Km, 2.5Km, 5Km, 10Km, 25Km, 50Km, 100Km
#' @return an EtrsSurface
#' @export etrsSurface
#'
#' @seealso etrsSurface.default, etrsMaxArea, etrsPropCal
# CODE --------------------------------------------------------------------
setMethod(
  f = "etrsSurface",signature = signature(
    input.surface = "SpatialPolygonsDataFrame",over.method.type = "character",surface.value.col = "numeric",cell.size = "numeric"
  ),etrsSurface.default
)
# Documentation -----------------------------------------------------------

#' Etrs (source or Ancillary) surface creation
#'
#'Depending on each type of indicator or variable to be integrated within the reference grid, a different type of integration should be decided and tested. Besides the method finally chosen to integrate, it is important to highlight that indicator figures given by area unit, e.g. by square kilometre, should be converted considering that each cell has a total area of 1 km 2.
#'
#' @param over.method.type "PropCal" in proportional calculation the cell takes a calculated value depending on the values of the units falling inside and their share within the cell. This method seems very appropriate for countable variables
#' @param surface.value.col the number of colum that keeps  the relative density of a cell with land-cover some type
#' @param cell.size: one of the resolutions of the grid  that is 0.5m, 1m, 2.5m, 5m, 10m, 25m, 50m, 100m, 250m, 500m, 1Km, 2.5Km, 5Km, 10Km, 25Km, 50Km, 100Km
#'
#' @return
#' @export etrsSurface
#' @seealso etrsSurface.default, etrsMaxArea, etrsPropCal
# CODE --------------------------------------------------------------------
setMethod(
  f = "etrsSurface",
  signature = signature(
    input.surface = "SpatialPolygonsDataFrame",over.method.type = "missing",surface.value = "missing",cell.size = "numeric"
  ),
  definition = function(input.surface,cell.size,...) {
    if (!identical(input.surface@proj4string,  CRS("+init=epsg:3035"))) {
      warning("Input surface isn't projected to ETRS-LAEA. Transforming .... ")
      input.surface <- EtrsTransform(input.surface)
    }
    over.method.type = "MaxArea"
    the.etrs.grid <-
      etrsGrid.default(obj = input.surface,cell.size = cell.size)

    source.surface <-
      etrsMaxArea(the.etrs.grid = the.etrs.grid,the.surface = input.surface)
    CELLVALUE <- "FEATURE"

    new(
      "EtrsSurface",
      source.surface,
      over.method.type = over.method.type,
      cell.size = cell.size,
      CELLVALUE = CELLVALUE
    )

  }
)
# Documentation -----------------------------------------------------------


#' Etrs (source or Ancillary) surface creation
#'
#'Depending on each type of indicator or variable to be integrated within the reference grid, a different type of integration should be decided and tested. Besides the method finally chosen to integrate, it is important to highlight that indicator figures given by area unit, e.g. by square kilometre, should be converted considering that each cell has a total area of 1 km 2.
#'
#' @param over.method.type "PropCal" in proportional calculation the cell takes a calculated value depending on the values of the units falling inside and their share within the cell. This method seems very appropriate for countable variables
#' @param surface.value.col the number of colum that keeps  the relative density of a cell with land-cover some type
#' @param cell.size: one of the resolutions of the grid  that is 0.5m, 1m, 2.5m, 5m, 10m, 25m, 50m, 100m, 250m, 500m, 1Km, 2.5Km, 5Km, 10Km, 25Km, 50Km, 100Km
#' @return EtrsSurface
#' @export
#' @seealso etrsSurface.default, etrsMaxArea, etrsPropCal
# CODE --------------------------------------------------------------------
setMethod(
  f = "etrsSurface",
  signature(
    input.surface = "SpatialPolygonsDataFrame", over.method.type = "ANY", surface.value = "missing", cell.size = "numeric"
  ),
  definition = function(input.surface,over.method.type,cell.size,...) {
    if (!is.character(over.method.type))
      stop("Only 'MaxArea' or 'PropCal' is valid over type methods")
    if (!(over.method.type %in% "MaxArea"))
      stop("surface.value is missing")

    etrsSurface(input.surface = input.surface, cell.size = cell.size)
  }
)

#' Simple tool to join etrsSurface with DataFrame
#'
#'
#' @param the.surface An input Surface
#' @param the.EtrsSurface an EtrsSurface object
#'
#' @return an EtrsSurface
#' @export joinMaxAreaSurfaceDataFrames
#'
joinMaxAreaSurfaceDataFrames <-
  function(the.surface = "SpatialPolygonsDataFrame",the.EtrsSurface = "EtrsSurface")
  {
    the.EtrsSurface@data <-
      merge(the.EtrsSurface@data,the.surface@data,by.x = "FEATURE",by.y = 0)

   row.names(the.EtrsSurface@data)<-the.EtrsSurface@data$CELLCODE
   the.EtrsSurface
  }


# etrsSurface2Spatial -----------------------------------------------------


#' Simple tool to convert a etrsSurface Object to Spatial
#'
#' @param the.etrs.surface
#'
#' @return SpatialPolygonsDataFrame
#' @export etrsSurface2Spdf
#'
etrsSurface2Spdf <- function(the.etrs.surface = "EtrsSurface") {
  sP <-
    SpatialPolygons(Srl = the.etrs.surface@polygons,proj4string = the.etrs.surface@proj4string)
  the.surface <-
    SpatialPolygonsDataFrame(sP,data = the.etrs.surface@data)
  the.surface
}


if (!isGeneric("etrsSurfacePar")) {
  setGeneric(
    name = "etrsSurfacePar",
    def = function(input.surface,over.method.type,surface.value.col,cell.size)
      standardGeneric("etrsSurfacePar")
  )
}else{
  stop("You have to remove the etrsSurfacePar generic before you displace it")
}
# Documentation -----------------------------------------------------------
#' Etrs (source or Ancillary) surface creation (Parrarel)
#'
#'Depending on each type of indicator or variable to be integrated within the reference grid, a different type of integration should be decided and tested. Besides the method finally chosen to integrate, it is important to highlight that indicator figures given by area unit, e.g. by square kilometre, should be converted considering that each cell has a total area of 1 km 2.
#' plush Parallel computation
#' Single Instruction, Multiple Data (SIMD)
#' Parallel functions within an R script
#' starts on single processor
#' runs looped elements on multiple(total system cpus - 1) 'slave' processors
#' returns results of all iterations to the original instance
#' @param over.method.type "MaxArea" The Maximum area criteria: the cell takes the value of the unit which covers most of the cell area. It should be a good option for uncountable variables
#' "PropCal" in proportional calculation the cell takes a calculated value depending on the values of the units falling inside and their share within the cell. This method seems very appropriate for countable variables
#' @param surface.value.col the number of colum that keeps  the relative density of a cell with land-cover some type
#' @param cell.size: one of the resolutions of the grid  that is 0.5m, 1m, 2.5m, 5m, 10m, 25m, 50m, 100m, 250m, 500m, 1Km, 2.5Km, 5Km, 10Km, 25Km, 50Km, 100Km
#' @param ...
#'
#' @return an EtrsSurface
#' @export etrsSurfacePar
#' @examples

# CODE --------------------------------------------------------------------
setMethod(
  f = "etrsSurfacePar",
  signature(
    input.surface = "SpatialPolygonsDataFrame", over.method.type = "character", surface.value.col = "missing", cell.size = "numeric"
  ),
  definition = function(input.surface,over.method.type,surface.value.col,cell.size) {
    if (!is.character(over.method.type))
      stop("Only 'MaxArea' or 'PropCal' is valid over type methods")
    if (!(over.method.type %in% "MaxArea"))
      stop("surface.value is missing")

     no_cores <- detectCores() - 1
    cl <- makeCluster(no_cores,type = "FORK")
    registerDoParallel(cores = cl)
    surface <-
      foreach(
        i = 1:length(input.surface),.combine = 'rbind', .packages = c('sp', 'DasyMapR')
      ) %dopar% {
        part.surface <- input.surface[i,]
        etrsSurface.default(
          input.surface = part.surface,over.method.type = over.method.type,surface.value.col = surface.value.col  ,cell.size = cell.size
        )
      }
    stopCluster(cl)
    new(
      "EtrsSurface",
      surface,
      over.method.type = over.method.type,
      cell.size = cell.size,
      CELLVALUE = "FEATURED"
    )

  }
)


#' etrsSurfacePar
#'
#' @param input.surface SpatialPolygonsDataFrame.
#' @param over.method.type ANY.
#' @param surface.value.col numeric.
#' @param cell.size numeric.
#'
#' @return an EtrsSurface
#' @export etrsSurfacePar
#'
#' @examples
setMethod(
  f = "etrsSurfacePar",
  signature(
    input.surface = "SpatialPolygonsDataFrame", over.method.type = "ANY", surface.value.col = "numeric", cell.size = "numeric"
  ),
  definition = function(input.surface,over.method.type,surface.value.col,cell.size) {
    if (!is.character(over.method.type))
      stop("Only 'MaxArea' or 'PropCal' is valid over type methods")


    no_cores <- detectCores() - 1
    cl <- makeCluster(no_cores,type = "FORK")
    registerDoParallel(cores = cl)


    rbind.SPDF <- function(...,makeUniqueIDs = TRUE) {
      dots = list(...)
      names(dots) <- NULL
      lst = lapply(dots, function(x)
        as(x, "SpatialPolygons"))
      lst$makeUniqueIDs = makeUniqueIDs
      pl = do.call(rbind.SpatialPolygons, lst)
      df = do.call(rbind, lapply(dots, function(x)
        x@data))
      SpatialPolygonsDataFrame(pl, df)

    }

    surface <-
      foreach(
        i = 1:length(input.surface),.combine = 'rbind.SPDF',.packages = c('sp','rgeos', 'DasyMapR')
      ) %dopar% {
        part.surface <- input.surface[i,]
        etrsSurface.default(
          input.surface = part.surface,over.method.type = over.method.type,surface.value.col = surface.value.col  ,cell.size = cell.size
        )
      }



    pl <-
      foreach(
        i = 1:length(surface),.combine = 'rbind',.packages = 'sp'
      ) %dopar% {
        srf <- surface[i,]
        as(srf, "SpatialPolygons")
      }

    surface@data$CELLCODE<-as.character(surface@data$CELLCODE)
    #surface@data$CELLCODE<-as.character(surface@data$EASTOFORIGIN)
    #surface@data$CELLCODE<-as.character(surface@data$CELLCODE)
    df <- split(surface@data, surface@data[,"CELLCODE"])

    surface@data <-
      foreach(i = df, .combine = 'rbind') %dopar% {
        aggregate(i[,"CELLVALUE"],list(
          CELLCODES=i[,"CELLCODE"],EASTOFORIGIN = i[,"EASTOFORIGIN"],NORTHOFORIGIN =
            i[,"NORTHOFORIGIN"]
        ),sum,na.rm = TRUE)
      }

    row.names(surface@data)<-surface@data[,"CELLCODES"]


    pl <-
      pl[which(!is.na(match(
        row.names(pl),row.names(surface@data)
      ))),]


    stopCluster(cl)


    surface <- SpatialPolygonsDataFrame(pl,surface@data)

    surface

    new(
      "EtrsSurface",
      surface,
      over.method.type = over.method.type,
      cell.size = cell.size,
      CELLVALUE = "SURVALUE"
    )
  }

)
