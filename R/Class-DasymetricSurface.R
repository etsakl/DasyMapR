#' The DasymetricSurface class
#'
#' @description
#' The DasyemetricSurface class holds the basic information for dasymetric surface represanation in ETRS-LAEA grid form
#' @slot  SpatialPolygonsDataFrame. the input surface plus new data columns
#' @slot over.method.type character. MaxArea for categorical data PropCal for numeric #'values
#' @slot cell.size numeric. Indexing the column of data frame that contains the value of #'interest
#' @slot CELLVALUE character. The size of the cell (the new map unit)
#' @return An DasymetricSurface Object
#'
#' @examples
#' # test Dasymetric  -------------------------------------------------------
#' # Make a spatial polygon
#' S<-readWKT("POLYGON((0 0,2000 0,2000 2000,0 2000,0 0))",id = "S",p4s = CRS("+init=epsg:3035"))
#' df<-cbind(AREA=gArea(S),VALUE=1000)
#' row.names(df)<-sapply(slot(S,"polygons"),function(x) slot(x,"ID"))
#' df<-as.data.frame(df)
#' S<-SpatialPolygonsDataFrame(S,data = df,match.ID = TRUE)
#' # Covert actuall value to density
#' S<-ActuallVal2Density(input.surface = S,surface.value.col = 2,area.unit = 1e+06)
#' # plot
#' X11(width=12,height = 12)
#' split.screen(figs = c(3,2))
#' screen(1)
#' plot(S)
#' title("the input surface")
#' text(coordinates(S)[,1],coordinates(S)[,2],S[["VALUE"]])
#' text(coordinates(S)[,1],coordinates(S)[,2],S[["VALUE"]])
#' #grided
#' S.grided <- etrsSourceSurface(input.surface = S,over.method.type = "PropCal",surface.value.col = 2,cell.size = 500)
#' screen(2)
#' plot(S.grided)
#' title("... Projected to ETRS-LAEA 500 m")
#' text(coordinates(S.grided)[,1],coordinates(S.grided)[,2],S.grided[["CELLVALUE"]])
#'
#' #the ancillary surface
#' A<-readWKT("POLYGON((0 0,1000 0,1000 500,0 500,0 0))",id="A" ,p4s = CRS("+init=epsg:3035" ))
#' df<-cbind(AREA=gArea(A),RelDens=1)
#' row.names(df)<-sapply(slot(A,"polygons"),function(x) slot(x,"ID"))
#' df<-as.data.frame(df)
#' A<-SpatialPolygonsDataFrame(A,data = df,match.ID = TRUE)
#' screen(3)
#' plot(S,border="lightgrey")
#' plot(A,add=T)
#' title("the ancillary surface")
#' text(coordinates(A)[,1],coordinates(A)[,2],A[["RelDens"]])
#' #grided
#' A.grided <- etrsAncillarySurface(input.surface = A,over.method.type = "PropCal",surface.value.col = 2,cell.size = 500,binary = FALSE)
#' screen(4)
#' plot(S.grided,border="lightgrey")
#' plot(A.grided,add=T)
#' title("... Projected to ETRS-LAEA 500 m")
#' text(coordinates(A.grided)[,1],coordinates(A.grided)[,2],A.grided[["WCELLWEIGHT"]])
#' #EtrsDasymetric
#' screen(6)
#' D.grided<-EtrsDasymetricSurface(input.surface.grided = S.grided,ancillary.grided = A.grided,actuall.value = TRUE)
#' plot(S.grided, border = "lightgrey")
#' plot(D.grided,add=T)
#' title("Finally the Dasymetric Surface")
#' text(coordinates(D.grided)[,1],coordinates(D.grided)[,2],D.grided[["DASYCELL"]])
#' @include Class-EtrsSurface.R
#' @include Class-EtrsSourceSurface.R
#' @include Class-EtrsAncillarySurface.R
#' @export EtrsDasymetricSurface
EtrsDasymetricSurface <- setClass(
  "EtrsDasymetricSurface",representation(
    "EtrsSurface"
  ),
  validity <-
    function(obj) {

    }
)

#' Proportional and weighted calculation
#'
#' @description the cell takes also a proportionally
#' calculated value, but this value is weighted for each cell, according to an external
#' variable (e.g. population). This method can be applied to improve the territorial
#' distribution of a socioeconomic indicator.
#' Cell value = W c Σ ( V i * Share i )
#' V i = Value of unit i
#' Share i = Share of unit i within the cell,
#'  W c = weight assigned to cell
#'
#' @param etrs.grided.source.sur EtrsSourceSurface
#' @param ancillary.grided  an EtrsAncillarySurface
#' @return a EtrsDasymetricSurface
#' @export etrsPropWeightedValue
#' @include Class-EtrsCells.R
#' @include Class-EtrsGrid.R
#' @example
#'
etrsPropWeightedValue <-function(input.surface.grided = "EtrsSourceSurface",ancillary.grided="EtrsAncillarySurface" ) {

    cell.size<-input.surface.grided@cell.size
    etrs.cell.codes.columns<-input.surface.grided@etrs.cell.codes.columns

    input.surface <- etrsSurface2Spdf(input.surface.grided)
    anc.sp <- etrsSurface2Spdf(ancillary.grided)
    cell.size <- ancillary.grided@cell.size


    cell.names.u <-
      sapply(slot(
        gIntersection(
          input.surface,anc.sp,byid = TRUE,drop_lower_td = TRUE
        ),"polygons"
      ),function(x)
        slot(x,"ID"))

    cell.names <-
      cbind(
        CELLCODE = sapply(strsplit(cell.names.u,split = " "), function(x)
          x[1]),FEATURE = sapply(strsplit(cell.names.u,split = " "), function(x)
            x[2])
      )

    input.surface.grided <-
      input.surface.grided[which(!is.na(match(
        row.names(input.surface.grided),cell.names[,"CELLCODE"]
      ))),]

    ancillary.grided <-
      ancillary.grided[which(!is.na(match(
        row.names(ancillary.grided),cell.names[,"CELLCODE"]
      ))),]

    input.surface.grided <-
      sp::merge(
        input.surface.grided,ancillary.grided@data,by = 0,all= FALSE #c("CELLCODE","EASTOFORIGIN","NORTHOFORGIN")
      )


    input.surface.grided[["DASYCELL"]] <- input.surface.grided[["CELLVALUE"]]*input.surface.grided[["WCELLWEIGHT"]]


    input.surface.grided<-na.exclude(input.surface.grided[,c(2,3,4,5,9,10)])
    colnames(input.surface.grided@data)<-c("CELLCODE","EASTOFORIGIN","NORTHOFORIGIN","CELLVALUE","WCELLWEIGHT","DASYCELL")


    new("EtrsDasymetricSurface",
        input.surface.grided,
        over.method.type = "Dasymetric",
        cell.size = cell.size,
        CELLVALUE = "DASYCELL",
        etrs.cell.codes.columns = etrs.cell.codes.columns
    )

  }

#' Grid Based Dasymetric Mapping
#'
#' @description
#' P_mu = (R_A x P_A ) x N / E
#' P_cell = (R_A x P_A /P_A ) x (N / A_T ) / E = (R_A x N / A_T ) / E
#' Where,
#'   P_cell is the population of a cell,
#'   R_A is the relative density of a cell with land-cover type A,
#'   P_A is the proportion of cells of land-cover type A in the
#'   enumeration unit.
#'   N is the actual population of enumeration unit
#'   (i.e., census block B group)
#'   E is the expected population of enumeration unit calculated
#'   using the relative densities.
#'   A_T is the total number of cells in the enumeration unit.
#'   P_A / P_A cancels P_A out of the equation, i.e., not used in the cell-
#'     based method.
#' @param input.surface.grided
#' @param ancillary.grided
#'
#' @return EtrsDasymetricSurface
#' @example
#'
#' @include Class-EtrsSurface.R
#' @export
EtrsDasymetricSurface <-
  function(input.surface.grided = "EtrsSourceSurface", ancillary.grided = "EtrsAncillarySurface" # ,actuall.value = TRUE
           ,...) {

    cell.size<-input.surface.grided@cell.size
    etrs.cell.codes.columns<-input.surface.grided@etrs.cell.codes.columns

    input.surface <- etrsSurface2Spdf(input.surface.grided)
    anc.sp <- etrsSurface2Spdf(ancillary.grided)
    cell.size <- ancillary.grided@cell.size

    #   N is the actual population of enumeration unit
    #   Population and Population Density
    #   Popu. Density = Popu. / Area
    #   Popu. = Popu. Density x Area

    # given CELLVALUE as literal
#--- 15-5 comment this
    #
    #     if (actuall.value) {
#       N <- sum(input.surface@data$CELLVALUE)
#     }else{
#       #rates, densities and proportions
    N <- sum(input.surface@data$CELLVALUE * cell.size ^ 2)
#     }
#----
    #   The code  combines  enumeration units with an ancillary dataset to create an output  with values that correspond to unique combinations of enumeration units and ancillary classes



       cell.names.u <-
      sapply(slot(
        gIntersection(
          input.surface,anc.sp,byid = TRUE,drop_lower_td = TRUE
        ),"polygons"
      ),function(x)
        slot(x,"ID"))

    cell.names <-
      cbind(
        CELLCODE = sapply(strsplit(cell.names.u,split = " "), function(x)
          x[1]),FEATURE = sapply(strsplit(cell.names.u,split = " "), function(x)
            x[2])
      )


    # A_T is the total number of cells in the enumeration unit.

    A_T <- length(cell.names.u)

    # E is the expected population of enumeration unit calculated
    # using the relative densities.

    input.surface.grided <-
      input.surface.grided[which(!is.na(match(
        row.names(input.surface.grided),cell.names[,"CELLCODE"]
      ))),]

    ancillary.grided <-
      ancillary.grided[which(!is.na(match(
        row.names(ancillary.grided),cell.names[,"CELLCODE"]
      ))),]

    input.surface.grided <-
      sp::merge(
        input.surface.grided,ancillary.grided@data,by = 0,all= FALSE #c("CELLCODE","EASTOFORIGIN","NORTHOFORGIN")
      )


      input.surface.grided[["DASYCELL"]] <- input.surface.grided[["CELLVALUE"]]* length(input.surface.grided) / A_T

      E <- sum( na.exclude(input.surface.grided[["DASYCELL"]] * input.surface.grided[["WCELLWEIGHT"]]*cell.size^2))

      input.surface.grided[["DASYCELL"]] <-  input.surface.grided[["WCELLWEIGHT"]]* input.surface.grided[["DASYCELL"]] * N/E

input.surface.grided<-na.exclude(input.surface.grided[,c(2,3,4,5,9,10)])
colnames(input.surface.grided@data)<-c("CELLCODE","EASTOFORIGIN","NORTHOFORIGIN","CELLVALUE","WCELLWEIGHT","DASYCELL")


    new("EtrsDasymetricSurface",
         input.surface.grided,
         over.method.type = "Dasymetric",
         cell.size = cell.size,
         CELLVALUE = "DASYCELL",
         etrs.cell.codes.columns = etrs.cell.codes.columns
        )


  }

if (!isGeneric("EtrsDasymetricSurface")) {
  setGeneric(
    name = "EtrsDasymetricSurface",
    def = function(input.surface.grided, ancillary.grided #,actuall.value
                   ,...)
      standardGeneric("EtrsDasymetricSurface")
  )
}else{
  stop("You have to remove the EtrsDasymetricSurface generic before you displace it")
}


#' EtrsDasymetricSurface method
#'
#' @param input.surface.grided EtrsSourceSurface.
#' @param ancillary.grided EtrsAncillarySurface.
#' @param actuall.value logical.
#'
#' @return
#' @export
#'
#' @examples
#' @include Class-EtrsSurface.R
setMethod(
  "EtrsDasymetricSurface",signature = signature(
    input.surface.grided = "EtrsSourceSurface", ancillary.grided = "EtrsAncillarySurface" #,actuall.value = "logical"
  ),EtrsDasymetricSurface
)

if(!isGeneric("etrsDasymetric2Ancillary")){
  setGeneric(
    name = "etrsDasymetric2Ancillary",
    def=function(dasymetric.surface)
      standardGeneric("etrsDasymetric2Ancillary")
  )
}else{
  stop("You have to remove the etrsDasymetric2Ancillary generic before you displace it")
}


#' A method to convert a DasyMetric surface to Ancillary
#'
#' @param dasymetric.surface EtrsDasymetricSurface.
#'
#' @return EtrsAncillarySurface
#' @export
#'
#' @examples
#'
setMethod(f="etrsDasymetric2Ancillary",
          signature = signature(dasymetric.surface="EtrsDasymetricSurface"),
          definition = function(dasymetric.surface){

            dasymetric.surface@data<-dasymetric.surface@data[,c("CELLCODE","EASTOFORIGIN","NORTHOFORIGIN","DASYCELL")]

            colnames(dasymetric.surface@data)[4] <- "WCELLWEIGHT"

            new("EtrsAncillarySurface",
    dasymetric.surface
    )

          }
          )

if(!isGeneric("etrsDasymetric2Raster")){
  setGeneric(
    name = "etrsDasymetric2Raster",
    def=function(dasymetric.surface){
      standardGeneric("etrsDasymetric2Raster")
      }

 )



}else{
  stop("You have to remove the etrsDasymetric2Raster generic before you displace it")
}


#'
#' A simple method to produce a floating point raster
#'
#' @description
#' @param dasymetric.surface EtrsDasymetricSurface.
#'
#' @return a Raster object and a geo tif file in working directory
#' @export etrsDasymetric2Raster
#'
#' @examples
#'
#' data(DASY_GDP.rda)
#' par(mar = c(0.1, 0.1, 0.1, 0.1))
#' DASY_GPD_RASTER <- etrsDasymetric2Raster(dasymetric.surface = DASY_GPD)
#' rw.colors <- grey.colors
#' image(DASY_GPD_RASTER, col = rw.colors(5))

#' A simple method to produce a rfloating point aster floating point
#'
#' @param dasymetric.surface EtrsDasymetricSurface.
#'
#' @return a Raster object and a geo tif file in working directory
#' @export
#'
#' @examples
#'

#'
setMethod(f="etrsDasymetric2Raster",
          signature = signature(dasymetric.surface="EtrsDasymetricSurface"),
          definition = function(dasymetric.surface){

             r<-raster()
            projection(r)<-DASY_GPD@proj4string
            extent(r)<-bbox(DASY_GPD)
            res(r)<-DASY_GPD@cell.size
            DASY_GPD_RASTER<-rasterize(DASY_GPD,r,field=DASY_GPD@data$DASYCELL)
            rf<-writeRaster(DASY_GPD_RASTER,filename = "DASY_GPD_RASTER.tif",format="GTiff",overwrite=TRUE)
            DASY_GPD_RASTER
          }
)



