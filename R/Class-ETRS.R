#' Virtual Parent Class for ETRS objects
#' @description The ETRS class contains (extends) the Spatial Class. As a result of that has the slots of the parent class : @bbox and proj4string.
#' @slot etrs.cell.codes.columns character: a character vector c("CELLCODE","EASTOFORIGIN","NORTHORIGIN") The Enviromental European Agency suggests to be provided, except "CELLCODE", for computations and analysis the lower left corner coordinate in meters, formatted as two separate integers attribute values named "EOFORIGIN" and "NOFORIGIN"
#' @slot  VIRTUAL.  Actually only to declare the ETRS class abstract
#' @return One cannot create an ETRS instance inasmuch as ETRS is VIRTUAL class
#' @import sp
#' @import methods
#' @export ETRS
ETRS <- setClass(
  Class = "ETRS",
  representation(etrs.cell.codes.columns = "character","VIRTUAL"),

  prototype = prototype(
    etrs.cell.codes.columns = c("CELLCODES","EASTOFORIGIN","NORTHOFORIGIN")
  )
  ,
  validity = function(object) {
    cat("~~~ ETRS validity ~~~\n")
    # Checks EEA shp files combability

    #     if (length(object@etrs.cell.codes.columns) != 3)
    #       stop("To be EEA combatible you need East, North, Code columns")
    #
    #     if (!all(object@etrs.cell.codes.columns %in% names(object@data)))
    #       stop(
    #         "the etrs \"CELLCODES\" \"EASTOFORIGIN\" \"NORTHOFORIGIN\"  columns must
    #         be present in attribute table"
    #       )
  }
  ,


  contains = "Spatial"
)

# generic CheckEtrsValidity-----------------------------------------------------------------

if (!isGeneric("CheckEtrsValidity")) {
  setGeneric(
    name = "CheckEtrsValidity", def = function(object) {
      standardGeneric("CheckEtrsValidity")
    }
  )

}else{
  stop("You have to remove the CheckEtrsValidity generic before you displace it")
}

# method -CheckEtrsValidity-----------------------------------------------------------------

#' Checks the ETRS validity of a Spatial Object
#' @description Checks the ETRS validity of a Spatial Object
#' \code{CheckEtrsValidity()} actually checks if the given object is Spatial
#' and also if it is projected in ETRS-LAEA
#' @param ETRS : Accepts as an argument a ETRS object
#'
#' @return TRUE if everything is ok
#' @export CheckEtrsValidity
#'
#' @examples
#' xc <- round(runif(10, min = 4321000, max = 4323000), 2)
#' yc <- round(runif(10, min = 3210000, max = 3212000), 2)
#' xy <- cbind(xc, yc)
#' # a SpatialPoints object
#' xy.sp <- SpatialPoints(xy, proj4string = CRS("+init=epsg:2100"))
#' # CheckEtrsValidity(xy.sp)
#' xy.sp@proj4string<-CRS("+init=epsg:3035")
#' CheckEtrsValidity(xy.sp)
setMethod(
  f = "CheckEtrsValidity",
  signature = "Spatial",
  definition = function(object) {
    # Only spatial objects have CRS
    if (!is(object,"Spatial"))
      stop("Only Saptial objects are valid")

    # Which must be ETRS89-LAEA
    if (!identical(object@proj4string,  CRS("+init=epsg:3035")))
      stop(
        "Your Spatial object must be projected to ETRS89-LAEA. Please try etrsTransform() function and try again"
      )

    return(TRUE)
  }

)


# generic CheckEtrsResolution-----------------------------------------------------------------


if (!isGeneric("CheckEtrsResolution")) {
  setGeneric(
    name = "CheckEtrsResolution", def = function(cell.size) {
      standardGeneric("CheckEtrsResolution")
    }
  )
}else{
  stop("You have to remove the CheckEtrsResolution generic before you displace it")
}


# method CheckEtrsResolution------------------------------------------------------------------
#' Checks if the given resolution is INSPIRED compatible
#'
#' @description  The grid is defined as a hierarchical one in metric coordinates to the power of 10.The resolution of the grid is 1m, 10m, 100m, 1000m, 10,000m, 100,000m. Althought EEA recommend grid size of metric resolution in standard size 100 m, 1 km, 10 km and 100 km. Alternatively use 25 m or 250 m for analysis purposes, where standard 100 m or 1 km grid size is not appropriate.
#' @param cell.size numeric one of the resolutions of the grid that are 0.5m, 1m, 2.5m, 5m, 10m, 25m, 50m, 100m, 250m, 500m, 1Km, 2.5Km, 5Km, 10Km, 25Km, 50Km, 100Km
#'
#' @return TRUE if everything is ok if it is not returns 100km cell-size based grid
#' @export CheckEtrsResolution
#'
#' @examples
#'
#' CheckEtrsResolution(cell.size=1000)
#' CheckEtrsResolution(cell.size=125)
#'
setMethod(
  f = "CheckEtrsResolution",
  signature(cell.size = "numeric"),
  definition = function(cell.size) {
    # EEA 2008 compatible (10^n / 2^m) n=c(0:5) and m=c(c0:2)
    primary.level <- c(0:5)
    quadtree.level <- c(0:2)
    quadtree.level.row <- rep(quadtree.level,length(primary.level))
    primary.level.row <-
      rep(primary.level,each = length(quadtree.level))
    value.resolution <-
      sort((10 ^ primary.level.row) / (2 ^ quadtree.level.row))
    # geoagraphical grid of 0.25m, 05m not recomented from EEA so are excluded
    value.resolution <-
      value.resolution[!value.resolution %in% value.resolution[1:2]]

    # check if cell size is EEA valid
    if (!is.element(cell.size,value.resolution)) {
      warnings("This is not valid cell size. continuing with 10km cell size")
      cell.size <- 100000
    }
    return(TRUE)
  }
)
# generic EtrsTableCodes-----------------------------------------------------------------

if (!isGeneric("EtrsTableCodes")) {
  setGeneric(
    name = "EtrsTableCodes",def = function(eastings.northings,cell.size) {
      standardGeneric("EtrsTableCodes")
    }
  )
}else{
  stop("You have to remove EtrsTableCodes generic before you replace it")
}

#  EtrsTableCodes -------------------------------------------------

#' A method that produces a data frame of cell codes and lower corner easting
#' northings
#'
#' @param eastings.northings a matrix populated with lower corner grid coordinates
#' @param cell.size: one of the resolutions of the grid  that is 0.5m, 1m, 2.5m, 5m, 10m, 25m, 50m, 100m, 250m, 500m, 1Km, 2.5Km, 5Km, 10Km, 25Km, 50Km, 100Km
#'
#' @return dataframe
#' @export EtrsTableCodes
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
#'df<-EtrsTableCodes(eastings.northings,1000)
#'df
setMethod(
  "EtrsTableCodes",
  signature(eastings.northings = "matrix",cell.size =
              "numeric"),
  definition = function(eastings.northings, cell.size) {
    # Call the cell code identifier for ETRS
    etrs.cell.code <-
      EtrsCellCodes(eastings.northings,cell.size)
    etrs.table.cell.codes <-
      as.data.frame(
        cbind(
          CELLCODE = etrs.cell.code,EASTOFORIGIN = eastings.northings[,1],NORTHOFORIGIN =
            as.numeric(as.character(eastings.northings[,2]))
        ),stringAsfactor = FALSE
      )
    rownames(etrs.table.cell.codes) <- etrs.cell.code
    etrs.table.cell.codes
  }
)

# generic EtrsCheckCodeColumns --------------------------------------------
if (!isGeneric("EtrsCheckCodeColumns")) {
  setGeneric(
    name = "EtrsCheckCodeColumns", def = function(object) {
      standardGeneric("EtrsCheckCodeColumns")
    }
  )
}else{
  stop()
}
#' checks if colum names data are etrs table.codes
#'
#' Is the slot etrs.table.cell.code identical to data colum names?
#' @param Etrs an etrs object
#'
#' @export EtrsCheckCodeColumns
#' @examples
#'
setMethod(
  "EtrsCheckCodeColumns",
  signature("ETRS") ,
  definition =   function(object) {
    # check if the codes in data frame is etrs codes
    # Call EtrsTableCodes
    etrs.table.cell.codes <- EtrsTableCodes(object)
    if (!object@data %in% etrs.table.cell.codes)
      stop("Not Valid ETRS codes")
    return(TRUE)
  }
)

# generic -----------------------------------------------------------------
if (!isGeneric("EtrsCellCodes")) {
  setGeneric(
    name = "EtrsCellCodes", def = function(eastings.northings,cell.size,NE) {
      standardGeneric("EtrsCellCodes")
    }
  )
}else{
  stop("You have to remove the EtrsTableCodes generic before you displace it")
}

# Method ------------------------------------------------------------------


#' A cell code identifier for ETRS
#'
#' Define the cell size prefix
#'
#' @param eastings.northings matrix.
#' @param cell.size numeric.
#'
#' @return etrs cell code vector
#' @export EtrsCellCodes
#' @rdname r-methods
#' @examples
#' bb <- bbox(cbind(c(4321000, 4323000), c(3210000,3212000)))
#' cell.size = 1000
#' # Calculate the limits of the grid
#' bb.outer.limits <-  cbind((floor(bb[, 1] / cell.size)) * cell.size, (ceiling(bb[, 2] / cell.size)) * cell.size)
#' # Calculates eastings
#' eastings <-seq(from = bb.outer.limits[1, 1], to = bb.outer.limits[1, 2], by = cell.size)
#' # Calculates northings
#' northings <-seq(from = bb.outer.limits[2, 1], to = bb.outer.limits[2, 2], by = cell.size)
#' eastings.northings <- as.matrix(expand.grid(eastings, northings))
#' low.corner.cell.etrs.points<-etrsPoints(eastings.northings = eastings.northings,cell.size =1000)
#' EtrsCellCodes<-EtrsCellCodes(eastings.northings,cell.size=1000)
#'  
setMethod(
  f = "EtrsCellCodes",

  signature(
    eastings.northings = "matrix",cell.size = "numeric",NE = "missing"
  ),

  definition = function(eastings.northings,cell.size) {
    # A cell code identifier for ETRS
    # Define the cell size prefix
    prefix <-
      paste("",as(
        ifelse(cell.size >= 1000,cell.size / 1000,cell.size),"character"
      ),as(ifelse(cell.size >= 1000,"km","m"),"character"),sep = "")
    #Identify the number of zeros to remove from Eastings and Northings Values
    nz <-
      as.numeric(unlist(strsplit(as.character(
        format(cell.size,scientific = TRUE)
      ),split = ".e\\+"))[2])
    # Define string value based on Easting
    eaststr <-
      paste("E",as.character(eastings.northings[,1] / 10 ^ nz),sep = "")
    #Define string value based on Northings
    northststr <-
      paste("N",as.character(eastings.northings[,2] / 10 ^ nz),sep = "")
    # Concatenate strings into cell code

    etrs.cell.code <- paste(prefix,eaststr,northststr,sep = "")
    #
    etrs.cell.code
  }
)
#' A cell code identifier for ETRS
#'
#' Define the cell size prefix
#'
#' @param eastings.northings matrix.
#' @param cell.size numeric.
#'
#' @return etrs cell code vector
#' @export EtrsCellCodes
#' @rdname r-methods
#' @examples
#' bb <- bbox(cbind(c(4321000, 4323000), c(3210000,3212000)))
#' cell.size = 1000
#' # Calculate the limits of the grid
#' bb.outer.limits <-  cbind((floor(bb[, 1] / cell.size)) * cell.size, (ceiling(bb[, 2] / cell.size)) * cell.size)
#' # Calculates eastings
#' eastings <-seq(from = bb.outer.limits[1, 1], to = bb.outer.limits[1, 2], by = cell.size)
#' # Calculates northings
#' northings <-seq(from = bb.outer.limits[2, 1], to = bb.outer.limits[2, 2], by = cell.size)
#' eastings.northings <- as.matrix(expand.grid(eastings, northings))
#' low.corner.cell.etrs.points<-etrsPoints(eastings.northings = eastings.northings,cell.size =1000)
#' EtrsCellCodes<-EtrsCellCodes(eastings.northings,cell.size=1000,NE=TRUE)

setMethod(
  f = "EtrsCellCodes",

  signature(
    eastings.northings = "matrix",cell.size = "numeric",NE = "logical"
  ),

  definition = function(eastings.northings,cell.size,NE) {
    # A cell code identifier for ETRS
    # Define the cell size prefix
    prefix <-
      paste("",as(
        ifelse(cell.size >= 1000,cell.size / 1000,cell.size),"character"
      ),as(ifelse(cell.size >= 1000,"km","m"),"character"),sep = "")
    #Identify the number of zeros to remove from Eastings and Northings Values
    nz <-
      as.numeric(unlist(strsplit(as.character(
        format(cell.size,scientific = TRUE)
      ),split = ".e\\+"))[2])
    # Define string value based on Easting
    eaststr <-
      paste("E",as.character(eastings.northings[,1] / 10 ^ nz),sep = "")
    #Define string value based on Northings
    northststr <-
      paste("N",as.character(eastings.northings[,2] / 10 ^ nz),sep = "")
    # Concatenate strings into cell code
   if(!NE){
     etrs.cell.code <- paste(prefix,eaststr,northststr,sep = "")
   }else{

     etrs.cell.code <- paste(prefix,northststr,eaststr,sep = "")
   }
    #
    etrs.cell.code
  }
)


# generic -----------------------------------------------------------------
if (!isGeneric("etrsReverseCellCode")) {
  setGeneric(
    name = "etrsReverseCellCode", def = function(df,cell.code.col) {
      standardGeneric("etrsReverseCellCode")
    }
  )
}else{
  stop("You have to remove the etrsReverseCellCode generic before you displace it")
}

#' Reversing etrs cell codew from resoloution easting northings to res northing eastings
#'
#' @param df .  A data frame that has a Etrs reversed ceel code column
#' @param cell.code.col . Define the column of the reversed cellcode
#' @return a data frame with CELLCODE and ID = CELLCODE
#'  @description It 's possible to find out, out there data not comabatible to Inspire.
#'  so reversin is a option. Such an example the GEOASTAT_grid_EU_POP .... files
#' the method also set the data frame row names as etrs cell codes. As a result of it it can be used to as a spatial etrs grid
#' @export etrsReverseCellCode
#' @example
#'
setMethod(
f="etrsReverseCellCode",
signature = signature(df="data.frame",cell.code.col="numeric"),
  definition=function(df = 'data.frame',cell.code.col = 'numeric') {
    #pr\arallel computing frame settings
    no_cores <- detectCores() - 1
    cl <- makeCluster(no_cores,type = "FORK")
    registerDoParallel(cores = cl)

    #keep the resolution
    res <-
      paste0(unlist(strsplit(df[1,cell.code.col],split = "m"))[1],"m")

    # remove duplicates
    if(any(duplicated(df[,cell.code.col]))) {
      df <- df[which(!duplicated(df[,cell.code.col])),]
      warning("Cell codes not unique... trying to drop duplicates")
    }

     # split the df in equal parts
    div <- seq_len(abs(no_cores))
    splitf <- max(div[nrow(df) %% div == 0L])

     df <- split(df,f = rep_len(1:splitf, nrow(df)))

     df <-
      foreach(
        df.part = df,.combine = 'rbind',.packages = 'base',.inorder = T
      ) %dopar% {
        rcc <-
          paste0(res, as.character(unlist(sapply(sapply(sapply(strsplit(df.part[,cell.code.col],split = "m"),
                                                               function(x)
                                                                 x[2]),
                                                        function(x)
                                                          strsplit(x,split = "E")),
                                                 function(x)
                                                   paste0("E",x[2],x[1])))))


        df.part[,"CELLCODE"] <- rcc
        return(df.part)
      }


    stopCluster(cl)
    row.names(df) <- df[,"CELLCODE"]
    df
  }
)
