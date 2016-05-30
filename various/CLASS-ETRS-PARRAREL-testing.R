
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
#' \dontrun{
#' data("NUTSV9_LEAC")
#' NUTSV9_LEAC_GR25 <- NUTSV9_LEAC[which(!is.na(match(NUTSV9_LEAC[["N3CD"]],"GR25"))),]
#' # The Pararrel version
#' system.time(etrsSurfacePar(NUTSV9_LEAC_GR25,cell.size=10000))
#' # The Serial Computing
#' system.time(etrsSurface(NUTSV9_LEAC_GR25,cell.size=10000))
#'}
#'

# CODE --------------------------------------------------------------------
setMethod(
  f = "etrsSurfacePar",
  signature(
    input.surface = "SpatialPolygonsDataFrame", over.method.type = "ANY", surface.value.col = "missing", cell.size = "numeric"
  ),
  definition = function(input.surface,over.method.type,cell.size) {
    if (!is.character(over.method.type))
      stop("Only 'MaxArea' or 'PropCal' is valid over type methods")
    if (!(over.method.type %in% "MaxArea"))
      stop("surface.value is missing")


    no_cores <- detectCores() - 1
    cl <- makeCluster(no_cores,type = "FORK")
    registerDoParallel(cores = cl)

    etrssurface <-
      foreach(
        i = 1:length(input.surface),.combine = 'rbind',.packages = c('sp', 'DasyMapR')
      ) %dopar% {
        part.surface <- input.surface[i,]
        etrsSurface(input.surface = part.surface, cell.size = cell.size)
      }
    stopCluster(cl)
    etrssurface
  }
)

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
    input.surface = "SpatialPolygonsDataFrame", over.method.type = "character", surface.value.col = "numeric", cell.size = "numeric"
  ),
  definition = function(input.surface,over.method.type,surface.value.col,cell.size) {
    if (!is.character(over.method.type))
      stop("Only 'MaxArea' or 'PropCal' is valid over type methods")
    if (!(over.method.type %in% "MaxArea"))
      stop("surface.value is missing")


    no_cores <- detectCores() - 1
    cl <- makeCluster(no_cores,type = "FORK")
    registerDoParallel(cores = cl)

    etrssurface <-
      foreach(
        i = 1:length(input.surface),.combine = 'rbind',.packages = c('sp', 'DasyMapR')
      ) %dopar% {
        part.surface <- input.surface[i,]
        etrsSurface.default(
          input.surface = part.surface,over.method.type = over.method.type,surface.value.col = surface.value.col  ,cell.size = cell.size
        )
      }
    stopCluster(cl)
    etrssurface
  }
)
