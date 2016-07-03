if (!isGeneric("pntsattr2surface")) {
  setGeneric(
    name = "pntsattr2surface",
    def = function(sppdf, sppntdf) {
      standardGeneric("pntsattr2surface")
    }
  )
} else{
  stop("You have to remove the pntsattr2surface generic before you displace it")
}

#' pntsattr2surface is an ancillary utility for point surface combining attributes
#' @description If the polygon contains 1 point then takes its attributes. if the polygon contains more than 2 points voronoi method of package deldir it's used to subdevide the region. If no point is contained in the region thn NA is provided as values
#' @param sppdf A sptialpolygonsdatframeobject
#' @param sppntdf A spatialpointsdataframe object
#'
#' @return A spattial polygons data frame objec
#' @import deldir
#' @export pntsattr2surface
#'
#' @examples
setMethod(
  f = "pntsattr2surface",

  signature(sppdf = "SpatialPolygonsDataFrame",
            sppntdf = "SpatialPointsDataFrame"),
  definition = function(sppdf,sppntdf) {


    rbind.SPDF <- function(..., makeUniqueIDs = TRUE) {
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

    oria_ota_pccoded <-
      foreach(
        p = 1:length(sppdf),
        .combine = 'rbind.SPDF',
        .packages = c('sp')
      ) %do% {
        p_ota <- sppdf[p, ]
        pnts <- sppntdf[p_ota, ]
        pnts <- pnts[which(!duplicated(coordinates(pnts))), ]

        #If the polygon contains 1 point then takes its attributes
        if (length(pnts) == 1) {
          p_ota@data <- cbind(p_ota@data, pnts@data)
        }

        #if the polygon contains more than 2 points voronoi method of package deldir it's used to subsudevide the region
        #require(deldir)

        if (length(pnts) > 1) {
          bb <- bbox(p_ota)
          rw <- as.numeric(t(bb))
          z <- as.character(pnts@data$POSTCODE)
          vor <-
            deldir(coordinates(pnts)[, 1], coordinates(pnts)[, 2], rw = rw)
          w <- tile.list(vor)
          polys <- vector(mode = 'list', length = length(w))
          require(sp)
          for (i in seq(along = polys)) {
            pcrds <- cbind(w[[i]]$x, w[[i]]$y)
            pcrds <- rbind(pcrds, pcrds[1,])
            polys[[i]] <-
              Polygons(list(Polygon(pcrds)), ID = as.character(i))
          }

          SP <-
            SpatialPolygons(polys, proj4string = CRS("+init = epsg:3035"))
          SP_vor <- gIntersection(SP, p_ota, byid = TRUE)
          rn <- sapply(slot(SP_vor, 'polygons'), function(x)
            slot(x, 'ID'))
          df <- p_ota@data
          df <- df[rep(seq_len(nrow(df)), each = length(SP_vor)),]
          row.names(df) <- rn
          p_ota <- SpatialPolygonsDataFrame(SP_vor, data = df)

          p_ota@data <- cbind(p_ota@data, pnts@data)
        }
        # if no point is contained in the region thn NA is provided as values
        if (length(pnts) == 0) {
          df <- (GeoPC_GR_Places_etrs[1, ])@data
          df[1,] <- NA
          pnts <-
            SpatialPointsDataFrame(
              coords = gCentroid(p_ota),
              data = df,
              proj4string = CRS("+init = epsg:3035")
            )
          p_ota@data <- cbind(p_ota@data, pnts@data)
        }

        p_ota
      }
  }
)
