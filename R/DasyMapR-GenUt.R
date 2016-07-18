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
  definition = function(sppdf, sppntdf) {
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
        p_ota <- sppdf[p,]
        pnts <- sppntdf[p_ota,]
        pnts <- pnts[which(!duplicated(coordinates(pnts))),]

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
            pcrds <- rbind(pcrds, pcrds[1, ])
            polys[[i]] <-
              Polygons(list(Polygon(pcrds)), ID = as.character(i))
          }

          SP <-
            SpatialPolygons(polys, proj4string = CRS("+init=epsg:3035"))
          SP_vor <- gIntersection(SP, p_ota, byid = TRUE)
          rn <- sapply(slot(SP_vor, 'polygons'), function(x)
            slot(x, 'ID'))
          df <- p_ota@data
          df <- df[rep(seq_len(nrow(df)), each = length(SP_vor)), ]
          row.names(df) <- rn
          p_ota <- SpatialPolygonsDataFrame(SP_vor, data = df)

          p_ota@data <- cbind(p_ota@data, pnts@data)
        }
        # if no point is contained in the region thn NA is provided as values
        if (length(pnts) == 0) {
          df <- (sppdf[1,])@data
          df[1, ] <- NA
          pnts <-
            SpatialPointsDataFrame(
              coords = gCentroid(p_ota),
              data = df,
              proj4string = CRS("+init=epsg:3035")
            )
          p_ota@data <- cbind(p_ota@data, pnts@data)
        }

        p_ota
      }
  }
)




# Vignette 2 --------------------------------------------------------------

# dasymapPlot -------------------------------------------------------------

if (!isGeneric("dasymapPlot")) {
  setGeneric(
    name = "dasymapPlot",
    def = function(aEtrsSurface, col.value) {
      standardGeneric("dasymapPlot")
    }
  )
} else{
  stop("You have to remove the dasymapPlot generic before you displace it")
}

#' a utility for plotting EtrsSurface combining attributes
#' @description produce a 5 class choroplethe map for en ETRS surface
#' @param sppdf EtrsSurface
#' @param column of the class value
#'
#' @return a plot
#' @import classInt RColorBrewer
#' @export dasymapPlot
#'
#' @examples
setMethod(
  f = "dasymapPlot",
  signature = signature(aEtrsSurface = "EtrsSurface", col.value = "numeric"),

  definition = function(aEtrsSurface, col.value) {
    srf <- aEtrsSurface
    add.alpha(pal <- brewer.pal(5, "Reds"), .5)
    q5 <-
      classIntervals(srf@data[, col.value],
                     n = 5,
                     style = "fisher",
                     dataPrecision = 0)
    q5Colours <- findColours(q5, pal)
    # srf<-spTransform(srf,CRS("+init=epsg:4326"))
    # bb<-bbox(srf)
    # base.map<-get_stamenmap(bbox = bb,zoom = 9, maptype = "toner-2011",crop = T)
    # image(base.map)
    plot(srf, col = q5Colours, border = NA)
    legend(
      "topleft",
      fill = attr(q5Colours, "palette"),
      legend = names(attr(q5Colours, "table")),
      bty = "n"
    )
  })

  # wgsTransform ----------------------------------------------------------
  if (!isGeneric("wgsTransform")) {
    setGeneric(
      name = "wgsTransform",
      def = function(obj) {
        standardGeneric("wgsTransform")
      }
    )
  } else{
    stop("You have to remove the wgsTransform generic before you displace it")
  }


#' Transforms to wgs crs
#'
#' @description usefull for Google,Osm e.t.c.
#' @param EtrsSurface
#'
#' @return EtrsSurface
#' @export wgsTransform
#'
#' @examples

setMethod(
  f = "wgsTransform",
  signature = signature(obj = "Spatial"),
  definition = function(obj) {
    obj <- spTransform(obj, CRS("+init=epsg:4326"))
    return(obj)
  }
)

# dasymapPlot.leaflet ------------------------------------------------------------

if (!isGeneric("dasymapPlot.leaflet")) {
  setGeneric(
    name = "dasymapPlot.leaflet",
    def = function(aEtrsSurface, col.value) {
      standardGeneric("dasymapPlot.leaflet")
    }
  )
} else{
  stop("You have to remove the dasymapPlot generic before you displace it")
}

#' a utility for plot EtrsSurface choroPleth (lealet version)
#'
#' @description (lealet version)
#' @param aEtrsSurface EtrsSurface.
#' @param col.value numeric.
#'
#' @return a leaflet plot
#' @import leaflet classInt RColorBrewer sp
#' @export dasymapPlot.leaflet
#' @examples
setMethod(
  f = "dasymapPlot.leaflet",
  signature = signature(aEtrsSurface = "EtrsSurface", col.value = "numeric"),

  definition =  function(aEtrsSurface, col.value) {
    srf <- aEtrsSurface
    qpal <- colorNumeric("Reds", srf[[col.value]], n = 5)
    srf <- spTransform(srf, CRS("+init=epsg:4326"))

    value.popup <-
      paste0("<strong>Value: </strong>", srf[[col.value]])

    map <-
      leaflet(srf) %>% addTiles() %>% addProviderTiles("Stamen.Toner")
    map  %>%
      addPolygons(
        stroke = FALSE ,
        smoothFactor = 0.2 ,
        color = ~ qpal(srf[[col.value]]),
        fillOpacity = .9,
        popup = value.popup
      ) %>%
      addLegend(
        "bottomright",
        pal = qpal,
        values =  ~ srf[[col.value]],
        title = "HOU Applicants",
        opacity = .9
      )

  }
)

# raster2Ancillary --------------------------------------------------------


if (!isGeneric("raster2Ancillary")) {
  setGeneric(
    name = "raster2Ancillary",
    def = function(aRaster, cell.size, attr_divisor) {
      standardGeneric("raster2Ancillary")
    }
  )
} else{
  stop("You have to remove the raster2Ancillary generic before you displace it")
}

#' create a EtrsAncillary Surface from a Raster oBject
#'
#' @param aRaster raster.
#' @param cell.size numeric
#' @param attr_divisor numeric.
#'
#' @return EtrsAncillarySurface
#' @import raster
#' @export raster2Ancillary
#' @examples
setMethod(
  f = "raster2Ancillary",
  signature = signature(
    aRaster = "Raster",
    cell.size = "numeric",
    attr_divisor = "numeric"
  ),
  definition = function(aRaster, cell.size, attr_divisor = 1)
  {
    library(raster)
    r <- res(aRaster)
    f <- cell.size / r
    aRaster.agg <- aggregate(aRaster, fact = f)
    raster.surface <- rasterToPolygons(aRaster.agg, dissolve = T)
    colnames(raster.surface@data) <- c("value")
    raster.surface[['value']] <-
      raster.surface[['value']] / attr_divisor
    raster.surface <-
      etrsSurfacePar(
        raster.surface,
        over.method.type = "PropCal",
        surface.value.col = 1,
        cell.size = cell.size
      )
    colnames(raster.surface@data)[4] <- "WCELLWEIGHT"
    new(
      "EtrsSurface",
      raster.surface,
      over.method.type = "MaxArea",
      cell.size = cell.size,
      CELLVALUE = "WCELLWEIGHT"
    )

  }

)
if (!isGeneric("etrsDasymetric2Source")) {
  setGeneric(
    name = "etrsDasymetric2Source",
    def = function(d) {
      standardGeneric("etrsDasymetric2Source")
    }
  )
} else{
  stop("You have to remove the etrsDasymetric2Source generic before you displace it")
}

#' simple utillity converts EtrsDasymetric 2  EtrsSourceSurface
#'
#' @param d EtrsDasymericSurface.
#'
#' @return EtrsSourceSurface
#' @export etrsDasymetric2Source
#'
#' @examples
setMethod(
  f = "etrsDasymetric2Source",
  signature = signature(d = "EtrsDasymetricSurface"),

  definition = function(d) {
    row.names(d@data) <-
      sapply(slot(d, "polygons"), function(x)
        slot(x, "ID"))
    d@data <- d@data[, c(1:3, 6)]
    colnames(d@data)[4] <- "CELLVALUE"
    d
  }
)
