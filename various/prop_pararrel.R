input.surface = NUTSV9_LEAC_GR25
over.method.type = "PropCal"
surface.value.col = 1
cell.size = 10000


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

    browser()

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



