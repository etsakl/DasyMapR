raster2Ancillary<-function(aRaster,cell.size,attr_divisor=1)
  {
  library(raster)
  r<-raster::crop("raster","aSpatialSurface")
  pnts<-as.data.frame(rasterToPoints(aRaster))
  colnames(pnts)<-c("X","Y","attr")
  coordinates(pnts)<-c("X","Y")
  proj4string(pnts)<-CRS("+init=epsg:3035")
  raster.surface<- etrsPoint2Grid(pnts,point.value.col = (pnts$attr/attr_divisor),cell.size = cell.size,mean.flag = T)
raster.surface<-etrsSurface2Spdf(raster.surface)
  new(
    "EtrsSurface",
    raster.surface,
    over.method.type = "MaxArea",
    cell.size = cell.size,
    CELLVALUE = "WCELLWEIGHT"
  )

}
