plot(the.grid.sp)
plot(SpatialPoints(cell.centers, CRS("+init=epsg:3035")), pch = "C", add = TRUE)
plot(SpatialPoints(eastings.northings, CRS("+init=epsg:3035")), pch = "EN", add = TRUE)
plot(x, add = TRUE, pch = "x")
plot(SpatialPoints(t(bb.outer.limits), CRS("+init=epsg:3035")), pch = "X", add = TRUE, col = 2)
plot(SpatialPoints(bb, CRS("+init=epsg:3035")), add = TRUE, pch = 2)
plot(SpatialPoints(cell.centers, CRS("+init=epsg:3035")), add = TRUE, pch = "v")

load("Function-TestEtrsPoints")

TestEtrsPoints()
spdf <- testetrspoints.list[[1]]
df <- testetrspoints.list[[2]]
str <- testetrspoints.list[[3]]
mat <- testetrspoints.list[[4]]

TestEtrsGrid <- function() {
  #### TEST for Create EtrsVectorGrid Require package sp
  require("sp")
  require("rgdal")
  ## just some random points near th origin of ETRS89-LAEA
  xc <- round(runif(10, min = 4321000, max = 4323000), 2)
  yc <- round(runif(10, min = 3210000, max = 3212000), 2)
  xy <- cbind(xc, yc)
  ## a SpatialPoints object
  xy.sp <- SpatialPoints(xy, proj4string = CRS("+init=epsg:3035"))
  ## An ETRS89-LAEA_1km grid contins the points
  cell.size <- 2000
  aGrid <- CreateEtrsVectorGrid(x = xy.sp, cell.size = cell.size)
  aGrid
}

TestEtrsPoints <- function() {
  require(sp)
  require(rgdal)
  # Test for EtrsPoint
  xc <- round(runif(20, min = 4321000, max = 4323000), 2)
  yc <- round(runif(20, min = 3210000, max = 3212000), 2)
  xy <- cbind(xc, yc)
  bb <- bbox(xy)
  cell.size = 1000
  # Calculate the limits of the grid
  bb.outer.limits <-
    cbind((floor(bb[, 1] / cell.size)) * cell.size, (ceiling(bb[, 2] / cell.size)) * cell.size)
  # Calculates eastings
  eastings <-
    seq(from = bb.outer.limits[1, 1], to = bb.outer.limits[1, 2], by = cell.size)
  # Calculates northings
  northings <-
    seq(from = bb.outer.limits[2, 1], to = bb.outer.limits[2, 2], by = cell.size)
  eastings.northings <- expand.grid(eastings, northings)


  # A cell code identifier for ETRS Define the cell size prefix
  prefix <-
    paste("", as(
      ifelse(cell.size >= 1000, cell.size / 1000, cell.size), "character"
    ), as(ifelse(cell.size >= 1000, "km",
                 "m"), "character"), sep = "")
  # Identify the number of zeros to remove from Eastings and Northings Values
  nz <-
    as.numeric(unlist(strsplit(as.character(
      format(cell.size, scientific = TRUE)
    ), split = ".e\\+"))[2])
  # Define string value based on Easting
  eaststr <-
    paste("E", as.character(eastings.northings[, 1] / 10 ^ nz), sep = "")
  # Define string value based on Northings
  northststr <-
    paste("N", as.character(eastings.northings[, 2] / 10 ^ nz), sep = "")
  # Concatenate strings into cell code
  etrs.cell.code <- paste(prefix, eaststr, northststr, sep = "")
  etrs.table.cell.codes <-
    as.data.frame(
      cbind(
        CELLCODES = etrs.cell.code, EASTOFORIGIN = eastings.northings[, 1], NORTHOFORIGIN = eastings.northings[,2]
      )
    )
  etrs.table.cell.codes

  eastings.northing.mat <- as.matrix(eastings.northings)

  eastings.northings.spdf <-
    SpatialPointsDataFrame(
      eastings.northing.mat, data = etrs.table.cell.codes, proj4string = CRS("+init=epsg:3035")
    )


  testetrspoints.list <<-
    list(
      eastings.northings.spdf, etrs.table.cell.codes, etrs.cell.code, eastings.northing.mat
    )
}
### RUN
spdf <- testetrspoints.list[[1]]
df <- testetrspoints.list[[2]]
str <- testetrspoints.list[[3]]
mat <- testetrspoints.list[[4]]



# test etrsGrid -------------------------------------------------------
testetrsGrid2 <- function() {
  x <- round(runif(1, min = 4320200, max = 4322000), 1)
  y <- round(runif(1, min = 3210000, max = 3212000), 1)
  xy <- cbind(x, y)
  size = round(runif(1,min = 100,max = 2000),0)
  SpatialPolygons(list(Polygons(list(Polygon(
    cbind(c(x,x + size,x + size,x,x),c(y,y,y + size,y + size,y))
  )),round(
    runif(1,min = 1,max = 100),0
  ))),proj4string = CRS("+init=epsg:3035"))

}


# test the ETRS-GRID over
#the.grid <- TestEtrsGrid()
#plot(the.grid)
grd.list <- list()
for (i in 1:5) {
  grd.list[i] <- paste("grd",i,sep = "")
}
for (n in grd.list) {
  assign(n,testetrsGrid2())
}
grd <-
  SpatialPolygons(c(grd1@polygons,grd3@polygons),proj4string = grd1@proj4string)
grd.id <- sapply(slot(grd, "polygons"), function(x)
  slot(x, "ID"))
grd.area <- sapply(slot(grd, "polygons"), function(x)
  slot(x, "area"))
grd.df <- data.frame(cbind(grd.id,grd.area))
rownames(grd.df) <- grd.id
the.surface <- SpatialPolygonsDataFrame(grd,grd.df)

plot(the.surface,col = rgb(1,1,0,.5))

plot(the.grid,add = TRUE,col = rgb(0,0,1,.1))

# to get all ID's
#
sapply(slot(mySpatialPolygons, "polygons"), function(x)
  slot(x, "ID"))


# test etrsGrid proportional -------------------------------------------------------
testpropcal <- function() {
  x <- round(runif(1, min = 4320200, max = 4322000), 1)
  y <- round(runif(1, min = 3210000, max = 3212000), 1)
  xy <- cbind(x, y)
  size = round(runif(1,min = 1500,max = 2000),0)
  pl1 <-
    Polygons(list(Polygon(cbind(
      c(x,x + size,x + size,x,x),c(y,y,y + size,y + size,y)
    ))),round(runif(1,min = 1,max = 100),0))
  x <- x + size

  pl2 <-
    Polygons(list(Polygon(cbind(
      c(x,x + size,x + size,x,x),c(y,y,y + 2 * size,y + 2 * size,y)
    ))),round(runif(1,min = 1,max = 100),0))
  sps <-
    SpatialPolygons(list(pl1,pl2),proj4string = CRS("+init=epsg:3035"))

  df <-
    data.frame(val=c(10,20),row.names = sapply(slot(sps, "polygons"), function(x)
      slot(x, "ID")))
sps<-SpatialPolygonsDataFrame(sps,data = df)
}
sPs<-testpropcal()

plot(grd1)
plot(sPs,col=c(rgb(.25,0,0,.5),rgb(0,.25,0,.5)),add =TRUE)
plot(pcs,a)
x.y<-(coordinates(pcs))
text(x.y[,1],x.y[,2],pcs@data$CELLVALUE)
plot(pcs,add=TRUE,border="2")

#color to polygon
 plot(pcs,col=plotrix:::rescale(pcs@data$CELLVALUE,c(pcs@data$CELLVALUE)))




# test for Proportional and weighted calculation --------------------------
 # dummy surface
 sPs<-testpropcal()
 # the grid
 egrd<- etrsGrid(obj = sPs,cell.size = 500 ,over.method.type = "2")
 epcv <- etrsPropValue(the.etrs.grid = egrd,the.surface = sPs,sur.value = surf1@data$val)
 # dummy cell weights

cell.weight <- c(rep(10,length.out=length(epcv@data$CELLCODE)/2),rep(5,length.out=length(epcv@data$CELLCODE)/2) )
#cell.weight[length(epcv@data$CELLCODE)%% 2:length(epcv@data$CELLCODE)] <- 5
epwcv<-etrsPropWeightedValue(epcv,cell.weight)
plot(epwcv)
x.y<-coordinates(epwcv)
text(x.y[,1],x.y[,2],labels = epwcv[["CELLVALUE"]])
x.y[,2]<-x.y[,2]-100
text(x.y[,1],x.y[,2],labels = epwcv[["CELLWEIGHT"]])
x.y[,2]<-x.y[,2]-100
text(x.y[,1],x.y[,2],labels = epwcv[["WCELLVALUE"]])


# present sourcesurface

s.s.propcal <- etrsSourceSurface(sPs, over.method.type = "PropCal", surface.value = sPs@data$val, cell.size = 500)

s.s.maxarea<-etrsSourceSurface(input.surface = sPs,cell.size = 500)

plot(EtrsTransform(sPs),col=c(rgb(1,0,0,.2),rgb(0,1,0,.2)))
x.y<-coordinates(s.s.maxarea)
text(x.y[,1],x.y[,2],labels = s.s.maxarea@data$FEATURE)

s.s.maxarea<-etrsSurface(input.surface = sPs,cell.size = 500)
s.s.propcal<-etrsSurface(input.surface = sPs,over.method.type = "PropCal",surface.value = sPs@data$val,100)
sPs.etrs<-EtrsTransform(sPs)
plot(sPs.etrs)
plot(s.s.propcal,add=TRUE,border="blue")
plot(s.s.maxarea,add=TRUE,border="red")

# ancillary test ----------------------------------------------------------

testpropcal2 <- function() {
  x <- round(runif(1, min = 4320200, max = 4322000), 1)
  y <- round(runif(1, min = 3210000, max = 3212000), 1)
  xy <- cbind(x, y)
  size = round(runif(1,min = 1500,max = 2000),0)
  pl1 <-
    Polygons(list(Polygon(cbind(
      c(x,x + size,x + size,x,x),c(y,y,y + size,y + size,y)
    ))),round(runif(1,min = 1,max = 100),0))
  x <- x + size

  pl2 <-
    Polygons(list(Polygon(cbind(
      c(x,x + size,x + size,x,x),c(y,y,y + size,y + size,y)
    ))),round(runif(1,min = 1,max = 100),0))
  sps <-
    SpatialPolygons(list(pl1,pl2),proj4string = CRS("+init=epsg:2100"))

  df <-
    data.frame(val=c(10,20),bin=c(1,1),row.names = sapply(slot(sps, "polygons"), function(x)
      slot(x, "ID")))
  ancS<-SpatialPolygonsDataFrame(sps,data = df)
}

ancS<-testpropcal2()

#
# SpatialPolygons ---------------------------------------------------------
sapply(slot(aGrid,"polygons"),function(x) slot(x,"ID"))
rownames(df)<-sapply(slot(aGrid,"polygons"),function(x) slot(x,"ID"))
df<-as.data.frame(df)
# example("EtrsTableCodes,matrix,numeric-method")
bb <- bbox(cbind(c(4320000, 4321000), c(3210000,3211000)))
cell.size = 1000
# Calculate the limits of the grid
bb.outer.limits <- cbind((floor(bb[, 1] / cell.size)) * cell.size, (ceiling(bb[, 2] / cell.size)) * cell.size)
# Calculates eastings
eastings <- seq(from = bb.outer.limits[1, 1], to = bb.outer.limits[1, 2], by = cell.size)
# Calculates northings
northings <-seq(from = bb.outer.limits[2, 1], to = bb.outer.limits[2, 2], by = cell.size)
eastings.northings <- as.matrix(expand.grid(eastings, northings))
low.corner.cell.etrs.points<-etrsPoints(eastings.northings = eastings.northings,cell.size =1000)
df <-EtrsTableCodes(eastings.northings,1000)
sp.df <- etrsCells(df,1000)
summary(sp.df)
sp<-etrsCells(df,1000)
sp.df<-SpatialPolygonsDataFrame(sp,df)
sp.df.grd<-etrsGrid(sp.df,1000)
is(sp.df.grd)
slotNames(sp.df.grd)
sp.df<-etrsGrid2Spdf(sp.df.grd)
sp.df[["VALUE"]]<-seq(10,90,10)




# EtrsDasymetric OLD ------------------------------------------------------
#
#   x <- round(runif(1, min = 4320200, max = 4322000), 1)
#   y <- round(runif(1, min = 3210000, max = 3212000), 1)
#   xy <- cbind(x, y)
#   size = round(runif(1,min = 1500,max = 2000),0)
#   pl1 <-
#     Polygons(list(Polygon(cbind(
#       c(x,x + size,x + size,x,x),c(y,y,y + size,y + size,y)
#     ))),round(runif(1,min = 1,max = 100),0))
#   x <- x + size
#
#   pl2 <-
#     Polygons(list(Polygon(cbind(
#       c(x,x + size,x + size,x,x),c(y,y,y + 2 * size,y + 2 * size,y)
#     ))),round(runif(1,min = 1,max = 100),0))
#   input.surface <-
#     SpatialPolygons(list(pl1,pl2),proj4string = CRS("+init=epsg:3035"))
#
#   df <-
#     data.frame(AREA = sapply(slot(input.surface, "polygons"), function(x)
#       slot(x, "area")),VALUE=c(10,20),row.names = sapply(slot(input.surface, "polygons"), function(x) slot(x, "ID")))
#   input.surface<-SpatialPolygonsDataFrame(input.surface,data = df)
# input.surface.grided<-etrsSourceSurface(input.surface = input.surface,over.method.type = "PropCal",surface.value.col = 2,cell.size = 500)
#x<-x-size
# size = round(runif(1,min = 1500,max = 1500),1)
# pl1 <-
#   Polygons(list(Polygon(cbind(
#     c(x,x + size,x + size,x,x),c(y,y,y + size,y + size,y)
#   ))),round(runif(1,min = 1,max = 100),0))
# x <- x + size
#
# pl2 <-
#   Polygons(list(Polygon(cbind(
#     c(x,x + size,x + size,x,x),c(y,y,y + size,y + size,y)
#   ))),round(runif(1,min = 1,max = 100),0))
# ancillary <-SpatialPolygons(list(pl1,pl2),proj4string = CRS("+init=epsg:3035"))
#
# df <-data.frame(RelDens=c(0.15,0.85),bin=c(1,1),row.names = sapply(slot(ancillary, "polygons"), function(x)slot(x, "ID")))
# ancillary<-SpatialPolygonsDataFrame(ancillary,data = df)
# ancillary.grided<-etrsAncillarySurface(input.surface = ancillary,over.method.type = "PropCal",surface.value.col = 1,cell.size = 500,binary = FALSE)
# par(mfrow=c(1,2))
#plot(input.surface,border=2,lwd=3,col=rgb(.4,input.surface@data$VALUE/100,0,.25))
#text(coordinates(input.surface)[,1],coordinates(input.surface)[,2],paste("VALUE=",input.surface@data$VALUE,sep=" "),cex=1)
#text(coordinates(input.surface)[,1],coordinates(input.surface)[,2]+150,paste("ID=",row.names(input.surface@data),sep=" "),cex=1.2)
#text(coordinates(input.surface.grided)[,1],coordinates(input.surface.grided)[,2],input.surface.grided@data$CELLVALUE,col=4)
#plot(input.surface.grided,add=TRUE,lty=3,border=4)
#plot(ancillary,lwd=3,border=3,lty=7)
#plot(ancillary.grided,add=TRUE,lwd=3,border=5,lty=7)
#
