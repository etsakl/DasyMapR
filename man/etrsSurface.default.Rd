% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class-EtrsSurface.R
\name{etrsSurface.default}
\alias{etrsSurface.default}
\title{Etrs (source or Ancillary) surface creation}
\usage{
etrsSurface.default(input.surface = "SpatialPolygonsDataFrame",
  over.method.type = "character", surface.value.col = "numeric",
  cell.size = "numeric", ...)
}
\arguments{
\item{over.method.type}{"MaxArea" The Maximum area criteria: the cell takes the value of the unit which covers most of the cell area. It should be a good option for uncountable variables
"PropCal" in proportional calculation the cell takes a calculated value depending on the values of the units falling inside and their share within the cell. This method seems very appropriate for countable variables}

\item{surface.value.col}{the number of colum that keeps  the relative density of a cell with land-cover some type}

\item{...}{}

\item{cell.size:}{one of the resolutions of the grid  that is 0.5m, 1m, 2.5m, 5m, 10m, 25m, 50m, 100m, 250m, 500m, 1Km, 2.5Km, 5Km, 10Km, 25Km, 50Km, 100Km}
}
\value{
an EtrsSurface
}
\description{
Depending on each type of indicator or variable to be integrated within the reference grid, a different type of integration should be decided and tested. Besides the method finally chosen to integrate, it is important to highlight that indicator figures given by area unit, e.g. by square kilometre, should be converted considering that each cell has a total area of 1 km 2.
}
\examples{

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
x<-x-size
y<-y+size
pl3 <-
  Polygons(list(Polygon(cbind(
    c(x,x + size,x + size,x,x),c(y,y,y +  size,y +  size,y)
  ))),round(runif(1,min = 1,max = 100),0))
sps <-
  SpatialPolygons(list(pl1,pl2,pl3),proj4string = CRS("+init=epsg:2100"))
df <-
  data.frame(val=c("R5","R40","R80"),row.names = sapply(slot(sps, "polygons"), function(x)
    slot(x, "ID")))
sps<-SpatialPolygonsDataFrame(sps,data = df)
}
Source.Surface <- testpropcal()

# Uses the default etrsSurface method
Source.Surface.MaxArea <-
etrsSurface(input.surface = Source.Surface, over.method.type = "MaxArea", cell.size = 1000)
summary(Source.Surface.MaxArea)
}
\seealso{
etrsMaxArea, etrsPropValue
}

