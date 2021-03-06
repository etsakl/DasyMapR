% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class-EtrsGrid.R
\name{etrsPoint2Grid}
\alias{etrsPoint2Grid}
\title{Produce a etrs aggregating point data}
\usage{
etrsPoint2Grid(obj = "SpatialPointsDataFrame", point.value.col = "numeric",
  cell.size = "numeric", mean.flag = FALSE)
}
\arguments{
\item{obj}{a SpatialPointsDataFrame object}

\item{point.value.col}{The value that is going to be aggregated}

\item{cell.size}{the resolution of the grid}

\item{mean.flag}{False for \code{sum()} the value TRUE \code{mean()} the value}
}
\value{
SpatialPointsDataFrame
}
\description{
Produce a etrs aggregating point data
}
\examples{
xc <- round(runif(10, min = 4321000, max = 4323000), 2)
yc <- round(runif(10, min = 3210000, max = 3212000), 2)
xy <- cbind(xc, yc)
xy.sp <- SpatialPoints(xy, proj4string = CRS("+init=epsg:3035"))
df<-as.data.frame(rep(1,length(xy.sp)))
colnames(df)<-"VALUE"
xy.sp<-SpatialPointsDataFrame(xy.sp,df)
aGrid <- etrsPoint2Grid(obj = xy.sp, point.value.col=1,cell.size = 1000,mean.flag =FALSE)
X11(width = 10,10)
split.screen(figs=c(1,2))
screen(1)
plot(aGrid)
title("Some Points ...")
plot(xy.sp,add=TRUE,pch=21,bg=rgb(1,0,0,.1),cex=xy.sp[["VALUE"]])
screen(2)
 plot(aGrid)
plot(gCentroid(etrsGrid2Spdf(aGrid),byid=TRUE),add=TRUE,pch=21,bg=rgb(1,0,0,.1),cex=aGrid[["VALUE"]])
title(paste("aggregated in the",aGrid@the.grid.name))
}

