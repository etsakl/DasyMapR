% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DasyMapR-GenUt.R
\docType{methods}
\name{pntsattr2surface,SpatialPolygonsDataFrame,SpatialPointsDataFrame-method}
\alias{pntsattr2surface,SpatialPolygonsDataFrame,SpatialPointsDataFrame-method}
\title{pntsattr2surface is an ancillary utility for point surface combining attributes}
\usage{

  \S4method{pntsattr2surface}{SpatialPolygonsDataFrame,SpatialPointsDataFrame}(sppdf,
  sppntdf)
}
\arguments{
\item{sppdf}{A sptialpolygonsdatframeobject}

\item{sppntdf}{A spatialpointsdataframe object}
}
\value{
A spattial polygons data frame objec
}
\description{
If the polygon contains 1 point then takes its attributes. if the polygon contains more than 2 points voronoi method of package deldir it's used to subdevide the region. If no point is contained in the region thn NA is provided as values
}

