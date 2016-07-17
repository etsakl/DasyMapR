#' Produce a colorplethe Map for DasyMapR surfaces
#'
#' Produce a colorpleth map of 5 classe for DasyMapR surfaces using fisher-jenks calssification
#' @param aEtrsSurface  An an EtrsSourceSurface EtrsAncillarySurface DasymetricSurface
#' @param col.value The column that keeps the values
#'
#' @return a colorpleth map of 5 classes
#' @export dasymapPlot
#' @import RColorBrewer GISTools
#' @examples
#'
dasymapPlot<-function(aEtrsSurface,col.value){
  srf<-aEtrsSurface
add.alpha(pal<-brewer.pal(5,"Reds"),.5)
q5<-classIntervals(srf@data[,col.value],n=5,style = "fisher", dataPrecision=0)
q5Colours<-findColours(q5,pal)
plot(aEtrsSurface,col=q5Colours)
legend("topleft",fill=attr(q5Colours,"palette"),legend=names(attr(q5Colours,"table")),bty="n")
}
