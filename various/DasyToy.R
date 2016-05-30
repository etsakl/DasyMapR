#Make a spatial polygon
S<-readWKT("POLYGON((0 0,1000 0,1000 1000,0 1000,0 0))",id = "S",p4s = CRS("+init=epsg:3035"))
df<-cbind(AREA=gArea(S),VALUE=10)
row.names(df)<-sapply(slot(S,"polygons"),function(x) slot(x,"ID"))
df<-as.data.frame(df)
S<-SpatialPolygonsDataFrame(S,data = df,match.ID = TRUE)
#plot
dev.off()
X11(width=12,height = 12)
split.screen(figs = c(3,2))
screen(1)
plot(S)
title("the input surface")
text(coordinates(S)[,1],coordinates(S)[,2],S[["VALUE"]])
#grided
S.grided <- etrsSourceSurface(input.surface = S,over.method.type = "PropCal",surface.value.col = 2,cell.size = 500)
screen(2)
plot(S.grided)
title("... Projected to ETRS-LAEA 500 m")
text(coordinates(S.grided)[,1],coordinates(S.grided)[,2],S.grided[["CELLVALUE"]])

#the ancillary surface
A<-readWKT("POLYGON((0 0,1000 0,1000 500,0 500,0 0))",id="A" ,p4s = CRS("+init=epsg:3035" ))
df<-cbind(AREA=gArea(A),RelDens=1)
row.names(df)<-sapply(slot(A,"polygons"),function(x) slot(x,"ID"))
df<-as.data.frame(df)
A<-SpatialPolygonsDataFrame(A,data = df,match.ID = TRUE)
screen(3)
plot(A)
title("the ancillary surface")
text(coordinates(A)[,1],coordinates(A)[,2],A[["RelDens"]])
#grided
A.grided <- etrsAncillarySurface(input.surface = A,over.method.type = "PropCal",surface.value.col = 2,cell.size = 500,binary = FALSE)
screen(4)
plot(A.grided)
title("... Projected to ETRS-LAEA 500 m")
text(coordinates(A.grided)[,1],coordinates(A.grided)[,2],A.grided[["WCELLWEIGHT"]])
#EtrsDasymetric
screen(6)
D.grided<-EtrsDasymetricSurface(input.surface.grided = S.grided,ancillary.grided = A.grided,actuall.value = TRUE)
plot(D.grided)
title("Finally the Dasymetric Surface")
text(coordinates(D.grided)[,1],coordinates(D.grided)[,2],D.grided[["DASYCELL"]])


