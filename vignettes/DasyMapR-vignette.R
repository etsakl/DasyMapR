## ----echo=FALSE----------------------------------------------------------
library(knitr)
opts_chunk$set(
tidy = TRUE,
cache = TRUE,
warning = FALSE,
message = FALSE
)

## ----eval=F,echo=TRUE,results='hide'-------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  install_github("etsakl/DasyMapR",build_vignettes=TRUE)
#  library(DasyMapR)

## ----echo=TRUE,message=FALSE---------------------------------------------
library(DasyMapR)
library(knitr)
DasyMapR.contains<-sort(ls("package:DasyMapR"))
kable(as.data.frame(DasyMapR.contains))

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  # Αν τρέξει ο κώδικας παρουσιάζονται τα διαθέσιμα δεδομένα
#  DasyMapR.data<-data(package ="DasyMapR")
#  print(DasyMapR.data)

## ----fig.show='hold'-----------------------------------------------------
# Φόρτωσε τα δεδομένα 
data("NUTS3_OCMG")
# Δίάλεξε κάποια
pp <- c("Ν. ΑΡΓΟΛΙΔΟΣ","Ν. ΚΟΡΙΝΘΙΑΣ","Ν. ΑΡΚΑΔΙΑΣ","Ν. ΜΕΣΣΗΝΙΑΣ","Ν. ΛΑΚΩΝΙΑΣ")
Π.ΠΕΛΟΠΟΝΝΗΣΟΥ <- NUTS3_OCMG[which(!is.na(match(NUTS3_OCMG[["NAME"]],pp))),]
par("mar"=c(.1,.1,.1,.1))
# και σχεδίασε τα
plot(NUTS3_OCMG)
plot(Π.ΠΕΛΟΠΟΝΝΗΣΟΥ,border=2,lwd=2,add=T)
plot(Π.ΠΕΛΟΠΟΝΝΗΣΟΥ,border=2,lwd=2)

## ------------------------------------------------------------------------
# Τα περιγράφικά σε πίνακα
kable(head(Π.ΠΕΛΟΠΟΝΝΗΣΟΥ@data[,3:10]))

## ----results='hide',fig.show='hold',message=FALSE,tidy=TRUE,cache=TRUE,echo=-2----
library(DasyMapR)
par("mar"=c(.1,.1,.1,.1))
# Καλεί την EtrsTransForm
srf.grd.max <- etrsSurface(input.surface = Π.ΠΕΛΟΠΟΝΝΗΣΟΥ,over.method.type = "MaxArea",cell.size = 10000)
plot(Π.ΠΕΛΟΠΟΝΝΗΣΟΥ)
plot(srf.grd.max)

## ------------------------------------------------------------------------
kable(head(srf.grd.max@data))

## ------------------------------------------------------------------------
srf.grd.max.full<-joinMaxAreaSurfaceDataFrames(the.surface = NUTS3_OCMG, the.EtrsSurface = srf.grd.max)
kable(head(srf.grd.max.full@data[,c(7:12)]))

## ------------------------------------------------------------------------
# Θα κρατήσουμε μόνο τον πλυθισμό του 2001 
Π.ΠΕΛΟΠΟΝΝΗΣΟΥ.2001<-Π.ΠΕΛΟΠΟΝΝΗΣΟΥ[,c(3,7)]
kable(Π.ΠΕΛΟΠΟΝΝΗΣΟΥ.2001@data)

## ------------------------------------------------------------------------
Π.ΠΕΛΟΠΟΝΝΗΣΟΥ.2001<-ActuallVal2Density(input.surface = Π.ΠΕΛΟΠΟΝΝΗΣΟΥ.2001,surface.value.col = 2,area.unit = 1e+06)
kable(Π.ΠΕΛΟΠΟΝΝΗΣΟΥ.2001@data)

## ----fig.show='hold',tidy=TRUE,warning=FALSE,message=FALSE,results='hide'----
srf.grd.prop<-etrsSourceSurface(input.surface = Π.ΠΕΛΟΠΟΝΝΗΣΟΥ.2001,over.method.type = "PropCal",surface.value.col = 4,cell.size = 10000)
par("mar"=c(.1,.1,.1,.1))
plot(Π.ΠΕΛΟΠΟΝΝΗΣΟΥ)
plot(srf.grd.prop)

## ------------------------------------------------------------------------
kable(head(srf.grd.prop@data))

## ------------------------------------------------------------------------
wd<-getwd()
setwd(wd)
surface.detailed.table<-readRDS(".surface.detailed.table.rds")
kable(head(surface.detailed.table,5))

## ----fig.show='hold'-----------------------------------------------------
# Η κλήση της EtrsTrasnform για αλλαγη του CRS
Π.ΠΕΛΟΠΟΝΝΗΣΟΥ.ETRS <- EtrsTransform(Π.ΠΕΛΟΠΟΝΝΗΣΟΥ)
par("mar"=c(.1,.1,.1,.1))
plot(Π.ΠΕΛΟΠΟΝΝΗΣΟΥ.ETRS,border=2,lwd=2)
plot(srf.grd.max,col=rgb(0,1,0,0.1),add=TRUE)
plot(Π.ΠΕΛΟΠΟΝΝΗΣΟΥ.ETRS,border=2,lwd=2)
plot(srf.grd.prop,col=rgb(0,1,0,0.1),add=TRUE)

## ----fig.show='hold',results='hide'--------------------------------------
par(mar = c(0.1, 0.1, 0.1, 0.1))
ΑΡΓΟΛΙΔΑ <- NUTS3_OCMG[which(!is.na(match(NUTS3_OCMG[["NAME"]], "Ν. ΑΡΓΟΛΙΔΟΣ"))),]
plot(Π.ΠΕΛΟΠΟΝΝΗΣΟΥ)
plot(ΑΡΓΟΛΙΔΑ, add = TRUE, lwd = 2, border = 2)
# Εδώ καλείται η EtrsTransform απευθείας
ΑΡΓΟΛΙΔΑ.ETRS<-EtrsTransform(ΑΡΓΟΛΙΔΑ)
# Με την κλήση της etrsSourceSurface παράγεται η επιφανεια πηγή
source.surface <- etrsSourceSurface(input.surface = Π.ΠΕΛΟΠΟΝΝΗΣΟΥ.2001[3,], 
    over.method.type = "PropCal", surface.value.col = 4, cell.size = 1000)
plot(source.surface,col=rgb(0,1,0,0.01),lwd=.5,border="lightgrey")
plot(ΑΡΓΟΛΙΔΑ.ETRS, add = TRUE, lwd = 2, border = 2)

## ------------------------------------------------------------------------
data("CLC2000_CODES")
kable(head(CLC2000_CODES))

## ----eval=FALSE----------------------------------------------------------
#  # Τα δύο .shp files περιέχονται στο folder corine
#   setwd(system.file("data/corine",package="DasyMapR"))
#   # Με βάση τα όρια της περιοχής ...
#   bb<-bbox(ΑΡΓΟΛΙΔΑ.ETRS)
#   # διμιουργησε νέα .shp files που περιέχουν όσα δεδομένα χρειάζομαι
#   ogr2ogr(".","clc_cliped",spat =  c(bb[,1],bb[,2]))
#   dsn<-setwd("clc_cliped/")
#   # φόρτωσε τα ως SpatialPolygonsDataFrames
#   CLC2000_POLY_ARGOLIDA111<-readOGR(".","clc00_v2_code_111")
#   CLC2000_POLY_ARGOLIDA112<-readOGR(".","clc00_v2_code_112")
#   # και ένωσε τα
#   CLC2000.ARGOLIDA.RES<-rbind.SpatialPolygonsDataFrame(CLC2000_POLY_ARGOLIDA111,CLC2000_POLY_ARGOLIDA112,makeUniqueIDs = T)
#   # τέλος σώστα στο δίσκο ως dataset
#   setwd(system.file("data",package = "DasyMapR"))
#  # Αφαιρέθηκαν 2 πολύγωνα με πρόβλημα στην γεωμετρία
#   CLC2000.ARGOLIDA.RES<-CLC2000.ARGOLIDA.RES[-which(row.names(CLC2000.ARGOLIDA.RES)==8),]
#  CLC2000.ARGOLIDA.RES<-CLC2000.ARGOLIDA.RES[-which(row.names(CLC2000.ARGOLIDA.RES)%in% "01"),]
#   devtools::use_data(CLC2000.ARGOLIDA.RES,overwrite = T)

## ------------------------------------------------------------------------
#Φορτώνουμε τα δεδομένα 
data("CLC2000.ARGOLIDA.RES")
# Τι Περιέχει το σετ;
kable(head(CLC2000.ARGOLIDA.RES@data,3))

## ------------------------------------------------------------------------
ΠΕΡΙΟΧΕΣ.ΚΑΤΟΙΚΙΑΣ <- merge(x = CLC2000.ARGOLIDA.RES,y = CLC2000_CODES,by.x="code_00",by.y="Code_00")
kable(head(ΠΕΡΙΟΧΕΣ.ΚΑΤΟΙΚΙΑΣ@data,3))

## ----fig.height=5,fig.width=7,tidy=TRUE----------------------------------
par("mar"=c(.1,.1,.1,.1))
ReDens111<-round(3/4,2)
ReDens112<-round(1/4,2)
ΠΕΡΙΟΧΕΣ.ΚΑΤΟΙΚΙΑΣ@data[which(ΠΕΡΙΟΧΕΣ.ΚΑΤΟΙΚΙΑΣ@data[,"code_00"] == 111),"ReDens"]<-ReDens111
ΠΕΡΙΟΧΕΣ.ΚΑΤΟΙΚΙΑΣ@data[which(ΠΕΡΙΟΧΕΣ.ΚΑΤΟΙΚΙΑΣ@data[,"code_00"] == 112),"ReDens"]<-ReDens112
kable(head(ΠΕΡΙΟΧΕΣ.ΚΑΤΟΙΚΙΑΣ@data))
plot(ΑΡΓΟΛΙΔΑ.ETRS,lwd=2,border=2)
plot(ΠΕΡΙΟΧΕΣ.ΚΑΤΟΙΚΙΑΣ,col="purple",add=TRUE)
data("NUTSV9_LEAC")
plot(NUTSV9_LEAC,add=TRUE,border="lightgrey")

## ----results='hide',fig.height=5,fig.width=7-----------------------------
par("mar"=c(.1,.1,.1,.1))
data("NUTSV9_LEAC")
the.ancillary.surface.bf <- etrsAncillarySurface(input.surface = ΠΕΡΙΟΧΕΣ.ΚΑΤΟΙΚΙΑΣ,over.method.type = "PropCal",surface.value.col = 3,cell.size = 1000,binary = FALSE)
plot(the.ancillary.surface.bf,col= "purple")
plot(ΑΡΓΟΛΙΔΑ.ETRS, add = TRUE, lwd = 2, border = 2)
plot(NUTSV9_LEAC,add=TRUE,border="lightgrey")

## ------------------------------------------------------------------------
kable(head(the.ancillary.surface.bf@data))
surface.detailed.table.bf<-readRDS(".surface.detailed.table.rds")

## ----results='hide',fig.height=5,fig.width=7-----------------------------
par("mar"=c(.1,.1,.1,.1))
ΠΕΡΙΟΧΕΣ.ΚΑΤΟΙΚΙΑΣ@data[which(ΠΕΡΙΟΧΕΣ.ΚΑΤΟΙΚΙΑΣ@data[,"code_00"] == c(111,112)),"ReDens"]<-1
the.ancillary.surface.bt <- etrsAncillarySurface(input.surface = ΠΕΡΙΟΧΕΣ.ΚΑΤΟΙΚΙΑΣ,over.method.type = "PropCal",surface.value.col = 3,cell.size = 1000,binary = TRUE)
plot(the.ancillary.surface.bt,col= "purple4")
plot(the.ancillary.surface.bt[which(the.ancillary.surface.bt[["WCELLWEIGHT"]]==0),],col= "lightgrey",add=T)
plot(ΑΡΓΟΛΙΔΑ.ETRS, add = TRUE, lwd = 2, border = 2)
plot(NUTSV9_LEAC,add=TRUE,border="lightgrey")

## ------------------------------------------------------------------------
kable(head(the.ancillary.surface.bt@data))
surface.detailed.table.bt<-readRDS(".surface.detailed.table.rds")

## ------------------------------------------------------------------------
wd<-getwd()
setwd(wd)
kable(head(surface.detailed.table.bf))

## ----fig.height=5,fig.width=7--------------------------------------------
par("mar"=c(.1,.1,.1,.1))
dasymetric.surface<-EtrsDasymetricSurface(input.surface.grided = source.surface,ancillary.grided =the.ancillary.surface.bf  ,actuall.value = FALSE)
plot(dasymetric.surface)
plot(ΑΡΓΟΛΙΔΑ.ETRS,add=T,border="red",lwd=2)
kable(head(dasymetric.surface@data))

## ------------------------------------------------------------------------
data("GEOSTAT_grid_EU_POP_2006_1k_V1_1_1")
kable(head(GEOSTAT_grid_EU_POP_2006_1k_V1_1_1))
GR_POP_2006<-GEOSTAT_grid_EU_POP_2006_1k_V1_1_1[which(GEOSTAT_grid_EU_POP_2006_1k_V1_1_1[,'CNTR_CODE'] %in% "EL"),]

## ------------------------------------------------------------------------
GR_POP_2006<-as.data.frame(GR_POP_2006)
GR_POP_2006<-etrsReverseCellCode(df = GR_POP_2006,cell.code.col = 1)
kable(head(GR_POP_2006))

## ----results='hide',fig.height=5,fig.width=7-----------------------------
par("mar"=c(.1,.1,.1,.1))
GR251<-NUTSV9_LEAC[which(!is.na(match(NUTSV9_LEAC[["N3CD"]],"GR251"))),]
GR251<-EtrsTransform(GR251)
GR251.grd<-etrsGrid(GR251,cell.size = 1000)
GR251.grd<-merge(GR251.grd,GR_POP_2006,by=0,all=F)
plot(GR251.grd)
plot(ΑΡΓΟΛΙΔΑ.ETRS,add=TRUE,border=2,lwd=3)

## ----warning=FALSE,message=FALSE,fig.show='hold'-------------------------
par("mar"=c(.1,.1,.1,.1))
# Όρια NUTS 2006
GR25<-NUTSV9_LEAC[which(!is.na(match(NUTSV9_LEAC[["N2CD"]],"GR25"))),]
kable(head(GR25@data))
#Νέα όρια NUTS 2013
data("NUTS_2013_01M_EL")
kable(head(NUTS_2013_01M_EL@data))
NUTS_2013_01M_65<-EtrsTransform(NUTS_2013_01M_EL[grep('^EL65',NUTS_2013_01M_EL[["NUTS_ID"]]) ,])
plot(NUTS_2013_01M_65)
plot(GR25)

## ------------------------------------------------------------------------
info<-search_eurostat("NUTS 3")
kable(info[c(23:30),c(1,2)])

## ------------------------------------------------------------------------
#nama_10r_3gdp <- get_eurostat(id = "nama_10r_3gdp" ,filters = list(time=2006),time_format = "num")

## ------------------------------------------------------------------------
data("nama_10r_3gdp")
dat<-nama_10r_3gdp

## ------------------------------------------------------------------------
GDP651_2006<-dat[grep('^EL651',dat$geo),] 
kable(label_eurostat(GDP651_2006))

## ------------------------------------------------------------------------
par("mar"=c(.1,.1,.1,.1))
NUTS_2013_01M_651<-EtrsTransform(NUTS_2013_01M_EL[grep('^EL651',NUTS_2013_01M_EL[["NUTS_ID"]]) ,])
kable(NUTS_2013_01M_651@data)

## ------------------------------------------------------------------------
NUTS_2013_01M_651<-merge(NUTS_2013_01M_651,GDP651_2006[1,],by.x="NUTS_ID",by.y="geo",all=FALSE)

## ----fig.height=5,fig.width=7--------------------------------------------
row.names(NUTS_2013_01M_651@data)<-sapply(slot(NUTS_2013_01M_651, "polygons"), function(x) slot(x, "ID"))
plot(NUTS_2013_01M_651)
kable(NUTS_2013_01M_651@data)

## ----results='hide',fig.height=5,fig.width=7-----------------------------
par("mar"=c(.1,.1,.1,.1))
NUTS_2013_01M_651_GDP<-etrsSourceSurface(input.surface = NUTS_2013_01M_651,over.method.type = "PropCal",surface.value.col = 7,cell.size = 1000)
plot(NUTS_2013_01M_651_GDP)

## ------------------------------------------------------------------------
kable(head(NUTS_2013_01M_651_GDP@data))

## ----results='hide',warnings='hide',message='hide'-----------------------
kable(head(dasymetric.surface@data))
POP_2001_ancillary<- etrsDasymetric2Ancillary(dasymetric.surface)
row.names(POP_2001_ancillary@data)<-sapply(slot(POP_2001_ancillary, "polygons"), function(x) slot(x, "ID"))

## ----fig.height=5,fig.width=7--------------------------------------------
par("mar"=c(0.1,0.1,0.1,0.1))
DASY_GPD<-etrsPropWeightedValue(input.surface.grided = NUTS_2013_01M_651_GDP, ancillary.grided = POP_2001_ancillary)
plot(DASY_GPD)
plot(ΑΡΓΟΛΙΔΑ.ETRS,add=T,border=2,lwd=2)
kable(head(DASY_GPD))

## ----fig.height=5,fig.width=7--------------------------------------------
par("mar"=c(0.1,0.1,0.1,0.1))
DASY_GPD_RASTER<-etrsDasymetric2Raster(dasymetric.surface = DASY_GPD)
rw.colors<-grey.colors
image(DASY_GPD_RASTER,col=rw.colors(5))
plot(ΑΡΓΟΛΙΔΑ.ETRS,add=T,border=2,lwd=2)

