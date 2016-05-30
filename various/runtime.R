setwd("~/ownCloud/plsthesis/DasyMapR/data/nutsv9_leac")
nuts3<-readOGR(".",layer = "NUTSV9_LEAC")
nuts3.gr<-nuts3[which(!is.na(match(nuts3[["N0CD"]],"GR"))),]
nuts3.gr25<-nuts3[which(!is.na(match(nuts3[["N2_3CD"]],"GR25"))),]


ptm <- proc.time()

nuts3.gr25.grided <-
  etrsSurface(
    input.surface = nuts3.gr,cell.size = 1000
  )

par.time<-proc.time() - ptm

ptm <- proc.time()
nomoi.1k.grd<-etrsSourceSurface(nomoi,over.method.type = "PropCal",surface.value.col = 6,cell.size = 1000)


par.time<-proc.time() - ptm



# Calculate the number of cores
data("NUTSV9_LEAC")
NUTSV9_LEAC_GR25 <- NUTSV9_LEAC[which(!is.na(match(NUTSV9_LEAC[["N2CD"]],"GR25"))),]
plot(NUTSV9_LEAC_GR25,lwd=2)

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores,type = "FORK")
registerDoParallel(cores = cl)

system.time(
NUTSV9_LEAC_GR25_1km_grided<- foreach(i=1:length(NUTSV9_LEAC_GR25),.packages =c('sp', 'DasyMapR')) %dopar% {

  input.surface <- NUTSV9_LEAC_GR25[i,]
  etrsSourceSurface(input.surface,cell.size = 10000)

}
)
stopCluster(cl)

plot(NUTSV9_LEAC_GR25_1km_grided,add=T)

#etrs seq
system.time(
max.area<-etrsSourceSurface(input.surface = NUTSV9_LEAC_GR25,cell.size = 10000)
)

plot(max.area,add=T,border=2)

# sytem time for MaxArea and PropCal
#


st<-list()
st[1]<-system.time(
  s1<-etrsSurface(input.surface = NUTSV9_LEAC_GR25,over.method.type = "MaxArea",cell.size = 10000)

)

st[2]<-system.time(
 s2<- etrsSurfacePar(input.surface = NUTSV9_LEAC_GR25,over.method.type = "MaxArea",cell.size = 10000)
)

st[3]<-system.time(
  s3<-etrsSurface(NUTSV9_LEAC_GR25,over.method.type = "PropCal",surface.value.col = 1,cell.size = 10000)

)

st[4]<-system.time(
  s4<-etrsSurfacePar(NUTSV9_LEAC_GR25,over.method.type = "PropCal",surface.value.col = 1,cell.size = 10000)
)

