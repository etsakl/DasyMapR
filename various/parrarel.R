# etrsParrarel
#
#
#
# Calculate the number of cores
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cores = cl)

load("./data/ETRS89.LAEA.100km.RData")
ETRS89.LAEA.100km <- etrsGrid2Spdf(ETRS89.LAEA.100km)
NUTS_3.over.grid <- ETRS89.LAEA.100km[NUTS_3,]

plot(NUTS_3.over.grid)

maxArea<-foreach(i=1:length(NUTS_3),.combine='gUnion',.packages = 'DasyMapR') %dopar% {

etrsSourceSurface(input.surface = NUTS_3,cell.size = 100000)

}

stopCluster()
