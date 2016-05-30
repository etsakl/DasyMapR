mongo<-mongo.create()
mongo
mongo.is.connected(mongo)
if(mongo.is.connected(mongo) == TRUE) {
  mongo.get.databases(mongo)
}