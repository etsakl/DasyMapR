
formatted_address<- geocoded["formatted_address"]

formatted_address_splited<-strsplit(formatted_address[,1],split = ", ")

formatted_address_splited_2<-sapply(formatted_address_splited,FUN = function(x) strsplit(x[2],split = " "))


formatted_address_splited_3 <- sapply(
  formatted_address_splited_2,
  FUN =
    function(x) {
      if (length(x) == 3) {
        paste0(x[2]," ", x[3])
      } else{
        i  <- length(x) - 3
        paste0(x[2  +  i], " ",x[3  +  i])
      }
    })

geocoded["POSTCODE"]<-formatted_address_splited_3
