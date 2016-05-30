#' Reversin codefrom resoloution easting northin to res northin easting
#'  @description It 's possible to find out out there data not comabatible to Inspire.
#'  so reversin is a option.
#'
etrsReverseCellCode <-
  function(df = 'data.frame',cell.code.col = 'numeric') {
    #pr\arallel computing frame settings
    no_cores <- detectCores() - 1
    cl <- makeCluster(no_cores,type = "FORK")
    registerDoParallel(cores = cl)

    #keep the resolution
    res <-
      paste0(unlist(strsplit(df[1,cell.code.col],split = "m"))[1],"m")

    # split the df in equal parts
    div <- seq_len(abs(no_cores))
    splitf <- max(div[nrow(df) %% div == 0L])
    df <- split(df,f = rep_len(1:splitf, nrow(df)))

    df <-
      foreach(
        df.part = df,.combine = 'rbind',.packages = 'base',.inorder = T
      ) %dopar% {
        rcc <-
          paste0(res, as.character(unlist(sapply(sapply(sapply(strsplit(df.part[,cell.code.col],split = "m"),
                                                               function(x)
                                                                 x[2]),
                                                        function(x)
                                                          strsplit(x,split = "E")),
                                                 function(x)
                                                   paste0("E",x[2],x[1])))))


        df.part[,"CELLCODE"] <- rcc
        return(df.part)
      }


    stopCluster(cl)
    row.names(df) <- df[,"CELLCODE"]
    df
  }
