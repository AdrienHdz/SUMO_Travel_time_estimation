# Created by Adrien Hernandez and Mohamad Elmasri


graph.network <- function(data.train = NULL, L = NULL, data.timebins = NULL ){
  
  # A.0 We are starting to transform our data so that it takes on a network form. 
  # So, we transform the linkId variable into two variables: LinkId.from and LinkId.to. 
  # Since a link represents a segment of a road network, 
  # we can thus know the origin and the destination of a vehicle for each link of this network.
  # We add a variable called N, which represents the number of times that an ijk link 
  # (i = linkId.from, j = linkId.to etk = timebins) is present in our data set. 
  # Since there is no destination j (linkId.to) for the last ijk link of the trip of a vehicle, 
  # we consider its value as NA.
  data.train[, linkId.from := linkId, by = trip]
  data.train[, linkId.to := shift(linkId, type = 'lead'), by = trip]
  data.train[, N:=.N, by =list(linkId.from, linkId.to, timeBins) ]
  
  # A.1 Variance and mean velocity computation
  # mu and sd2: For each ijk link, we calculate the mean and the variance of the speed for all 
  # the times the ijk link is taken. Consequently, we also obtain NA values 
  # for the variance of the speed of the ijk links which were taken only 1 time (N = 1) in the data set.
  graph.stat = data.train[, .(mu = mean(1/speed),  sd2 = var(1/speed), N=N[1]) ,by = list(linkId.from, linkId.to, timeBins)]
  
  # We create the structure of the graph network
  full.graph = graph.stat[, .(timeBins = data.timebins) ,by = list(linkId.from, linkId.to)]
  
  # We merge mu and sd2 calculated above to the structure of our graph network
  graph.stat = merge(full.graph, graph.stat, by= c('linkId.from', 'linkId.to', 'timeBins'), all.x = TRUE)
  graph.stat$N[is.na(graph.stat$N)]<-0
  
  # mu.from.tb and sd2.from.tb: We calculate the mean and the variance of the speed 
  # for all the times when the ik link (linkid.from and timebins) is borrowed more 
  # than 10 times (without looking at the destination). 
  # That is to say that the value of mu.from.tb and sd2.from.tb exists if and only if, 
  # the link ik has a sum of N greater than 10 (where 10 is a hyperparameter )
  graph.stat.from.tb = data.train[, .(mu = mean(1/speed),
                                      sd2 = var(1/speed), N= .N) ,
                                  by = list(linkId.from, timeBins)]
  
  # mu.tb and sd2.tb: We calculate the mean and the variance of the speed for each timebins.
  graph.stat.tb = data.train[, .(mu = mean(1/speed),
                                 sd2 = var(1/speed), N= .N) ,by = list(timeBins)]
  
  # we combine the three in the same data frame
  graph.stat.full =  merge(graph.stat,
                           graph.stat.from.tb[N>L, .(mu.from.tb = mu,
                                                     sd2.from.tb = sd2, linkId.from, timeBins)],
                           all.x = TRUE,
                           by.x=c('linkId.from', 'timeBins'),
                           by.y = c('linkId.from', 'timeBins'))
  graph.stat.full =  merge(graph.stat.full,
                           graph.stat.tb[N>L, .(mu.tb = mu, sd2.tb = sd2, timeBins)],
                           all.x = TRUE, by.x=c('timeBins'), by.y = c('timeBins'))
  
  # A.2 For each ijk link, if the value of mu and / or sd2 is missing, then the variance and the mean 
  # of the ijk link are the values of mu.from.tb and sd2.from.tb. 
  # If these values are also missing, then the mean and the variance of the ijk link become 
  # the values of mu.tb and sd2.tb.
  # ( Sd2 is actually the standard deviation so we calculate the square root of sd2 to find the variance.)
  graph.stat.full[, muspeed := ifelse(is.na(mu) | N < L, ifelse(is.na(mu.from.tb), mu.tb, mu.from.tb), mu)]
  graph.stat.full[,  imputed_mean := ifelse(muspeed== mu, FALSE, TRUE )]
  
  graph.stat.full[, sdspeed := ifelse(is.na(sd2), ifelse(is.na(sd2.from.tb), sd2.tb, sd2.from.tb), sd2)]
  graph.stat.full[, imputed_sd := ifelse(sdspeed == sd2  & !is.na(sd2) , FALSE, TRUE)]
  
  graph.stat.full = graph.stat.full[,.(linkId.from,
                                       linkId.to,
                                       timeBins,
                                       mean = muspeed,
                                       sd = sqrt(sdspeed),
                                       imputed_sd,
                                       imputed_mean )]
  # Merging with the train dataset
  data.train = merge(data.train, graph.stat.full, all.x = TRUE, by = c('linkId.from', 'linkId.to', 'timeBins'), sort = FALSE)[order(trip,time)]
  
  # Returning variables
  obj <- list(graph.stat.full = graph.stat.full, data.train = data.train)
  class(obj) = append(class(obj), "graph.network", after=0)
  invisible(obj)
}


traveltimeCLT <- function(obj.data.train = NULL, obj.graph.stat.full = NULL, M = NULL, bin = NULL, rules = NULL){
  
  set.seed(2020)
  
  # A.0 Sampling train dataset according to model input
  t = obj.data.train[, .N , by=trip][N>10][, trip]
  sample_timebin = obj.data.train[trip %in% t, .(timeBins= if(all(timeBins == timeBins[1])) timeBins[1] else 'remove' ), by=trip]
  sample_timebin = sample_timebin[!timeBins == 'remove']
  
  if(is.null(bin)){
    samp = sample_timebin[, trip]
    tt = obj.data.train
    xx = tt[, .I[.N>10],by = trip]
  } else {
    samp = sample_timebin[timeBins == bin][, trip]
    samp = samp[sample.int(length(samp),M)]
    tt = obj.data.train[trip %in% samp]
    xx = tt[trip %in% samp, .I[.N>10],by = trip]
  }
  
  # A.1 Get rho. The algorithm only keeps trips ("trips") that have more than 10 ijk links.
  # On these links, we will create and use a function called "get rho" allowing to calculate 
  # the autocovariance of the speed with a lag up to 5 for each trip. 
  # If desired, the function also allows calculation specifically for AM or PM timebins.
  rho = tt[xx$V1][,    drop((acf((1/speed - mean)/(sd+1e-5), plot=FALSE, lag.max=5))[[1]]), by = trip]
  a = rho[, V1[2] ,by = trip][, V1]
  rho = round(mean(a), 2)
  print(paste0("Mean of autocorrelation: ", rho))

  # A.2 Model computation through the param_zeta function:
  # Now, we create our model which is a function (param_zeta), allowing to calculate the cumulative 
  # variance and taking as parameters: t0 (a real time which will be converted into time bins by a 
  # function calling the package traveltimeHMM), rho, linkfrom, linkto, len and sequence = FALSE.
  
  # The param_zeta function uses 2 loops: The first calculates the cumulative variance for all links - 1 
  # (without the last link which is a NA) per trip.
  # The second loop takes into account the missing value for the LinkId.to of the last link of ijk, 
  # since there is no destination for the last link of the route.
  # The cumulative variance is calculated in each loop by this line of code:  

  # Where vcumlative adjusts to each iteration of the loop; len [i] ^ 2 is the distance traveled for each 
  # link ijk to the square; d $ sd2 is the standard deviation of the speed for each ijk link of each trip; 
  # rho is the autocovariance of the speed calculated by the function get_rho; lenprev is the distance traveled 
  # to the previous link (lenprev = 1, for the first ijk link of each trip); vprev is the variance of the previous 
  # link (vprev = 1, for the first ijk link of each trip).
  
  # This function has 2 outputs:
  # tt (traveltime): It represents the distance for the link ijk * the average speed for the same link ijk. (t = 0, for the first ijk link of each trip).
  # S : It is the square root of the cumulative variance for each ijk link.
  
  param_zeta<-function(t0, rho,linkfrom, linkto, len, sequence = FALSE){
    t0 = as.POSIXlt(t0)
    time_bins <- rules2timebins(rules)
    tbin = time_bins(t0)
    g = obj.graph.stat.full[linkId.from %in% linkfrom]
    t = 0                              
    vcumlative = 0                               
    vprev = 1
    lenprev = 1
    if(sequence){
      t.seq = v.seq = numeric(length(len))
    }
    for(i  in 1:(length(len) - 1)){
      d = g[linkId.from == linkfrom[i] & linkId.to == linkto[i] & timeBins == tbin]
      t = t + len[i]*d$mean
      tbin = time_bins(t0 + t)
      vcumlative = vcumlative + len[i]^2  * d$sd^2  + 2*rho* len[i]*lenprev * d$sd * vprev
      vprev = d$sd
      lenprev = len[i]
      if(sequence){
        v.seq[i] = vcumlative
        t.seq[i] = t
      }
    }
    i = i+1
    if(is.na(linkto[i])){
      d = g[linkId.from == linkfrom[i] & timeBins == tbin & is.na(linkId.to)]
    }else{
      d = g[linkId.from == linkfrom[i] & linkId.to == linkto[i] & timeBins == tbin]
    }
    t = t + len[i]*d$mean
    vcumlative = vcumlative + len[i]^2 * d$sd^2 + 2* rho * len[i]*lenprev *d$sd *vprev
    if(sequence){
      v.seq[i] = vcumlative
      t.seq[i] = t
    }
    s = sqrt(vcumlative)
    if(sequence){
      list(tt =t, sd = s, v.seq = sqrt(v.seq), t.seq = t.seq)
    }else{
      list(tt =t, sd = s)
    }}
    
  # A.3 Residual variance.
  # We create a residual variance function, allowing to supply the residual variance taking as input: DB (data), rho, etsamp = NULL. 
  # This function uses the param_zeta function seen above.
  
  # First, it calculates obstt, which is the sum of tt (travel time) for each trip.
  # Then we add to the data D a column of the residues called res and calculated in the following way:
  if(is.null(samp)){
    A = obj.data.train[order(trip, time)]
    } else{
      A = obj.data.train[trip %in% samp][order(trip, time)]}
  B = A[, param_zeta(time[1], rho, linkId.from, linkId.to, length), by = trip]
  D = merge(B, A[, .(obstt= sum(tt)), trip])
  D[, res := (obstt - tt)/sd]
  res = list(db = D, res.sd = sd(D$res))
  v = res$res.sd
  print("This is the mean of the residuals")
  print(v)
  
  
  # Return variables
  obj <- list(variance = v, rho = rho)
  
  class(obj) = append(class(obj), "traveltime", after=0)
  
  invisible(obj)
}
  

predict.traveltimeCLT <- function(obj.traveltime = NULL, obj.graph.stat.full = NULL, data.test = NULL, bin = NULL , rules = NULL){
  
  # A.0 We are starting to transform our data so that it takes on a network form. 
  # So, we transform the linkId variable into two variables: LinkId.from and LinkId.to. 
  # Since a link represents a segment of a road network, 
  # we can thus know the origin and the destination of a vehicle for each link of this network.
  # We add a variable called N, which represents the number of times that an ijk link 
  # (i = linkId.from, j = linkId.to etk = timebins) is present in our data set. 
  # Since there is no destination j (linkId.to) for the last ijk link of the trip of a vehicle, 
  # we consider its value as NA.
  data.test[, linkId.from := linkId, by = trip]
  data.test[, linkId.to := shift(linkId, type = 'lead'), by = trip]
  data.test[, N:=.N, by =list(linkId.from, linkId.to, timeBins) ]
  
  
  # Sampling test data according to the model inputs
  samp.test = data.test[, timeBins[1], trip][V1 == bin , trip]
  
  
  # A.1 Model computation through the param_zeta function:
  # Now, we create our model which is a function (param_zeta), allowing to calculate the cumulative 
  # variance and taking as parameters: t0 (a real time which will be converted into time bins by a 
  # function calling the package traveltimeHMM), rho, linkfrom, linkto, len and sequence = FALSE.
  
  # The param_zeta function uses 2 loops: The first calculates the cumulative variance for all links - 1 
  # (without the last link which is a NA) per trip.
  # The second loop takes into account the missing value for the LinkId.to of the last link of ijk, 
  # since there is no destination for the last link of the route.
  # The cumulative variance is calculated in each loop by this line of code:  
  
  # Where vcumlative adjusts to each iteration of the loop; len [i] ^ 2 is the distance traveled for each 
  # link ijk to the square; d $ sd2 is the standard deviation of the speed for each ijk link of each trip; 
  # rho is the autocovariance of the speed calculated by the function get_rho; lenprev is the distance traveled 
  # to the previous link (lenprev = 1, for the first ijk link of each trip); vprev is the variance of the previous 
  # link (vprev = 1, for the first ijk link of each trip).
  
  # This function has 2 outputs:
  # tt (traveltime): It represents the distance for the link ijk * the average speed for the same link ijk. (t = 0, for the first ijk link of each trip).
  # S : It is the square root of the cumulative variance for each ijk link.
  param_zeta<-function(t0, rho,linkfrom, linkto, len, sequence = FALSE){
    t0 = as.POSIXlt(t0)
    time_bins <- rules2timebins(rules)
    tbin = time_bins(t0)
    g = obj.graph.stat.full[linkId.from %in% linkfrom]
    t = 0                               # time (mean)
    vcumlative = 0                                 # variance
    vprev = 1
    lenprev = 1
    if(sequence){
      t.seq = v.seq = numeric(length(len))
    }
    for(i  in 1:(length(len) - 1)){
      d = g[linkId.from == linkfrom[i] & linkId.to == linkto[i] & timeBins == tbin]
      t = t + len[i]*d$mean
      tbin = time_bins(t0 + t)
      vcumlative = vcumlative + len[i]^2  * d$sd^2  + 2*rho* len[i]*lenprev * d$sd * vprev
      vprev = d$sd
      lenprev = len[i]
      if(sequence){
        v.seq[i] = vcumlative
        t.seq[i] = t
      }
    }
    ## last linkId.to is NA
    i = i+1
    if(is.na(linkto[i])){
      d = g[linkId.from == linkfrom[i] & timeBins == tbin & is.na(linkId.to)]
    }else{
      d = g[linkId.from == linkfrom[i] & linkId.to == linkto[i] & timeBins == tbin]
    }
    t = t + len[i]*d$mean
    vcumlative = vcumlative + len[i]^2 * d$sd^2 + 2* rho * len[i]*lenprev *d$sd *vprev
    if(sequence){
      v.seq[i] = vcumlative
      t.seq[i] = t
    }
    s = sqrt(vcumlative)
    if(sequence){
      list(tt =t, sd = s, v.seq = sqrt(v.seq), t.seq = t.seq)
    }else{
      list(tt =t, sd = s)
    }}
  
  
  # A.2 Residual variance.
  # We create a residual variance function, allowing to supply the residual variance taking as input: DB (data), rho, etsamp = NULL. 
  # This function uses the param_zeta function seen above.
  
  # First, it calculates obstt, which is the sum of tt (travel time) for each trip.
  # Then we add to the data D a column of the residues called res and calculated in the following way:
  A = data.test[trip %in% samp.test, param_zeta(time[1], obj.traveltime$rho, linkId.from, linkId.to, length), by = trip]
  
  # We merge to be able to get the observed travel time, the real travel time, and the variance
  B = merge(data.test[, .(obstt = sum(tt)), trip], A)
  
  # A.3 We get the model estimations
  numerical_res<-function(tt, est, sym.q){
    list(
      MAREgeo = 100*exp(mean(log(abs(tt - est)/tt))),
      RMSE = sqrt(mean((tt - est)^2)),
      MAE  = mean(abs(tt - est)),
      ME  = mean(tt- est),
      MAPE = 100*mean(abs(tt - est)/tt),
      emprical.cov = 100*mean(abs(tt-est)<=sym.q),
      PI.length = mean(2*sym.q),
      PI.rel.length = 100*mean(2*sym.q/tt)
    )
  }
  # We get the results using the mean of the residuals v
  print("Model estimation using mean of the residuals v")
  print(t(B[ , numerical_res(obstt, tt, sd *qnorm(0.975)*obj.traveltime$variance)]))
  # We get the results without using the mean of the residuals v
  print("Model estimation without using mean of the residuals v")
  print(t(B[ , numerical_res(obstt, tt, sd *qnorm(0.975))]))
  
}


create_test_trips<-function(M = 500, db, min.n = 1){
  db[, linkId.from := linkId, by = trip]
  db[, linkId.to := shift(linkId, type = 'lead'), by = trip]
  db[, N:=.N, by =list(linkId.from, linkId.to, timeBins) ]
  A = db[, .(N = N[1]), by =list(linkId.from, linkId.to, timeBins) ]
  A[, N.left := 0 ] 
  setkey(A, linkId.from, linkId.to, timeBins)
  
  bank = c()
  unique.trips =db[, min(N>1)  , trip][V1 ==1][, trip]
  
  repeat{
    n = length(unique.trips)
    car = unique.trips[sample.int(n,1)]
    k = db[trip==car, .(linkId.from, linkId.to, timeBins)]
    
    if(A[k, all(N -N.left - 1> 0 )]){
      bank = c(bank,car)              ## add the trip
      unique.trips = setdiff(unique.trips, bank) ## removing all smapled trips
      ## updating A
      A[k, N.left:= N.left + 1]
      ## break if number of samples reached
      if(length(bank) >=M)
        break
    }
    
  }
  bank
}
