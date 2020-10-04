# Created by Adrien Hernandez and Mohamad Elmasri

library(traveltimeHMM)
library(dplyr)
library(tidyr)
library(data.table)

rules = list(
  list(start='6:30', end= '9:00', days = 0:6, tag='MR'),
  list(start='15:00', end= '18:00', days = 0:6, tag='ER')
)

traveltimeCLT <- function(data_train = NULL, l = 2, M = 1000, data_TimeBins = c("MR", "ER", "Other"), bin = "MR", tb_rules = rules){
  
  # Creating column linkId.from, linkId.to and N
  data_train[, linkId.from := linkId, by = trip]
  data_train[, linkId.to := shift(linkId, type = 'lead'), by = trip]
  data_train[, N:=.N, by =list(linkId.from, linkId.to, timeBins) ]
  
  L = l  # minimum number of observations
  graph.stat = data_train[, .(mu = mean(1/speed),  sd2 = var(1/speed), N=N[1]) ,by = list(linkId.from, linkId.to, timeBins)]
  
  full.graph = graph.stat[, .(timeBins = data_TimeBins) ,by = list(linkId.from, linkId.to)]
  
  graph.stat = merge(full.graph, graph.stat, by= c('linkId.from', 'linkId.to', 'timeBins'), all.x = TRUE)
  graph.stat$N[is.na(graph.stat$N)]<-0
  
  graph.stat.from.tb = data_train[, .(mu = mean(1/speed),
                                 sd2 = var(1/speed), N= .N) ,
                             by = list(linkId.from, timeBins)]
  
  graph.stat.tb = data_train[, .(mu = mean(1/speed),
                            sd2 = var(1/speed), N= .N) ,by = list(timeBins)]
  
  graph.stat.full =  merge(graph.stat,
                           graph.stat.from.tb[N>L, .(mu.from.tb = mu,
                                                     sd2.from.tb = sd2, linkId.from, timeBins)],
                           all.x = TRUE,
                           by.x=c('linkId.from', 'timeBins'),
                           by.y = c('linkId.from', 'timeBins'))
  graph.stat.full =  merge(graph.stat.full,
                           graph.stat.tb[N>L, .(mu.tb = mu, sd2.tb = sd2, timeBins)],
                           all.x = TRUE, by.x=c('timeBins'), by.y = c('timeBins'))
  
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
  
  data_train = merge(data_train, graph.stat.full, all.x = TRUE, by = c('linkId.from', 'linkId.to', 'timeBins'), sort = FALSE)[order(trip,time)]
  
  set.seed(2020)
  
  # Sample_time_bins_Sumo
  t = data_train[, .N , by=trip][N>10][, trip]
  sample_timebin = data_train[trip %in% t, .(timeBins= if(all(timeBins == timeBins[1])) timeBins[1] else 'remove' ), by=trip]
  sample_timebin = sample_timebin[!timeBins == 'remove']
  samp = sample_timebin[timeBins == bin][, trip]
  samp = samp[sample.int(length(samp),M)]
  
  # get rho
 
  if(is.null(samp)){
    tt = train
    xx = tt[, .I[.N>10],by = trip]
    } else{
    tt = data_train[trip %in% samp]
    xx = tt[trip %in% samp, .I[.N>10],by = trip]
    }
    rho = tt[xx$V1][,    drop((acf((1/speed - mean)/(sd+1e-5), plot=FALSE, lag.max=5))[[1]]), by = trip]
    # We then remove the first autocorrelation from each trip (trip) since it is 100% using this line of code.
    a = rho[, V1[2] ,by = trip][, V1]
    
    rho = round(mean(a), 2)
    print("This is the mean of the autocorrelation")
    print(rho)
    
    # Algorithm
    
  param_zeta<-function(t0, rho,linkfrom, linkto, len, sequence = FALSE){
    tb_rules = rules
    t0 = as.POSIXlt(t0)
    time_bins <- rules2timebins(tb_rules)
    tbin = time_bins(t0)
    g = graph.stat.full[linkId.from %in% linkfrom]
    t = 0                               # time (mean)
    vcumlative = 0                                 # variance
    vprev = 1
    lenprev = 1
    if(sequence){
      t.seq = v.seq = numeric(length(len))
    }
    for(i  in 1:(length(len) - 1)){
      ##for(i in 1:81){
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
    
    # Residual_variance 
    
  if(!is.null(samp)){
    A = data_train[trip %in% samp][order(trip, time)]
    } else{
    A = data_train[order(trip, time)]}
  
    B = A[, param_zeta(time[1], rho, linkId.from, linkId.to, length), by = trip]
    D = merge(B, A[, .(obstt= sum(tt)), trip])
    D[, res := (obstt - tt)/sd]
    res = list(db = D, res.sd = sd(D$res))
    v = res$res.sd
    print("This is the mean of the residuals")
    print(v)
    return(v)
}
  
predict.traveltimeCLT <- function(data_test = NULL, bin = "MR"){
  
  data_test[, linkId.from := linkId, by = trip]
  data_test[, linkId.to := shift(linkId, type = 'lead'), by = trip]
  data_test[, N:=.N, by =list(linkId.from, linkId.to, timeBins) ]
  
  
  # We take trips from the test set that occured in the morning
  samp_test = data_test[, timeBins[1], trip][V1 == bin , trip]
  
  
  # We apply the method on the test data
  A = data_test[trip %in% samp_test, param_zeta(time[1], rho, linkId.from, linkId.to, length), by = trip]
  
  # We merge to be able to get the observed travel time, the real travel time, and the variance
  B = merge(data_test[, .(obstt = sum(tt)), trip], A)
  
  # We get the results using the mean of the residuals v
  
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
  
  print("Model estimation using V")
  print(t(B[ , numerical_res(obstt, tt, sd *qnorm(0.975)*v)]))
  # We get the results without using v
  print("Model estimation without V")
  print(t(B[ , numerical_res(obstt, tt, sd *qnorm(0.975))]))
  
}

montoto <- traveltimeCLT(data_train = train)
