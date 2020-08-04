
library(dplyr)
source("dmc/dmc.R")
load("img/finaldats.RData")

convert_stimtrial_forthres <- function (stimtrialvec) {
  
  for (i in 1:length(stimtrialvec)){
    if(i!=1) stimtrialvec[i] <- max(stimtrialvec[i-1], stimtrialvec[i])
  }
  stimtrialvec
}

finaldats <- finaldats %>% group_by(s,cond,D) %>% 
  mutate(stimtrialthres = convert_stimtrial_forthres(stimtrialcounter)) %>% 
  select(1:5, stimtrialthres, everything())

finaldats <- as.data.frame(finaldats)

finaldats$stimtrialcounter <- finaldats$stimtrialthres-1


load_model ("LBA", "lbaN_B_learning_exp.R")

mapmeanv_same <-
  empty.map(list(
    S = c("n", "w", "p"),
    cond = c("S", "M"),
    D = c("1", "2"),
    R = c("N", "W", "P")
  ), 
  levels=c(
    "SnN","SwN","SnW","SwW","pP",
    "MnN","MwN","MnW","MwW","fa"))

mapmeanv_same [1:36] <- c(
  "SnN","SwN","SwN",
  "MnN","MwN","MwN",
  "SnN","SwN","SwN",
  "MnN","MwN","MwN",
  
  "SnW","SwW","SwW",
  "MnW","MwW","MwW",
  "SnW","SwW","SwW",
  "MnW","MwW","MwW",
  
  "fa","fa","pP",
  "fa","fa","pP",
  "fa","fa","pP",
  "fa","fa","pP"
)


map_reactive_difference_same <-
  empty.map(list(
    S = c("n", "w", "p"),
    cond = c("S", "M"),
    D = c("1", "2"),
    R = c("N", "W", "P")
  ),
  levels = c("pm_W","pm_N",
             "zero"))

map_reactive_difference_same[1:36] <- c( 
  "zero","zero","pm_N",
  "zero","zero","pm_N",
  "zero","zero","pm_N",
  "zero","zero","pm_N",
  
  "zero","zero","pm_W",
  "zero", "zero", "pm_W",
  "zero","zero","pm_W",
  "zero", "zero", "pm_W",
  
  "zero", "zero", "zero",
  "zero","zero", "zero",
  "zero", "zero", "zero",
  "zero","zero", "zero"
  
)


mapsdv <-
  empty.map(list(
    S = c("n", "w", "p"),
    cond = c("S", "M"),
    D = c("1", "2"),
    R = c("N", "W", "P")
  ), 
  levels=c('t', 'f'))

mapsdv[1:36] <- c(
  "t","f","f",
  "t","f","f",
  "t","f","f",
  "t","f","f",
  
  "f","t","f",
  "f","t","f",
  "f","t","f",
  "f","t","f",
  
  "f","f","t",
  "f","f","t",
  "f","f","t",
  "f","f","t"
)


map_rep_LRN <-
  empty.map(list(
    S = c("n", "w", "p"),
    cond = c("S", "M"),
    D = c("1", "2"),
    R = c("N", "W", "P")
  ), 
  levels=c("SPP",
           "MPP", "none"))

map_rep_LRN[1:36] <- c(
  "none","none","none",
  "none","none","none",
  "none","none","none",
  "none","none","none",
  
  "none","none","none",
  "none","none","none",
  "none","none","none",
  "none","none","none",
  
  "none","none","SPP",
  "none","none","MPP",
  "none","none","SPP",
  "none","none","MPP"
)


learningexp_alphbound_model <- model.dmc(
  p.map = list(
    A = "1",B = c("cond", "D", "R"), t0 = "1", mean_v = c("MAPMV"),
    sd_v = c("MAPSDV"), st0 = "1", LRN="MAPLRN",
    N = "cond", inh = c("MAPINH"), pmf="1", inh_ex_rel="1", 
    ALPH="1"
  ),
  match.map = list(
    M = list(n = "N", w = "W", p = "P"), 
    MAPMV = mapmeanv_same,
    MAPINH = map_reactive_difference_same,
    MAPSDV = mapsdv,
    MAPLRN=map_rep_LRN
  ),
  factors = list(
    S = c("n", "w", "p"), cond = c("S", "M"), D = c("1", "2")
  ),
  cvs="stimtrialcounter",
  constants = c(
    N.S = 3, N.M = 3, st0 = 0, sd_v.f = 1, inh.zero=0, pmf = -Inf,
    LRN.none=0, inh_ex_rel=1
  ),
  responses = c("N", "W", "P"),type = "normN"
)



learningexp_alphbound_p.vector  <- c(t0=0.3,A=0.5,
                                sd_v.t = 1,
               
 B.M.1.N = 1,B.M.1.W = 1, B.M.1.P = 1, 
 B.S.1.N = 1,B.S.1.W = 1, B.S.1.P = 1,
 B.M.2.N = 1,B.M.2.W = 1, B.M.2.P = 1, 
 B.S.2.N = 1,B.S.2.W = 1, B.S.2.P = 1,

 mean_v.SnN = 1,mean_v.SwN = 0, 
 mean_v.MnN = 1 , mean_v.MwN = 0, 
 mean_v.SnW = 0, mean_v.SwW = 1,  
 mean_v.MnW =0,  mean_v.MwW = 1, 
 mean_v.fa= -1, mean_v.pP =1 ,
 inh.pm_W=0,  inh.pm_N=0,  
 LRN.SPP=0, LRN.MPP=0, ALPH=0
 
 
 )

learningexp_alphbound_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(learningexp_alphbound_p.vector)),
  p1=learningexp_alphbound_p.vector,                           
  p2=c(1,1,1,rep(1, 12), rep(2, 15)),
  lower=c(0.1, 0,0, rep(0, 12),rep(NA, 12), rep(NA, 2), 0),
  upper=c(1,10, rep(Inf, length(learningexp_alphbound_p.vector)-2))
)

learningexp_alphbound_64 <- data.model.dmc(finaldats,
                                   learningexp_alphbound_model)

learningexp_alphbound_samples <- h.samples.dmc(nmc = 180,
                                          learningexp_alphbound_p.prior,
                                          learningexp_alphbound_64, thin=20)

save(learningexp_alphbound_samples, 
     file="learningexp_alphbound_samples.RData")


