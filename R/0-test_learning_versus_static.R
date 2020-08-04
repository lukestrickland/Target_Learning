

###Test the exponential law of practice front end


#test the learning model versus a static PMDC
#to make sure the learning likelihoods etc. are appropriate

source("dmc/dmc.R")
source("dmc/dmc_extras.R")
library(dplyr)
load_model ("LBA", "lbaN_B_learning_exp.R")


load("img/finaldats.RData")

convert_stimtrial_forthres <- function (stimtrialvec) {
  
  for (i in 1:length(stimtrialvec)){
    if(i!=1) stimtrialvec[i] <- max(stimtrialvec[i-1], stimtrialvec[i])
  }
  stimtrialvec
}

finaldats$stimtrialcounter[finaldats$cond=="S"] <-  ceiling(finaldats$stimtrialcounter
                                                            [finaldats$cond=="S"]/8)

finaldats <- finaldats %>% group_by(s,cond,D) %>% 
  mutate(stimtrialthres = convert_stimtrial_forthres(stimtrialcounter)) %>% 
  select(1:5, stimtrialthres, everything())

finaldats <- as.data.frame(finaldats)

finaldats$stimtrialcounter <- finaldats$stimtrialthres-1



mapmeanv <-
  empty.map(list(
    S = c("n", "w", "p"),
    cond = c("S", "M"),
    D = c("1", "2"),
    R = c("N", "W", "P")
  ), 
  levels=c(
    "SnN","SwN","SnW","SwW","SpP",
    "MnN","MwN","MnW","MwW","MpP", "fa"))

mapmeanv[1:36] <- c(
  "SnN","SwN","SwN",
  "MnN","MwN","MwN",
  "SnN","SwN","SwN",
  "MnN","MwN","MwN",
  
  "SnW","SwW","SwW",
  "MnW","MwW","MwW",
  "SnW","SwW","SwW",
  "MnW","MwW","MwW",
  
  "fa","fa","SpP",
  "fa","fa","MpP",
  "fa","fa","SpP",
  "fa","fa","MpP"
)


map_reactive_difference <-
  empty.map(list(
    S = c("n", "w", "p"),
    cond = c("S", "M"),
    D = c("1", "2"),
    R = c("N", "W", "P")
  ),
  levels = c("Mpm_W","Mpm_N",
             "Spm_W","Spm_N",
             "zero"))

map_reactive_difference[1:36] <- c( 
  "zero","zero","Spm_N",
  "zero","zero","Mpm_N",
  "zero","zero","Spm_N",
  "zero","zero","Mpm_N",
  
  "zero","zero","Spm_W",
  "zero", "zero", "Mpm_W",
  "zero","zero","Spm_W",
  "zero", "zero", "Mpm_W",
  
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

PMDC_learningall_model <- model.dmc(
  p.map = list(
    A = "1",B = c("cond", "D", "R"), t0 = "1", mean_v = c("MAPMV"),
    sd_v = c("MAPSDV"), st0 = "1", LRN="MAPLRN",
    N = "cond", inh = c("MAPINH"), pmf="1", inh_ex_rel="1", 
    ALPH="1"
  ),
  match.map = list(
    M = list(n = "N", w = "W", p = "P"), MAPMV = mapmeanv,
    MAPINH = map_reactive_difference,
    MAPSDV = mapsdv,
    MAPLRN=map_rep_LRN
  ),
  factors = list(
    S = c("n", "w", "p"), cond = c("S", "M"), D = c("1", "2")
  ),
  cvs="stimtrialcounter",
  constants = c(
    N.S = 3, N.M = 3, st0 = 0, sd_v.f = 1, inh.zero=0, pmf = -Inf,
    LRN.none=0
  ),
  responses = c("N", "W", "P"),type = "normN"
)

tests <- c()
for (i in 0:7){
  
  LRN.SPP=0; LRN.MPP=2; inh_ex_rel=1; ALPH=0.8
  N=i
  
  
  PMDC_learningall_p.vector  <- c(t0=0.3,A=0.5,
                                  sd_v.t = 1,
                                  
                                  B.M.1.N = 1,B.M.1.W = 1, B.M.1.P = 1, 
                                  B.S.1.N = 1,B.S.1.W = 1, B.S.1.P = 1,
                                  B.M.2.N = 1,B.M.2.W = 1, B.M.2.P = 1, 
                                  B.S.2.N = 1,B.S.2.W = 1, B.S.2.P = 1,
                                  
                                  
                                  mean_v.SnN = 1,mean_v.SwN = 0, 
                                  mean_v.MnN = 1 , mean_v.MwN = 0, 
                                  mean_v.SnW = 0, mean_v.SwW = 1,  
                                  mean_v.MnW =0,  mean_v.MwW = 1, 
                                  mean_v.fa= -1, mean_v.SpP =1 ,mean_v.MpP = 1,
                                  inh.Mpm_W=1,  inh.Mpm_N=1,  inh.Spm_W=1,  inh.Spm_N=1,
                                  LRN.SPP=LRN.SPP, LRN.MPP=LRN.MPP, inh_ex_rel=inh_ex_rel, ALPH=ALPH
                                  
                                  
  )
  
  
  
  PMDC_learningall <- data.model.dmc(finaldats,
                                     PMDC_learningall_model)
  
  
  load_model ("LBA", "lbaN_B_pmf_probit.R")
  
  PMDC_nolearning_model <- model.dmc(
    p.map = list(
      A = "1",B = c("cond", "D", "R"), t0 = "1", mean_v = c("MAPMV"),
      sd_v = c("MAPSDV"), st0 = "1", 
      N = "cond", inh = c("MAPINH"), pmf="1"),
    match.map = list(
      M = list(n = "N", w = "W", p = "P"), MAPMV = mapmeanv,
      MAPINH = map_reactive_difference,
      MAPSDV = mapsdv
    ),
    factors = list(
      S = c("n", "w", "p"), cond = c("S", "M"), D = c("1", "2")
    ),
    cvs="stimtrialcounter",
    constants = c(
      N.S = 3, N.M = 3, st0 = 0, sd_v.f = 1, inh.zero=0, pmf = -Inf
    ),
    responses = c("N", "W", "P"),type = "normN"
  )
  
  
  
  PMDC_nolearning_p.vector  <- c(t0=0.3,A=0.5,
                                  sd_v.t = 1,
                                  
                                  B.M.1.N = 1,B.M.1.W = 1, B.M.1.P = 1, 
                                  B.S.1.N = 1,B.S.1.W = 1, B.S.1.P = 1,
                                  B.M.2.N = 1,B.M.2.W = 1, B.M.2.P = 1, 
                                  B.S.2.N = 1,B.S.2.W = 1, B.S.2.P = 1,
                                  
                                  
                                  mean_v.SnN = 1,mean_v.SwN = 0, 
                                  mean_v.MnN = 1 , mean_v.MwN = 0, 
                                  mean_v.SnW = 0, mean_v.SwW = 1,  
                                  mean_v.MnW =0,  mean_v.MwW = 1, 
                                  mean_v.fa= -1, mean_v.SpP = 1- LRN.SPP*exp(-ALPH*N)
                                 
                                 ,mean_v.MpP = 1- LRN.MPP*exp(-ALPH*N),
                                  inh.Mpm_W=1 - inh_ex_rel*LRN.MPP*exp(-ALPH*N) 
                                 ,  inh.Mpm_N=1- inh_ex_rel*LRN.MPP*exp(-ALPH*N), 
                                 inh.Spm_W=1- inh_ex_rel*LRN.SPP*exp(-ALPH*N),  
                                 inh.Spm_N=1- inh_ex_rel*LRN.SPP*exp(-ALPH*N)
                                  
                                  
  )
  
  PMDC_nolearning <- data.model.dmc(finaldats,
                                    PMDC_nolearning_model)
  
  
  
  
  # for (1 in 1:length(PMDC_learningall))
  
  load_model ("LBA", "lbaN_B_learning_exp.R")
  data = PMDC_learningall[[1]]
  p.vector= PMDC_learningall_p.vector
  
  test1= likelihood.dmc(p.vector, data)[data$stimtrialcounter==N]
  
  load_model ("LBA", "lbaN_B_pmf_probit.R")
  data = PMDC_nolearning[[1]]
  p.vector = PMDC_nolearning_p.vector 
  
  test2= likelihood.dmc(p.vector, data)[data$stimtrialcounter==N]
  
  tests[i] <- all.equal(test1, test2)
}




##checking likelihood and random match
source("dmc/dmc.R")
source("dmc/dmc_extras.R")
load("samples_data/learningexp_alphbound_samples.RData")
ns <- table(learningexp_alphbound_samples[[1]]$data[,names(attr(PMDC_learningall_model,"factors"))],
            dnn=names(attr(PMDC_learningall_model,"factors")))


load_model ("LBA", "lbaN_B_learning_exp.R")

plot.invisible <- function(...){
  ff <- tempfile()
  png(filename=ff)
  res <- plot.cell.density(...)
  dev.off()
  unlink(ff)
  res
}


facs <- lapply(strsplit(dimnames(PMDC_learningall_model)[[1]],".",fixed=TRUE),
               function(x){x[-length(x)]})
facs <- facs[1:(length(facs)/length(attr(PMDC_learningall_model,"responses")))] 

levs <- attr(PMDC_learningall_model,"factors")
fnams <- names(levs)
facs <- data.frame(t(matrix(unlist(facs),nrow=length(fnams))))
names(facs) <- fnams



LRN.SPP=0; LRN.MPP=2; inh_ex_rel=1; ALPH=0.8


PMDC_learningall_p.vector  <- c(t0=0.3,A=0.5,
                                sd_v.t = 1,
                                
                                B.M.1.N = 1,B.M.1.W = 1, B.M.1.P = 1, 
                                B.S.1.N = 1,B.S.1.W = 1, B.S.1.P = 1,
                                B.M.2.N = 1,B.M.2.W = 1, B.M.2.P = 1, 
                                B.S.2.N = 1,B.S.2.W = 1, B.S.2.P = 1,
                                
                                
                                mean_v.SnN = 1,mean_v.SwN = 0, 
                                mean_v.MnN = 1 , mean_v.MwN = 0, 
                                mean_v.SnW = 0, mean_v.SwW = 1,  
                                mean_v.MnW =0,  mean_v.MwW = 1, 
                                mean_v.fa= -1, mean_v.SpP =1 ,mean_v.MpP = 1,
                                inh.Mpm_W=1,  inh.Mpm_N=1,  inh.Spm_W=1,  inh.Spm_N=1,
                                LRN.SPP=LRN.SPP, LRN.MPP=LRN.MPP, inh_ex_rel=inh_ex_rel, ALPH=ALPH
                                
                                
)


check_cell <- function (S, cond, R,D, stimrepnum, ns, simnum=1e5, ymax=NA) {
  
  ###Get factors 
  facs <- lapply(strsplit(dimnames(PMDC_learningall_model)[[1]],".",fixed=TRUE),
                 function(x){x[-length(x)]})
  facs <- facs[1:(length(facs)/length(attr(PMDC_learningall_model,"responses")))] 
  
  levs <- attr(PMDC_learningall_model,"factors")
  fnams <- names(levs)
  facs <- data.frame(t(matrix(unlist(facs),nrow=length(fnams))))
  names(facs) <- fnams
  
  
  
  which_cell <- paste(S,cond,D, sep=".")
  
  stimreps <- data.frame(stimtrialcounter=rep(stimrepnum,simnum))
  stimreps$stimtrialcounter <- as.numeric(stimreps$stimtrialcounter)
  attr(stimreps, "row.facs") <- rep(which_cell, simnum)
  
  
  notD <- c("1", "2")[!c("1", "2") %in% D]
  notcond <- c("S", "M")[!c("S", "M") %in% cond]
  notS <- c("n", "w", "p")[!c("n", "w", "p") %in% S]
  
  ns[S,cond,D] <- simnum
  ns[notS,,] <- 0
  ns[,notcond,] <- 0
  ns[,,notD] <- 0
  
  # set.seed(1)
  data <- simulate.dmc(PMDC_learningall_p.vector, PMDC_learningall_model, 
                       n = ns , cvs= stimreps)
  
  ps <- table(data$R)/dim(data)[1]
  
  dns <- plot.invisible(data,xlim=c(0,7), ymax=ymax, save.density=TRUE)
  data2 <- data.frame(RT = dns[[R]]$x)
  data2$S <- S
  data2$cond <- cond
  data2$R <- R
  data2$D <- D
  data2$stimtrialcounter <-stimrepnum
  levels(data2$S) <- c("n", "w", "p")
  levels(data2$cond) <- c("S", "M")
  levels(data2$R) = c("N", "W", "P")
  levels(data2$D) = c("1", "2")
  dm = data.model.dmc(data2, PMDC_learningall_model)
  
  d <- likelihood.dmc(PMDC_learningall_p.vector, dm, min.like=-Inf)
  
  plot(dns[[R]]$x,dns[[R]]$y,type="l", main=paste(which_cell, R))
  lines(dns[[R]]$x,d,col="red")
  
}

par(mfrow=c(3,3))

for (i in unique(facs$S)){
  for (j in unique(facs$cond)) {
    for (k in unique(facs$D)){
      for (m in c("N", "W", "P"))
        check_cell(S=i, cond=j, D=k, R=m, stimrepnum=7, ns=ns, simnum=1e5)
      
    }
    
  }
  
}

check_cell(S="p", cond="S", D="1", R="P", stimrepnum=64, ns=ns)