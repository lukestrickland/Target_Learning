rm(list=ls())

setwd("~/Associative_Learning")
source("dmc/dmc.R")
load_model ("LBA", "lbaN_B_learning_exp.R")


load("~/Associative_Learning/recov_exp_learning_alphbound.RData")

recov_exp_learning_alphbound <- h.RUN.dmc(recov_exp_learning_alphbound,
                                           cores = length(recov_exp_learning_alphbound))

save(recov_exp_learning_alphbound, file= "recov_exp_learning_alphbound_finished.RData")