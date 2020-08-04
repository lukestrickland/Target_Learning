source("dmc/dmc.R")
source("dmc/dmc_extras.R")

load_model ("LBA", "lbaN_B_learning_exp.R")
load("samples_data/learningexp_alphbound_samples.RData")
model <- attr(learningexp_alphbound_samples[[1]]$data, "model")

facs <- names(attr(model,"factors"))

cvs <- 
  learningexp_alphbound_samples[[1]]$data[,attr(model,"cvs"), drop=FALSE]

attr(cvs,"row.facs") <- apply(apply(
  learningexp_alphbound_samples[[1]]$data[,facs,drop=FALSE],2,as.character),1,paste,collapse=".")


msds <- get.msds(learningexp_alphbound_samples)
sim.p.vector <- msds$M; names(sim.p.vector) <- rownames(msds)

ns <- get.ns.dmc(learningexp_alphbound_samples[[1]])

#simulate 100 participants

for (i in 1:100) {
  data <- simulate.dmc(sim.p.vector,model, n=ns, cvs=cvs)
  data <- cbind(i, data)
  if (i ==1) okdats <- data else okdats <- rbind(okdats, data)
}

#make into a multi-subject data frame 
names(okdats)[1] <- "s"
okdats$s <-factor(okdats$s)
data.model <- data.model.dmc(okdats,model)

recov_exp_learning_alphbound <- h.samples.dmc(nmc = 180,learningexp_alphbound_samples[[1]]$p.prior,
                                         data.model, thin=20)

save(recov_exp_learning_alphbound, file="recov_exp_learning_alphbound.RData")




