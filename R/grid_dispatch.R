# source("dmc/dmc.R")
# load_model ("LBA", "lbaN_B_learning_nothres.R")
# 
# run.grid.dmc("recov_linear_rate_learning",model.dir ="LBA",
#              model.file="lbaN_B_learning_nothres.R",user="ljs392",
#              n.add=60, wall.hours = 300,
#              GB = 3, max.try=1000)


source("dmc/dmc.R")
load_model ("LBA", "lbaN_B_learning_exp.R")

run.grid.dmc("recov_exp_learning_alphbound",model.dir ="LBA",
             model.file="lbaN_B_learning_exp.R",user="ljs392",
             n.add=60, wall.hours = 300,
             GB = 3, max.try=10)
