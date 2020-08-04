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

# finaldats$stimtrialcounter[finaldats$cond=="S"] <-  ceiling(finaldats$stimtrialcounter
#                                                             [finaldats$cond=="S"]/8)

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
 inh.Mpm_W=0,  inh.Mpm_N=0,  inh.Spm_W=0,  inh.Spm_N=0,
 LRN.SPP=0.0, LRN.MPP=0.0, inh_ex_rel=0, ALPH=1
 
 
 )

PMDC_learningall <- data.model.dmc(finaldats,
                                   PMDC_learningall_model)

#get all factor names
facs <- names(attr(PMDC_learningall_model,"factors"))

#get covariate row names
cv_rownames <- apply(apply(
  PMDC_learningall[[1]][,facs,drop=FALSE],2,as.character),1,paste,collapse=".")

#get coavariates
stimreps = as.data.frame(rep(0:7, ceiling(
  length(cv_rownames) / 8
))[1:length(cv_rownames)])

#give appropriate rownames
colnames(stimreps) <- "stimtrialcounter"

stimreps = stimreps[grepl( "p", cv_rownames),,drop=F]

attr(stimreps,"row.facs") <- rep(cv_rownames[grepl( "p", cv_rownames)],100)

#get appropriate trial numbers
ns <- table(PMDC_learningall[[1]][,facs],dnn=facs)
ns["w",,] <- 0
ns["n",,] <- 0
ns <- ns * 100

# stimreps[attr(stimreps, "row.facs")]
# 
# is.single <- grepl("S" ,attr(stimreps, "row.facs"))
# 
# table(stimreps$stimtrialcounter, attr(stimreps, "row.facs"))
# 
# trialnums = c()
# for (n_reps in 0:7) {
#   stim_ind_S = sample(which(is.single & stimreps==0), replace=FALSE, size=151)
#   stim_ind_M = sample(which(!is.single & stimreps==0), replace=FALSE, size=151)
#   stim_ind = c(stim_ind_S, stim_ind_M)
#   trialnums = c(trialnums, stim_ind)
# }
# 
# stimreps = stimreps[trialnums,]

# stimreps = rbind(stimreps, stimreps, stimreps)

stimreps= do.call("rbind", replicate(100, stimreps, simplify = FALSE))


#simulate
# test_data <- simulate.dmc(
#   p.vector = PMDC_learningall_p.vector,
#   model = PMDC_learningall_model,
#   n = ns*3,
#   cvs = stimreps
# )

# PM_acc <- test_data %>% filter (S == "p") %>% group_by(cond, stimtrialcounter)  %>% 
#   summarise (PMacc = mean(R =="P"))
# 
# ggplot(PM_acc, aes(factor(stimtrialcounter), PMacc)) +
#   geom_point(stat = "identity", aes(colour = cond), size=3)+
#   ylab("PM acc")+ geom_line(aes(group=cond,y=PMacc, col=cond), linetype=2) +ylim(0,1)


library(shiny)


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
                                mean_v.fa= -1, mean_v.SpP =1 ,mean_v.MpP = 0.5,
                                inh.Mpm_W=0.5,  inh.Mpm_N=0.5,  inh.Spm_W=0,  inh.Spm_N=0,
                                LRN.SPP=0.0, LRN.MPP=0.0, inh_ex_rel=0, ALPH=1
                                
                                
)

theme_set(theme_simple()+theme(axis.title=element_text(size=16))) 

ui <- fluidPage(
  titlePanel(title=h4("PM learning effects", align="center")),
  sidebarPanel( 
    sliderInput("MPP", "Multi lag:",min = -2, max = 2,step=0.05,value=c(1)),
    sliderInput("SPP", "Single lag:",min = -2, max = 2,step=0.05,value=c(1)),
    sliderInput("MBP", "Initial multi threshold (P)",min = 0, max = 2,step=0.05,value=c(1)),
    sliderInput("SBP", "Initial single threshold (P)",min = 0, max = 2,step=0.05,value=c(1)),
    sliderInput("inhrel", "Inhibition learning ratio:",min = -2, max = 2,step=0.05,value=c(1)),
    sliderInput("ALPH", "ALPH",min = 0, max = 2,step=0.05,value=c(1)),
    sliderInput("Asymptote_pP", "Asymptote_pP",min = 0, max = 4,step=0.05,value=c(1)),
    sliderInput("Asymptote_inh_W", "Asymptote_inh_W",min = 0, max = 2,step=0.05,value=c(1)),
    sliderInput("Asymptote_inh_N", "Asymptote_inh_N",min = 0, max = 2,step=0.05,value=c(1))
    
    ),
  mainPanel(plotOutput("plot2")))

server <- function(input,output){
  
  simmed_data <- reactive({
  
  PMDC_learningall_p.vector  <- c(t0=0.3,A=0.5,
                                  sd_v.t = 1,
                                  
                                  B.M.1.N = 1,B.M.1.W = 1, B.M.1.P = input$MBP, 
                                  B.S.1.N = 1,B.S.1.W = 1, B.S.1.P = input$SBP,
                                  B.M.2.N = 1,B.M.2.W = 1, B.M.2.P = input$MBP, 
                                  B.S.2.N = 1,B.S.2.W = 1, B.S.2.P = input$SBP,
                                  
                                  
                                  mean_v.SnN = 1,mean_v.SwN = 0, 
                                  mean_v.MnN = 1 , mean_v.MwN = 0, 
                                  mean_v.SnW = 0, mean_v.SwW = 1,  
                                  mean_v.MnW =0,  mean_v.MwW = 1, 
                                  mean_v.fa= -1, mean_v.SpP =input$Asymptote_pP ,mean_v.MpP = input$Asymptote_pP,
                                  inh.Mpm_W=input$Asymptote_inh_W,  inh.Mpm_N=input$Asymptote_inh_N,  
                                  inh.Spm_W=input$Asymptote_inh_W,  inh.Spm_N=input$Asymptote_inh_N,
                                  LRN.SPP=input$SPP, LRN.MPP=input$MPP, inh_ex_rel=input$inhrel,
                                  ALPH=input$ALPH
                                  
                                  
  )
  
  test_data <- simulate.dmc(
    p.vector = PMDC_learningall_p.vector,
    model = PMDC_learningall_model,
    n = ns,
    cvs = stimreps
  )
  
  test_data$stimtrialcounter= as.numeric(test_data$stimtrialcounter)
  
  PM_acc <- test_data %>% filter (S == "p") %>% group_by(cond, stimtrialcounter)  %>% 
    summarise (PMacc = mean(R =="P"))
  
  PM_RT <- test_data %>% filter (S == "p" & R=="P") %>% group_by(cond, stimtrialcounter)  %>% 
    summarise (PMRT = mean(RT))
  
  OT_RT <- test_data %>% filter (S == "p" & R!="P") %>% group_by(cond, stimtrialcounter)  %>% 
    summarise (PMRT = mean(RT))
  
  return (list(PM_acc, PM_RT, OT_RT))
  
  })
  
  # plots <- grid.arrange(ggplot(simmed_data()[[1]], aes(factor(stimtrialcounter), PMacc)) +
  #                geom_point(stat = "identity", aes(colour = cond), size=3)+
  #                ylab("PM acc")+ xlab("Number of repetitions") +
  #                geom_line(aes(group=cond,y=PMacc, col=cond), linetype=2) +ylim(0,1),
  #              
  #              ggplot(simmed_data()[[2]], aes(factor(stimtrialcounter), PMacc)) +
  #                geom_point(stat = "identity", aes(colour = cond), size=3)+
  #                ylab("PM acc")+ xlab("Number of repetitions") +
  #                geom_line(aes(group=cond,y=PMRT, col=cond), linetype=2) +ylim(0,1)
  #              )

  
  output$plot2<-renderPlot({
    # browser()
    grid.arrange(ggplot(simmed_data()[[1]], aes(stimtrialcounter, PMacc)) +
                   geom_point(stat = "identity", aes(colour = cond), size=3)+
                   ylab("PM acc")+ xlab("Number of repetitions") + 
                   geom_line(aes(group=cond,y=PMacc, col=cond), linetype=2) +ylim(0,1) +xlim(0,7),
                 
                 ggplot(simmed_data()[[2]], aes(stimtrialcounter, PMRT)) +
                   geom_point(stat = "identity", aes(colour = cond), size=3)+
                   ylab("PM RT")+ xlab("Number of repetitions") +
                   geom_line(aes(group=cond,y=PMRT, col=cond), linetype=2)+xlim(0,7)+ylim(0,3),
                 
                 ggplot(simmed_data()[[3]], aes(stimtrialcounter, PMRT)) +
                   geom_point(stat = "identity", aes(colour = cond), size=3)+
                   ylab("OT RT")+ xlab("Number of repetitions") +
                   geom_line(aes(group=cond,y=PMRT, col=cond), linetype=2)+xlim(0,7)+ylim(0,3),
                 
                 layout_matrix=matrix(c(1,2,3,NA), ncol=2)
    )
    },
    height = 800,width = 800) 
  
  
  }

shinyApp(ui, server)



