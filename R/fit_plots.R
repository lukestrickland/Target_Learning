
plot_fits <- function(pp){
  
  avfit <- GET.fitgglist.dmc(pp, factors=c("S", "cond"))
  avfit_noR <- GET.fitgglist.dmc(pp, factors=c("S", "cond"), noR=TRUE)
  
  PM_acc <-
    avfit[['pps']][avfit[['pps']]$R == "P" &
                     avfit[['pps']]$S == "p",]
  PM_acc  <- PM_acc[,!colnames(PM_acc) %in% "R"]
  plotPM <-
    ggplot.RP.dmc(PM_acc, xaxis = "cond") + xlab("") + ylab("")
  
  ongoing_acc <-
    avfit[['pps']][avfit[['pps']]$R != "P" &
                     avfit[['pps']]$S != "p" &
                     toupper(avfit[['pps']]$S) ==
                     (avfit[['pps']]$R),]
  ongoing_acc <- ongoing_acc[,-3]
  plotOT <-
    ggplot.RP.dmc(ongoing_acc, xaxis = "cond") + ylim(0.7, 1) + xlab("") +
    ylab("Accuracy")
  
  ongoing_RT <-
    avfit[['RTs']][avfit[['RTs']]$R != "P" &
                     avfit[['RTs']]$S != "p",]
  plotOTRT <-
    ggplot.RT.dmc(ongoing_RT, xaxis = "cond") + xlab("") + ylab ("RT (seconds)")
  
  PMRT <-
    avfit_noR[['RTs']][avfit_noR[['RTs']]$S == "p" &
                         !(is.na(avfit_noR[['RTs']]$data)),]
  plotPMRT <-
    ggplot.RT.dmc(PMRT, xaxis = "cond") + ylim(0.25, 1.25) + xlab("") + ylab("")
  
  grid.arrange(plotOT, plotOTRT, plotPM, plotPMRT, layout_matrix = cbind(c(1, 2, 2), c(1, 2, 2), c(3, 4, NA)))
}



plot_Bs <- function(msds) {
  Bs <- msds[grep ("B", rownames(msds)), ]
  ## new thing make a data fram efor ggplot
  Bs$PM <- NA
  Bs$day <- NA
  Bs$R <- NA
  Bs$PM[grep ("S", rownames(Bs))] <- "Single"
  Bs$PM[grep ("M", rownames(Bs))] <- "Multi"
  Bs$R[grep ("N", rownames(Bs))] <- "Non-word"
  Bs$R[grep ("W", rownames(Bs))] <- "Word"
  Bs$R[grep ("P", rownames(Bs))] <- "PM"
  Bs$day[grep ("1", rownames(Bs))] <- "One"
  Bs$day[grep ("2", rownames(Bs))] <- "Two"
  
  plot.df <- Bs
  plot.df$day <- factor(plot.df$day, levels = c("One", "Two"))
  plot.df$R <- factor(plot.df$R, levels = c("Non-word", "Word", "PM"))
  Bs <- ggplot(plot.df, aes(factor(day), M)) +
    geom_point(stat = "identity", aes(shape = PM)) +
    geom_errorbar(aes(
      ymax = M + SD,
      ymin = M - SD,
      width = 0.2
    )) +
    xlab("Day") + ylab("Threshold") +
    theme(axis.line.x = element_line(),
          axis.line.y = element_line()) + geom_line(aes(group = PM, y = M), linetype =
                                                      2) +
    facet_grid(. ~ R)
  Bs
}

plot_mvs <- function(msds) {
  mvs <- msds[grep ("mean_v", rownames(msds)), ]
  mvs$PM <- NA
  mvs$R <- NA
  mvs$S <- NA
  SPM <- grep ("S", rownames(mvs))
  MPM <- grep ("M", rownames(mvs))
  mvs$PM[SPM] <- "Single"
  mvs$PM[MPM] <- "Multi"
  mvs$R[grep ("N", rownames(mvs))] <-
    "Non-word"
  mvs$R[grep ("W", rownames(mvs))] <-
    "Word"
  mvs$R[grep ("P", rownames(mvs))] <- "PM"
  mvs$R[grep ("WN", rownames(mvs))] <- "Non-word"
  
  mvs$S[grep ("n", rownames(mvs))] <-
    "Non-word Trial"
  mvs$S[grep ("w", rownames(mvs))] <-
    "Word Trial"
  mvs$S[grep ("p", rownames(mvs))] <- "PM Trial"
  mvs$PM[rownames(mvs) == "mean_v.fa"] <-
    "False alarm"
  mvs$R[rownames(mvs) == "mean_v.fa"] <- "PM"
  mvs$S[rownames(mvs) == "mean_v.fa"] <- "Word"
  fa2 <- mvs[rownames(mvs) == "mean_v.fa", ]
  fa2$S <- "Non-word"
  mvs <- rbind(mvs, fa2)
  
  
  plot.df <- mvs
  plot.df$PM <-
    factor(plot.df$PM, levels = c("Single", "Multi", "False alarm"))
  plot.df$S <-
    factor(plot.df$S, levels = c("Non-word Trial", "Word Trial", "PM Trial"))
  plot.df$R <- factor(plot.df$R, levels = c("Non-word", "Word", "PM"))
  plot.df <- plot.df[!plot.df$PM == "False alarm", ]
  names(plot.df)[4] <- "Accumulator"
  
  V <- ggplot(plot.df, aes(factor(PM), M)) +
    geom_point(stat = "identity", aes(shape = Accumulator), size = 2.5) +
    geom_errorbar(aes(
      ymax = M + SD,
      ymin = M - SD,
      width = 0.3
    )) +
    xlab("PM condition") + ylab("Accumulation Rate") +
    # theme(text = element_text(size=24)) +
    theme(axis.line.x = element_line(),
          axis.line.y = element_line()) + geom_line(aes(group = Accumulator, y =
                                                          M), linetype = 2) +
    facet_grid(. ~ S, scales = "free", space = "free")  +
    labs(Accumulator = "Latent Accumulator")
  
  V
}