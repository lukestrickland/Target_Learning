#Inputs: a list of csv files within data/ to stack together
#Assumes each csv name starts with p*_
#Outputs: A data frame of those csvs read and stacked up
stack_dats <- function(files) {
  for (i in 1:length(files)) {
    single_df <-
      read.csv(paste("data/", files[i], sep = ""), stringsAsFactors =
                 FALSE)
    ppt <- str_match(files[i], "p.*(?=_d)")[1]
    single_df$s <- ppt
    if (i == 1) {
      full_data <- single_df
    } else {
      full_data <- rbind(full_data, single_df[colnames(full_data)])
    }
  }
  full_data
}

#Inputs: a data frame with an RT column
#and a FACTOR 's' column (for subject) 
#Outputs: a similar data frame, 
#except with any RTs below 0.2 seconds removed
#and RTs above the mean + (IQR/1.349)*3
#note that IQR/1.349 is a robust 
#approximation of standard deviation.
#means and IQR of RT calculated for each subject

clean <- function(df) {
  dfc <- df
  n = tapply(df$RT, list(df$s), length)
  ns = tapply(df$RT, list(df$s), length)
  mn = tapply(df$RT, list(df$s), mean)
  sd = tapply(df$RT, list(df$s), IQR)
  upper <- mn + 3 * (sd / 1.349)
  lower <- 0.2
  bad <- logical(dim(df)[1])
  levs <- paste(df$s, sep = ".")
  for (i in levels(df$s)) {
    lev <- i
    bad[levs == lev] <-
      df[levs == lev, "RT"] > upper[i] | df[levs == lev, "RT"] < lower
  }
  df = df[!bad, ]
  nok = tapply(df$RT, list(df$s), length)
  pbad = 100 - 100 * nok / n
  nok = tapply(df$RT, list(df$s), length)
  pbad = 100 - 100 * nok / ns
  print(sort(pbad))
  print(mean(pbad, na.rm = T))
  df
}


#Inputs:(df) a data frame from a 3-choice PM task
#with an R column fo response
#and a FACTOR 's' column (for subject) 
# (keyassign) a list of possible
# key assignments to word, nonword PM
# (keybalance) a vector of how those 
# keys are assigned across participant numers

#Outputs: A data frame with the key assignments
# of word, non-word PM allocated appropriately

assign_keys <- function(df, keyassign, keybalance) {
  for (i in 1:length(unique(df$s))) {
    ##Get key balance for this participant
    subject <- as.numeric(str_sub(unique(df$s)[i], 2))
    kb <- keybalance[subject + 1]
    responsekeys <- keyassign[kb][[1]]
    ##Assign word, non-word, PM appropriately
    df$R[df$s == unique(df$s)[i] &
           df$R == responsekeys[1]] <- "W"
    df$R[df$s == unique(df$s)[i] &
           df$R == responsekeys[2]] <- "N"
    df$R[df$s == unique(df$s)[i] &
           df$R == responsekeys[3]] <- "P"
  }
  df
}

##ggplot theme

theme_simple <- function (base_size = 12, base_family = "") {
  theme_gray(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      panel.background = element_rect(fill="white"),
      panel.grid.minor.y = element_blank(),
      legend.key = element_rect(fill="white", colour= "white"),
      strip.background = element_rect(fill="white")
      
    )   
}

#Between-subjects standard error
se <- function(x) {
  sqrt(var(x)/length(x))
}

round_ps <- function(df) {
  
  df$p.value <- format.pval(
    as.numeric(sprintf("%.3f",df$p.value)),
    digits=2, eps= 0.001)
  df
}


make_model_table <- function (model_summary) {
  
  model_table <- data.frame(Effect= rownames(model_summary), 
                            df = model_summary$Df, Chisq= model_summary$Chisq, 
                            p.value = model_summary$Pr)
  model_table$Chisq <- round(model_table$Chisq, 3)
  round_ps(model_table)
}


se2 <- function(df,facs,sfac="s",dvnam="y",ws=TRUE,ci="SE") {
  df <- as.data.frame(df)
  df <- df[,c(names(df)[names(df)!=dvnam],dvnam)]
  dvnam=dim(df)[2]
  for (i in 1:(dim(df)[2]-1)) df[,i] <- factor(df[,i])  
  if (ws) {
    smns <- tapply(df[,dvnam],df[,sfac],mean, na.rm=T)
    smn <- df[,sfac]
    levels(smn) <- smns
    df[,dvnam] <- df[,dvnam]-as.numeric(as.character(smn))  
  }
  mn=tapply(df[,dvnam],df[,facs],mean, na.rm=T)
  se=tapply(df[,dvnam],df[,facs],sd, na.rm=T)
  ns <- length(levels(df[,sfac]))
  if (ws) {
    m <- prod(dim(se))
    ns <- ns*(m-1)/m
  }
  if (is.na(ci)) mn else {
    if (ci=="SE") se/sqrt(ns) else
      qt(1-(100-ci)/200,ns-1)*se/sqrt(ns)
  }
}

find_bad_trials <- function(S, R, trial_counter, nafter=2) {
  PM_S_or_R <- trial_counter[S=="p"|R=="P"]
  badtrials <- Reduce(c, lapply(PM_S_or_R, function(x)
    (x + 1):(x + nafter)))
  trial_counter %in% badtrials
}


arr2df=function(arr) 
{
  if (is.null(dim(arr))) out=data.frame(y=arr) else {
    dn=dimnames(arr)
    if (length(dn)==1) {
      out=cbind.data.frame(factor(dn[[1]],dn[[1]]),arr)
      names(out)=c(names(dn),"y")
      row.names(out)=NULL
    } else {
      tmp=vector(mode="list",length=length(dn))
      names(tmp)=names(dn)
      k=1
      for (j in names(dn)) {
        n=length(dn[[j]])
        tmp[[j]]=gl(n,k,length(arr),dn[[j]])
        k=k*n
      }
      out=cbind(data.frame(tmp),y=as.vector(arr))
      row.names(out)=NULL
    }
  }
  out
}




#functions to calculate quantities for posterior predictive summaries

get.diff.PM.Rtype <- function(df) {

  NcW <- length(df$RT[df$E=="I" & df$R=="W" & (df$S=="pw"|df$S=="pn")])/
    length(df$RT[df$E=="I"& (df$S=="pw"|df$S=="pn")])
  
  WcW <- length(df$RT[df$E=="U" & df$R=="W"& (df$S=="pw"|df$S=="pn")])/
  length(df$RT[df$E=="U"& (df$S=="pw"|df$S=="pn")])
  
  NcN <- length(df$RT[df$E=="I" & df$R=="N"& (df$S=="pw"|df$S=="pn")])/
  length(df$RT[df$E=="I"& (df$S=="pw"|df$S=="pn")])
  
  WcN <- length(df$RT[df$E=="U" & df$R=="N"& (df$S=="pw"|df$S=="pn")])/
  length(df$RT[df$E=="U"& (df$S=="pw"|df$S=="pn")])
# 
#   out <- c(NcW-WcW+WcN-NcN)
#   names(out) <- c("NcW-WcW+WcN-NcN")
  out <- mean(c((NcW-WcW), (WcN-NcN)))
  names(out) <- c("NcW-WcW+WcN-NcN")
  out
}


get.diff.PM.RT <- function(df) {

  NcW <- mean(df$RT[df$E=="I" & df$R=="W" & (df$S=="pw"|df$S=="pn")])
  WcW <- mean(df$RT[df$E=="U" & df$R=="W"& (df$S=="pw"|df$S=="pn")])
  
  NcN <- mean(df$RT[df$E=="I" & df$R=="N"& (df$S=="pw"|df$S=="pn")])
  WcN <- mean(df$RT[df$E=="U" & df$R=="N"& (df$S=="pw"|df$S=="pn")])

  # out <- c((WcW-NcW) + (NcN-WcN))
  # names(out) <- c("(WcW-NcW) + (NcN-WcN)")
  
  out <- mean(c((WcW-NcW),(NcN-WcN)))
  names(out) <- c("(WcW-NcW) + (NcN-WcN)")
  
  out
}

get.diff.OT.Rtype <- function(df) {

  NcW <- length(df$RT[df$E=="I" & df$R=="W" & (df$S=="ww"|df$S=="nn")])/
    length(df$RT[df$E=="I"& (df$S=="ww"|df$S=="nn")])
  
  WcW <- length(df$RT[df$E=="U" & df$R=="W"& (df$S=="ww"|df$S=="nn")])/
  length(df$RT[df$E=="U"& (df$S=="ww"|df$S=="nn")])
  
  NcN <- length(df$RT[df$E=="I" & df$R=="N"& (df$S=="ww"|df$S=="nn")])/
  length(df$RT[df$E=="I"& (df$S=="ww"|df$S=="nn")])
  
  WcN <- length(df$RT[df$E=="U" & df$R=="N"& (df$S=="ww"|df$S=="nn")])/
  length(df$RT[df$E=="U"& (df$S=="ww"|df$S=="nn")])
  
  # out <- c(NcW-WcW+WcN-NcN)
  # names(out) <- c("NcW-WcW+WcN-NcN")
  
  out <- mean(c((NcW-WcW), (WcN-NcN)))
  names(out) <- c("NcW-WcW+WcN-NcN")
  
  out
}



get.diff.OT <- function(df) {

  SW <- 
    mean(df$RT[df$cond == "S" & df$R == "W" & df$S == "w"])
  MW <- 
    mean(df$RT[df$cond == "M" & df$R == "W" & df$S == "w"])
  
  SN <- 
    mean(df$RT[df$cond == "S" & df$R == "N" & df$S == "n"])
  MN <- 
    mean(df$RT[df$cond == "M" & df$R == "N" & df$S == "n"])
  
  out <- c(MW-SW, MN-SN)
  names(out) <- c("MW-SW", "MN-SN")
  out
}

get.diff.PM <- function(df) {

  SPRT <- 
    mean(df$RT[df$cond == "S" & df$S == "p" & df$R == "P"], na.rm=T)
  MPRT <- 
    mean(df$RT[df$cond == "M" & df$S == "p" & df$R == "P"], na.rm=T)
  
  SP <- 
    length(df$RT[df$S=="p" & df$R=="P" & df$cond=="S"] )/
    length(df$RT[df$S=="p" & df$cond=="S"] )
  
  MP <- 
    length(df$RT[df$S=="p" & df$R=="P" & df$cond=="M"] )/
    length(df$RT[df$S=="p" & df$cond=="M"] )
  
  out <- c(MPRT-SPRT, SP-MP)
  names(out) <- c("MPRT-SPRT", "SP-MP")
  out
}


get.learning.diff <- function(df) {

  SPRT1 <- 
    mean(df$RT[df$cond == "S" & df$S == "p" & df$R == "P" & df$stimtrialcounter %in% c(0,1)], na.rm=T)
  
  SPRT8 <- 
    mean(df$RT[df$cond == "S" & df$S == "p" & df$R == "P"& df$stimtrialcounter %in% c(6,7)], na.rm=T)
  
  MPRT1 <- 
    mean(df$RT[df$cond == "M" & df$S == "p" & df$R == "P" & df$stimtrialcounter %in% c(0,1)], na.rm=T)
  
  MPRT8 <- 
    mean(df$RT[df$cond == "M" & df$S == "p" & df$R == "P"& df$stimtrialcounter %in% c(6,7)], na.rm=T)
  
  SP1 <- 
    length(df$RT[df$S=="p" & df$R=="P" & df$cond=="S"& df$stimtrialcounter %in% c(0,1)] )/
    length(df$RT[df$S=="p" & df$cond=="S"& df$stimtrialcounter %in% c(0,1)] )
  
  SP8 <- 
    length(df$RT[df$S=="p" & df$R=="P" & df$cond=="S"& df$stimtrialcounter %in% c(6,7)] )/
    length(df$RT[df$S=="p" & df$cond=="S"& df$stimtrialcounter %in% c(6,7)] )
  
  MP1 <- 
    length(df$RT[df$S=="p" & df$R=="P" & df$cond=="M"& df$stimtrialcounter %in% c(0,1)] )/
    length(df$RT[df$S=="p" & df$cond=="M"& df$stimtrialcounter %in% c(0,1)] )
  
  MP8 <- 
    length(df$RT[df$S=="p" & df$R=="P" & df$cond=="M"& df$stimtrialcounter %in% c(6,7)] )/
    length(df$RT[df$S=="p" & df$cond=="M"& df$stimtrialcounter %in% c(6,7)] )
  
  out <- c(MPRT1-MPRT8, SPRT1-SPRT8, MP8- MP1, SP8 - SP1)
  names(out) <- c("MPRT1-MPRT8", "SPRT1-SPRT8",
                  "MP8- MP1", "SP8- SP1")
  out
}








get.diff.PM.perf <- function(df) {

  NcNP <- length(df$RT[df$E=="I" & df$R=="P" & df$S=="pn"])/
    length(df$RT[df$E=="I"& df$S=="pn"])
  
  WcNP <- length(df$RT[df$E=="U" & df$R=="P" & df$S=="pn"])/
  length(df$RT[df$E=="U"& df$S=="pn"])
  
  NcWP <- length(df$RT[df$E=="I" & df$R=="P" & df$S=="pw"])/
    length(df$RT[df$E=="I"& df$S=="pw"])
  
  WcWP <- length(df$RT[df$E=="U" & df$R=="P" & df$S=="pw"])/
  length(df$RT[df$E=="U"& df$S=="pw"])
  # 
  out <- c(NcNP - WcNP+ WcWP - NcWP)
  names(out) <- c("NcP-WcP+WcP-NcP")
  
  # out <- mean(c((NcNP-WcNP), (WcWP - NcWP)))
  # names(out) <- c("NcP-WcP+WcP-NcP")
  out
}



get.pc.effect.predicted <- function(df){
  out<- cbind(df,(df$mean/df$data) *100)
  names(out)[length(out)] <- "pc"
  out
}

