library(tidyverse)
library(dplyr)
library(stringr)
source("R/functions.R")

#list.files function lists everything inside the "data"
#directory
data_files <- list.files("data")
#Using grepl to filter out any files that are practice data
#or recognition memory data (making a list that is just the main data)
main_block_files <-  data_files[!grepl("practice|RM", data_files)]

#####Examine main experimental data
#using stack_dats function to put together
#the different files
okdats <- stack_dats(main_block_files)

#Remove any untested data, marked with RT= -1
okdats <- okdats[okdats$RT != -1,]

#convert stimulus names to lower case
#following my usual convention
okdats$S <- tolower(okdats$S)

#have a look to see the number of each type
#of stimulus in each condition each day.
#make sure nothing broken
table(okdats$S, okdats$cond, okdats$day)
table(okdats$s, okdats$S, okdats$cond, okdats$day)

okdats <-
  okdats %>% group_by(s, cond, day) %>% mutate(trial_counter=1:length(RT))

#Find bad PMs

okdats <- okdats %>% group_by(s, cond, day) %>% mutate(trial_counter=1:length(RT)) %>% 
  mutate(badtrials = find_bad_trials(S,R,trial_counter, nafter=1))

okdats <-
  okdats %>% group_by(s, stim, cond, day) %>% mutate(stimtrialcounter=1:length(RT))

okdats <-
  okdats %>% group_by(s, S, cond, day) %>% mutate(Scounter=1:length(RT))

#Create marker for within each block of 8 PM trials (to track learning effects across
#different stimuli in multi)
okdats$counter2 <- okdats$stimtrialcounter
okdats$counter2[okdats$cond=="single"] <-  ceiling(okdats$counter2[okdats$cond=="single"]/8)
okdats <- okdats %>% group_by(s, S, cond, day, counter2) %>% 
  mutate(withincounter=1:length(RT))
okdats$withincounter[okdats$S!="p"] <- NA


okdats$quarter<- cut(okdats$trial_counter, c(0,162, 322, 484, 644))

after_break_trials <- c(1,2,333,334)

test_trialnums <- okdats %>% filter(!(trial_counter %in% after_break_trials))
table(test_trialnums$quarter, 
      test_trialnums$s, 
      test_trialnums$cond, 
      test_trialnums$day)



#Try dropping trials after PM and first 2 trials of block to make sure doesn't change anything
#NOTE this is to create finaldats_cuts for robustness testing of the model results
#To create standard finaldats comment out this filter
okdats <- okdats %>% filter(!(trial_counter %in% after_break_trials) & 
                                    !badtrials)

table(okdats$quarter, okdats$s)

##Currently responses are list as the
#actual key they pressed.
#Rescore this to 'word', 'nonword'
#or 'PM' key
#
#The response keys varied based
#on participants. The function
#I wrote assign_keys will 
#assign the correct keys from keyassign
#to the correct participants using keybalance
#key assignments - word/nonword/PM
keyassign <-
  list(
    c('d', 's', 'j'),
    c('s', 'd', 'j'),
    c('j', 'k', 'd'),
    c('k', 'j', 'd')
  )

#keybalance: distribution of key assignments
#over participants
keybalance <- rep(c(1,1,2,2,3,3,4,4), 8)

okdats <- assign_keys(okdats, keyassign, keybalance)

okdats <- rbind(okdats, p22_old)

#Print responses that were not one of the assigned response keys
okdats$R[!okdats$R %in% c("N", "W", "P")]
#Remove such responses
okdats <- okdats[okdats$R %in% c("N", "W", "P"),]

#have a look at Rs
test = table( okdats$s, okdats$prestim_R)
test[,32]
table(okdats$s) - test[,32]
p21 <- okdats[okdats$s=='p21',]

##Have a look at the spread of response times.
hist(okdats$RT, breaks = 150)
#There is one really long RT. Use RT trimming procedure
okdats$s <- factor(okdats$s)

#todo break down RT by block

finaldats <- clean(okdats)
hist(finaldats$RT, breaks = 150)

##Make data frame for subsequent analyses
#dropping a bunch of other columns

#  # # # ####

finaldats <- finaldats[,c("s", "day", "block", "quarter", "stimtrialcounter", "withincounter",
                          "Scounter", "S", "cond", "R", "RT")]
names(finaldats)[2] <- "D"
finaldats$cond <- factor(finaldats$cond, levels=c("single", "multi"), labels=c("S", "M"))
finaldats$S <- factor(finaldats$S)
finaldats$R <- factor(finaldats$R)
finaldats$D <- factor(finaldats$D)

#assure consistent level order in S and R (for DMC)
finaldats$R <- factor(finaldats$R, levels=c("N", "W", "P"))
finaldats$S <- factor(finaldats$S, levels=c("n", "w", "p"))

#Save off this clean data structure for future analysis
# save(finaldats, file="img/finaldats.RData")

#Have a look at practice trials
practice_files <-  data_files[grepl("practice", data_files)]
practicedats <- stack_dats(practice_files)

practicedats$S <- factor(practicedats$S)
practicedats$cond <- factor(practicedats$cond)

table(practicedats$s,practicedats$S, practicedats$cond, practicedats$day)
hist(practicedats$RT[practicedats$RT < 3], breaks = 24, xlim = c(0, 1))

#Summarise practice accuracy and RT
practicedats %>%
  group_by(S, cond, day) %>%
  summarise(mean_acc = mean(C == R))

practicedats %>%
  group_by(S, cond) %>%
  summarise(mean_RT = mean(RT))


#Examine RM test data
#These are the tests after the block 
test_RM_files <-  data_files[grepl("test_RM", data_files)]
RMdats <- stack_dats(test_RM_files)

table(RMdats$s, RMdats$C, RMdats$cond, RMdats$day)
hist(RMdats$RT[RMdats$RT < 3], breaks = 64,  xlim = c(0, 3))

RMdats %>%
  group_by(cond, C) %>%
  summarise(mean_acc = mean(C == R))


test = RMdats %>%
  group_by(s,cond,day, C) %>%
  summarise(mean_acc = mean(C == R))

plot(test$mean_acc, ylim=c(0,1))
abline(a=0.95, b=0)
test[test$mean_acc<0.9,]

RMdats %>%
  group_by(cond, C) %>%
  summarise(mean_RT = mean(RT))



#Examine RM practice data
#these are the practice trials 
#that repeat until participants get 100%

RM_files <-  data_files[grepl("RM", data_files)]
prac_RM_files <- RM_files[!grepl("test_RM", RM_files)]
prac_RMdats <- stack_dats(prac_RM_files)

table(prac_RMdats$C, prac_RMdats$cond, prac_RMdats$day)
hist(prac_RMdats$RT[prac_RMdats$RT < 3], breaks = 64,  xlim = c(0, 3))

prac_RMdats %>%
  group_by(cond, C) %>%
  summarise(mean_acc = mean(C == R))

prac_RMdats %>%
  group_by(cond, C) %>%
  summarise(mean_RT = mean(RT))

nblocks <- prac_RMdats %>% group_by(s, cond, day) %>% 
  summarise(nblocks=length(unique(count)))

nblocks %>% group_by(cond) %>% summarise(M=mean(nblocks), SD=sd(nblocks))

##Combine data

uniquestim <- okdats %>% group_by(s, cond, day) %>% 
  filter(S=='p') %>% distinct(stim)

uniquestim$s <- as.character(uniquestim$s)

RMstim <- RMdats %>% group_by(s, cond, day) %>% 
  filter(C=='y') %>% distinct(stim)

RMstim[RMstim$s=='p8',]
uniquestim[uniquestim$s=='p8',]

full_join(RMstim[RMstim$s=='p4',], uniquestim[RMstim$s=='p4',], by="stim")

#Join performance data from main block and RM block
joinedperf <- left_join(RMdats[RMdats$C=='y',], okdats[okdats$S=='p',], by = c("stim", "s"))
joinedperf[joinedperf$cond.y=='multi',]

multi <- joinedperf[joinedperf$cond.y=='multi',]





#NEW FROM REVISION (put under stack_dats):
#Look at participant 22 who had 0% accuracy

# p22_old_files <- paste("excluded_data/old_22/",
#                        list.files("excluded_data/old_22"), sep="")
# 
# p22_task <- p22_old_files[!grepl("*practice*|*RM*|*note*", p22_old_files)]
# 
# p22_old <- rbind(
#   read.csv(p22_task[1], stringsAsFactors = FALSE),
#   read.csv(p22_task[2], stringsAsFactors = FALSE)
# )
# 
# p22_old$s <- "p22_old"
# 
# okdats <- rbind(okdats, p22_old)

#put under keybalance

# 
# p22_old <- okdats %>% filter(s=="p22_old")
# 
# 
# subject <- as.numeric(str_sub("p22", 2))
# kb <- keybalance[subject + 1]
# responsekeys <- keyassign[kb][[1]]
# ##Assign word, non-word, PM appropriately
# p22_old$R[p22_old$R == responsekeys[1]] <- "W"
# p22_old$R[p22_old$R == responsekeys[2]] <- "N"
# p22_old$R[p22_old$R == responsekeys[3]] <- "P"

