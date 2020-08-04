files <- list.files("demographics")


for (i in 1:(length(files))) {
  
  demos <- read.csv(paste("demographics/", files[i], sep = ""), 
                    colClasses=c(gender="character", age="numeric"))
  
  if(i == 1) all_demos <- demos else all_demos <- rbind(all_demos, demos)
  
}

sum(toupper(substr(all_demos$gender,1,1))=="M")
min(all_demos$age)
max(all_demos$age)
mean(all_demos$age)
sd(all_demos$age)

