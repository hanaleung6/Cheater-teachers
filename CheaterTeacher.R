library(tidyverse)


# task 1

children_score <- read.csv("ChildrenScores.csv")

# the proportion of positive residual for each Y3 teacher.
Y3_teacher <- children_score %>% 
  mutate(residual = scoreY3-meanScore) %>%
  group_by(teacherY3) %>%
  summarise(positive_residual_proportion = sum(residual>0)/n()) %>%
  rename(teacher = teacherY3)


# the proportion of positive residual for each Y4 teacher.
Y4_teacher <- children_score %>% 
  mutate(residual = scoreY4-meanScore) %>%
  group_by(teacherY4) %>%
  summarise(positive_residual_proportion = sum(residual>0)/n()) %>%
  rename(teacher = teacherY4)


# the proportion of positive residual for each Y5 teacher.
Y5_teacher <- children_score %>% 
  mutate(residual = scoreY5-meanScore) %>%
  group_by(teacherY5) %>%
  summarise(positive_residual_proportion = sum(residual>0)/n()) %>%
  rename(teacher = teacherY5)


# the proportion of positive residual for each Y6 teacher.
Y6_teacher <- children_score %>% 
  mutate(residual = scoreY6-meanScore) %>%
  group_by(teacherY6) %>%
  summarise(positive_residual_proportion = sum(residual>0)/n()) %>%
  rename(teacher = teacherY6)


# the proportion of positive residual for each Y7 teacher.
Y7_teacher <- children_score %>% 
  mutate(residual = scoreY7-meanScore) %>%
  group_by(teacherY7) %>%
  summarise(positive_residual_proportion = sum(residual>0)/n()) %>%
  rename(teacher = teacherY7)


# combine all teachers       
all_teachers <- rbind(Y3_teacher, Y4_teacher, Y5_teacher, Y6_teacher, Y7_teacher)

# proportion of positive residuals by teacher 
hist(all_teachers$positive_residual_proportion, 
     xlab = "Proportion of positive residuals", 
     main = "Histogram of proportion of positive residuals by Teacher")

cheater_teachers <- all_teachers %>% filter(positive_residual_proportion > 0.74) 

print(cheater_teachers$teacher)

cheater_teachers <- read.csv("TeamK-cheater-teacher.csv")

# information about how all residuals distribute
hist(all_teachers$positive_residual_proportion, plot = FALSE, breaks = seq(0, 1, 0.05))

# write a csv file containing all cheater ID
#write.table(cheater_teachers$teacher, "TeamK-cheater-teacher.csv", row.names = FALSE, col.names = "CheatID")


# task 2
# noncheat
Y3_noncheat_residual <- children_score %>% 
  filter(!teacherY3 %in% cheater_teachers$CheatID) %>% 
  mutate(residual = scoreY3 - meanScore) %>% 
  select(teacherY3, scoreY3, residual) %>%
  rename(teacher = teacherY3, score = scoreY3)

Y4_noncheat_residual <- children_score %>% 
  filter(!teacherY4 %in% cheater_teachers$CheatID) %>% 
  mutate(residual = scoreY4 - meanScore) %>% 
  select(teacherY4, scoreY4, residual) %>%
  rename(teacher = teacherY4, score = scoreY4)

Y5_noncheat_residual <- children_score %>% 
  filter(!teacherY5 %in% cheater_teachers$CheatID) %>% 
  mutate(residual = scoreY5 - meanScore) %>% 
  select(teacherY5, scoreY5, residual) %>%
  rename(teacher = teacherY5, score = scoreY5)

Y6_noncheat_residual <- children_score %>% 
  filter(!teacherY6 %in% cheater_teachers$CheatID) %>% 
  mutate(residual = scoreY6 - meanScore) %>% 
  select(teacherY6, scoreY6, residual) %>%
  rename(teacher = teacherY6, score = scoreY6)

Y7_noncheat_residual <- children_score %>% 
  filter(!teacherY7 %in% cheater_teachers$CheatID) %>% 
  mutate(residual = scoreY7 - meanScore) %>% 
  select(teacherY7, scoreY7, residual) %>%
  rename(teacher = teacherY7, score = scoreY7)

noncheat_residual <- rbind(Y3_noncheat_residual, 
                                Y4_noncheat_residual, 
                                Y5_noncheat_residual, 
                                Y6_noncheat_residual, 
                                Y7_noncheat_residual) 

mean_noncheat <- mean(noncheat_residual$residual)

sd_noncheat <- sd(noncheat_residual$residual)

#-----------------------------------------------------------------------------------
# cheat
new_means <- vector()
for (i in 1:length(children_score$teacherY3)) {
  sum <- 0
  count <- 0
  if (!children_score$teacherY3[i] %in% cheater_teachers$CheatID){
    sum <- sum + children_score$scoreY3[i]
    count <- count + 1
  }
  if (!children_score$teacherY4[i] %in% cheater_teachers$CheatID){
    sum <- sum + children_score$scoreY4[i]
    count <- count + 1
  }
  if (!children_score$teacherY5[i] %in% cheater_teachers$CheatID){
    sum <- sum + children_score$scoreY5[i]
    count <- count + 1
  }
  if (!children_score$teacherY6[i] %in% cheater_teachers$CheatID){
    sum <- sum + children_score$scoreY6[i]
    count <- count + 1
  }
  if (!children_score$teacherY7[i] %in% cheater_teachers$CheatID){
    sum <- sum + children_score$scoreY7[i]
    count <- count + 1
  }
  mean <- sum / count
  new_means <- c(new_means, mean)
}
new_means
children_score$new_means <- new_means


Y3_cheat_residual <- children_score %>% 
  filter(teacherY3 %in% cheater_teachers$CheatID) %>% 
  mutate(residual = scoreY3 - new_means) %>% 
  select(teacherY3, scoreY3, residual) %>%
  rename(teacher = teacherY3, score = scoreY3)

Y4_cheat_residual <- children_score %>% 
  filter(teacherY4 %in% cheater_teachers$CheatID) %>% 
  mutate(residual = scoreY4 - new_means) %>% 
  select(teacherY4, scoreY4, residual) %>%
  rename(teacher = teacherY4, score = scoreY4)

Y5_cheat_residual <- children_score %>% 
  filter(teacherY5 %in% cheater_teachers$CheatID) %>% 
  mutate(residual = scoreY5 - new_means) %>% 
  select(teacherY5, scoreY5, residual) %>%
  rename(teacher = teacherY5, score = scoreY5)

Y6_cheat_residual <- children_score %>% 
  filter(teacherY6 %in% cheater_teachers$CheatID) %>% 
  mutate(residual = scoreY6 - new_means) %>% 
  select(teacherY6, scoreY6, residual) %>%
  rename(teacher = teacherY6, score = scoreY6)

Y7_cheat_residual <- children_score %>% 
  filter(teacherY7 %in% cheater_teachers$CheatID) %>% 
  mutate(residual = scoreY7 - new_means) %>% 
  select(teacherY7, scoreY7, residual) %>%
  rename(teacher = teacherY7, score = scoreY7)

cheat_residual <- rbind(Y3_cheat_residual, 
                        Y4_cheat_residual, 
                        Y5_cheat_residual, 
                        Y6_cheat_residual, 
                        Y7_cheat_residual) 
#-----------------------------------------------------------------------------------


# maximum likelihood function:

maximumlikelihood_func <- function(residual,mean_noncheat, sd_noncheat, startvec=c(0, 0, 5)){
  
  objective.func <- function(pars){
    
    q <- pars[1]
    mu <- pars[2]
    std <- pars[3]
    
    # sum of negative probability density from the normal distribution
    negative_log_likelihood <- -sum(log((1-q) * dnorm(residual, mean = mean_noncheat, sd = sd_noncheat, log = FALSE) + 
                                          q * dnorm(residual, mu, std, log = FALSE)))
    
    ## Enter code here to fix up any cases where your objective is NA or infinity:
    obj <- negative_log_likelihood
    if(is.na(obj) | is.infinite(obj)){
      obj <- (abs(q)+abs(mu))*1e10
    }
    ## Return the objective, i.e. the quantity you want to minimize:
    return(obj)
  }
  
  maximumlikelihood.fit <- nlm(f=objective.func, p=startvec)
  
  return(maximumlikelihood.fit)
}


cheat_info = maximumlikelihood_func(cheat_residual$residual, mean_noncheat, sd_noncheat)
cheat_info
#_____________________________________________________________
# the proportion of cheat student within the cheat classes
(p = cheat_info$estimate[1])

# mean from cheat teachers
(cheat_mean = cheat_info$estimate[2])

# standard deviation from cheat teachers
(cheat_std = cheat_info$estimate[3])
#_____________________________________________________________


# boostrap function:

maximumlikelihoodBootstrap.func <- function(residual, mean_noncheat,sd_noncheat, startvec=c(0, 0, 5), nboot=100){
  
  ndat <- length(residual)
  
  boot.res <- data.frame(bootrep=1:nboot, ML.a=rep(NA, nboot), ML.b=rep(NA, nboot))
  
  for(i in 1:nboot){
    resampleRows <- sample(1:ndat, size=ndat, replace=T)
    
    dat.boot <- residual[resampleRows]
    
    ## Fit the maximum likelihood model to the resampled data, dat.boot:
    MLfit.boot <- maximumlikelihood_func(dat.boot, mean_noncheat, sd_noncheat, startvec)
    
    ## Enter the estimated values into row i of boot.res:
    boot.res$ML.a[i] <- MLfit.boot$estimate[1]
    boot.res$ML.b[i] <- MLfit.boot$estimate[2]
    boot.res$ML.c[i] <- MLfit.boot$estimate[3]
    ## End of bootstrap replicate i. Return to the top of the loop for the next bootstrap resample.
  }
  ## We've now finished the loop, so we've filled up the whole of boot.res.
  ## Find the standard errors for each parameter as the sample standard deviation of the bootstrap estimates:
  se.a.ML <- sd(boot.res$ML.a)
  se.b.ML <- sd(boot.res$ML.b)
  se.c.ML <- sd(boot.res$ML.c)
  ## Find the 95% confidence intervals for a and b as the 2.5% and 97.5% quantiles of the ordered sample:
  CI.a.ML <- quantile(boot.res$ML.a, probs=c(0.025, 0.975))
  CI.b.ML <- quantile(boot.res$ML.b, probs=c(0.025, 0.975))
  CI.c.ML <- quantile(boot.res$ML.c, probs=c(0.025, 0.975))
  ## Return items of interest:
  return(list(stderror=c(ML.q=se.a.ML, ML.mean=se.b.ML, ML.std=se.c.ML), CI.q.ML=CI.a.ML, CI.mean.ML=CI.b.ML, CI.std.ML=CI.c.ML))
}



noncheat_boots_info = maximumlikelihoodBootstrap.func(cheat_residual$residual, mean_noncheat, sd_noncheat)
noncheat_boots_info

# plot
x = cheat_residual$residual
hist(x, freq = FALSE, xlab = "Residuals", main = "Histogram of residuals")
curve((1-p)*(dnorm(x,mean=mean_noncheat,sd=sd_noncheat))+(p*(dnorm(x,mean=cheat_mean,sd=cheat_std))),add=TRUE)

curve((1-p)*(dnorm(x,mean=mean_noncheat,sd=sd_noncheat)),add=TRUE, col = "red")
curve(p*(dnorm(x,mean=cheat_mean,sd=cheat_std)),add=TRUE, col = "blue")
legend(-10.5, 0.08, legend=c("Cheat residuals",  "Manipulated score residuals", 
                            "Unmanipulated score residuals"),
       col=c("red", "blue", "black"), lty=1, cex=0.8)




