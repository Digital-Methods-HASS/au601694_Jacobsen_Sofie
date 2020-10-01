#generate dummy data
create_data <- function() {
  record_id <- 1:150
  age<- sample(18:65, 150, replace = TRUE)
  gender <- sample(1:3, 150, replace = TRUE)
  sias_1 <- sample(0:3, 150, replace = TRUE)
  sias_2 <- sample(0:3, 150, replace = TRUE)
  sias_3 <- sample(0:3, 150, replace = TRUE)
  sias_4 <- sample(0:3, 150, replace = TRUE)
  sias_5 <- sample(0:3, 150, replace = TRUE)
  sias_6 <- sample(0:3, 150, replace = TRUE)
  sias_7 <- sample(0:3, 150, replace = TRUE)
  sias_8 <- sample(0:3, 150, replace = TRUE)
  sias_9 <- sample(0:3, 150, replace = TRUE)
  sias_10 <- sample(0:3, 150, replace = TRUE)
  sias_11 <- sample(0:3, 150, replace = TRUE)
  sias_12 <- sample(0:3, 150, replace = TRUE)
  sias_13 <- sample(0:3, 150, replace = TRUE)
  sias_14 <- sample(0:3, 150, replace = TRUE)
  sias_15 <- sample(0:3, 150, replace = TRUE)
  sias_16 <- sample(0:3, 150, replace = TRUE)
  sias_17 <- sample(0:3, 150, replace = TRUE)
  sias_18 <- sample(0:3, 150, replace = TRUE)
  sias_19 <- sample(0:3, 150, replace = TRUE)
  sias_20 <- sample(0:3, 150, replace = TRUE)
  sias_complete <- sample(2:2, 150, replace = TRUE)
  cusados_1<- sample(0:4, 150, replace = TRUE)
  cusados_2<- sample(0:4, 150, replace = TRUE)
  cusados_3<- sample(0:4, 150, replace = TRUE)
  cusados_4<- sample(0:4, 150, replace = TRUE)
  cusados_5<- sample(0:4, 150, replace = TRUE)
  cusados_6<- sample(0:4, 150, replace = TRUE)
  cusados_7<- sample(0:4, 150, replace = TRUE)
  cusados_8<- sample(0:4, 150, replace = TRUE)
  cusados_9<- sample(0:4, 150, replace = TRUE)
  cusados_10<- sample(0:4, 150, replace = TRUE)
  cusados_11<- sample(0:4, 150, replace = TRUE)
  cusados_12<- sample(0:4, 150, replace = TRUE)
  cusados_complete<- sample(2:2, 150, replace = TRUE)
  
  dummy_data <- data.frame(record_id, gender, age, cusados_1, cusados_2, cusados_3, cusados_4, cusados_5, cusados_6, cusados_7, cusados_8, cusados_9, cusados_10, cusados_11, cusados_12, cusados_complete, sias_1, sias_2, sias_3, sias_4, sias_5, sias_6, sias_7, sias_8, sias_9, sias_10, sias_11, sias_12, sias_13, sias_14, sias_15, sias_16, sias_17, sias_18, sias_19, sias_20, sias_complete)
  return(dummy_data)
}

#get scores
get_SIAS_CUSADOS <- function(data){
  # create scores and set the classes right for further analysis
  scores <- data
  scores$age <- as.integer(scores$age)
  scores$gender <- as.factor(scores$gender)
  
  #change gender into factor with names
  #levels(data$gender)=c("Female","Male","Other")
  scores$gender<- revalue(scores$gender, c("1"="Female", "2"="Male", "3"="Other"))
  
  #CUSADOS
  scores <- scores %>%
    mutate(cusados = rowSums(.[4:15]))%>%
    drop_na()
  #Sias
  columnsToReverse <- c('sias_5', 'sias_9', 'sias_11')
  scores[ ,columnsToReverse] = 4 - scores[ ,columnsToReverse]
  scores <- scores %>% 
    mutate(sias = rowSums(.[17:36])) %>%
    drop_na()
  #remove un-needed variables
  scores <- select(scores, record_id, gender, age, sias, cusados)
  
  #calculate above threshold inidividuals
  scores$sias_above_clin <- ifelse(scores$sias > 42, 1, 0) #a score of 1 means diagnosis
  scores$cusados_above_clin <- ifelse(scores$cusados > 15, 1, 0) #a score of 1 means diagnosis
  scores$both_above_clin <- ifelse(scores$cusados_above_clin+scores$sias_above_clin == 2, 1, 0) #a score of 1 means diagnosis in both
  
  #Change names of variables
  scores <- rename(scores, c("age" = "Age", "gender" = "Gender", "sias" = "SIAS", "cusados" = "CUSADOS"))
  return(scores)
}

#plot the data with thresholds
plot_SAD_scales <- function(scores){
  scores$Gender <- as.factor(scores$Gender)
  ggplot(data = scores)+
    geom_smooth(method = 'lm', aes(SIAS, CUSADOS)) +
    geom_point(aes(SIAS, CUSADOS, color = Age, pch = Gender)) +
    geom_vline(xintercept = 43, color = "red") + #sias threshold    
    geom_hline(yintercept = 16, color = "red") + #cusados threshold
    theme_minimal()
}

#do analysis
bayes_analysis <- function(scores){
  
  #scale variables
  scores$CUSADOS_scaled <- scale(scores$CUSADOS)
  scores$SIAS_scaled <- scale(scores$SIAS)
  
  # Define the formula - designing the model. We have a between-study design. 
  SAD_f0 <- bf(SIAS_scaled ~ 1 + CUSADOS_scaled)
  
  # data_bays <- select(data, threshold_scaled, SIAS_scaled)
  # write.csv(data_bays, "Peter-data.csv")
  
  #if you scale y and x see riccardos video for assignment 3. If not, intercept should be the avarage of y and I don't know about the betas
  # Design the priors
  prior_f0 <- get_prior(SAD_f0, family = gaussian, scores) # The family = Guassian, because the outcome is a continuous variable
  view(prior_f0)
  # We see that we need a beta prior and a sigma prior
  summary(scores$SIAS_scaled) 
  priorSAD <- c(
    prior(normal(0, 1), class = Intercept), # the intercept prior needs to reflect the average y-value (where SIAS is 0, threshold is supposed to be average)
    prior(normal(0, 0.3), class = b), # beta is the expectation of the difference between schizophrenia and controls. We say that the beta is normally distributed, and that the mean is 4 and the standard deviation is 1.
    prior(normal(0.5, 1), class = sigma) # sigma is the average error that we expect. 
  ) 
  
  # Test the priors. We want to check whether the priors make any sense.
  SAD_PriorCheck_m <- brm( # m stands for model
    formula = SAD_f0,
    data = scores,
    family = gaussian, # Gaussian because the outcome is continuous
    prior = priorSAD,
    sample_prior = "only",# we sample the prior in order to test the prior
    control = list(adapt_delta = 0.9)
  )
  
  # We check the what the predictions look like given the prior and not the data. We set the number of simulations to 100.
  pp_check(SAD_PriorCheck_m, nsamples = 100)
  
  # What we see is that the prior has a very long tail. In order to fix this we make a prior for the sigma - in order to expect an error (we edited the first one)
  
  ## Fitting the model
  SAD_m <- brm(
    formula = SAD_f0,
    data = scores,
    family = gaussian,
    prior = priorSAD,
    sample_prior = T,
    control = list(adapt_delta = 0.9)
  )
  
  # Posterior predictive check. # We want to look at whether the posterior has learned from the prior, which is what we expect when we look at the posterior predictive check
  pp_check(SAD_m, nsamples = 100)
  # The data looks good!
  # The light blue is the prior for the difference between schizophrenia and control (it is very spread, which means that it is very uncertain). 
  # The dark blue is the posterior (this is much more certain - must less variance), which tells us that it has actually learned from the data, and makes more confident predictions. 
  
  # Conclusion: the prior is not off, and the posterior has learned from the data. This is good!
  return(SAD_m)
}

