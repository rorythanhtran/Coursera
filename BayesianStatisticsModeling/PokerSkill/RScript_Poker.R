#Bayseian Statistics: Modeling & Technique 
#Thanh Tran 
#Capstone project 
library(ggplot2)
install.packages("rjags")
library(rjags)
rm(list=ls())
getwd()
setwd("C:/Users/User/DataScienceProjects/PokerSkills")

data <- read.csv("poker_skill.dat")
data <- read.table("poker_skill.dat", header = FALSE, sep = " ", fill = TRUE)
data_clean <- data[, c(8, 15, 22, 26)]  # Skill, Hand, Limit, Final Cash Balance
colnames(data_clean) <- c("Skill", "Hand", "Limit", "Final_Cash_Balance") #Rename

# Descriptive summary of the dataset
summary(data_clean)

# Check for missing values
missing_values <- colSums(is.na(data_clean))
# Check for missing values in the Final Cash Balance column
sum(is.na(data_clean$Final_Cash_Balance))
data_clean <- na.omit(data_clean)

# Check the frequency distribution of categorical variables (Skill, Hand, Limit)
skill_freq <- table(data_clean$Skill)
hand_freq <- table(data_clean$Hand)
limit_freq <- table(data_clean$Limit)

# Scatter plot for Skill vs Final Cash Balance
ggplot(data_clean, aes(x = factor(Skill), y = Final_Cash_Balance, color = factor(Skill))) +
  geom_jitter(width = 0.2, height = 0) +
  labs(title = "Final Cash Balance by Skill Level", x = "Skill Level", y = "Final Cash Balance (euros)") +
  scale_color_manual(values = c("lightblue", "lightgreen")) +
  theme_minimal()

# Scatter plot for Hand vs Final Cash Balance
ggplot(data_clean, aes(x = factor(Hand), y = Final_Cash_Balance, color = factor(Hand))) +
  geom_jitter(width = 0.2, height = 0) +
  labs(title = "Final Cash Balance by Hand Quality", x = "Hand Quality", y = "Final Cash Balance (euros)") +
  scale_color_manual(values = c("lightcoral", "lightyellow", "lightgreen")) +
  theme_minimal()

# Scatter plot for Limit vs Final Cash Balance
ggplot(data_clean, aes(x = factor(Limit), y = Final_Cash_Balance, color = factor(Limit))) +
  geom_jitter(width = 0.2, height = 0) +
  labs(title = "Final Cash Balance by Bet Limit", x = "Bet Limit", y = "Final Cash Balance (euros)") +
  scale_color_manual(values = c("lightpink", "lightblue")) +
  theme_minimal()


# Define the model (JAGS syntax)
model_string <- "
model {
  for(i in 1:N) {
    Final_Cash_Balance[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha + beta_skill[Skill[i]] + beta_hand[Hand[i]] + beta_limit[Limit[i]]
  }
  
  # Priors
  alpha ~ dnorm(0, 0.0001)
  for(s in 1:2) {
    beta_skill[s] ~ dnorm(0, 0.0001) # Priors for Skill effects
  }
  for(h in 1:3) {
    beta_hand[h] ~ dnorm(0, 0.0001) # Priors for Hand quality effects
  }
  for(l in 1:2) {
    beta_limit[l] ~ dnorm(0, 0.0001) # Priors for Bet Limit effects
  }
  
  tau ~ dgamma(0.1, 0.1) # Prior for precision (inverse of variance)
}
"

# Prepare data for JAGS
data_jags <- list(
  N = nrow(data_clean),
  Final_Cash_Balance = data_clean$Final_Cash_Balance,
  Skill = as.numeric(data_clean$Skill),
  Hand = as.numeric(data_clean$Hand),
  Limit = as.numeric(data_clean$Limit)
)

# Initial values for MCMC chains
inits <- function() {
  list(alpha = 0, beta_skill = c(0, 0), beta_hand = c(0, 0, 0), beta_limit = c(0, 0), tau = 1)
}

# Set up the JAGS model
model <- jags.model(textConnection(model_string), data = data_jags, inits = inits, n.chains = 3)

# Burn-in and sampling
update(model, 1000)
samples <- coda.samples(model, variable.names = c("alpha", "beta_skill", "beta_hand", "beta_limit", "tau"), n.iter = 5000)

# Check convergence with Gelman-Rubin diagnostic
gelman.diag(samples)

# Define the new model (with interaction between Skill and Hand Quality)
model_string_new <- "
model {
  for(i in 1:N) {
    Final_Cash_Balance[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha + beta_skill[Skill[i]] + beta_hand[Hand[i]] + beta_limit[Limit[i]] + beta_interaction[Skill[i], Hand[i]]
  }

  # Priors
  alpha ~ dnorm(0, 0.0001)  # Prior for intercept
  for(s in 1:2) {
    beta_skill[s] ~ dnorm(0, 0.01) # Priors for Skill effects (tighter)
  }
  for(h in 1:3) {
    beta_hand[h] ~ dnorm(0, 0.01) # Priors for Hand Quality effects (tighter)
  }
  for(l in 1:2) {
    beta_limit[l] ~ dnorm(0, 0.01) # Priors for Bet Limit effects (tighter)
  }
  for(s in 1:2) {
    for(h in 1:3) {
      beta_interaction[s, h] ~ dnorm(0, 0.01) # Interaction term between Skill and Hand Quality
    }
  }

  tau ~ dgamma(0.1, 0.1) # Prior for precision (inverse of variance)
}
"

# Prepare the data for JAGS
data_jags_new <- list(
  N = nrow(data_clean),
  Final_Cash_Balance = data_clean$Final_Cash_Balance,
  Skill = as.numeric(data_clean$Skill),
  Hand = as.numeric(data_clean$Hand),
  Limit = as.numeric(data_clean$Limit)
)

# Initial values for MCMC chains
inits_new <- function() {
  list(alpha = 0, beta_skill = c(0, 0), beta_hand = c(0, 0, 0), beta_limit = c(0, 0), beta_interaction = matrix(0, 2, 3), tau = 1)
}

# Set up the JAGS model
model_new <- jags.model(textConnection(model_string_new), data = data_jags_new, inits = inits_new, n.chains = 3)

# Burn-in and sampling
update(model_new, 2000) 
samples_new <- coda.samples(model_new, variable.names = c("alpha", "beta_skill", "beta_hand", "beta_limit", "beta_interaction", "tau"), n.iter = 10000)

# Summary of the results
summary(samples_new)

gelman.diag(samples_new)
