#Read data and create dataframe
successful_data <-  read.csv("successful.txt")[,1]
unsuccessful_data <- read.csv("unsuccessful.txt")[,1]
successful_data <- data.frame(successful_data, 1)
unsuccessful_data <- data.frame(unsuccessful_data, 0)



#Question 1 

#Q1.a
#Success probability function

success_prob <- function(x,mu,gamma){
  1/ (1+exp(-(x-mu)*exp(-gamma)))
}

#Plot datapoints together with line
plot(successful_data, col = "blue", xlim=c(0,35), ylim=c(-1,2))
points(unsuccessful_data, col = "red")

lines(seq(0,35), success_prob(seq(0,35), 12, 0.3))



#Q1.b
#log posterior function
logpost <- function (mu, gamma, x, z){
  eta <- (x-mu)*exp(-gamma)
  log_lik <- sum(eta*z-log(1+exp(eta)))
  return (log_lik)
}

#Combined data
z_combined <- c(successful_data$X1, unsuccessful_data$X0)
x_combined <- c(successful_data$successful_data, unsuccessful_data$unsuccessful_data)

logpost(10,0.5,x_combined,z_combined)

#Q1.c
#mu and gamma values
mu_values <- seq(10, 20, length.out = 20)
gamma_values <- seq(0.1, 2.0, length.out = 20)

#Matrix for log posteriors
log_matrix <- matrix(NA, nrow = length(mu_values), ncol = length(gamma_values))

#Calculate log posteriors
for (i in seq_along(mu_values)){
  for (j in seq_along(gamma_values)){
    log_matrix[i,j] <- -logpost(mu_values[i], gamma_values[j], x_combined, z_combined)
  }
}

#Normalize and exponentiate for image plot
posterior_probabilites <- log_matrix

posterior_probabilites_neg <- -posterior_probabilites              
posterior_probabilites_neg_shifted <- posterior_probabilites_neg - max(posterior_probabilites_neg)   
posterior_matrix_1 <- exp(posterior_probabilites_neg_shifted)
posterior_matrix_1<- posterior_matrix / sum(posterior_matrix_1)

#Plot the values using image
image(x = mu_values, y = gamma_values, z = posterior_matrix_1, xlab=expression(mu), ylab=expression(gamma), main="Posterior probabilities")

#Q2
#Rewritten success function
p_success <- function(x, mu, gamma){
  1 / (1+ exp(-(x - mu) * exp(-gamma))) 
}


#Negative log likelihood
neg_loglik <- function(params){
  mu <- params[1]
  gamma <- params[2]
  
   log_success <- sum(log(p_success(successful_data$successful_data, mu, gamma)))
   log_unsuccess <- sum(log(1-p_success(unsuccessful_data$unsuccessful_data, mu, gamma)))
   
   return (-(log_success+log_unsuccess))
}

#Optimization
result <- nlm(neg_loglik, p = c(mu=10, gamma=1.0))
result 


#Q3

#Q3.a
#Seed and values
set.seed(321)
theta <- 13
mu <- 15
gamma = 1
b1 = 10
b2 = -6
b3 = -1

#Simulated values from gamma distribution
simulate_values <- rgamma(10000, shape = 2.5, rate=0.25)

#Probability of success
probabil_success <- exp((simulate_values - mu) * exp(-gamma)) / (1 + exp((simulate_values - mu) * exp(-gamma)))

#z values from bin
z <- rbinom(10000, size=1, prob=probabil_success)

#Support or not 
support <- simulate_values >= theta

#Utility
utility <- ifelse(support & z == 1, b1,
                  ifelse(support & z == 0, b2, b3))


#Q3.b
#Expected utility
expected_utility <- mean(utility)
expected_utility


#Q3.c
#Expepected utilities for different theta
expected_utilities <- numeric(length(theta_values))
theta_values <- seq(10,20, length.out = 100)
for (l in seq_along(theta_values)){
  support_Q3 <- simulate_values >= theta_values[l]
  utility_Q3 <- ifelse(support_Q3 & z == 1, b1,
                       ifelse(support_Q3 & z== 0, b2, b3))
  expected_utilities[l] <- mean(utility_Q3)
}

#plot the different expected utilities
plot(theta_values,expected_utilities, type = "l", lwd = 2, 
     xlab = expression(theta), ylab = "Expected Utility",
     main = expression("Approx. Expected Utility vs " * theta))


#Q4

#Density function for gamma
f_x <- function(x) dgamma(x, shape = 2.5, rate = 0.25)

#Expected utility integral
expected_utility_integral <- function(mu, gamma, theta){
  
  first_integral <- integrate(function(x) b3*f_x(x), lower = 0, upper = theta)$value
  second_integral <- integrate(function(x) f_x(x)*(b1*p_success(x,mu,gamma)+b2*(1-p_success(x,mu,gamma))), lower = theta, upper = Inf)$value
  return (first_integral+second_integral)
}


expected_utilities_integrated <- numeric(length(theta_values))

#evalutate for different theta values
for (k in seq_along(theta_values)){
  expected_utilities_integrated[k] <- expected_utility_integral(15,1,theta_values[k])
}
#Plot the utilities for different theta
plot(theta_values,expected_utilities_integrated, type = "l", lwd = 2, 
     xlab = expression(theta), ylab = "Expected Utility",
     main = expression("Approx. Expected Utility vs " * theta))


#Q.5
#Optimize for lambda
opt <- optimize(expected_utility_integral, c(0,30), mu = 14.559763, gamma=1.066268, maximum = TRUE)
opt


#Q.6

#Normalize and exponentiate 
logpost_mat <- -posterior_probabilites              
logpost_shifted <- logpost_mat - max(logpost_mat)   
posterior_matrix <- exp(logpost_shifted)
posterior_matrix <- posterior_matrix / sum(posterior_matrix)



#Expected utility, average of posterior values
expected_utility_posterior_avg <- function(theta){
  total <- 0 
  
  for (i in seq_along(mu_values)){
    for (j in seq_along(gamma_values)){
      total <- total + posterior_matrix[i,j] * expected_utility_integral(mu_values[i], gamma_values[j], theta)
    }
  }
  return (total)
}

#Optimize to find theta value
opt_posterior <- optimize(expected_utility_posterior_avg, interval = c(0, 30), maximum = TRUE)
opt_posterior




