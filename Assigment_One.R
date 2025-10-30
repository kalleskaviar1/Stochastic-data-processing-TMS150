bikesharing <- read.csv("bikesharing.csv", header=TRUE)
bikesharing <- data.frame(bikesharing)


#Transform data
install.packages("chron")
library(chron)
bikesharing$timestamp <- as.chron(bikesharing$timestamp)

dates(bikesharing$timestamp)

# create new columns in the dataset
bikesharing$hrs<-hours(bikesharing$timestamp)
bikesharing$mnths<-months(bikesharing$timestamp) 

# redefine categorical variables
bikesharing$weather_code <- factor(bikesharing$weather_code,levels=c(1,2,3,4,7,10,26,94),
                                   labels=c("clear","semiclear","brkclouds","cloudy","lghtrain","thunderstorm",
                                            "snow","freezing"))
bikesharing$is_holiday <- factor(bikesharing$is_holiday,levels=c(0,1), labels =
                                   c("nonholiday","holiday"))
bikesharing$is_weekend<- factor(bikesharing$is_weekend,levels=c(0,1), labels =
                                  c("weekday","weekend"))
bikesharing$season<- factor(bikesharing$season,levels=c(0,1,2,3), labels =
                              c("spring","summer","fall","winter"))

bikesharing




#Question i

#Boxplot of number of shared bikes per season
boxplot(bikesharing$cnt~bikesharing$season, xlab = "Season", ylab = "Number of shared bikes")

#Boxplot of number of shared bikes at each hour
boxplot(bikesharing$cnt~bikesharing$hrs, xlab = "Hour", ylab = "Number of shared bikes")


#Question ii

#Plot number of shared bikes and humitidy
plot(bikesharing$cnt~bikesharing$hum)

#Create new subset excluding rush hour 

data_norush <- subset(bikesharing, !((hrs>=7 & hrs<=9) | (hrs>=17 & hrs<=18)))
data_norush

#Create new subset excluding rush hour and filters for spring
data_norush_spring <- subset(data_norush, season=="spring")
data_norush_spring

plot(data_norush_spring$cnt~data_norush_spring$hum)


#Boxcox transformation
library(MASS)

model_for_boxcox <- lm(data_norush_spring$cnt+1~data_norush_spring$hum)

#Box-Cox transform
bc <- boxcox(model_for_boxcox)

#Id for optimal lambda
id_max <- which(bc$y==max(bc$y)) 
#Optimal lambda
lambda <- bc$x[id_max]


#Calculate the transformed response
data_norush_spring$cnt_bc <- (data_norush_spring$cnt^lambda-1)/lambda

#Final model with transformed response 
final_model <- lm(data_norush_spring$cnt_bc~data_norush_spring$hum)

#Plot Transformed response vs Humidity
plot(data_norush_spring$cnt_bc~data_norush_spring$hum, xlab = "Humidity", ylab = "Transformed response")

#Plot Model residuals vs Humidity
plot(final_model$residuals~data_norush_spring$hum, xlab = "Humidity", ylab = "Model residuals")


#Question iii

set.seed(321)

#Extract parameters from model
beta0 <- coef(final_model)[1]
beta1 <- coef(final_model)[2]

#Calculate sigma_hat
sigma_hat <- summary(final_model)$sigma

#Humidity value to be used
hum_val <- 40

#Predicted transformed mean
yhat_lambda <- beta0 + beta1*hum_val

#Number of simulations
B <- 2000

#Simulate transformed responses
eps_new <- rnorm(B, mean = 0, sd = sigma_hat)
y_lambda_sim <- yhat_lambda + eps_new

#Back transform
y_sim <- (lambda * y_lambda_sim + 1)^(1/lambda)

#Calculate 95% prediction interval
pred_interval <- quantile(y_sim, probs = c(0.025, 0.975))


#Question iv

set.seed(321)

#Extract length of data and make train and test dataset
N <- nrow(data_norush_spring)
n <- floor(0.8 * N)
train_id <- sample(seq_len(N),n)
train <- data_norush_spring[train_id,]
test <- data_norush_spring[-train_id,]

#Gamma values to test
gamma_values <-  seq(0.5, 2, length.out = 16)

#Initialize empty pmse object to be used
pmse <- numeric(length(gamma_values))

for (i in seq(gamma_values)){
  #Gamma values
  gamma <- gamma_values[i]
  
  #Model
  model <- lm(cnt_bc ~ I(hum^gamma), data = train)
  
  #Predictions
  prediction <- predict(model, newdata = test)
  
  #pMSE
  pmse[i] <- sqrt(mean((test$cnt_bc-prediction)^2))
}
#Plot gamma values vs sqrt(pMSE)
plot(gamma_values, pmse, xlab = expression(gamma), ylab = expression(sqrt(pMSE)))


#Question V

set.seed(321)

#Creates a grid of all 10 plots
par(mfrow = c(2, 5))

for (j in 1:10){
  N <- nrow(data_norush_spring)
  n <- floor(0.8 * N)
  train_id <- sample(seq_len(N),n)
  train <- data_norush_spring[train_id,]
  test <- data_norush_spring[-train_id,]
  
  #Gamma values to test
  gamma_values <-  seq(0.5, 2, length.out = 16)
  
  #Initialize empty pmse object to be used
  pmse <- numeric(length(gamma_values))
  
  for (i in seq(gamma_values)){
    #Gamma values
    gamma <- gamma_values[i]
    
    #Model
    model <- lm(cnt_bc ~ I(hum^gamma), data = train)
    
    #Predictions
    prediction <- predict(model, newdata = test)
    
    #pMSE
    pmse[i] <- sqrt(mean((test$cnt_bc-prediction)^2))
  }
  #Plot gamma values vs sqrt(pMSE)
  plot(gamma_values, pmse, xlab = expression(gamma), ylab = expression(sqrt(pMSE)))
  
}



#Question vi 


set.seed(321)

#Coeffients and sigma from previous model
beta0_star <- coef(final_model)[1]
beta1_star <- coef(final_model)[2]
sigma_star <- summary(final_model)$sigma

#Humidity values
hum_values <- data_norush_spring$hum
n = length(hum_values)

#Number of iterations
B <- 2000

#Empty to store future results, True or False
ci0_cover <- logical(B)
ci1_cover <- logical(B)

counter0 <- 0
counter1 <- 1
for (j in 1:B){
  #New equation for response
  y_new <- beta0_star + beta1_star*hum_values + rnorm(n, mean = 0, sd = sigma_star)
  
  #Simulated model
  simulated_model <- lm(y_new ~ hum_values)
  
  #Confidence interval
  ci <- confint(simulated_model, level = 0.8)
  
  #Check if they are covered in the interval
  ci0_cover[j] <- (beta0_star >= ci[1,1] & beta0_star <= ci[1,2])
  ci1_cover[j] <- (beta1_star >= ci[2,1] & beta1_star <= ci[2,2])

}
#Check proportion of covered coefficents in interval
coverage_beta0 <- mean(ci0_cover)
coverage_beta1 <- mean(ci1_cover)

coverage_beta0
coverage_beta1


