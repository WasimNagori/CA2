# Load the assigned dataset
sleep_data <- read.csv("sleep(1).csv")

# Reviewing structure and statistical summary of the dataset
str(sleep_data)
summary(sleep_data)

# Preparing the data
names(sleep_data)[names(sleep_data) == "sr.1"] <- "sh"
sleep_data$stress <- factor(sleep_data$sl, labels = c("low", "medium low", "medium", "medium high", "high"))

# Examine initial linearity between variables in the dataset
library(psych)
pairs.panels(sleep_data,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals


# Examine linearity in more detail
scatter.smooth(x = sleep_data$sr,
               y = sleep_data$sl,
               xlab = "Snoring rate",
               ylab = "Stress level", main = "Correlation of stress level ~ snoring rate")
cor(sleep_data$sr, sleep_data$sl)

# 2
scatter.smooth(x = sleep_data$rr,
               y = sleep_data$sl,
               main = "stress level ~ respiration rate",
               xlab = "Respiration rate",
               ylab = "Stress level")
cor(sleep_data$rr, sleep_data$sl)


# 3
scatter.smooth(x = sleep_data$t,
               y = sleep_data$sl,
               main = "stress level ~ body temperature",
               xlab = "Body Temperature",
               ylab = "Stress level")
paste("Correlation for stress and body temperature: ", cor(sleep_data$sl, sleep_data$t))

# 4
scatter.smooth(x = sleep_data$lm,
               y = sleep_data$sl,
               main = "stress level ~ limb movement",
               xlab = "Limb movement",
               ylab = "Stress level")
paste("Correlation for stress and limb movement: ", cor(sleep_data$sl, sleep_data$lm))


# 5
scatter.smooth(x = sleep_data$bo,
               y = sleep_data$sl,
               main = "stress level ~ blood oxygen",
               xlab = "Blood oxygen",
               ylab = "Stress level")
paste("Correlation for stress and blood oxygen: ", cor(sleep_data$sl, sleep_data$bo))

# 6
scatter.smooth(x = sleep_data$rem,
               y = sleep_data$sl,
               main = "stress level ~ rapid eye movement",
               xlab = "Rapid Eye Movement",
               ylab = "Stress level")
paste("Correlation for stress and rapid eye movement: ", cor(sleep_data$sl, sleep_data$rem))

# 7
scatter.smooth(x = sleep_data$sh,
               y = sleep_data$sl,
               main = "stress level ~ sleeping hours",
               xlab = "Sleeping hours",
               ylab = "Stress level")
paste("Correlation for stress and sleeping hours: ", cor(sleep_data$sl, sleep_data$sh))

# 8
scatter.smooth(x = sleep_data$hr,
               y = sleep_data$sl,
               main = "stress level ~ heart rate",
               xlab = "Heart rate",
               ylab = "Stress level")
paste("Correlation for stress and heart rate: ", cor(sleep_data$sl, sleep_data$hr))





# Check for outliers
opar <- par(no.readonly = TRUE)
par(mfrow = c(4, 2)) # divide graph area in 3 rows by 2 columns
attach(sleep_data)
boxplot(sl,
        main = "Stress level",
        sub = paste("Outlier rows: ",
                    boxplot.stats(sl)$out)) # box plot for 'stress level'
boxplot(sr,
        main = "Snoring rate",
        sub = paste("Outlier rows: ",
                    boxplot.stats(sr)$out)) # box plot for 'snoring rate'
boxplot(rr,
        main = "Respiration rate",
        sub = paste("Outlier rows: ",
                    boxplot.stats(rr)$out)) # box plot for 'Respiration rate'
boxplot(t,
        main = "Body temperature",
        sub = paste("Outlier rows: ",
                    boxplot.stats(t)$out)) # box plot for 'Body temperature'
boxplot(lm,
        main = "Limb movement",
        sub = paste("Outlier rows: ",
                    boxplot.stats(lm)$out)) # box plot for 'Limb movement'
boxplot(bo,
        main = "Blood oxygen",
        sub = paste("Outlier rows: ",
                    boxplot.stats(bo)$out)) # box plot for 'Blood oxygen'
boxplot(rem,
        main = "Rapid Eye Movement",
        sub = paste("Outlier rows: ",
                    boxplot.stats(rem)$out)) # box plot for 'Rapid Eye Movement'

boxplot(sh,
        main = "Sleeping hours",
        sub = paste("Outlier rows: ",
                    boxplot.stats(sh)$out)) # box plot for 'Sleeping hours'

boxplot(hr,
        main = "Heart rate",
        sub = paste("Outlier rows: ",
                    boxplot.stats(hr)$out)) # box plot for 'Heart rate'
detach(sleep_data)
par(opar)


#install.packages("e1071")
library(e1071)
opar <- par(no.readonly = TRUE)
par(mfrow = c(4,2))# divide graph area into 1 row x 2 cols
attach(sleep_data)
plot(density(sl),
     main = "Density plot : Stress Level",
     ylab = "Frequency", xlab = "Stress level",
     sub = paste("Skewness : ", round(e1071::skewness(sl), 2)))
# fill the area under the plot
polygon(density(sl), col = "red")


plot(density(sr),
     main = "Density plot : Snoring rate",
     ylab = "Frequency", xlab = "Snoring rate",
     sub = paste("Skewness : ", round(e1071::skewness(sr), 2)))
# fill the area under the plot
polygon(density(sr), col = "red")


plot(density(rr),
     main = "Density plot : Respiration rate",
     ylab = "Frequency", xlab = "Respiration rate",
     sub = paste("Skewness : ", round(e1071::skewness(rr), 2)))
# fill the area under the plot
polygon(density(rr), col = "red")


plot(density(t),
     main = "Density plot : Body Temperature",
     ylab = "Frequency", xlab = "Body Temperature",
     sub = paste("Skewness : ", round(e1071::skewness(t), 2)))
# fill the area under the plot
polygon(density(t), col = "red")


plot(density(lm),
     main = "Density plot : Limb movement",
     ylab = "Frequency", xlab = "Limb movement",
     sub = paste("Skewness : ", round(e1071::skewness(lm), 2)))
# fill the area under the plot
polygon(density(lm), col = "red")


plot(density(bo),
     main = "Density plot : Blood oxygen",
     ylab = "Frequency", xlab = "Blood oxygen",
     sub = paste("Skewness : ", round(e1071::skewness(bo), 2)))
# fill the area under the plot
polygon(density(bo), col = "red")


plot(density(rem),
     main = "Density plot : Rapid eye movement",
     ylab = "Frequency", xlab = "Rapid eye movement",
     sub = paste("Skewness : ", round(e1071::skewness(rem), 2)))
# fill the area under the plot
polygon(density(rem), col = "red")


plot(density(sh),
     main = "Density plot : Sleeping hours",
     ylab = "Frequency", xlab = "Sleeping hours",
     sub = paste("Skewness : ", round(e1071::skewness(sh), 2)))
# fill the area under the plot
polygon(density(sh), col = "red")


plot(density(hr),
     main = "Density plot : Heart rate",
     ylab = "Frequency", xlab = "Heart rate",
     sub = paste("Skewness : ", round(e1071::skewness(hr), 2)))
# fill the area under the plot
polygon(density(hr), col = "red")
detach(sleep_data)
par(opar)

paste("Skewness for Snoring rate : ", round(e1071::skewness(sleep_data$sr), 2))
paste("Skewness for Respiration rate : ", round(e1071::skewness(sleep_data$rr), 2))
paste("Skewness for Body temperature : ", round(e1071::skewness(sleep_data$t), 2))
paste("Skewness for Limb movement : ", round(e1071::skewness(sleep_data$lm), 2))
paste("Skewness for Blood oxygen : ", round(e1071::skewness(sleep_data$bo), 2))
paste("Skewness for Rapid eye movement : ", round(e1071::skewness(sleep_data$rem), 2))
paste("Skewness for Sleeping hours : ", round(e1071::skewness(sleep_data$sh), 2))
paste("Skewness for Heart rate : ", round(e1071::skewness(sleep_data$hr), 2))
paste("Skewness for Stress level : ", round(e1071::skewness(sleep_data$sl), 2))

# Check normality of the variables
shapiro.test(sleep_data$sr)
shapiro.test(sleep_data$rr)
shapiro.test(sleep_data$t)
shapiro.test(sleep_data$lm)
shapiro.test(sleep_data$bo)
shapiro.test(sleep_data$rem)
shapiro.test(sleep_data$sh)
shapiro.test(sleep_data$hr)
shapiro.test(sleep_data$sl)
# p-value indices that the data is not normally distributed
# If p-value < 0.05 then variable is not normally distributed

# Snoring rate is not normally distributed (p-value = 2.2e-16)
# Respiration rate is not normally distributed (p-value = 1.571e-15)
# Body temperature is not normally distributed (p-value = 6.969e-09)
# Limb movement is not normally distributed (p-value = 3.211e-14)
# Blood oxygen is not normally distributed (p-value = 1.324e-11)
# Rapid eye movement is not normally distributed (p-value = 1.313e-15)
# Sleeping hours is not normally distributed (p-value = 2.2e-16)
# Heart rate is not normally distributed (p-value = 1.571e-15)
# Stress level is not normally distributed (p-value = 2.2e-16)


# Load the necessary library
library(MASS)

# Transform sr
Box = boxcox(sleep_data$sr ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_sr = (sleep_data$sr^(lambda) - 1)/lambda
hist(transformed_sr)
shapiro.test(transformed_sr)


# Transform rr
Box = boxcox(sleep_data$rr ~ 1, lambda = seq(-6,6,0.1))
Cox = data.frame(Box$x, Box$y)   
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
cox_smallest_y[1,]                                  
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_rr = (sleep_data$rr ^ lambda - 1)/lambda
hist(transformed_rr)
shapiro.test(transformed_rr)

# Transform t
Box = boxcox(sleep_data$t ~ 1, lambda = seq(-6,6,0.1))
Cox = data.frame(Box$x, Box$y)   
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
cox_smallest_y[1,]                                  
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_t = (sleep_data$t ^ lambda - 1)/lambda
hist(transformed_t)
shapiro.test(transformed_t)

# Transform lm
Box = boxcox(sleep_data$lm ~ 1, lambda = seq(-6,6,0.1))
Cox = data.frame(Box$x, Box$y)   
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
cox_smallest_y[1,]                                  
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_lm = (sleep_data$lm ^ lambda - 1)/lambda
hist(transformed_lm)
shapiro.test(transformed_lm)

# Transform bo
Box = boxcox(sleep_data$bo ~ 1, lambda = seq(-6,6,0.1))
Cox = data.frame(Box$x, Box$y)   
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
cox_smallest_y[1,]                                  
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_bo = (sleep_data$bo ^ lambda - 1)/lambda
hist(transformed_bo)
shapiro.test(transformed_bo)

# Transform rem
Box = boxcox(sleep_data$rem ~ 1, lambda = seq(-6,6,0.1))
Cox = data.frame(Box$x, Box$y)   
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
cox_smallest_y[1,]                                  
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_rem = (sleep_data$rem ^ lambda - 1)/lambda
hist(transformed_rem)
shapiro.test(transformed_rem)

# Transform sh
library(dplyr)
filtered_data <- sleep_data %>%
  filter(sh > 0)
Box = boxcox(filtered_data$sh ~ 1, lambda = seq(-6,6,0.1))
Cox = data.frame(Box$x, Box$y)   
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
cox_smallest_y[1,]                                  
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_sh = (sleep_data$sh ^ lambda - 1)/lambda
hist(transformed_sh)
shapiro.test(transformed_sh)

# Transform hr
Box = boxcox(sleep_data$hr ~ 1, lambda = seq(-6,6,0.1))
Cox = data.frame(Box$x, Box$y)   
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
cox_smallest_y[1,]                                  
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_hr = (sleep_data$hr ^ lambda - 1)/lambda
hist(transformed_hr)
shapiro.test(transformed_hr)


# Since the variables are still not normalised
# Tukey’s Ladder of Powers transformation
#install.packages("rcompanion")
library(rcompanion)
transform_tukey_sr = transformTukey(sleep_data$sr, plotit=FALSE)
# -0.05 indaictes that "-1 * x ^ lambda" is required
transformed_sr <- -1 * sleep_data$sr ^ -0.05
shapiro.test(transformed_sr)
hist(transformed_sr)

transform_tukey_rr = transformTukey(sleep_data$rr, plotit=FALSE)
# -1.075 indaictes that "-1 * x ^ lambda" is required
transformed_rr <- -1 * sleep_data$rr ^ -1.075
shapiro.test(transformed_rr)
hist(transformed_rr)

transform_tukey_t = transformTukey(sleep_data$t, plotit=FALSE)
# 4.55 indaictes that "x ^ lambda" is required
transformed_t <- sleep_data$t ^ 4.55
shapiro.test(transformed_t)
hist(transformed_t)

transform_tukey_lm = transformTukey(sleep_data$lm, plotit=FALSE)
# 0.425 indaictes that "x ^ lambda" is required
transformed_lm <- sleep_data$lm ^ 0.425
shapiro.test(transformed_lm)
hist(transformed_lm)

transform_tukey_bo = transformTukey(sleep_data$bo, plotit=FALSE)
# 4.3 indaictes that "x ^ lambda" is required
transformed_bo <- sleep_data$bo ^ 4.3
shapiro.test(transformed_bo)
hist(transformed_bo)

transform_tukey_rem = transformTukey(sleep_data$rem, plotit=FALSE)
# 3.025 indaictes that "x ^ lambda" is required
transformed_rem <- sleep_data$rem ^ 3.025
shapiro.test(transformed_rem)
hist(transformed_rem)

transform_tukey_sh = transformTukey(sleep_data$sh, plotit=FALSE)
# 0.775 indaictes that "x ^ lambda" is required
transformed_sh <- sleep_data$sh ^ 0.775
shapiro.test(transformed_sh)
hist(transformed_sh)

transform_tukey_hr = transformTukey(sleep_data$hr, plotit=FALSE)
# -1.55 indaictes that "-1 * x ^ lambda" is required
transformed_hr <- -1 * sleep_data$hr ^ -1.55
shapiro.test(transformed_hr)
hist(transformed_hr)

transform_tukey_sl = transformTukey(sleep_data$sl, plotit=FALSE)
# 0.925 indaictes that "x ^ lambda" is required
transformed_sl <- sleep_data$sl ^ 0.925
shapiro.test(transformed_sl)
hist(transformed_sl)


# Converting data in data frame
sleep_data$transformed_sr <- transformed_sr
sleep_data$transformed_rr <- transformed_rr
sleep_data$transformed_t <- transformed_t
sleep_data$transformed_lm <- transformed_lm
sleep_data$transformed_bo <- transformed_bo
sleep_data$transformed_rem <- transformed_rem
sleep_data$transformed_sh <- transformed_sh
sleep_data$transformed_hr <- transformed_hr
sleep_data$transformed_sl <- transformed_sl



attach(sleep_data)

# Split the data into training and testing
set.seed(1)
no_rows_data <- nrow(sleep_data)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

training_data <- sleep_data[sample, ]
testing_data <- sleep_data[-sample, ]
library(nnet)
unmodified_model <- multinom(sl ~ sr + rr + t + bo + lm + rem + sh + hr, 
                             data=training_data)
options(scipen=999)
summary(unmodified_model)


# Examine which combination of independent variables best fits the model
#install.packages("leaps")
library(leaps)
# Regression Subset Selection
MLR_subset_selection <-regsubsets(sl ~ sr + rr + t + bo + lm + rem + sh + hr, 
                                  data=training_data, nbest=6)
plot(MLR_subset_selection, scale="adjr2")

# Smallest AIC is best
# Seems that sl ~ sr + rr + t + bo + lm + rem + sh + hr is best.
stepAIC(unmodified_model, direction="backward")


modified_model <- multinom(transformed_sl ~ transformed_sr + transformed_rr 
                     + transformed_t + transformed_bo + transformed_lm + 
                       transformed_rem + transformed_sh + transformed_hr, 
                     data=training_data)
summary(modified_model)


# We can examine best fitting model using leaps
MLR_subset_selection_modified <-regsubsets(transformed_sl ~ transformed_sr + transformed_rr 
                                           + transformed_t + transformed_bo + transformed_lm + 
                                             transformed_rem + transformed_sh + transformed_hr, 
                                           data=training_data, nbest=6)
plot(MLR_subset_selection_modified, scale="adjr2")
stepAIC(modified_model, direction="backward")
# It seems that the untransformed model
# could be more accurate than the transformed model 


confint(unmodified_model)
confint(modified_model)

# Model forecast 

# Get predicted classes
predicted <- predict(unmodified_model, newdata = testing_data, type = "class")
# Create confusion matrix
conf_matrix <- table(testing_data$sl, predicted)
print(conf_matrix)

# Calculate classification accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(accuracy)

# Get log-likelihood and deviance
log_likelihood <- unmodified_model$logLik
deviance <- unmodified_model$deviance
print(log_likelihood)
print(deviance)

# Get predicted classes for the testing set
predicted <- predict(unmodified_model, newdata = testing_data, type = "class")
# Calculate predictive accuracy
predictive_accuracy <- sum(predicted == testing_data$sl) / nrow(testing_data)
print(predictive_accuracy)










