# Import Libraries
library(data.table)
library(svMisc)
library(caret)
library(randomForest)
library(Metrics)
library(rsample)
library("plyr") # used to rename varibles in a data frame
library(dplyr)
require(spdep)
library(irr)
library(pROC)

library(raster)
library(data.table)
library(viridis)
library(knitr)

# Autocorrelation Function
autoCorrelation <- function(shapeFile, depVariable, error)
{
  w <- 1/ as.matrix(dist(coordinates(shapeFile)))
  diag(w) <- 0
  print(moran.test(depVariable, mat2listw(w)))
  print(moran.test(error, mat2listw(w)))
  print(moran.plot(error, mat2listw(w)))
  title(main="Moran's I Scatter Plot")
}

setwd("/home/nick/Downloads")
county_shp <- shapefile(file.path(data_dir, "cb_2015_us_county_20m", "cb_2015_us_county_20m.shp"))
county_variables <- readRDS("county_independent_vars.RDS")
Y <- readRDS("county_dependent_vars.RDS")

party <- Y$vote_pct
df <- as.data.table(county_variables)
df <- df[1:3114, ]
df$votes_pct <- party

#
#Observed <- Y$total_cases_pc
#Observed <- Y$total_deaths_pc
Observed <- Y$unemploy
Y <- Observed


df[, county := NULL]
df[, votes_pct := NULL]
df <- df %>% mutate_all(funs(ifelse(is.na(.), 0, .)))
df$Y <- Y
df <- sapply(df, as.numeric )
df_x <- df


setwd("/home/nick/git_data_challenge/Data-Challenge-GIS713/cleandata")
votes <- fread("county_pres_Result2016.csv")

dem <- votes$per_dem
rep <- votes$per_gop
id <- votes$countyFIPS
voteDF <- data.frame(id, dem, rep)


df <- merge(df, voteDF, by.x="geoID", by.y="id") # only 3111
county_shp$COUNTYFP <- as.numeric(as.character(county_shp$COUNTYFP))
df$geoID <- as.numeric(as.character(df$geoID))
county_shp <- merge(county_shp, df, by.x="GEOID", by.y="geoID")

county_shp <- county_shp[!is.na(county_shp$dem), ]
county_shp <- county_shp[!is.na(county_shp$rep), ]

df <- data.frame(county_shp[10:length(names(county_shp))])

# Alternative method to split data
set.seed(007)
train_index <- createDataPartition(df$Y, p=0.7, list = FALSE)
training <- df[train_index, ]
testing <- df[-train_index, ]

# Seperate x and y data - training
x_train <- training[-c(17)]
x_train <- as.data.frame(x_train)
norm <- preProcess(x_train, method = c('center', 'scale'))
x_train <- predict(norm, x_train)
y_train <- training[, names(training) %in% c('Y')] 
training <- cbind(x_train, y_train)
colnames(training)[colnames(training) == 'y_train'] <- 'Y'

# Seperate x and y data - testing
x_test <- testing[-c(17)]
x_test <- as.data.frame(x_test)
norm <- preProcess(x_test, method = c('center', 'scale'))
x_test <- predict(norm, x_test)
y_test <- testing[, names(testing) %in% c('Y')]
testing <- cbind(x_test, y_test)
colnames(testing)[colnames(testing) == 'y_test'] <- 'Y'

# Create new full dataset in order of test and training data
all_data <- rbind(x_train, x_test)
all_data_y <- c(y_train, y_test)
all_data <- cbind(all_data, all_data_y)
colnames(all_data)[colnames(all_data)== 'all_data_y'] <- 'Y'


####################################################################################################################################################
# Tuned Models 
####################################################################################################################################################
set.seed(007)

rf <- randomForest(x = training[, !names(training) %in% c('Y', 'Observed', 'GEOID')],
                   y = training$Y,
                   data = training,
                   mtry = floor((ncol(training)-3)/3),
                   ntrees = 500,
                   importance = TRUE)

# rf <- randomForest(x = training[, !names(training) %in% c('Y', 'Observed', 'GEOID')],
#                    y = training$Y,
#                    xtest = testing[, !names(testing) %in% c('Observed', 'GEOID', 'Y')],
#                    ytest = testing$Y,
#                    data = training,
#                    mtry = floor((ncol(training)-3)/3),
#                    ntrees = 500,
#                    importance = TRUE)
###################################################################################################################################################
# Check Errors - trained, tested, and validated
####################################################################################################################################################
# Extract Validation Errors
err <- sqrt(rf$mse) 
validation <- sqrt(rf$test$mse)

# Compare Error Rates
tibble::tibble(
  'Training Error' = err,
  'Test Error' = validation,
  ntrees = 1:rf$ntree
) %>%
  gather(Metric, RMSE, -ntrees) %>%
  ggplot(aes(ntrees, RMSE, color=Metric)) +
  geom_line() +
  scale_y_continuous(labels = scales::identity_pal()) +
  xlab('Number of Trees')


###################################################################################################################################################
# # Get model summary
####################################################################################################################################################
rf

importance(rf)
varImpPlot(rf)
####################################################################################################################################################
# 
####################################################################################################################################################
# Set natural space observations for training, testing, and prediction
observed_train <- training$Y
observed_test <- testing$Y
observed_all <- c(observed_train, observed_test)

# Trained Errors
predicted_train <- predict(rf, training[, !names(training) %in% c('Y')])
errors <- abs(observed_train - predicted_train)
error <- median(errors)
sprintf('Trained MAE %f', error)    # "Trained MAE 0.002994"

# Trained Correlation 
results <- data.frame(cbind(predicted_train, observed_train))
cor_matrix <- cor(results, use='all.obs', method='pearson')
cor_matrix[1,2]^2                   #Natural Space: 0.9415818    

# Tested Errors
predicted_test <- predict(rf, testing[, !names(testing) %in% c('Y')])
errors <- abs(observed_test - predicted_test)
error <- median(errors)
sprintf('Tested MAE %f', error)    

# Tested Correlation 
results <- data.frame(cbind(predicted_test, observed_test))
cor_matrix <- cor(results, use='all.obs', method='pearson')
cor_matrix[1,2]^2                  

# Validation Errors
predicted_all <- predict(rf, all_data[, !names(all_data) %in% c('Y')])
errors <- abs(observed_all - predicted_all)
error <- median(errors)
sprintf('Full Data MAE %f', error)    

# Tested Correlation 
results <- data.frame(cbind(predicted_all, observed_all))
cor_matrix <- cor(results, use='all.obs', method='pearson')
cor_matrix[1,2]^2

# Add errors to shapefile
county_shp$res <- errors

# Spatial Autocorrelation
autoCorrelation(county_shp, county_shp$Y, county_shp$res)


