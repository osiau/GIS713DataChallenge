####################################################################################################################################################
#
#  Random Forest and Linear Model Code
#    for 48 hour data challenge
#    for Dr. Joshua Gray
#    @author Nicholas Grokhowsky
#
####################################################################################################################################################
# Import Libraries
####################################################################################################################################################
library(car)
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
library(tidyverse)
library(randomForestExplainer)
library(sp)
library(spatialEco)
####################################################################################################################################################
# Autocorrelation Function
####################################################################################################################################################
autoCorrelation <- function(shapeFile, depVariable, error)
{
  w <- 1/ as.matrix(dist(coordinates(shapeFile)))
  diag(w) <- 0
  print(moran.test(depVariable, mat2listw(w)))
  print(moran.test(error, mat2listw(w)))
  local <- localmoran(error, mat2listw(w))
  return(local)
}
####################################################################################################################################################
# Load and wrangle data
####################################################################################################################################################
# Load county shape file and independent sociodemographic variables
setwd("/home/nick/Downloads")
data_dir <- "/home/nick/Downloads"
county_shp <- shapefile(file.path(data_dir, "cb_2015_us_county_20m", "cb_2015_us_county_20m.shp"))
county_variables <- readRDS("county_independent_vars.RDS")
Y <- readRDS("county_dependent_vars.RDS")

# For the counts at re-open dates
reopens <- fread("reOpeningCounts.csv")

# Set sociodemographic data as data table and remove last 6 counties (Puerto Rico)
df <- as.data.table(county_variables)
df <- df[1:3214, ]

# Choose dependent variable
geoID <- Y$geoID
Y <- Y$total_cases_pc
#Y <- Y$total_deaths_pc
#Y <- Y$unemploy
#Y <- Y$vote_pct

Y <- data.frame(geoID, Y)
df <- merge(df, Y, by="geoID")

# In sociodemographic data - remove columns that are not needed, set all values that are NA to 0 and all column data types to numeric
df[, county := NULL]

# Impute all NA values to various options (0, knn, or mean)
#df <- df %>% mutate_all(funs(ifelse(is.na(.), 0, .))) ##### <- For Martine to explore -> I would check out the caret impute function but you can also use mean values by column
#set.seed(007)
#preProcess(df, "knnImpute")

for(i in 1:length(df$aqi_2019))
{
  count <- 0.0
  m <- 0.0
  
  countTwo <- 0.0
  mTwo <- 0.0
  
  if(!is.na(df$aqi_2019[i]))
  {
    count <- count + 1
    m <- m + df$aqi_2019[i] 
  }
  
  if(!is.na(df$frac_pubtransport[i]))
  {
    countTwo <- countTwo + 1
    mTwo <- mTwo + df$frac_pubtransport[i] 
  }
  
  m <- m / count
  mTwo <- mTwo/countTwo
  
}

for(i in 1:length(df$aqi_2019))
{
  if(is.na(df$aqi_2019[i]))
  {
    df$aqi_2019[i] <- m 
  }
  
  if(is.na(df$frac_pubtransport[i]))
  {
    df$frac_pubtransport[i] <- mTwo
  }
}



# Prepare data set column data types and create df_x for linear regression
geoID <- df$geoID
df <- sapply(df, as.numeric )
df <- as.data.frame(df)
df$geoID <- geoID
df_x <- df

# Get voter data and prepare sociodemographic and dpendent variable for merge
setwd("/home/nick/git_data_challenge/Data-Challenge-GIS713/cleandata")
votes <- fread("county_pres_Result2016.csv")
dem <- votes$per_dem
rep <- votes$per_gop
id <- votes$countyFIPS
voteDF <- data.frame(id, dem, rep)

# Merge data together (sociodemographic, voter data, and county shape file)
df$geoID <- as.character(df$geoID)
county_shp$GEOID <- as.character(county_shp$GEOID)
voteDF$id <- as.character(voteDF$id)
df <- merge(df, voteDF, by.x="geoID", by.y="id") 
county_shp <- merge(county_shp, df, by.x="GEOID", by.y="geoID")

# Cut down shapefile
nonContiguous <- c(60, 66, 69, 72, 78)
for(i in 1:length(nonContiguous))
{
  county_shp <- subset(county_shp, STATEFP != nonContiguous[i])
}
county_shp@data[is.na(county_shp@data)] <- 0.0
df <- data.frame(county_shp[10:length(names(county_shp))])

# Impute 0.0 values to 0.01 for log transformation
for(i in 1:length(df$Y))
{
  if(df$Y[i] == 0.0)
  {
    df$Y[i] = df$Y[i] + 0.000001
  }
}

# Log transform dependent variable
df$Y <- log(df$Y)
lm_df <- df
lm_Y <- df$Y
lm_x <- df[-c(18, 20)]

# Correlation matrix <- nothing fancy
cor(lm_x)
####################################################################################################################################################
# Linear Model
####################################################################################################################################################
# Get features that contribute to VIF scores < 5.0
setwd("/home/nick/Downloads")
colinearity <- dget('rfeVIF.R')

# Run best VIF score 5 times
for(i in 1:5)
{
  v <- colinearity(lm_x, lm_Y)
  v <- unlist(v)
  print(v)
  lm_x <- lm_x[, names(lm_x) %in% v]
}

# Get model with lowest AIC for n features # 7
eliminate <- dget('rfeAIC.R')
r <- eliminate(lm_x, lm_Y, 7)
lm_x <- lm_x[, names(lm_x) %in% r]

# Use features from AIC model to construct the correct data set to model and model
lm_df <- cbind(lm_Y, lm_x)
linearFit <- lm(lm_Y ~ ., data=lm_df)
summary(linearFit)
county_shp$res <- linearFit$residuals

# Spatial Autocorrelation
set.seed(007)
local <- autoCorrelation(county_shp, county_shp$Y, county_shp$res)

county_shp$var <- local[,3]
county_shp$p <- local[,5]
county_shp$I <- local[,1]

# Create LISA cluster plot
qualification <- county_shp$Y - mean(county_shp$Y)
I <- local[,1] - mean(local[,1])

quadrant <- vector(mode="numeric", length=nrow(local))
quadrant[qualification > 0 & I > 0] <- 4
quadrant[qualification < 0 & I < 0] <- 1
quadrant[qualification < 0 & I > 0] <- 2
quadrant[qualification > 0 & I < 0] <- 3
quadrant[county_shp$local[,5] > 0.1] <- 0

brks <- c(0,1,2,3,4)
colors <- c("white", "blue", rgb(0,0,1,alpha=0.4), rgb(1,0,0,alpha=0.4), "red")

ext <- c(-179.1743, 179.7739 * 0.5, 24.49813, 71.35256)
county_shp <- crop(county_shp, ext)

plot(county_shp, border=adjustcolor("gray", alpha.f=0.1), col=colors[findInterval(quadrant, brks, all.inside=FALSE)], main="LISA Cluster Map")
box()
y_axis <- extent(county_shp)[3] + 20.00
x_axis <- -179.7739 + 5.00
legend(x=x_axis, y=y_axis, legend = c("Insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n", cex=0.6)
####################################################################################################################################################
# Not Necessary
####################################################################################################################################################
# Split data
set.seed(007)
train_index <- createDataPartition(df$Y, p=0.8, list = FALSE)
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
# RF Models 
####################################################################################################################################################
set.seed(007)

###### OOB Model
rf <- randomForest(Y ~ ., 
                   data=df,
                   importance=TRUE)

# ###### Trained Model
# rf <- randomForest(x = training[, !names(training) %in% c('Y', 'Observed', 'GEOID')],
#                    y = training$Y,
#                    data = training,
#                    mtry = floor((ncol(training)-3)/3),
#                    ntrees = 500,
#                    importance = TRUE)

###### Tested
# rf <- randomForest(x = training[, !names(training) %in% c('Y', 'Observed', 'GEOID')],
#                    y = training$Y,
#                    xtest = testing[, !names(testing) %in% c('Observed', 'GEOID', 'Y')],
#                    ytest = testing$Y,
#                    data = training,
#                    mtry = floor((ncol(training)-3)/3),
#                    ntrees = 375,
#                    importance = TRUE)
###################################################################################################################################################
# Check Errors - trained, tested, and validated ### OOB Model is better than trained model 
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
summary(rf)
importance(rf)
varImpPlot(rf)
error <- rf$predicted - df$Y
county_shp$res <- error
county_shp$Y <- df$Y

# Spatial Autocorrelation
set.seed(007)
local <- autoCorrelation(county_shp, county_shp$Y, county_shp$res)


# Plot LISA Cluster Maps
county_shp$var <- local[,3]
county_shp$p <- local[,5]
county_shp$I <- local[,1]

qualification <- county_shp$Y - mean(county_shp$Y)
I <- local[,1] - mean(local[,1])

quadrant <- vector(mode="numeric", length=nrow(local))
quadrant[qualification > 0 & I > 0] <- 4
quadrant[qualification < 0 & I < 0] <- 1
quadrant[qualification < 0 & I > 0] <- 2
quadrant[qualification > 0 & I < 0] <- 3
quadrant[county_shp$local[,5] > 0.1] <- 0

brks <- c(0,1,2,3,4)
colors <- c("white", "blue", rgb(0,0,1,alpha=0.4), rgb(1,0,0,alpha=0.4), "red")

ext <- c(-179.1743, 179.7739 * 0.5, 24.49813, 71.35256)
county_shp <- crop(county_shp, ext)

plot(county_shp, border=adjustcolor("gray", alpha.f=0.1), col=colors[findInterval(quadrant, brks, all.inside=FALSE)], main="LISA Cluster Map")
box()
y_axis <- extent(county_shp)[3] + 20.00
x_axis <- -179.7739 + 5.00
legend(x=x_axis, y=y_axis, legend = c("Insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n", cex=0.6)

setwd("/home/nick/Downloads")
explain_forest(rf, interactions=TRUE, data=df)
####################################################################################################################################################
# Test Predictions
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
local <- autoCorrelation(county_shp, county_shp$Y, county_shp$res)