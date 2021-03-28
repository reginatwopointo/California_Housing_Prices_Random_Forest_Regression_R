#Read the data set into R using a data frame named housing
df <- read.csv("C:\\Users\\regin\\OneDrive\\?????????????? ????????\\UCLA 2020 Data Science\\Introduction to Data Science\\Final Machine Learning Project\\housing.csv", header = TRUE)
#Run the head() and tail() functions on the data frame to get a feel for the actual data values
head(df)
tail(df)
#Run the summary() function on the data frame to get a sense for the data classes, range of values for numeric variables, levels for any factor variable, and any NAs found
summary(df)
#longitude         latitude     housing_median_age  total_rooms    total_bedrooms  
#Min.   :-124.3   Min.   :32.54   Min.   : 1.00      Min.   :    2   Min.   :   1.0  
#1st Qu.:-121.8   1st Qu.:33.93   1st Qu.:18.00      1st Qu.: 1448   1st Qu.: 297.0  
#Median :-118.5   Median :34.26   Median :29.00      Median : 2127   Median : 435.0  
#Mean   :-119.6   Mean   :35.63   Mean   :28.64      Mean   : 2636   Mean   : 536.8  
#3rd Qu.:-118.0   3rd Qu.:37.71   3rd Qu.:37.00      3rd Qu.: 3148   3rd Qu.: 643.2  
#Max.   :-114.3   Max.   :41.95   Max.   :52.00      Max.   :39320   Max.   :6445.0  
#population      households     median_income     median_house_value
#Min.   :    3   Min.   :   1.0   Min.   : 0.4999   Min.   : 14999    
#1st Qu.:  787   1st Qu.: 280.0   1st Qu.: 2.5634   1st Qu.:119600    
#Median : 1166   Median : 409.0   Median : 3.5348   Median :179700    
#Mean   : 1425   Mean   : 499.5   Mean   : 3.8707   Mean   :206856    
#3rd Qu.: 1725   3rd Qu.: 605.0   3rd Qu.: 4.7432   3rd Qu.:264725    
#Max.   :35682   Max.   :6082.0   Max.   :15.0001   Max.   :500001    
#ocean_proximity mean_bedrooms       mean_rooms      
#<1H OCEAN :9136   Min.   : 0.1212   Min.   :  0.8461  
#INLAND    :6551   1st Qu.: 1.0055   1st Qu.:  4.4407  
#ISLAND    :   5   Median : 1.0489   Median :  5.2291  
#NEAR BAY  :2290   Mean   : 1.1009   Mean   :  5.4290  
#NEAR OCEAN:2658   3rd Qu.: 1.1000   3rd Qu.:  6.0524  
#Max.   :34.0667   Max.   :141.9091

#most of the missing values (NA = 207) are found in total_bedrooms variable
#median_housing_age is 29 years old
#median of total_rooms is 2127 while mean is 2636
#average population is 1425 and average households are 499.5 while average income is 3.87
#median house value is $179,700 

#Create histograms for each numeric variable
hist(df$longitude, freq=TRUE, xlab = "Longitude", main = "Longitude Histogram", col="light yellow")
#most of the houses are located on a longitude (-123:-122) and (-119:-117)

hist(df$latitude, freq=TRUE, xlab = "Latitude", main = "Latitude Histogram", col="light blue")
#most of the houses are located on a latitude 34 and 38

hist(df$housing_median_age, freq=TRUE, xlab = "Median House Age", main = "Median House Age Histogram", col="light blue")

hist(df$total_rooms, freq=TRUE, xlab = "Total Rooms", main = "Total Rooms Histogram", col="light blue")
#median house age seems to be evenly distributed. However, most of the age values fall into the range of 15 to 45

hist(df$total_bedrooms, freq=TRUE, xlab = "Total Bedrooms", main = "Total Bedrooms Histogram", col="light yellow")
#most amount of bedrooms are in the range of 0 to 500 and less in 500 to 1000 value range

hist(df$population, freq=TRUE, xlab = "Population", main = "Population Histogram", col="light yellow")
#population size is mostly 0 to 2500

hist(df$households, freq=TRUE, xlab = "Households", main = "Households Histogram", col="light yellow")
#number of households are concentrated in 0 to 500 with the frequency more than 10,000 and 500 to 1000 with the frequency of 5,000

hist(df$median_income, freq=TRUE, xlab = "Median Income", main = "Median Income Histogram", col="light blue")
#median income is not evenly distributed and most values lay in the range 1 to 6 with the highest frequency of 5000 in values 3 and 4

hist(df$median_house_value, freq=TRUE, xlab = "Median House Value", main = "Median House Value Histogram", col="light blue")
#most houses have median house value in $50,000 to $250,000 

# impute the missing data values
df$total_bedrooms[is.na(df$total_bedrooms)]<-median(df$total_bedrooms,na.rm=TRUE)
df$total_bedrooms
summary(df) #check if NA's are imputed

#view ocean_proximity variable levels
ocean_pr_cat <- levels(df$ocean_proximity)
ocean_pr_cat
#[1] "<1H OCEAN"  "INLAND"     "ISLAND"     "NEAR BAY"   "NEAR OCEAN"

# Define a user defined function to build a logical map of
# ocean_proximity variable based on passed value
binary_ocean_pr <- function(c) {return(df$ocean_proximity == c)}
# Test the binary_ocean_pr function
# Binary map of whether ocean_proximity=="<1H OCEAN"
binary_ocean_pr("<1H OCEAN")
# Now use sapply to loop through all ocean_proximity levels
housing_cat <- sapply(ocean_pr_cat, binary_ocean_pr)
housing_cat[10:12,]
#     <1H OCEAN INLAND ISLAND NEAR BAY NEAR OCEAN
#[1,]     FALSE  FALSE  FALSE     TRUE      FALSE
#[2,]     FALSE  FALSE  FALSE     TRUE      FALSE
#[3,]     FALSE  FALSE  FALSE     TRUE      FALSE
class(housing_cat)
#[1] "matrix"

#create two new variables: mean_number_bedrooms and mean_number_rooms
df$mean_bedrooms = df$total_bedrooms/df$households
df$mean_rooms = df$total_rooms/df$households
df$mean_bedrooms
df$mean_rooms
#drop columns and median_house_value for feature scaling
drop = c("total_bedrooms", "total_rooms", "ocean_proximity", "median_house_value")
df_dropped = df[ , ! (names(df) %in% drop)]

df_scaled <- scale(df_dropped)
head(df_scaled)

cleaned_housing = cbind(housing_cat, df_scaled, median_house_value=df$median_house_value)

cleaned_housing <- as.data.frame(cleaned_housing)
names(cleaned_housing)
# [1] "<1H OCEAN"          "INLAND"             "ISLAND"            
#[4] "NEAR BAY"           "NEAR OCEAN"         "longitude"         
#[7] "latitude"           "housing_median_age" "population"        
#[10] "households"         "median_income"      "mean_bedrooms"     
#[13] "mean_rooms"         "median_house_value"

#Create Training and Test Sets
library(randomForest)
data(cleaned_housing)

n <- nrow(cleaned_housing)  # Number of observations
ntrain <- round(n*0.75)  # 75% for training set
set.seed(1969) # Set a random seed so that same sample can be reproduced in future runs

tindex <- sample(n, ntrain)   # Create an index

train <- cleaned_housing[tindex,]   # Create training set
test <- cleaned_housing[-tindex,]   # Create test set

#Supervised Machine Learning - Regression
train_x <- train[,-14]
train_y <- as.vector(cleaned_housing[tindex,]$median_house_value) 

is.vector(train_y)
#[1] TRUE  

rf <- randomForest(x = train_x, y= train_y, ntree=500, importance=TRUE)
rf
#Call:
#randomForest(x = train_x, y = train_y, ntree = 500, importance = TRUE) 
#Type of random forest: regression
#Number of trees: 500
#No. of variables tried at each split: 4

#Mean of squared residuals: 2413938779
#% Var explained: 81.8

names(rf)
# [1] "call"            "type"            "predicted"       "mse"            
#[5] "rsq"             "oob.times"       "importance"      "importanceSD"   
#[9] "localImportance" "proximity"       "ntree"           "mtry"           
#[13] "forest"          "coefs"           "y"               "test"           
#[17] "inbag" 

#see Mean Squared Error (MSE) which is a measure of feature importance
rf$importance
#                      %IncMSE IncNodePurity
#<1H OCEAN          1614113148  4.411053e+12
#INLAND             4085472971  2.855689e+13
#ISLAND                2001513  7.494963e+10
#NEAR BAY            432454361  1.187606e+12
#NEAR OCEAN          490361786  2.046571e+12
#longitude          6600368034  2.304761e+13
#latitude           5244669004  2.025027e+13
#housing_median_age 1112241450  9.256361e+12
#population         1077965651  6.933560e+12
#households         1130179313  7.474894e+12
#median_income      8401358754  6.841596e+13
#mean_bedrooms       440097741  7.285389e+12
#mean_rooms         1882836851  2.019315e+13

#Evaluating Model Performance
# Not specifying a data source forces OOB predictions
oob_prediction = predict(rf)

# Now compute the training set RMSE
train_mse = mean(as.numeric((oob_prediction - train_y)^2))
oob_rmse = sqrt(train_mse)
oob_rmse
#[1] 49177.51

#Split the test set
test_x <- test[,-14]
test_y <- as.vector(cleaned_housing[-tindex,]$median_house_value) 

is.vector(train_y)
#[1] TRUE

y_pred = predict(rf , test_x)
# Now compute the test set RSME
test_mse = mean(((y_pred - test_y)^2))
test_rmse = sqrt(test_mse)
test_rmse
#[1] 49582.82

#the model score roughly the same on the training and testing data, 
#suggesting that it is not overfit and that it makes good predictions