# ---------------------------Preliminary Data analysis ------------------------------- #

rm(list = ls()) # Clearing the console of any previous variables  

# Reading in the required packages
require(data.table) # Faster and smoother data read and write capability compared to Base R
require(ggplot2) # Customizable and better visulaization compared to Base R
require(dplyr) # Easy and quick data cleaning and transformation
require(lubridate) # Package made for easy date manipulation
require(fitdistrplus) # Will help us identify which distribution a variable fits into
require(ggmap) #This provides us with map making capabilities
require(caret) #Will be used for stratified sampling
require(rpart) #Classification tree
require(hydroGOF) #rmse function

# It is easier to read in all the packages at once using sapply

# Reading in the dataset from the online source
# Naming variable considering possible future addition of yellow cab data
# Link for data obtained from http://www.nyc.gov/html/tlc/html/about/trip_record_data.shtml

gtaxi_data = fread("https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv")

#Checking the dimensions of the dataset
dim(gtaxi_data) # 1494926 x 21 data table

# Checking types of the data fields and example data values
str(gtaxi_data)

# Except for Store_and_fwd_flag (string), all other data fields are numerical/integer
# Some data fields classified as integers should be factors - payment type, trip type and VendorID
gtaxi_data$VendorID = as.factor(gtaxi_data$VendorID)
gtaxi_data$Trip_type = as.factor(gtaxi_data$Trip_type)
gtaxi_data$Payment_type = as.factor(gtaxi_data$Payment_type)
gtaxi_data$RateCodeID = as.factor(gtaxi_data$RateCodeID)

# Only NAs can be seen for Ehail_fee - checking for values in the field
print(unique(gtaxi_data$Ehail_fee))

# Only NA values are contained in the field. The field not contained in data dictionary either
# Removing NA Ehail_fee column
gtaxi_data$Ehail_fee = NULL

# Looking at the statistical summary of the data
summary(gtaxi_data)

# Summary shows some Latitude and Longitude data has 0's which is not right for NYC.
# There are negative fares, tips and total_amounts. Either bad data or meaning unclear.
# Might need data cleaning based on what appears in graphs and which fields will be needed


#Plotting the histogram of number of passengers to see distribution
qplot(Passenger_count, data=gtaxi_data, 
      geom="histogram", binwidth = 2)
#Distribution makes sense - the maximum frequency is of the bin <= 2

#Plotting the the Fare Amount to look at how many negative values are present
qplot(Fare_amount, data=gtaxi_data, 
      geom="histogram", binwidth = 1)
# The plot is highly skewed due to outliers. We still cannot say if the negative values are correct or not

#Checking the number of negative values and the number of positive outliers:
print("The number of Fare Amounts in the negative:")
print(sum(gtaxi_data$Fare_amount < 0)) #There are 2417

#Looking at a few negative entries to see what is different
gtaxi_data[gtaxi_data$Fare_amount <= -400, ]

#The negative entries are only of the payment type 3, 4 - no charge and dispute
#Confirming this statement
gtaxi_data %>%
  filter(Fare_amount < 0)%>%
  group_by(Payment_type)%>%
  summarize(type_count = n())

#There are 208 values which are of the cash and credit card type.

# -------------------------------------- Trip Distance Analysis ------------------------------

#Plotting a histogram of the trip distance
qplot(Trip_distance, data=gtaxi_data, 
      geom="histogram", binwidth = 1)

# Again the plot is highly skewed due to the outliers.
#The bulk of the data points are below 50 miles

#Checking number of extreme values for trip_distance
print("The number of trips with trip_distance > 50:")
sum(gtaxi_data$Trip_distance > 50) #Only 68 trips out of 1494926 greater than 50 miles
sum(gtaxi_data$Trip_distance > 20) #Only 3364 trips out of 1494926 greater than 20 miles

#Plotting the subset dataset for better look at the distribution
qplot(Trip_distance, data=gtaxi_data[gtaxi_data$Trip_distance < 20, ], 
      geom="histogram", binwidth = 1)

# We want to look at the distribution that best fits this variable to get a better understanding
# We will also plot the CDF and PDF first
# These functions use the fitdistrplus package
plotdist(gtaxi_data$Trip_distance, histo = TRUE, demp = TRUE)

# Now we will plot the Cullen Frey graph which helps us to figure out which distribution fits
# the data in the best manner
descdist(gtaxi_data$Trip_distance, discrete = FALSE, boot = 100)
# The bootstrap allows to normalize the random variability in the sample
# The distribution does not necessarily fit any parametric distribution

#Plotting the Cullen Frey for the subset of the data
descdist(gtaxi_data$Trip_distance[gtaxi_data$Trip_distance < 20], discrete = FALSE, boot = 100)
# This dsitribution does not fit any parametric dist. closely either but it does resemble the 
# beta and lognormal distributions.

#For a bivariate analysis, we can look at the scatter of trip distance with fare amount
#to see if the outliers were actual data points or bad data

qplot(Trip_distance, Fare_amount, data = gtaxi_data)
# There is a linearly increasing trend among the data but there are alot of points joined to
# the axes as well. These points indicate issues with the data or special cases/disputes.

# We can also look at this bivariate analysis in terms of the payment types 
qplot(Trip_distance, Fare_amount, data = gtaxi_data, colour = Payment_type)

qplot(Trip_distance, Fare_amount, data = gtaxi_data[Trip_distance > 50, ])
#The largest trip with trip distance ~ 600 has 0 fare amount indicating it is bad data.

# Another indicator to ensure if some points are bad data or not is to see the tip amounts for
# the trips as well
qplot(Trip_distance, Fare_amount, data = gtaxi_data, colour = Tip_amount)


# ------------------------------------ Time Analysis ------------------------------------------
# We have two time variables - the time of pickup and dropoff.
# We can use both of these to extract information about the taxi activity in the City.

# Will use the package lubridate to extract variables from the drop and pickup variables

##### Prospective variables: date of month, day of week, hour of day, trip time

# We will consider the pickup time as the primary time variable

# Date of month
gtaxi_data$month_date = mday(gtaxi_data$lpep_pickup_datetime)
unique(gtaxi_data$month_date) #Check to see conversion has happened

# Day of week
gtaxi_data$weekday = wday(gtaxi_data$lpep_pickup_datetime) # Sunday is 1 
unique(gtaxi_data$weekday)
gtaxi_data$weekend = 0
gtaxi_data$weekend[gtaxi_data$weekday %in% c(7, 1)] = 1
gtaxi_data$weekday = as.factor(gtaxi_data$weekday)

# Hour of day
gtaxi_data$pickup_hour = hour(gtaxi_data$lpep_pickup_datetime)
unique(gtaxi_data$pickup_hour)

gtaxi_data$ride_duration = difftime(gtaxi_data$Lpep_dropoff_datetime,
                                    gtaxi_data$lpep_pickup_datetime, units = "hours")

range(gtaxi_data$ride_duration)


# Now we have all the time variables we need. Reporting mean and median trip distance by hour

mean_trip = gtaxi_data %>%
  group_by(pickup_hour) %>%
  summarise(mean_distance = mean(Trip_distance))

# The trips are shorter from 9 AM to 9 PM and longest very early morning at 5 and 6.
print("The mean trip distances grouped by the hour of the day are:")
print(mean_trip)

median_trip = gtaxi_data %>%
  group_by(pickup_hour) %>%
  summarise(median_distance = median(Trip_distance))

# The median reveals the same info - shorter trips from 9-9 and longest early morning.
print("The median trip distances grouped by the hour of the day are:")
print(median_trip)


#We now need to look at trip to and from the NYC airport areas.

#For this, we have the variable RateCodeID that tells us if the trip was to or
#from one of the airports

# RateCodeID 2, 3 and 4 correspond to NYC airports.
# LaGuardia is not included in this. Maybe Green Taxis not allowed as 
# this is within the city??

# The numerical summary of trips to the airports
airport_trips = gtaxi_data %>%
  filter(RateCodeID %in% c(2, 3, 4)) %>%
  group_by(RateCodeID) %>%
  summarise(number_of_trips = n(), avg_fare = mean(Fare_amount), 
            avg_dist = mean(Trip_distance), 
            passengers = mean(Passenger_count), avg_tip = mean(Tip_amount))

print("A summary of the trips made to the airports around NYC is:")
print(airport_trips)

# We can also look at the payment methods for each airport
airport_payments = gtaxi_data %>%
  filter(RateCodeID %in% c(2, 3, 4)) %>%
  group_by(RateCodeID, Payment_type) %>%
  summarise(trip_count = n())

airport_payments = airport_payments %>%
  group_by(RateCodeID) %>%
  mutate(totals = sum(trip_count)) %>%
  mutate(pay_prop = trip_count/totals)

# As expected, most payments were cash or card. The proportion of 
# disputes and no charge seems to be higher for Newark compared to JFK. Also, more JFK flights
# are paid by cash.

# Finally, we can look at the travel times and days for each airport
airport_hour = gtaxi_data %>%
  filter(RateCodeID %in% c(2, 3, 4)) %>%
  group_by(RateCodeID, pickup_hour) %>%
  summarise(trip_count = n())

# To get a better look , we can plot this
ggplot(airport_hour, aes(x = pickup_hour, y = trip_count, 
                         fill = RateCodeID)) + 
  geom_bar(stat = "identity", position="dodge")

#Looking by day of week
airport_day = gtaxi_data %>%
  filter(RateCodeID %in% c(2, 3, 4)) %>%
  group_by(RateCodeID, weekday) %>%
  summarise(trip_count = n())

#Plotting
ggplot(airport_day, aes(x = weekday, y = trip_count, 
                        fill = RateCodeID)) + 
  geom_bar(stat = "identity", position="dodge")


#Using maps to see where all the pickups are located

########### Reference: https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/ggmap/ggmapCheatsheet.pdf


NY_Map = ggmap(get_map(location = 'new york', zoom = 9, source = 'stamen', 
                       maptype="toner"))

allpickups_Map = NY_Map + geom_point(data = gtaxi_data, aes(x = Pickup_longitude , y = Pickup_latitude), alpha = .5, color="darkred", size = 3)

print(allpickups_Map)
# Now plotting only airport pickups

airportpick_Map = NY_Map + geom_point(data = gtaxi_data[RateCodeID %in% c(2, 3, 4), ], aes(x = Pickup_longitude , y = Pickup_latitude), alpha = .5, color="darkblue", size = 3)

print(airportpick_Map)

#The two maps don't show a lot of information that we can use to distinguish

# -------------------------------------- MOdeling --------------------------------------------

# There are 0s for Total_amount variable. We will put tip amount as 0 for those cases.
gtaxi_data$Tip_percent = 0

# Calculate for other cases
gtaxi_data$Tip_percent[gtaxi_data$Total_amount > 0] = 
  (gtaxi_data$Tip_amount[gtaxi_data$Total_amount > 0] / 
     gtaxi_data$Total_amount[gtaxi_data$Total_amount > 0]) * 100

summary(gtaxi_data)
#Based on the summary, only trip type has 4 NA's. We will make those 1 (higher perentage).
gtaxi_data$Trip_type[is.na(gtaxi_data$Trip_type)] = 1

# Locations will be important in our predictions.
# There are 0's in these fields. We will input the means into these fields.
# I will take into account both pickup and drop to compute mean

avg_lat = mean(c(gtaxi_data$Pickup_latitude[gtaxi_data$Pickup_latitude > 0], 
                 gtaxi_data$Dropoff_latitude[gtaxi_data$Dropoff_latitude > 0]))

avg_lon = mean(c(gtaxi_data$Pickup_longitude[gtaxi_data$Pickup_longitude < 0], 
                 gtaxi_data$Dropoff_longitude[gtaxi_data$Dropoff_longitude < 0]))

# Replacing 0's with mean values
gtaxi_data$Pickup_latitude[gtaxi_data$Pickup_latitude == 0] = avg_lat
gtaxi_data$Dropoff_latitude[gtaxi_data$Dropoff_latitude == 0] = avg_lat

gtaxi_data$Pickup_longitude[gtaxi_data$Pickup_longitude == 0] = avg_lon 
gtaxi_data$Dropoff_longitude[gtaxi_data$Dropoff_longitude == 0] = avg_lon

# To validate a new sample, all the created fields will be needed
# Creating a function for preparing the data

data_prep = function(new_data){
  
  new_data$VendorID = as.factor(new_data$VendorID)
  new_data$Trip_type = as.factor(new_data$Trip_type)
  new_data$Payment_type = as.factor(new_data$Payment_type)
  new_data$RateCodeID = as.factor(new_data$RateCodeID)
  
  new_data$Ehail_fee = NULL
  
  new_data$month_date = mday(new_data$lpep_pickup_datetime)
  
  # Day of week
  new_data$weekday = wday(new_data$lpep_pickup_datetime) # Sunday is 1
  new_data$weekend = 0
  new_data$weekend[new_data$weekday %in% c(7, 1)] = 1
  new_data$weekday = as.factor(new_data$weekday)
  
  # Hour of day
  new_data$pickup_hour = hour(new_data$lpep_pickup_datetime)
  
  # Ride duration
  new_data$ride_duration = difftime(new_data$Lpep_dropoff_datetime,
                                    new_data$lpep_pickup_datetime, units = "hours")
  
  new_data$Tip_percent = 0
  
  # Calculate for other cases
  new_data$Tip_percent[new_data$Total_amount > 0] = 
    (new_data$Tip_amount[new_data$Total_amount > 0] / 
       new_data$Total_amount[new_data$Total_amount > 0]) * 100
  
  new_data = dplyr::select(new_data, -VendorID, - lpep_pickup_datetime, 
                           - Lpep_dropoff_datetime, - Store_and_fwd_flag, - Tip_amount,
                           - MTA_tax, - Extra,
                           - Tolls_amount, - improvement_surcharge, - Total_amount)
  
  return(new_data)
  
}

set.seed(11)
split_index = createDataPartition(gtaxi_data$Tip_percent, p = 0.8, list = FALSE, times = 1)

gtaxi_train = gtaxi_data[split_index, ]
gtaxi_test = gtaxi_data[-split_index, ]

gtaxi_train = dplyr::select(gtaxi_train, -VendorID, - lpep_pickup_datetime, 
                            - Lpep_dropoff_datetime, - Store_and_fwd_flag, - Tip_amount,
                            - MTA_tax, - Extra,
                            - Tolls_amount, - improvement_surcharge, - Total_amount)

gtaxi_test = dplyr::select(gtaxi_test, -VendorID, - lpep_pickup_datetime, 
                           - Lpep_dropoff_datetime, - Store_and_fwd_flag, - Tip_amount,
                           - MTA_tax, - Extra,
                           - Tolls_amount, - improvement_surcharge, - Total_amount)



# We will build a simple regression tree for this problem. 
# It does a good job of segregating the set on variable boundaries

# anova method is used for continuous predictions
tree_model = rpart(Tip_percent ~ . , data = gtaxi_train, method = "anova")

predict_test = predict(tree_model, newdata = gtaxi_test)
predictions_table = data.table(actual = gtaxi_test$Tip_percent, predicted = predict_test)

err = rmse(predictions_table$actual, predictions_table$predicted)
# This error term can only be evaluated relatively. We will need more samples and compute this
# error again to get a sense of what is the right amount.


# -------------------------------------- Distributions ----------------------------------------

gtaxi_data$avg_speed = 0

#Calculating the speed in miles per hour for ride durations > 0
gtaxi_data$avg_speed[gtaxi_data$ride_duration > 0] =
  gtaxi_data$Trip_distance[gtaxi_data$ride_duration > 0] /                                                      as.numeric(gtaxi_data$ride_duration[gtaxi_data$ride_duration > 0])

# To evaluate whether the speeds are the same in all the weeks of september,
# we will perform a one way anova test.
# Encoding a week variable -  we will add 1 extra day to the 3rd and 4th weeks
gtaxi_data$week = 0
gtaxi_data$week[gtaxi_data$month_date < 31] = 4
gtaxi_data$week[gtaxi_data$month_date < 23] = 3
gtaxi_data$week[gtaxi_data$month_date < 15] = 2
gtaxi_data$week[gtaxi_data$month_date < 8] = 1

# We can first explore difference in the 4 weeks by juts looking at a simple box plot
boxplot(gtaxi_data$avg_speed ~ as.factor(gtaxi_data$week), 
        xlab = "Week", ylab = "Average Speed")
title(main = "Speed Boxplot of the 4  weeks")

# The box plot is of no use because of the extreme outliers.

# We can perform a sanity check here - the average speeds in and around new york 
# would rarely exceed 100 mph. Checking to see how many such cases are there:
sum(gtaxi_data$avg_speed > 100) # 2919
sum(gtaxi_data$avg_speed > 150) # 2543

# This info might just indicate that the speed was very high but not as high as captured.
# I will remove these from the data for the hypothesis test so that they don't skew the results
gtaxi_data1 = gtaxi_data[gtaxi_data$avg_speed < 100, ]

# Plotting the box plot again
boxplot(gtaxi_data1$avg_speed ~ as.factor(gtaxi_data1$week), 
        xlab = "Week", ylab = "Average Speed")
title(main = "Speed Boxplot of the 4  weeks")

# From the boxplot, there appears to be very little to distinguish between the 4 weeks

# To validate this, we will perform the ANOVA test
anova_model = aov(gtaxi_data1$avg_speed ~ as.factor(gtaxi_data1$week))
summary(anova_model)

# The ANOVA model suggests that the means of the groups are different for sure.
# The F-value is very huge and hence the p-value is very small

# To check which two pairs are different, we can compute Tukey's pairwise comparison test.
pair_comp = TukeyHSD(anova_model, which = 'as.factor(gtaxi_data1$week)', conf.level = 0.95)
pair_comp

# From this test, we can see that all the weeks are statistically different from each other.


############# Making a hypothesis of average trip speed as a function of time of day

# First, we will calculate the correlation between these two fields
# We are going to take the hour as the proxy for time of day
cor(gtaxi_data1$avg_speed, gtaxi_data1$pickup_hour)
# Not a very high correlation but it is a negative one.

# To look at the avg_speed as a function of pickup_hour, we will run a simple regression

lin_model = lm(avg_speed ~ pickup_hour, data = gtaxi_data1)
summary(lin_model)
# The summary of the model suggests that the regression model is highly significant.

print(lin_model$coefficients)




# XGBOOST OPEX

#  Quickly also training an xgboost
remaining_dummies2 = as.data.frame(dummy(model_data2[ , c(2)]))
remaining_dummies3 = as.data.frame(dummy(model_data2[ , c(3)]))
remaining_dummies4 = as.data.frame(dummy(model_data2[ , c(4)]))
remaining_dummies5 = as.data.frame(dummy(model_data2[ , c(5)]))
remaining_dummies7 = as.data.frame(dummy(model_data2[ , c(7)]))

all_vars = cbind(model_data2, remaining_dummies2, remaining_dummies3, remaining_dummies4, 
                 remaining_dummies5, remaining_dummies7)

all_vars = all_vars[ , c(1,6,8:51)]

test1 = ddply(all_vars, "material", tail, 5)

train1 = dplyr::setdiff(all_vars, test1)

y = train1[, 18]

xgb <- xgboost(data = data.matrix(train1[,c(2:17, 19:46)]), 
               label = y, 
               eta = 0.1,
               max_depth = 15, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "merror",
               objective = "binary:logistic",
               num_class = 2,
               nthread = 3
)

xgb <- xgboost(data = data.matrix(train1[,c(2:17, 19:46)]), label = y,
               eta = 0.1,
               max_depth = 15, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1)

predict_xgb = predict(xgb, data.matrix(test1[,c(2:17, 19:46)]))

table(test1$stockout, predict_xgb > 0.9)

