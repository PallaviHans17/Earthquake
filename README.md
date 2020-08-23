# Visual Analysis of Earthquake

The goal of this project is to create an interactive web application. The application helps visualize all available catalog data sets in a complex way on a single map, displaying the long-term recurrence times of earthquakes and further, the earthquake information and the population data are used to identify the risk zones. The project describes in detail, how data visualization can be done using R Shiny and how it can be extended to demonstrate the result of the applied statistical models on the earthquake data. The application provides a user interface and server logic. Both are written in programming language R with help of its framework Shiny. All the modelling to identify the risk zones based on high population count and high predicted energy released is done using R. Furthermore, earthquake data visualization will turn out to be facilitative for the earthquake data analysis for either modelling or prediction. Here, in this study we are trying to represent the data through an interactive visualization tool providing some useful statistical information which would help to assess the geographical areas with highest risk and at which time of the year. This application serves the purpose of powerful and informative visualization of earthquakes and can be extended to be used in the study of other scientific purposes.

# DATA ACQUISITION

The dataset used for the study is publicly available at the United States Geological Survey (USGS)[1]. The dataset contains information of earthquakes with magnitude within the range of 5.0 to 9.1 that took place around the globe from October 31, 2010 to November 7, 2016. It has 11,023 occurred earthquakes with 22 parameters containing occurred earthquake details. The dataset contains the following parameters: time, latitude, longitude,  depth, magnitude, magnitudeType, NST, Gap, Dmin, RMS, NET, ID, Updated, Place, Type, Horizontal Error, DepthError, MagError, MagNst, Status, LocationSource, and MagSource. Along with the USGS data, population density data from NASA[3] were also recorded for the year 2020

# DATA WRANGLING

Some of parameters contained a lot of missing information which were removed from the dataset before conducting any exploratory analysis. These included: NST, Gap, Dmin, Rms, Horizontal Error, Depth Error, MagError, MagNST.
We also saw a few challenges in the time variable of the dataset before proceeding with the exploratory data analysis since the time variable is differently formatted so, we extract the date (i.e., complete date, month and year individually) using the as.POSIXlt function in R. Further, we used the “dplyr” library to group the dataset by Year, month, and Magnitude Type.

# DATA MANIPULATION

The USGS dataset was modified as per the requirement for the app. The data was loaded as “dataset” in R. Using the as.POSIXlt[6] function in R on the “time” column of the dataset, “date”, “year” and “month” were extracted to provide user with the date range option through the shiny app. Further, the dataset had information about the place for each of the earthquake which was used to extract the country from the “place” column using the sub string function from R and was stored in a new column named “country”:

dataset$country <-  sub('.*,\\s*', '', dataset$place)

Further, using the “countrycode” library of R to identify respective continents for each of the country. However, the library was unable to identify the continents for the places of earthquakes that had hypocentre in the sea hence, they were categorized as “others”. Finally, the continents were categorized as “Americas”, “Oceania”, “Asia”, “Europe”, “Africa” and “Others” and each of the earthquakes was categorized in one of the mentioned continents. The intent was to provide the app user with the ability to select earthquakes based on geographic region. 

library(countrycode)
dataset$continent <- countrycode(sourcevar = dataset[, "country"], origin = "country.name", destination = "continent")
dataset$continent[is.na(dataset$continent)] <- "Other"
 
Further, we have rescaled the values of magnitude and depth using a log transformation which will help us to visualize the different values better on the map in the app.

Then, we have designed the app for which the code is available on app.R

# MODELLING AND ANALYSIS

# WHAT IS THE PURPOSE?

The earthquake creates havoc when occurs in populated places, therefore it becomes necessary to conduct some research analysis of the earthquake. A lot of work has been done in the prediction and assessment of the earthquake disaster risk but here, the intent is to build a simple risk model which estimates the risk value of population in earthquake disaster and helps in assessing some high risk areas in the world which are prone to more lives lost.

# POPULATION RISK MODEL
# CONSIDERED FACTORS 

The development of the earthquake disaster is influenced by amount of energy it releases during the event. It is known that the magnitude is the logarithmic measure of the size of an earthquake at its source which implies that it can be used as an approximate measure to calculate the amount of energy released. Given 11,023 earthquake records in the dataset and the magnitude measure is available for each of the records, so we can calculate the estimated energy released by earthquake using:
E_i = exp{M_i}
where i corresponds to the ith earthquake out of the 11,023 earthquakes in the dataset. This gives us the estimated energy released for each of the earthquake in the dataset

# PROCESS OF FORMULATING THE RISK MODEL

Given an estimation of the energy released by each earthquake, firstly, we propose the idea of having a model which could predict the energy released if provided with limited information about the earthquake which includes the location and the time of the earthquake. The aim of the model is to predict the energy released by the model given limited information about the earthquake.
Firstly, we must understand the relationship between the location and time of an earthquake and the amount of energy it releases. To do this, we introduce a new quantity X_i for each earthquake which corresponds to the total energy that has been released in a radius D around the location of earthquake i, in the previous T days. Now, since the USGS dataset provides us with the time variable which includes the date and time at which the earthquake happened we can count for the value of T but we do not have any value for D in the dataset. However, it provides us with the latitude and longitude for each of the earthquake which could be used to calculate the Euclidean distance between any two location points. 
Since we know that the Euclidean distance or Euclidean metric is the "ordinary" straight-line distance between two points in Euclidean space however, here we are dealing with latitude and longitude and earth is not a flat surface so, we will use the spherical trigonometry to determine the Euclidean distance between two latitude longitude values. To calculate the value of D from the latitude and longitude the following function was used:

earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

Here, we have considered the radius as ~6378 Km (which is the longest at equator). Then, we converted the latitude and longitude values to radians. Further, we have used the Haversine Formula[12] which helps in determining the shortest distance between the two spheres to calculate the distance since it is more accurate with the problems associated with small distances and our dataset records numerous earthquakes within few kilometers for some locations. This function returns the Euclidean distance in kilometers.
However, the dataset has 11,023 earthquake records and it is computationally infeasible to calculate distance for each earthquake record with all the other earthquakes in the dataset. Also, since we have defined X_i as the total energy released in radius D in previous T days but here, we don’t consider the total energy released as the average of the total energy released instead we will assign a higher weight to the energy released by the earthquake which happened on (T-1)th day as compared energy released by the earthquake (T-2)th day. In statistics, we call this technique as Exponential Smoothing[13] which is used to smooth the time series data using the exponential window function. Here, we assign weight T to the energy released on the day before of the recorded earthquake, (T-1) weight to the earthquake which happened two days before the recorded earthquake while weight 1 to the earthquake which was recorded T days before the earthquake.


# OVERALL MODEL

Since the relationship between Energy released and X is estimated to be linear, therefore we use a simple linear model to predict the energy released. 
E_i = b_0 + b_1X_i + epsilon_i, 
where E_i is the predicted energy and epsilon is a Gaussian error with mean zero and variance sigma^2.
The model is the best fit for some optimal values of T and D and the optimal values of T and D can be determined using cross validation technique. To find the optimal values of the T and D, we use the prediction error method i.e., for each of the earthquakes in a test set, we use linear model to predict its magnitude using the corresponding covariate X, and recorded the prediction error. We split the dataset in 80:20 sample (training:test) and then, build the model on the 80% sample and then use the model thus built to predict the dependent variable on test data. Thus, we get the model predicted values for the 20% data (test) as well as the actuals (from the original dataset). 
Here, our criteria for the selection of best model is the MinMax accuracy[14] which is calculated as the mean of the ratio of minimum between actuals and predicted and the maximum between the actual and predicted. By calculating MinMax accuracy[14] measures which considers the average between the minimum and the maximum prediction, we found out the prediction accuracy of the model. 
Since the USGS dataset has 11,023 earthquake records and to check the model for different values of T and D, firstly, we consider a subset of 1000 records and run our model for the arbitrary values of T and D and the values which perform the best on the model will be selected to check on the complete dataset with 11,023 records.

For the best and the lowest values of T and D which get the highest accuracy, we check our model for these three T and D values on the complete dataset with 11,023 records:

Using the MinMax accuracy measure, the optimal value for T is 5 days and D is 7000 Km since it has the highest score of 92.79%. Provided with the optimal values of T and D we estimate the value of X and then runs a linear model and we get the following values of b_0, b_1. b_0 turns to be 17.41 while the b_1 turns out to be 0.9575. 

# ANALYSIS AND RESULTS
To conduct the analysis of the population risk model, the population data, which was produced by NASA based on data of 2020 global population is used in the form of gird using raster function of R.

The population grid reflects number of persons per square kilometer and as cited on NASA website they were produced as global rasters at 30 arc-second (~1 km at the equator) resolution. This plot helps us visualize the estimates of the population density and its availability as raster data will help us facilitate data integration with the Predicted Energy Calculated by the model.
The linear model provides us the Predicted energy release based on the information of the location (longitude and latitude) and the time of when the earthquake happens. With the help of these information, we can use the linear model to predict the energy released by the earthquake. To demonstrate, we have considered fictious earthquakes happening on the time same as records provided in the USGS dataset and using the linear model we predict the Energy Released by them and again using the rasterize function in R we plot the grid of the predicted energy released as:
 


However, the aim of the model is to identify the risk locations which have high predicted energy released and high population count so firstly, it is necessary to view the population data and predicted energy released on the same grid which is achieved my merging the raster form of population and predicted energy data in R using the merge function. The combined plot of both the Population Data and the Predicted Energy Released looks like:
 


Most of the energy is released in the oceans and there is no population around it however, there are some geographical regions in the grid which register energy release in the populated areas. This provides us the possibility of assessing some geographical locations with high population count and high predicted energy released. To start with this, we find locations which have released predicted energy greater than 500 and used the app to help visualize the same. The locations with high predicted energy release are highlighted in the map on the app.
 

When we assess the data, all the regions with high predicted energy released are not highly populated but since the aim of the model is to identify the locations with high energy release and high population count so, from the extracted data above we identify the locations which have high energy release and high population count. To be specific, we have identified the 15 locations which have high predicted energy release greater than 1,500 while the population count was greater than 20,000. The assessed risk zones are again visualized through the app.
 

Mostly, the earthquakes with high predicted energy release have happened over the same geographic region with high population count since we can see that in the map the circles overlap with each other indicating that these locations are at the highest risk since the frequency of earthquake occurrence with high energy released in densely populated areas is the highest for these locations.


