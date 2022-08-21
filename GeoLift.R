#------1. R environment setup------
#in this section we set up the R environment, install and load the required packages

#1.1 check your R version: 4.0.0 or newer
R.Version()

#1.2 install devtools and ausgynth and GeoLift
# install.packages("devtools", repos='http://cran.us.r-project.org')
# devtools::install_github("ebenmichael/augsynth")
# devtools::install_github("facebookincubator/GeoLift")

#1.2.1 install other packages
# install.packages(c("tidyverse", "ggplot2", "plotly", "readr","lubridate", "googlesheets4", "knitr", "gsynth","dplyr","doParallel", "MarketMatching"))

#1.3 load all the packages needed
library(tidyverse)
library(ggplot2)
library(plotly)
library(readr)
library(lubridate)
library(googlesheets4)
library(knitr)
library(GeoLift)
library(augsynth)
library(gsynth)
library(GeoLift)
library(dplyr)
library(doParallel)
library(foreach)
library(MarketMatching)


#------2. pre-test power analysis------

# in this section we will:
#   1. define how many markets we need to include in the test group 
#   2. select the markets for the test group
#   3. select the MDE of the test
#   4. estimate the best duration of the test 

#2.1 import data from your google sheet
# GeoLift_PreTest<- read_sheet('https://docs.google.com/spreadsheets/d/1z2vWCbrKrrGTHxnknMSDjk5tdopkukv_dMUNaaoWqEc/edit?usp=sharing')
GeoLift_PreTest <- read.csv(file.choose())

#2.2 read the data into the proper data.frame format needed 
GeoTestData_PreTest <- GeoDataRead(data = GeoLift_PreTest,
                                   date_id = "date",
                                   location_id = "geo",
                                   Y_id = "signups",
                                   format = "yyyy-mm-dd")

#2.3 plot the data to observe the trend and detect anomalies
GeoPlot(GeoTestData_PreTest,
        Y_id = "Y",
        time_id = "time",
        location_id = "location")


#2.4 Power Analysis

#2.5 select the ideal number of geos to include in the test group
resultsNum <- NumberLocations(data = GeoTestData_PreTest,
                              Y_id = "Y",
                              location_id = "location",
                              time_id = "time",
                              n_sim = 500,
                              treatment_periods = 15,# test lenght will be 15 days, if you are not sure enter a list of possible values
                              plot = TRUE,
                              power = 0.8,
                              alpha = 0.1,
                              fixed_effects = TRUE,
                              ProgressBar = TRUE)

#2.6 Market Selection: define which markets will be part of the test group
resultsSearch <- GeoLiftPower.search(data = GeoTestData_PreTest,
                                     treatment_periods = c(15),
                                     N = c(2,3,4), #this to be set based on the output of 2.5
                                     #horizon = 50, #defines at which time-stamp the power simulations will start, use ??GeoLiftPower.search for more details 
                                     Y_id = "Y",
                                     location_id = "location",
                                     time_id = "time",
                                     top_results = 20,
                                     alpha = 0.1,
                                     type = "pValue",
                                     fixed_effects = TRUE,
                                     ProgressBar = TRUE
                                     #,parallel = FALSE for windows users: set this parameter (removing the #) if the function does not work
)

#top 50 results
head(resultsSearch,50) 

#2.7Minimum Detectable Effect
resultsFind <- GeoLiftPowerFinder(data = GeoTestData_PreTest,
                                  treatment_periods = c(10,15), #two possible test lenghts
                                  N = c(2,3,4),
                                  Y_id = "Y",
                                  location_id = "location",
                                  time_id = "time",
                                  effect_size = seq(0, 0.5, 0.05), #defines the step of possible MDEs
                                  top_results = 5,
                                  alpha = 0.1,
                                  fixed_effects = TRUE,
                                  ProgressBar = TRUE,
                                  plot_best = TRUE
                                  #,parallel = FALSE for windows users: set this parameter (removing the #) if the function does not work
)

#MDE list of possible results 
head(resultsFind,10)

#2.8 find the duration and budget 
locs <- c("chicago", "portland")

resultsPow <- GeoLiftPower(GeoTestData_PreTest,
                           locations = locs,
                           effect_size = seq(0,0.25,0.01),
                           treatment_periods = c(10),
                           #horizon = 50,
                           Y_id = "Y",
                           location_id = "location",
                           time_id = "time",
                           cpic = 7.50) #Cost Per Incremental Conversion. This CPIC will allow us to estimate the budget needed to achieve the required lift for a well-powered test.

#plot the power curves
plot(resultsPow, actual_values = TRUE)


#------3. test results analysis ----

# in this section we will:
#   1. analyze the results of the test 
#   2. explore the test results


#3.1 load the test data

# GeoLift_PostTest<- read_sheet('https://docs.google.com/spreadsheets/d/1tLumoq7oJ0-Jb7MoEl8TM8d9wmq3R4OKBbVDgajeAZ8/edit?usp=sharing')

GeoLift_PostTest <- read.csv(file.choose())

#3.2 read the data into the proper data.frame format needed 
GeoTestData_Test <- GeoDataRead(data = GeoLift_PostTest,
                                date_id = "date",
                                location_id = "geo",
                                Y_id = "signups",
                                format = "yyyy-mm-dd")
head(GeoTestData_Test)

#visualize the time-series

GeoPlot(GeoTestData_Test,
        Y_id = "Y",
        time_id = "time",
        location_id = "location",
        treatment_start = 91)

#3.3 GeoLift Inference

GeoTest <- GeoLift(Y_id = "Y",
                   data = GeoTestData_Test,
                   locations = locs, #test locations
                   treatment_start_time = 91, 
                   treatment_end_time = 105)

#3.4 Results exploration

#summary of the results
summary(GeoTest)

#plot  to see how well the Synthetic Control Method fitted the data
plot(GeoTest, type = "Lift")

#plot the ATT ( Average Estimated Treatment Effect)
plot(GeoTest, type = "ATT")
