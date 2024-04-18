# This project is a data science project that predicts car prices in Qatar and recommends the best deals when presented with car that are put for sale.
# This is the R code file
#
###################################################################
## Data Wrangling
###################################################################
# In this section, we load and install packages we need.
# We also load a csv file from the data that was web scrapped.
# We then clean and filter the data.
###################################################################

########################################
### Import Packages
########################################
# Install and load the required packages and libraries.
if (!require(magrittr)) install.packages("magrittr") 
if (!require(dplyr)) install.packages("dplyr")
library(magrittr)
library(dplyr)
library(caret)
library(stringr)
library(scales)
########################################
### Data Importing
########################################
# Read the csv file from relative path 
data <- read.csv("input04Apr202410pm.csv")
nrow(data)
########################################
### Data Cleaning
########################################
# lets first understand what columns we have
head(data)
str(data)
# we have 16 columns listed above, some of which needs to be formatted and cleaned

# mileage column includes "," and "Km", so lets remove "Km" and comma 
# lets also convert it to numeric
data$mileage <- (gsub(" Km", "", data$mileage))
data$mileage <- (gsub(",", "", data$mileage))
data$mileage <- as.numeric(data$mileage)
# price column includes ",",lets remove "," and concert to numeric 
data$price <- (gsub(",", "", data$price))
data$price <- as.numeric(data$price)
# convert cylinder to a number 
data$cylinder <- as.integer(data$cylinder)

# Time column includes either sold date or last posted time 
# lets split it to date_sold and posted_time columns
data$date_sold <- NA
# Check if time column has "/" in it and transform the value to a date
data$date_sold <- ifelse(grepl("/", data$time), format(as.POSIXct(data$time, format = "%d/%m/%Y"), "%d-%b-%Y"), "")
# Create a new column called posted_time
data$posted_time <- NA
# Extract the number of hours or minutes from the time column and convert to hours and decimals
data$posted_time <- ifelse(grepl("hour", data$time), as.numeric(gsub(" hours ago", "", data$time)), "")
data$posted_time <- ifelse(grepl("minute", data$time), as.numeric(gsub(" minutes ago", "", data$time))/60, data$posted_time)
# With regards to the mileage column, later When we split data for training, testing
# and validation we don't want to remove records if they it existing in train and 
# don't exists in test or validate dataset, hence we have create tiers every 10,000
# to reduce chance of committing the record.
data <- data %>%
  mutate(mileage_range = case_when(
    mileage == 0 ~ 0,
    mileage > 0 ~ ceiling(mileage / 10000) * 10000
  ))
str(data)
########################################
### Data Filtering
########################################
# Price Filter ----
# Number of car on y axis are plotted against their price value on x axis, as you can see the listing drops significantly after around 500,000 QAR
# The number 
data %>%
  filter(price > 0) %>%  
  ggplot(aes(x = price)) +  
  geom_histogram(bins = 40, color = "black", fill = "#003CFF") +  
  labs(title = "Price Distribution - All", x = "Price", y = "Count") +
  scale_x_continuous(labels = comma_format())+
theme(
plot.title = element_text(hjust = 0.5), # center title
panel.grid.major = element_line(color = "gray", linetype = "dashed"), # add
#dashed lines
panel.grid.minor = element_blank() # Removing minor grid lines
)
# I have filtered the price less than 365,000. I am more familiar with car of prices with this range.
data %>%
  filter(price < 365000) %>%  
  ggplot(aes(x = price)) +  
  geom_histogram(bins = 40, color = "black", fill = "#003CFF") +  
  labs(title = "Price Distribution - Price < 365,000 QAR", x = "Price", y = "Count") +
  scale_x_continuous(labels = comma_format())+
theme(
plot.title = element_text(hjust = 0.5), # center title
panel.grid.major = element_line(color = "gray", linetype = "dashed"), # add
#dashed lines
panel.grid.minor = element_blank() # Removing minor grid lines
)
# After filtering the number of records have reduced as per below.
data <- data[data$price < 365000, ]
nrow(data)
# Year Filter ----
# As you can see below if the number of car listing of older cars is much less,
# so i have limited it to the last 10 years.
top_years <- head(sort(table(data$year), decreasing = TRUE), 20)
top_years_table <- data.frame(Make = names(top_years),Count = as.numeric(top_years))
top_years_table
# After filtering the number of records are as per below
data <- data[data$year > 2013, ]
nrow(data)
# Price Null Filter ----
# we checked that the price has 7 nulls, so lets remove it
summary(data$price)
data <- data[!is.na(data$price) & data$price != "", ]
nrow(data)
# Checking Data ----
# Now we are checking that all data is tidy and filtered
str(data)


###################################################################
## Data Visualization
###################################################################
# In this section, we explore the different dataset parameters and visualization.
###################################################################

# Before we start we just want to recap on some of the important columns.

# Make column is like "Mercedes", "Audi", etc.
# Model column is like "200", "A3", etc, which is a child of Make,
# Trim  column can vary like "standard", etc. which is also a child of model.
# cylinder column has how many cylinder the car has
# gear_type has is either automatic, manual, tiptronic, etc.

# This pie chart shows the number of car listings per make
# As you can see Toyota is the most popular make or brand in Qatar making up more than 1/3.
# In the next few charts we will focus on Toyota and Nissan model distribution over years
# Note that we have filtered out any make that is less than 3% that's why the Pie shows 37%  
# as larger than it should.

car_counts <- table(data$make)
car_counts <- car_counts[order(car_counts, decreasing = TRUE)]
percent <- car_counts/sum(car_counts)*100
car_counts <- car_counts[percent > 3]
pie(car_counts, labels = paste0(names(car_counts), " (", round(percent[percent > 1]), "%)"), main = "Car Listings per Make", col = rainbow(length(car_counts)))


# The below function returns the make based on the rank number
# this will be used in the next few charts 

get_make_rank <- function(rank_number) {
    car_counts <- table(data$make)
    car_counts <- car_counts[order(car_counts, decreasing = TRUE)]
    make_rank <- names(car_counts)[rank_number]
    return(make_rank)
}

# This chart shows Toyota models distribution over years
# The Y axis is the Toyota models and x axis is the years, the numbers
# in each block indicate the number of car listings.
# As you can see the most popular listing is Land cruisers model for 2023
make_rank_1 <- get_make_rank(1)  # Returns "Toyota"
data %>%
    filter(make == make_rank_1) %>%
    group_by(make, model, year) %>%
    summarise(count = n()) %>%
    ggplot(aes(x = year, y = model, fill = count)) +
    geom_tile() +
    labs(title = paste("Number of Listings for", make_rank_1, "and its Models"), x = "Year", y = "Model") +
    theme(
        plot.title = element_text(hjust = 0.5), # center title
        panel.grid.major = element_line(color = "gray", linetype = "dashed"), # add dashed lines
        panel.grid.minor = element_blank() # Removing minor grid lines
    ) +
    geom_text(aes(label = count), color = "white", size = 3, position = position_dodge(width = 1)) +
    scale_fill_gradient(low = "blue", high = "red") # Add color gradient


# Since Land Cruiser model is the most popular in Toyota,
# The Y axis is the Toyota- Land Cruiser trims and x axis is the years, the numbers
# in each block indicate the number of car listings.
# The below chart shows that GXR Twin Turbo is the most popular trim  in 2023.
    make_rank_1 <- get_make_rank(1)  # Returns "Toyota"
    data %>% 
        filter(make == make_rank_1 & model == "Land Cruiser") %>% 
        group_by(make, model, year, trim) %>% 
        summarise(count = n()) %>% 
        ggplot(aes(x = year, y = trim, fill = count)) + 
        geom_tile() + 
        labs(title = paste("Number of Listings for", make_rank_1, "Land Cruiser Trims"), x = "Year", y = "Trim") + 
        theme(
            plot.title = element_text(hjust = 0.5), # center title
            panel.grid.major = element_line(color = "gray", linetype = "dashed"), # add dashed lines
            panel.grid.minor = element_blank() # Removing minor grid lines
        ) + 
        geom_text(aes(label = count), color = "white", size = 3, position = position_dodge(width = 1)) + 
        scale_fill_gradient(low = "blue", high = "red") # Add color gradient



# This chart shows Nissan models distribution over years
# The Y axis is the Nissan models and x Axis is the years, the numbers
# in each block indicate the number of car listings.
# As you can see the most popular listing is Patrol model for 2023
make_rank_2 <- get_make_rank(2)  
data %>%
    filter(make == make_rank_2) %>%
    group_by(make, model, year) %>%
    summarise(count = n()) %>%
    ggplot(aes(x = year, y = model, fill = count)) +
    geom_tile() +
    labs(title = paste("Number of Listings for", make_rank_2, "and its Models"), x = "Year", y = "Model") +
    theme(
        plot.title = element_text(hjust = 0.5), # center title
        panel.grid.major = element_line(color = "gray", linetype = "dashed"), # add dashed lines
        panel.grid.minor = element_blank() # Removing minor grid lines
    ) +
    geom_text(aes(label = count), color = "white", size = 3, position = position_dodge(width = 1)) +
    scale_fill_gradient(low = "blue", high = "red") # Add color gradient



# The below chart to correlate years and mileage, for example median of 2014 is around 175,000 which
# means around car usually run for 17,500 km average per year.
data %>%
    filter(mileage <= 650000) %>% # there was 1 anomoly manuall removed.
    ggplot(aes(x = year, y = mileage)) +
    geom_point() +
    labs(title = "Correlation between Years and Mileage", x = "Year", y = "Mileage") +
    theme(
        plot.title = element_text(hjust = 0.5), # center title
        panel.grid.major = element_line(color = "gray", linetype = "dashed"), # add dashed lines
        panel.grid.minor = element_blank() # Removing minor grid lines
    ) +
    geom_boxplot(aes(group=year), fill = rainbow(length(unique(data$year))), alpha = 0.2) + # Add boxplot to show distribution of data
    scale_y_continuous(breaks = seq(0, max(data$mileage), 50000), labels = comma_format()) # Set y-axis breaks to every 50,000 km and add comma format to labels


# This plot shows the relationship between the car ages or years and their average price.
# The line was created utilizing the LOESS (Locally Estimated Scatterplot Smoothing) method.
#This provides an approximate relationship between a car's age and the mean price.
# The older the car the lower the price in this timeframe.

# Calculate car age based on the year column
data_v <- data %>%
    mutate(car_age = 2024 - year)

mean_price_age <- data_v %>%
    group_by(car_age) %>%
    summarise(mean_price = mean(price, na.rm = TRUE))

# plotting mean_price_age
ggplot(mean_price_age, aes(x = car_age, y = mean_price)) +
        #geom_point(color = "#c4553a", size = 3) + # setting color and size of points
        geom_smooth(method = "loess", color = "#6952b9", method.args = list(span = 0.15, degree = 1)) +
        labs(x = "Car Age (Years)", y = "Mean Price", title = "Mean Price by Car Age") +
        theme_minimal() +
        theme(
                plot.title = element_text(hjust = 0.5), # set title
                axis.text = element_text(size = 10), # set axis text size
                axis.title = element_text(size = 12), # set axis title size
                panel.grid.major = element_line(color = "gray", linetype = "dashed"), # add dashed grid lines
                panel.grid.minor = element_blank() # removing minor grid lines
        )+ # set custom fill colors
    scale_y_continuous(labels = comma_format()) 

   
# The below charts shows the top 10 make-models are the most liked, as this 
# site allows users to like a listing.

# Get the top 10 models based on the likes column
top_models <- head(data %>% group_by(make, model) %>% summarise(total_likes = sum(likes)) %>% arrange(desc(total_likes)), 10)

# Create a table with the top 10 models, their make, total likes, and number of likes
top_models_table <- data.frame(Make_Model = paste(top_models$make, top_models$model, sep = "-"), Total_Likes = top_models$total_likes, Number_of_Likes = top_models$total_likes)

# Create a bar chart to visualize the total likes and number of likes for the top 10 models
ggplot(top_models_table, aes(x = reorder(Make_Model, -Total_Likes), y = Total_Likes, fill = Make_Model)) +
    geom_bar(stat = "identity") +
    labs(title = "Top 10 Models with the Most Likes", x = "Make and Model", y = "Total Likes") +
    theme(
        plot.title = element_text(hjust = 0.5), # center title
        axis.text.x = element_text(angle = 45, hjust = 1), # rotate x-axis labels
        legend.position = "none" # remove legend
    ) +
    scale_fill_manual(values = rainbow(nrow(top_models_table))) + # set custom fill colors
    scale_y_continuous(labels = comma_format()) + # format y-axis labels with comma every 3 digits
    geom_text(aes(label = Number_of_Likes), vjust = -0.5) # add labels for number of likes above each bar



###################################################################
## Feature Engineering
###################################################################
# In this section, we create new columns and tables as new features that will be
# used to train our models and will be also used to predict the price outcome.
###################################################################


# The below columns were created/updated based on a feedback loop during training
# to improve model accuracy.

# Make Update 
# We are updating model column to be linked its parent column "Make", this will be 
# used with the model bias effect
data$model <- paste(data$make, data$model, sep = "-")
head(data$model)

# Trim Update 
# We are updating "trim" to be linked to its parent column "model". 
# We also have included "cylinder" and "gear_type" 
# this will be used with the "trim" bias effect
data$trim <- paste(data$make,  data$model, data$trim, data$cylinder, data$gear_type,sep = "-")
head(data$trim)

# Create 3 Columns Average Year Price Mileage 
# We are creating new columns by calculating per trim the average "year" and "price",
# this will be used for the price prediction
data <- data %>% group_by(trim) %>% mutate(avg_ymmt = mean(year))
data <- data %>% group_by(trim) %>% mutate(price_pmmt = mean(price))
# We are creating new columns by calculating per year the average mileage and price
# this will be used for the price prediction
data <- data %>% group_by(year) %>% mutate(avg_mileage = mean(mileage)+1) # to avoid division by zero
data <- data %>% group_by(year) %>% mutate(price_pmmty = mean(price)) 
head(data %>% select(make, model, year, price,avg_ymmt,price_pmmt,avg_mileage,price_pmmty))

# Create Table Average Year per trim 
# We are creating separate table for average "year" per "trim".
# this will be used for the price prediction
data_avg_ymmt <- data %>%
    group_by(trim) %>%
    mutate(avg_ymmt = mean(year))%>%
    select(trim, avg_ymmt) %>%
    unique()
head(data_avg_ymmt)

# Year Update 
# We are updating "year" column to include "trim"
# This will be used with the year bias effect 
data$year <- paste(data$trim, data$year, sep = "-")
head(data$year)

# Create column Average Mileage per Year 
# We are creating a new column by calculating per "year" the average "mileage_range"
data <- data %>% group_by(year) %>% mutate(avg_mileage_range = mean(mileage_range))
head(data %>% select(make, model, year,avg_mileage_range))

# Create table Average Mileage per Year 
# We are creating separate table for "average_mileage_range" per "year".
# this will be used for the price prediction
data_avg_mileage_range <- data %>% group_by(year) %>%
    mutate(avg_mileage_range = mean(mileage_range))%>%
    select(year, avg_mileage_range) %>%
    unique()
head(data_avg_mileage_range)

# Mileage_range update ---
# We are updating mileage_range with trim, as this combination will be needed later
# for the mileage effect (linear regression - bias) 
data$mileage_range <- paste( data$trim, data$mileage_range, sep = "-")
head(data %>% select(make, model, year,mileage_range))

# Create Table Average price per trim
# We are creating separate table for average "price" per "trim".
# this will be used for the price prediction
data_price_pmmt <- data %>%
    group_by(trim) %>%
    mutate(price_pmmt = mean(price))  %>%
    select(trim, price_pmmt) %>%
    unique()
head(data_price_pmmt)

# Create Table Average price per year
# We are creating separate table for average "price" per "year".
# this will be used for the price prediction
data_price_pmmty  <- data %>%
    group_by(year) %>%
    mutate(price_pmmty = mean(price)) %>%
    select(year, price_pmmty) %>%
    unique()
head(data_price_pmmty)

# Create Table Average Mileage per year
# We are creating separate table for average "mileage" per "year".
# this will be used for the price prediction
data_avg_mileage <- data %>%
    group_by(year) %>%
    mutate(avg_mileage = mean(mileage)+1)%>%
    select(year, avg_mileage) %>%
    unique()
head(data_avg_mileage)

###################################################################
## Data splitting
###################################################################
# In this section, we split our data into train_set, test_set and validate_set
# for training, testing and validation respectively.
# We have split training to be 90 % of data (train_test),
# and the remaining 10 % for validation. (validation_set)
# We have split training data to be 80% training (train_set) and 20% for testing (test_set).
# We have also removed records from test and validation set to be included in train set
# where certain columns are missing in test and validation.
###################################################################

# We have already loaded the caret package to be able split the data use "createDataPartition"
# Set the random seed in order to be able to repeat the results.
set.seed(123)

validate_index <- createDataPartition(y = data$price, times = 1, p = 0.1, list = FALSE)
train_test <- data[-validate_index,]
temp <- data[validate_index,]

validate_set <- temp %>% 
  semi_join(train_test, by = "make") %>%
  semi_join(train_test, by = "model") %>%
  semi_join(train_test, by = "trim") %>%
  semi_join(train_test, by = "year") %>%
  semi_join(train_test, by = "gear_type") %>%
  semi_join(train_test, by = "cylinder") %>%
  semi_join(train_test, by = "mileage_range")%>%
  semi_join(train_test, by = "personal")  # optional
# Add rows removed from validate_set test set back into train_test
removed <- anti_join(temp, validate_set)
train_test <- rbind(train_test, removed)

# Split the train_set data 1 time into 80% for training and 20% for testing.
test_index <- createDataPartition(y = train_test$price, times = 1, p = 0.2, list = FALSE)
train_set <- train_test[-test_index,]
temp <- train_test[test_index,]

test_set <- temp %>% 
  semi_join(train_set, by = "make") %>%
  semi_join(train_set, by = "model") %>%
  semi_join(train_set, by = "trim") %>%
  semi_join(train_set, by = "year") %>%
  semi_join(train_set, by = "gear_type") %>%
  semi_join(train_set, by = "cylinder") %>%
  semi_join(train_set, by = "mileage_range") %>%
  semi_join(train_set, by = "personal")  # optional

# Add rows removed from test_set test set back into train_set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

# remove rows with missing values in the price column 
# show last row in test_set
test_set[nrow(test_set),]
test_set <- test_set[!is.na(test_set$price) & test_set$price != "", ]
#test_set$price
train_set[nrow(train_set),]
train_set <- train_set[!is.na(train_set$price) & train_set$price != "", ]
#train_set$price
validate_set[nrow(validate_set),]
validate_set <- validate_set[!is.na(validate_set$price) & validate_set$price != "", ]
#validate_set$price
nrow(train_set)
nrow(test_set)
nrow(validate_set)

#write.csv(train_set, file = "train_set.csv", row.names = FALSE)
#write.csv(test_set, file = "test_set.csv", row.names = FALSE)
#write.csv(validate_set, file = "validate_set.csv", row.names = FALSE)

###################################################################
## Model Evaluation
###################################################################
# Here we define the function for Root Mean Squared Error used for evaluation model accuracy.
RMSE <- function(true_ratings, predicted_price){
sqrt(mean((true_ratings - predicted_price)^2))
}
###################################################################
## Method - Linear Regression
###################################################################
# My first method used is the Linear regression. I have used the bias effects of Make, 
# Model, Trim where Trim includes Gear_type and Cylinder.
# 
# As explained in the Feature Engineering section, for Make, Model and Trim, they 
# were updated and linked with each other to archive more meaningful bias and higher
# accuracy or lower RMSE.
# 
# I have not simply used bias effect on Year and mileage as above as the accuracy
# was not satisfactory. Instead, I have calculate their own linear regression 
# arriving at with values of a and b for each year and trim as a function a + bx 
# where x is the year of the car. This was particularly useful when we wanted to 
# predict car price for years that data was not trained on.
# 
# Similar to the year effect, I have implemented the mileage effect.
# 
##############################################


########################################
### Overall Mean Effect
########################################
# Overall Mean Effect
# We start by calculating the mean of all the pricing in the train_set.
# We then calculate the RMSE and include it in a table.
mu <- mean(train_set$price)
mu
naive_rmse <- RMSE(train_set$price, mu)
rmse_results <- data_frame(Method_on_train_test = "Overall Average", RMSE = naive_rmse, Improvement = naive_rmse-naive_rmse)
rmse_results %>% 
mutate(RMSE = format(RMSE, nsmall = 2, big.mark = ",")) %>% 
mutate(Improvement = format(Improvement, nsmall = 2, big.mark = ",")) %>% 
knitr::kable()
# The RMSE value 89,000 QAR is roughly 25,000 USD for car values less than 100,000 USD.
# This is based on overall mean.The average price of a car is 160,951 QAR ~ 44k USD.

########################################
### Make Effect
########################################
# We used mu as a baseline and calculated the make bias b_make.
# We then calculate the RMSE and include it in a table.

# Create Make Effect table
make_avgs <- train_set %>% 
  group_by(make) %>% 
  summarize(b_make = mean(price - mu))
head(make_avgs)

# Plot Make Effect
make_avgs %>% qplot(b_make, geom ="histogram", bins = 30, data = ., color = I("black"), fill = I("#003CFF"))+
labs(title = "Make Effect") +
theme(
plot.title = element_text(hjust = 0.5), # center title
panel.grid.major = element_line(color = "gray", linetype = "dashed"), # add
#dashed lines
panel.grid.minor = element_blank() # Removing minor grid lines
)

# Calculate the predicted price
# if the price is Na use the mean.
predicted_price <- mu + test_set %>% 
  left_join(make_avgs, by='make') %>%
  pull(b_make)

summary(predicted_price)

mean_predicted_rating <- mean(predicted_price, na.rm = TRUE)
predicted_price[is.na(predicted_price)] <- mean_predicted_rating 
summary(predicted_price)
# There was no NA in price predicted.

# Calculate the RMSE
model_1_rmse <- RMSE(predicted_price, test_set$price)
rmse_results <- bind_rows(
  rmse_results,
  data_frame(Method_on_train_test="Make Effect",
  RMSE = model_1_rmse ,
  Improvement = naive_rmse - model_1_rmse))
rmse_results %>% 
mutate(RMSE = format(RMSE, nsmall = 2, big.mark = ",")) %>% 
mutate(Improvement = format(Improvement, nsmall = 2, big.mark = ",")) %>% 
knitr::kable()
# As you can see RMSE has reduced significantly 14,350 QAR.
########################################
### Model Effect
########################################
# We used mu, Make as a baseline and calculated the Model bias b_model.
# We then calculate the RMSE and include it in a table.
# We have added a check to eliminate negative price values and limit to a 
# lower bound of 20% value of average price for that trim.

price_lower_boundary<- 0.2

# Create Model Effect table
model_avgs <- train_set %>% 
left_join(make_avgs, by='make') %>%
group_by(model) %>% 
summarize(b_model = mean(price - mu - b_make))
head(model_avgs)

# Plot Model Effect
model_avgs %>% qplot(b_model, geom ="histogram", bins = 30, data = ., color = I("black"), fill = I("#003CFF"))+
labs(title = "Model Effect") +
theme(
plot.title = element_text(hjust = 0.5), # center title
panel.grid.major = element_line(color = "gray", linetype = "dashed"), # add
#dashed lines
panel.grid.minor = element_blank() # Removing minor grid lines
)

# Calculate the predicted price
# if the price is Na use the mean.
predicted_price <- test_set %>% 
  left_join(make_avgs, by='make') %>%
  left_join(model_avgs, by='model') %>%
  mutate(pred = mu + b_make + b_model) %>%
  mutate(pred2 = ifelse((pred) >= (price_pmmt *price_lower_boundary), 
                       pred, 
                       price_pmmt *price_lower_boundary)) %>%
  pull(pred2)

summary(predicted_price)

mean_predicted_rating <- mean(predicted_price, na.rm = TRUE)
predicted_price[is.na(predicted_price)] <- mean_predicted_rating 
summary(predicted_price)
# There was no NA in price predicted.

# Calculate the RMSE
model_2_rmse <- RMSE(predicted_price, test_set$price)
rmse_results <- bind_rows(
  rmse_results,
  data_frame(Method_on_train_test="Make + Model Effect",
  RMSE = model_2_rmse ,
  Improvement = model_1_rmse - model_2_rmse))
rmse_results %>% 
mutate(RMSE = format(RMSE, nsmall = 2, big.mark = ",")) %>% 
mutate(Improvement = format(Improvement, nsmall = 2, big.mark = ",")) %>% 
knitr::kable()

# As you can see RMSE has reduced significantly 12,126 QAR.
########################################
### Trim Effect
########################################
# We used mu, Make, Model as a baseline and calculated the Trim bias b_trim.
# We then calculate the RMSE and include it in a table.
# We have added a check to eliminate negative price values and limit to a 
# lower bound of 20% value of average price for that trim.

# Create Trim Effect table
trim_avgs <- train_set %>% 
left_join(make_avgs, by='make') %>%
left_join(model_avgs, by='model') %>%
group_by(trim) %>% 
summarize(b_trim = mean(price - mu - b_make - b_model))
head(trim_avgs)
# Plot Trim Effect
trim_avgs %>% qplot(b_trim, geom ="histogram", bins = 30, data = ., color = I("black"), fill = I("#003CFF"))+
labs(title = "Trim Effect") +
theme(
plot.title = element_text(hjust = 0.5), # center title
panel.grid.major = element_line(color = "gray", linetype = "dashed"), # add
#dashed lines
panel.grid.minor = element_blank() # Removing minor grid lines
)


# Calculate the predicted price
# if the price is Na use the mean.
predicted_price <- test_set %>% 
  left_join(make_avgs, by='make') %>%
  left_join(model_avgs, by='model') %>%
  left_join(trim_avgs, by='trim') %>%
  mutate(pred = mu + b_make + b_model + b_trim) %>%
  mutate(pred2 = ifelse((pred) >= (price_pmmt *price_lower_boundary), 
                       pred, 
                       price_pmmt *price_lower_boundary)) %>%
  pull(pred2)

summary(predicted_price)

mean_predicted_rating <- mean(predicted_price, na.rm = TRUE)
predicted_price[is.na(predicted_price)] <- mean_predicted_rating 
summary(predicted_price)
# There was no NA in price predicted.

# Calculate the RMSE

model_3_rmse <- RMSE(predicted_price, test_set$price)
rmse_results <- bind_rows(
  rmse_results,
  data_frame(Method_on_train_test="Make + Model + Trim Effect",
  RMSE = model_3_rmse ,
  Improvement = model_2_rmse - model_3_rmse))
rmse_results %>% 
mutate(RMSE = format(RMSE, nsmall = 2, big.mark = ",")) %>% 
mutate(Improvement = format(Improvement, nsmall = 2, big.mark = ",")) %>% 
knitr::kable()
# As you can see RMSE has reduced significantly 23,170 QAR.

########################################
### Year Effect
########################################
# We used mu, Make, Model, Trim as a baseline and calculated the Year bias b_year.
# We then used linear regression to create a linear function where given any
# year we can calculate the bias effect of the year even if the year doesn't
# exist in the training data.
# We have used this link^[https://www.vedantu.com/formula/linear-regression-formula]
# for the linear regression formula.
# We then calculate the RMSE and include it in a table.
# We have added a check to eliminate negative price values and limit to a 
# lower bound of 20% value of average price for that trim.

# Create year Effect table
year_avgs <- train_set %>% 
left_join(make_avgs, by='make') %>%
left_join(model_avgs, by='model') %>%
left_join(trim_avgs, by='trim') %>%
#left_join(personal_avgs, by='personal') %>%
group_by(year) %>% 
summarize(b_year = mean(price - mu - b_make - b_model - b_trim ))
head(year_avgs)

# Plot Year Effect
year_avgs %>% qplot(b_year, geom ="histogram", bins = 30, data = ., color = I("black"), fill = I("#003CFF"))+
labs(title = "Year Effect") +
theme(
plot.title = element_text(hjust = 0.5), # center title
panel.grid.major = element_line(color = "gray", linetype = "dashed"), # add
#dashed lines
panel.grid.minor = element_blank() # Removing minor grid lines
)


#### Linear Regression

# First we extracted year and trim from year as they were combined earlier.
# We then calculated a and b, where b_year = a +bx and x is year2.

# Since you need more than 2 points to calculate a and b, in case of 1 record
# We used the same b_year bias effect for a and b is zero.

# In case we are predicting for a year that was not trained we are using the
# average price of the car for that particular trim with a factor of year_v=0.1 which is 
# how much the car will devalue per year deviated away from the average price per trim.

year_avgs_updated <- year_avgs
year_avgs_updated <- year_avgs_updated %>% mutate (year2 = substr(year, nchar(year) - 3, nchar(year)))
year_avgs_updated <- year_avgs_updated %>% mutate (year2 = as.numeric(year2))
year_avgs_updated <- year_avgs_updated %>% mutate (trim = substr(year,0, nchar(year) - 5))
head(year_avgs_updated)
# Calculate a and b
year_avgs_updated <- year_avgs_updated %>% group_by(trim) %>% mutate(n= n()) 
year_avgs_updated <- year_avgs_updated %>% group_by(trim) %>% mutate(x =  year2) 
year_avgs_updated <- year_avgs_updated %>% group_by(trim) %>% mutate(y =  b_year) 
year_avgs_updated <- year_avgs_updated %>% group_by(trim) %>% mutate(Ex =  sum(x)) 
year_avgs_updated <- year_avgs_updated %>% group_by(trim) %>% mutate(Ey =  sum(y)) 
year_avgs_updated <- year_avgs_updated %>% group_by(trim) %>% mutate(Exx =  sum(x*x)) 
year_avgs_updated <- year_avgs_updated %>% group_by(trim) %>% mutate(Exy =  sum(x*y)) 
year_avgs_updated <- year_avgs_updated %>% group_by(trim) %>% mutate(b = (n*Exy - Ex*Ey) / (n*Exx - (Ex*Ex))   ) 
year_avgs_updated <- year_avgs_updated %>% group_by(trim) %>% mutate(a = (Ey - b*Ex) / n)
year_avgs_updated <- year_avgs_updated %>%
mutate(a = ifelse(n == 1, b_year, a),
b = ifelse(n == 1, 0, b))
# the below is used to verify the results however for prediction we calculate again.
year_avgs_updated <- year_avgs_updated %>% mutate (b_year_updated = a + b*year2)
# below is used for prediction where you need a and b
year_avgs_uu <- year_avgs_updated %>% 
    select(trim, a, b) %>% 
    distinct()
head(year_avgs_uu)

year_v <- 0.1

# Calculate the predicted price
# if the price is Na use the mean.
predicted_price <- test_set %>% 
  left_join(make_avgs, by='make') %>%
  left_join(model_avgs, by='model') %>%
  left_join(trim_avgs, by='trim') %>%  
  left_join(year_avgs_uu, by='trim') %>% # check if trim is correct
  mutate(year_updated = substr(year, nchar(year) - 3, nchar(year))) %>%
  mutate(price_year_value = ifelse (b==0, (as.numeric(price_pmmt)* as.numeric(year_v) * (as.numeric(year_updated) - as.numeric(avg_ymmt))),0) ) %>%
  mutate(pred = mu + b_make + b_model + b_trim + (as.numeric(a) + as.numeric(b)*as.numeric(year_updated) ) 
  + price_year_value
  ) %>%
  mutate(pred2 = ifelse((pred) >= (price_pmmt *price_lower_boundary), 
                       pred, 
                       price_pmmt *price_lower_boundary))  %>%
  pull(pred2)
summary(predicted_price)

mean_predicted_rating <- mean(predicted_price, na.rm = TRUE)
predicted_price[is.na(predicted_price)] <- mean_predicted_rating 
summary(predicted_price)
# There was no NA in price predicted.

# Calculate the RMSE
model_4_rmse <- RMSE(predicted_price, test_set$price)
rmse_results <- bind_rows(
  rmse_results,
  data_frame(Method_on_train_test="Make + Model + Trim + Year Effect",
  RMSE = model_4_rmse ,
  Improvement = model_3_rmse - model_4_rmse))
rmse_results %>% 
mutate(RMSE = format(RMSE, nsmall = 2, big.mark = ",")) %>% 
mutate(Improvement = format(Improvement, nsmall = 2, big.mark = ",")) %>% 
knitr::kable()

# As you can see RMSE has reduced significantly 17,625 QAR.

########################################
### Mileage Effect
########################################
# We used Average_mileage_range table which was created in feature engineering.
# We then used linear regression to create a linear function where given any
# mileage_range we can calculate the bias effect of the mileage even if the mileage range
# doesn't # exist in the training data.
# We have used the same link^[https://www.vedantu.com/formula/linear-regression-formula]
# for the linear regression formula.
# We then calculate the RMSE and include it in a table.
# We have added a check to eliminate negative price values and limit to a 
# lower bound of 20% value of average price for that trim.

#### Linear Regression

# First we extracted year as it was linked to trim.
# We then calculated aa and bb. b_mileage = aa +bbx where x is mileage_range.
# Since you need more than 2 points to calculate aa and bb, in case of 1 record.
# We used the same avg_mileage_range bias effect for aa and bb is zero.
# We are using a factor of 0.16 as weightage of the mileage effect as it was tested.
# to have the highest effect by trial and error. We could have used sapply function.
# After calculating the average mileage_range for a particular trim and year,
# we then compared how far it is from the avg_milage_range for that trim, divided by 
# 100,000 mileage to create this mileage effect. This was also done via trial and error 
# to arrive to a good RMSE.
damru <- data_avg_mileage_range
damru <- damru %>% mutate (year2 = substr(year, nchar(year) - 3, nchar(year)))
damru <- damru %>% mutate (year2 = as.numeric(year2))
damru <- damru %>% mutate (trim = substr(year,0, nchar(year) - 5))
head(data_avg_mileage_range)

# Calculate aa and bb
damru <- damru %>% group_by(trim) %>% mutate(n= n()) 
damru <- damru %>% group_by(trim) %>% mutate(x =  year2) 
damru <- damru %>% group_by(trim) %>% mutate(y =  avg_mileage_range) 
damru <- damru %>% group_by(trim) %>% mutate(Ex =  sum(x)) 
damru <- damru %>% group_by(trim) %>% mutate(Ey =  sum(y)) 
damru <- damru %>% group_by(trim) %>% mutate(Exx =  sum(x*x)) 
damru <- damru %>% group_by(trim) %>% mutate(Exy =  sum(x*y)) 
damru <- damru %>% group_by(trim) %>% mutate(bb = (n*Exy - Ex*Ey) / (n*Exx - (Ex*Ex))   ) 
damru <- damru %>% group_by(trim) %>% mutate(aa = (Ey - bb*Ex) / n)

damru <- damru %>%
mutate(aa = ifelse(n == 1, avg_mileage_range, aa),
bb = ifelse(n == 1, 0, bb))

# the below is used to verify the results however for prediction we calculate again.
damru <- damru %>% mutate (avg_mileage_range_updated = ifelse ((aa + bb*year2)>0,aa + bb*year2,0 ))
# below is used for prediction where you need aa and bb
damruu <- damru %>% 
     select(trim, aa, bb) %>% 
     distinct()
head(damru)


mileage_v=0.16
mileage_v2= 0 # not used
mileage_v3= 10 # 10 * 10,000 = 100,000 km

# Calculate the predicted price
# if the price is Na use the mean.
predicted_price <- test_set %>% 
    left_join(make_avgs, by='make') %>%
    left_join(model_avgs, by='model') %>%
    left_join(trim_avgs, by='trim') %>%  
    left_join(year_avgs_uu, by='trim') %>%
    left_join(damruu, by='trim') %>%
    mutate(year_updated = substr(year, nchar(year) - 3, nchar(year))) %>%
    mutate(mileage_range_updated = case_when( mileage == 0 ~ 0,   mileage > 0 ~ ceiling(mileage / 10000) * 10000)) %>%
    mutate(price_year_value = ifelse (b==0, (as.numeric(price_pmmt)* as.numeric(year_v) * (as.numeric(year_updated) - as.numeric(avg_ymmt))),0) ) %>%
    mutate(avg_mileage_range_updated = (ifelse ((aa + bb* as.numeric(year_updated))>0,aa + bb* as.numeric(year_updated),0 ))  +mileage_v2) %>% # nolint
    mutate(pred1 = mu + b_make + b_model + b_trim + (as.numeric(a) + as.numeric(b)*as.numeric(year_updated) ) 
    + price_year_value
    ) %>%
    mutate(pred2 = pred1 + (mileage_v) * pred1 * (as.numeric(avg_mileage_range_updated ) - as.numeric(mileage_range_updated))/as.numeric(mileage_v3*10000)  )     %>%
    mutate(pred3 = ifelse((pred2) >= (price_pmmt *price_lower_boundary), 
                       pred2, 
                       price_pmmt *price_lower_boundary))  %>%
    pull(pred3)
summary(predicted_price)

mean_predicted_rating <- mean(predicted_price, na.rm = TRUE)
predicted_price[is.na(predicted_price)] <- mean_predicted_rating 
summary(predicted_price)
# There was no NA in price predicted.

# Calculate the RMSE
model_5_rmse <- RMSE(predicted_price, test_set$price)
rmse_results <- bind_rows(
  rmse_results,
  data_frame(Method_on_train_test="Make + Model + Trim + Year + Mileage Effect",
  RMSE = model_5_rmse ,
  Improvement = model_4_rmse - model_5_rmse))
rmse_results %>% 
mutate(RMSE = format(RMSE, nsmall = 2, big.mark = ",")) %>% 
mutate(Improvement = format(Improvement, nsmall = 2, big.mark = ",")) %>% 
knitr::kable()

# As you can see RMSE has reduced significantly 2,431 QAR.

########################################
### Validation Results
########################################
# We have previously tested against our test_set test dataset.
# We will now test against the unseen validate_set validation dataset.

# Predict price
predicted_price <- validate_set %>% 
    left_join(make_avgs, by='make') %>%
    left_join(model_avgs, by='model') %>%
    left_join(trim_avgs, by='trim') %>%  
    left_join(year_avgs_uu, by='trim') %>% # check if trim is correct
    left_join(damruu, by='trim') %>%
    mutate(year_updated = substr(year, nchar(year) - 3, nchar(year))) %>%
    mutate(mileage_range_updated = case_when( mileage == 0 ~ 0,   mileage > 0 ~ ceiling(mileage / 10000) * 10000)) %>%
    mutate(price_year_value = ifelse (b==0, (as.numeric(price_pmmt)* as.numeric(year_v) * (as.numeric(year_updated) - as.numeric(avg_ymmt))),0) ) %>%
    mutate(avg_mileage_range_updated = (ifelse ((aa + bb* as.numeric(year_updated))>0,aa + bb* as.numeric(year_updated),0 ))  +mileage_v2) %>% # nolint
    mutate(pred1 = mu + b_make + b_model + b_trim + (as.numeric(a) + as.numeric(b)*as.numeric(year_updated) ) 
    + price_year_value
    ) %>%
    mutate(pred2 = pred1 + (mileage_v) * pred1 * (as.numeric(avg_mileage_range_updated ) - as.numeric(mileage_range_updated))/as.numeric(mileage_v3*10000)  )     %>%
    mutate(pred3 = ifelse((pred2) >= (price_pmmt *price_lower_boundary), 
                       pred2, 
                       price_pmmt *price_lower_boundary))  %>%
    pull(pred3)
summary(predicted_price)

mean_predicted_rating <- mean(predicted_price, na.rm = TRUE)
predicted_price[is.na(predicted_price)] <- mean_predicted_rating 
summary(predicted_price)
# There was no NA in price predicted.

# Calculate the RMSE
model_6_rmse <- RMSE(predicted_price, validate_set$price)
rmse_results <- bind_rows(
  rmse_results,
  data_frame(Method_on_validation="Method 01 - Linear Regression",
  RMSE = model_6_rmse ,
  Improvement = model_5_rmse - model_6_rmse))
rmse_results %>% 
mutate(RMSE = format(RMSE, nsmall = 2, big.mark = ",")) %>% 
mutate(Improvement = format(Improvement, nsmall = 2, big.mark = ",")) %>% 
knitr::kable()


# As you can see Final RMSE on validation is 20,225 vs 19,763 on test dataset.

# The RMSE has dropped down from 89,000 to around 20,000. 

# For those who are familiar with the Netflix project, 
# if we compare with the Netflix target goal RMSE of 0.865, 
# the 20,000 RMSE is roughly 3 times better. (20,000*5/365000)

########################################
### Predict Car Price - User input 
########################################
# In this section, we can predict the price of a car based on the below user input.
# 
# This has been used to test the model manually.
# 
# Create a list of a few car and its features, you can change this as you like.
########################################

# Create a list of a few car and its features, you can change this as you like.
new_data <- data.frame(make = "Toyota", model = "Land Cruiser", trim = "GXR",
personal = "", year = 2015,  gear_type = "Automatic", cylinder = 8, mileage = 10000)
new_row <- data.frame(make = "Genesis", model = "G70", trim = "Standard", 
personal = "", year = 2019, gear_type = "Automatic", cylinder = 6, mileage = 90000)
new_data <- rbind(new_data, new_row)
new_row <- data.frame(make = "Nissan", model = "Patrol", trim = "SE",
personal = "", year = 2014, gear_type = "Automatic", cylinder = 8, mileage = 125000)
new_data <- rbind(new_data, new_row)
new_row <- data.frame(make = "Toyota", model = "Land Cruiser", trim = "GXR",
personal = "", year = 2013, gear_type = "Automatic", cylinder = 6, mileage = 175000)
new_data <- rbind(new_data, new_row)
new_row <- data.frame(make = "Chevrolet", model = "Camaro", trim = "ZL1",
personal = "", year = 2014, gear_type = "Automatic", cylinder = 8, mileage = 10000)
new_data <- rbind(new_data, new_row)
new_row <- data.frame(make = "Alfa Romeo", model = "4 C", trim = "Standard",
personal = "", year = 2019, gear_type = "Automatic", cylinder = 4, mileage = 10000)
new_data <- rbind(new_data, new_row)
new_row <- data.frame(make = "Audi", model = "A8", trim = "4.0",
personal = "", year = 2019, gear_type = "Automatic", cylinder = 8, mileage = 100000)
new_data <- rbind(new_data, new_row)
new_data

# Data wrangling by updating mileage_range, year, model and trim
new_data$mileage_range <- ifelse(new_data$mileage == 0, 0, ceiling(new_data$mileage / 10000) * 10000)
new_data$year <- paste(new_data$make, new_data$make, new_data$model, new_data$trim, new_data$year, sep = "-")
new_data$model <- paste(new_data$make, new_data$model, sep = "-")
new_data$trim <- paste( new_data$make, new_data$model, new_data$trim,new_data$cylinder, new_data$gear_type, sep = "-")
new_data

# predict using above linear regression model
predicted_price <- new_data %>% 
  left_join(make_avgs, by='make') %>%
  left_join(model_avgs, by='model') %>%
  left_join(trim_avgs, by='trim') %>%  
  left_join(year_avgs_uu, by='trim') %>%
  left_join(data_price_pmmt, by='trim') %>%
  left_join(data_avg_ymmt, by='trim') %>%  
  left_join(data_avg_mileage_range, by='year') %>% 
  left_join(damruu, by='trim') %>%
  mutate(year_updated = substr(year, nchar(year) - 3, nchar(year))) %>%
  mutate(mileage_range_updated = case_when( mileage == 0 ~ 0,   mileage > 0 ~ ceiling(mileage / 10000) * 10000)) %>%
  mutate(price_year_value = ifelse (b==0, (as.numeric(price_pmmt)* as.numeric(year_v) * (as.numeric(year_updated) - as.numeric(avg_ymmt))),0) ) %>%
  mutate(avg_mileage_range_updated = (ifelse ((aa + bb* as.numeric(year_updated))>0,aa + bb* as.numeric(year_updated),0 ))  +mileage_v2) %>% # nolint
  mutate(pred1 = mu + b_make + b_model + b_trim + (as.numeric(a) + as.numeric(b)*as.numeric(year_updated) ) 
         + price_year_value
  ) %>%
  mutate(pred2 = pred1 + (mileage_v) * pred1 * (as.numeric(avg_mileage_range_updated ) - as.numeric(mileage_range_updated))/as.numeric(mileage_v3*10000)  )     %>%  
  mutate(pred3 = ifelse((pred2) >= (price_pmmt *price_lower_boundary), 
                        pred2, 
                        price_pmmt *price_lower_boundary))

# add Year and mileage effects to the table
predicted_price<- predicted_price %>% mutate(Year_Effect = pred1- price_pmmt)
predicted_price<- predicted_price %>% mutate(Mileage_Effect = pred2- pred1) 
# add some other parameters used for debugging
predicted_price <- predicted_price %>%
  group_by(trim) %>%
  mutate(pred3_pmmt = mean(pred3))
predicted_price <- predicted_price %>%
  group_by(trim) %>%
  mutate(pmmt_difference = (price_pmmt - pred3_pmmt))
predicted_price <- predicted_price %>%
  group_by(trim) %>%
  mutate(pmmt_absolute = (price_pmmt - pred3_pmmt)/price_pmmt*100)

# print predicted price
predicted_price %>% select( trim, year, mileage, L_Pred_Price = pred3) %>%
mutate(L_Pred_Price = format(L_Pred_Price, nsmall = 2, big.mark = ",")) %>%
mutate(mileage = format(mileage, nsmall = 2, big.mark = ",")) %>%
 knitr::kable()

########################################
### Export Recommendation on Full Data
########################################

# In this section, we will use the model above and predict the prices of
# the full dataset (train, test, validate). We will then export files to provide
# the best recommended car deals.

# A Linear Regression Full data export folder will be generated and
# a subfolder will be created based on run time time stamp.
# A few files will be created used for debugging, however there are 2 export files
# that are of interest.

# _Export_Formated.csv contains full data along with predicted price and comparison 
# against actual price. You can filter by column Price_Gap_Percentage with the
# the highest positive value to get the best deals.

# _Export_Formated_Filtered.csv is the same as the earlier file, however has been 
# sorted and filtered with the best deals per trim, with Price_Gap_Percentage > 20
# having mileage less than 80,000 km and which is  still available for sale (Not sold).

# Predict Price
data2 <- data %>% 
    left_join(make_avgs, by='make') %>%
    left_join(model_avgs, by='model') %>%
    left_join(trim_avgs, by='trim') %>%  
    left_join(year_avgs_uu, by='trim') %>%
    left_join(damruu, by='trim') %>%
    mutate(year_updated = substr(year, nchar(year) - 3, nchar(year))) %>%
    mutate(mileage_range_updated = case_when( mileage == 0 ~ 0,   mileage > 0 ~ ceiling(mileage / 10000) * 10000)) %>% # nolint
    mutate(price_year_value = ifelse (b==0, (as.numeric(price_pmmt)* as.numeric(year_v) * (as.numeric(year_updated) - as.numeric(avg_ymmt))),0) ) %>%
    mutate(avg_mileage_range_updated = (ifelse ((aa + bb* as.numeric(year_updated))>0,aa + bb* as.numeric(year_updated),0 ))  +mileage_v2) %>% # nolint
    mutate(pred1 = mu + b_make + b_model + b_trim + (as.numeric(a) + as.numeric(b)*as.numeric(year_updated) ) 
    + price_year_value
    ) %>%
    mutate(pred2 = pred1 + (mileage_v) * pred1 * (as.numeric(avg_mileage_range_updated ) - as.numeric(mileage_range_updated))/as.numeric(mileage_v3*10000)  )     %>%
    mutate(pred3 = ifelse((pred2) >= (price_pmmt *price_lower_boundary), 
                       pred2, 
                       price_pmmt *price_lower_boundary))  
# Calculate the difference between the predicted and actual price
data2<- data2 %>% mutate(price_difference = pred3 - price) 
# Calculate the percentage difference based on the price
data2<- data2 %>% mutate(percentage_difference = (price_difference / price) * 100)    
data2<- data2 %>% mutate(percentage_difference_updated = ifelse (percentage_difference<=0.01 & percentage_difference>= -0.01 ,0,percentage_difference))    
# add Year and mileage effects to the table
data2<- data2 %>% mutate(absolut_difference = abs(price_difference))
data2<- data2 %>% mutate(Year_Effect = pred1- price_pmmt)
data2<- data2 %>% mutate(Mileage_Effect = pred2- pred1) 
# add some other parameters used for debugging
data2 <- data2 %>%
    group_by(trim) %>%
    mutate(pred3_pmmt = mean(pred3))
data2 <- data2 %>%
    group_by(trim) %>%
    mutate(pmmt_difference = (price_pmmt - pred3_pmmt))
data2 <- data2 %>%
    group_by(trim) %>%
    mutate(pmmt_absolute = (price_pmmt - pred3_pmmt)/price_pmmt*100)

# create folder for linear regression and automatic timestamp subfolder
version_folder <- "01. Linear Regression Model - Full Data Export"

# Check if the folder already exists
if (!dir.exists(version_folder)) {
    # Create the folder
    dir.create(version_folder)
}
# Get the current date and time
current_datetime <- Sys.time()
# Create a folder name based on date and time
folder_name <- format(current_datetime, "%Y%b%d_%H-%M-%S")
# Create a folder with current_datetime inside the version folder
folder_path <- paste0(version_folder, "/", folder_name)
dir.create(folder_path)
folder_path
L_Full_path<-folder_path

# exporting full data with predictions
write.csv(data2, file = paste0(folder_path, "/_Export_full.csv"), row.names = FALSE)

# Renaming and selecting Columns to be more user friendly 
data3<- data2 %>% select(
Id = web.scraper.order,
Link = car.href,
Make = make,
Model = model,
Trim= trim,
Year = year_updated,
Price= price,
Mileage =mileage,
Date_sold= date_sold,
Average_Year_per_Trim=avg_ymmt,
#Average_Milerange_per_Trim_Year= avg_mileage_range,
Average_Milerange_per_Trim_Year_LR= avg_mileage_range_updated,  
Average_Price_per_Trim =price_pmmt,
Year_Effect,
Mileage_Effect,
L_Predicted_Price = pred3,  
L_Price_Gap_Percentage =percentage_difference_updated,
)

# Formatting number with 2 digits and comma
data3 <- data3 %>%
   mutate(Price = format(Price, nsmall = 0, big.mark = ",")) %>%
   mutate(Mileage = format(Mileage, nsmall = 0, big.mark = ",")) %>%
   mutate(Average_Year_per_Trim = format(Average_Year_per_Trim, nsmall = 2, big.mark = ",")) %>%
   mutate(Average_Price_per_Trim = format(Average_Price_per_Trim, nsmall = 2, big.mark = ",")) %>%
   #mutate(Average_Milerange_per_Trim_Year = format(Average_Milerange_per_Trim_Year, nsmall = 2, big.mark = ",")) %>%
   mutate(Average_Milerange_per_Trim_Year_LR = format(Average_Milerange_per_Trim_Year_LR, nsmall = 2, big.mark = ",")) %>%
   mutate(Year_Effect = format(Year_Effect, nsmall = 0, big.mark = ",")) %>%
   mutate(Mileage_Effect = format(Mileage_Effect, nsmall = 0, big.mark = ",")) %>%
   mutate(L_Predicted_Price = format(L_Predicted_Price, nsmall = 0, big.mark = ",")) 

# Sequencing data by trim then by Price_Gap_Percentage
data3 <- data3 %>%
    arrange(Trim, desc(L_Price_Gap_Percentage)) %>%
    group_by(Trim) %>%
    mutate(Count = n())

# export full recommendation file for best car deals.
write.csv(data3, file = paste0(folder_path, "/_Export_Formated.csv"), row.names = FALSE)

# Filtering by high Price_Gap_Percentage < 20, where there are many training data 
# count > 10, not sold, and mileage < 80,000 km
data4 <- data3 %>%
    arrange(Trim, desc(L_Price_Gap_Percentage)) %>%
    filter(Count > 10) %>%
    filter(L_Price_Gap_Percentage > 20) %>%
    filter(Date_sold == "") %>%
    mutate( mileage2 = (gsub(",", "", Mileage))) %>%
    mutate( mileage2 = as.numeric(mileage2)) %>%
    filter((mileage2) < 80000)

# Reducing the number of results by top 2 per trim.
data4 <- data4 %>%
    group_by(Trim) %>% 
    #arrange(Trim, desc(Price_Gap_Pecentage)) %>%
    #arrange(desc(Price_Gap_Pecentage)) %>%
    top_n(2, L_Price_Gap_Percentage)
    #top_n(if (Count/10<1,1,Count/10), Price_Gap_Pecentage)

# Prearranging Column order.
data4 <- data4 %>%
select(
Id,
Link,
Make,
Model,
Trim,
Year,
Price,
Mileage,
L_Predicted_Price,
L_Price_Gap_Percentage,
Average_Year_per_Trim,
#Average_Milerange_per_Trim_Year,
Average_Milerange_per_Trim_Year_LR,
Average_Price_per_Trim,
Year_Effect,
Mileage_Effect
)
# export full recommendation file for best car deals which is sorted and filtered.
write.csv(data4, file = paste0(folder_path, "/_Export_Formated_Filtered.csv"), row.names = FALSE)

# Below we can save the model for future use. model 48 have been saved.
# save.image(file = "Model_48.RData")

# Navigate to "01. Linear Regression Model - Full Data Export" and select the most recent folder, you
# will find _Export_Formated_Filtered.csv which will provide you with the best car
# deals. 

# You can open the file _Export_Formated_Filtered.csv and view the car deals, all
# of which have much lower actual price compared to our prediction. Upon verification,
# we realize many of those of are great deals.

# If you open via excel Columns A to H are the original data, where G is the actual price and I is the predicted price.
# The 13560 listing have been shortlisted to around 90 deals where their actual price is way lower
# than predicted. This is being recommended by the system indicating a possible good deal.

# The last 5 columns also explain how this prediction came about. This offers interpretability.
# These columns show the average year, mileage and price for this trim, then year 
# and mileage price effects. This explains the prediction and gives us reassurance.

# For wider selection, you can refer to the _Export_Formated.csv where you have the full analysis.
# This has been a very exciting journey for me and an important milestone so far.

# The below table has a sample list of best deals. As you can see the actually price is way
# lower (at least 20%) than the predicted. These are a great deals. If you check one by one like I did,
# when you compare the same or similar trim and year you'll notice that these are the lowest prices.
data4<-data4 %>%
  mutate(L_Predicted_Price = round(as.numeric((gsub(",", "", L_Predicted_Price))), digits = 0)) %>% 
  mutate(L_Price_Gap_Percentage = round(as.numeric((gsub(",", "", L_Price_Gap_Percentage))), digits = 0))  %>%
  mutate(L_Predicted_Price = format(L_Predicted_Price, nsmall = 0, big.mark = ",")) %>%
  mutate(L_Price_Gap_Percentage = format(L_Price_Gap_Percentage, nsmall = 0, big.mark = ",")) 

L_Full_Table<-data4 %>% 
    group_by(Trim) %>% 
    slice(1) %>% 
    select(Trim, Year, Mileage, Price = Price,Pred_Price= L_Predicted_Price,Gap_Perc=L_Price_Gap_Percentage) %>%
    #mutate(Mileage = format(Mileage, nsmall = 2, big.mark = ",")) %>% 
    #head(20) %>% 
    knitr::kable()
L_Full_Table

########################################
### Export Recommendation on Recent Data
########################################
# In this section we will run the same model above on fresh new data web scrapped.
# The system will then automatically provide recommendation of which cars are the best deals. 
# This is a powerful and useful tool.

# Below can be used to avoid rerunning the entire code above.

# load("Model_48.RData")
# nrow(data4)

# the below can be run again in case we loaded the model 48 and started a new session.

# Install the packages if not already installed
if (!require(magrittr)) install.packages("magrittr")
if (!require(dplyr)) install.packages("dplyr")
# Load the packages
library(magrittr)
library(dplyr)
library(caret)
library(stringr)

# Read the csv file from relative path
data_p <- read.csv("pred09Apr202411am.csv")
nrow(data_p)

# same data cleaning and filtering process used earlier ---

# Clean mileage column from "Km" and comma and convert to numeric
data_p$mileage <- (gsub(" Km", "", data_p$mileage))
data_p$mileage <- (gsub(",", "", data_p$mileage))
data_p$mileage <- as.numeric(data_p$mileage)
# Convert cylinder to number
data_p$cylinder <- as.integer(data_p$cylinder)
# Clean price column from comma and convert to numeric
data_p$price <- (gsub(",", "", data_p$price))
data_p$price <- as.numeric(data_p$price)
# Extract 2 columns from time, sold_date and posted time
data_p$date_sold <- NA
data_p$date_sold <- ifelse(grepl("/", data_p$time), format(as.POSIXct(data_p$time, format = "%d/%m/%Y"), "%d-%b-%Y"), "")
data_p$posted_time <- NA
data_p$posted_time <- ifelse(grepl("hour", data_p$time), as.numeric(gsub(" hours ago", "", data_p$time)), "")
data_p$posted_time <- ifelse(grepl("minute", data_p$time), as.numeric(gsub(" minutes ago", "", data_p$time))/60, data$posted_time)
# Create new column mileage_range from mileage
data_p <- data_p %>%
  mutate(mileage_range = case_when(
    mileage == 0 ~ 0,
    mileage > 0 ~ ceiling(mileage / 10000) * 10000
  ))
# Filter data where price < 365,000 QAR and year > 2013.
data_p <- data_p[data_p$price < 365000, ]
data_p <- data_p[data_p$year > 2013, ]
# set set value and lower boundary threshold
price_lower_boundary<- 0.2
set.seed(123)
# remove rows with missing values in the price column
data_p <- data_p[!is.na(data_p$price) & data_p$price != "", ]
nrow(data_p)
# feature engineering by update model ,trim, year and mileage range.
data_p$model <- paste(data_p$make, data_p$model, sep = "-")
data_p$trim <- paste(data_p$make,  data_p$model, data_p$trim, data_p$cylinder, data_p$gear_type,sep = "-")
data_p$year <- paste(data_p$trim, data_p$year, sep = "-")
data_p$mileage_range <- paste( data_p$trim, data_p$mileage_range, sep = "-")

# Check new data received is less than the full data earlier
nrow(data)
nrow(data_p)

# Predict Price
data2 <- data_p %>% 
    left_join(make_avgs, by='make') %>%
    left_join(model_avgs, by='model') %>%
    left_join(trim_avgs, by='trim') %>%  
    left_join(year_avgs_uu, by='trim') %>% 
    left_join(damruu, by='trim') %>%
    left_join(data_price_pmmt, by='trim') %>%
    left_join(data_avg_ymmt, by='trim') %>% 
    left_join(data_avg_mileage_range, by='year') %>%
    mutate(year_updated = substr(year, nchar(year) - 3, nchar(year))) %>%
    mutate(mileage_range_updated = case_when( mileage == 0 ~ 0,   mileage > 0 ~ ceiling(mileage / 10000) * 10000)) %>% # nolint
    mutate(price_year_value = ifelse (b==0, (as.numeric(price_pmmt)* as.numeric(year_v) * (as.numeric(year_updated) - as.numeric(avg_ymmt))),0) ) %>%
    mutate(avg_mileage_range_updated = (ifelse ((aa + bb* as.numeric(year_updated))>0,aa + bb* as.numeric(year_updated),0 ))  +mileage_v2) %>% # nolint
    mutate(pred1 = mu + b_make + b_model + b_trim + (as.numeric(a) + as.numeric(b)*as.numeric(year_updated) ) 
    + price_year_value
    ) %>%
    mutate(pred2 = pred1 + (mileage_v) * pred1 * (as.numeric(avg_mileage_range_updated ) - as.numeric(mileage_range_updated))/as.numeric(mileage_v3*10000)  )     %>%
    mutate(pred3 = ifelse((pred2) >= (price_pmmt *price_lower_boundary), 
                       pred2, 
                       price_pmmt *price_lower_boundary))  
# Calculate the difference between the predicted and actual price
data2<- data2 %>% mutate(price_difference = pred3 - price) 
# Calculate the percentage difference based on the price
data2<- data2 %>% mutate(percentage_difference = (price_difference / price) * 100)    
data2<- data2 %>% mutate(percentage_difference_updated = ifelse (percentage_difference<=0.01 & percentage_difference>= -0.01 ,0,percentage_difference))    
# add Year and mileage effects to the table
data2<- data2 %>% mutate(absolut_difference = abs(price_difference))
data2<- data2 %>% mutate(Year_Effect = pred1- price_pmmt)
data2<- data2 %>% mutate(Mileage_Effect = pred2- pred1) 
# add some other parameters used for debugging
data2 <- data2 %>%
    group_by(trim) %>%
    mutate(pred3_pmmt = mean(pred3))
data2 <- data2 %>%
    group_by(trim) %>%
    mutate(pmmt_difference = (price_pmmt - pred3_pmmt))
data2 <- data2 %>%
    group_by(trim) %>%
    mutate(pmmt_absolute = (price_pmmt - pred3_pmmt)/price_pmmt*100)

# create output folder
version_folder <- "02. Linear Regression Model - Recent Data Export"

# Check if the folder already exists
if (!dir.exists(version_folder)) {
    # Create the folder
    dir.create(version_folder)
}

# Get the current date and time
current_datetime <- Sys.time()
# Create a folder name based on date and time
folder_name <- format(current_datetime, "%Y%b%d_%H-%M-%S")

# Create a folder with current_datetime inside the version folder
folder_path <- paste0(version_folder, "/", folder_name)
dir.create(folder_path)
folder_path
L_Recent_path<-folder_path

# export files for debugging
write.csv(data2, file = paste0(folder_path, "/_Export_full.csv"), row.names = FALSE)
# Renaming and selecting Columns to be more user friendly
data3<- data2 %>% select(
Id = web.scraper.order,
Link = car.href,
Make = make,
Model = model,
Trim= trim,
Year = year_updated,
Price= price,
Mileage =mileage,
Date_sold= date_sold,
Average_Year_per_Trim=avg_ymmt,
#Average_Milerange_per_Trim_Year= avg_mileage_range,
Average_Milerange_per_Trim_Year_LR= avg_mileage_range_updated,  
Average_Price_per_Trim =price_pmmt,
Year_Effect,
Mileage_Effect,
L_Predicted_Price = pred3,  
L_Price_Gap_Percentage =percentage_difference_updated,
)
# Formatting number with 2 digits and comma
data3 <- data3 %>%
   mutate(Price = format(Price, nsmall = 0, big.mark = ",")) %>%
   mutate(Mileage = format(Mileage, nsmall = 0, big.mark = ",")) %>%
   mutate(Average_Year_per_Trim = format(Average_Year_per_Trim, nsmall = 2, big.mark = ",")) %>%
   mutate(Average_Price_per_Trim = format(Average_Price_per_Trim, nsmall = 2, big.mark = ",")) %>%
   #mutate(Average_Milerange_per_Trim_Year = format(Average_Milerange_per_Trim_Year, nsmall = 2, big.mark = ",")) %>%
   mutate(Average_Milerange_per_Trim_Year_LR = format(Average_Milerange_per_Trim_Year_LR, nsmall = 2, big.mark = ",")) %>%
   mutate(Year_Effect = format(Year_Effect, nsmall = 0, big.mark = ",")) %>%
   mutate(Mileage_Effect = format(Mileage_Effect, nsmall = 0, big.mark = ",")) %>%
   mutate(L_Predicted_Price = format(L_Predicted_Price, nsmall = 0, big.mark = ",")) 
# Sequencing data by trim then by Price_Gap_Percentage
data3 <- data3 %>%
    arrange(Trim, desc(L_Price_Gap_Percentage)) %>%
    group_by(Trim) %>%
    mutate(Count = n()) 
# export full recommendation file for best car deals.
write.csv(data3, file = paste0(folder_path, "/_Export_Formated.csv"), row.names = FALSE)
# Filtering by high Price_Gap_Percentage > 10
data4 <- data3 %>%
    arrange(Trim, desc(L_Price_Gap_Percentage)) %>%
    filter(L_Price_Gap_Percentage > 10) #%>%
# Rearranging Column order.
data4 <- data4 %>%
select(
Id,
Link,
Make,
Model,
Trim,
Year,
Price,
Mileage,
L_Predicted_Price,
L_Price_Gap_Percentage,
Average_Year_per_Trim,
#Average_Milerange_per_Trim_Year,
Average_Milerange_per_Trim_Year_LR,
Average_Price_per_Trim,
Year_Effect,
Mileage_Effect
)
# export full recommendation file for best car deals which is sorted and filtered.
write.csv(data4, file = paste0(folder_path, "/_Export_Formated_Filtered.csv"), row.names = FALSE)
nrow(data4)

# Navigate to "02. Linear Regression Model - Recent Data Export" and find most recent folder, you
# will find _Export_Formated_Filtered.csv which will provide you with the best car
# deals. 
# You can open the file _Export_Formated_Filtered.csv and view the car deals, all
# of which have much lower actual price compared to our prediction. Upon verification,
# we realize many of those are great deals. For wider selection, you can
# refer to the _Export_Formated.csv where you can finetune your search results.

# The below table has a sample list of best deals. As you can see the actually price is way
# lower (at least 10%) than the predicted. These are a great deals. If you check one by one like I did,
# when you compare the same or similar trim and year you'll notice that these are the lowest prices.
data4<-data4 %>%
  mutate(L_Predicted_Price = round(as.numeric((gsub(",", "", L_Predicted_Price))), digits = 0)) %>% 
  mutate(L_Price_Gap_Percentage = round(as.numeric((gsub(",", "", L_Price_Gap_Percentage))), digits = 0))  %>%
  mutate(L_Predicted_Price = format(L_Predicted_Price, nsmall = 0, big.mark = ",")) %>%
  mutate(L_Price_Gap_Percentage = format(L_Price_Gap_Percentage, nsmall = 0, big.mark = ","))

L_Recent_Table<- data4 %>% 
    group_by(Trim) %>% 
    slice(1) %>% 
    select(Trim, Year, Mileage, Price = Price,Pred_Price= L_Predicted_Price,Gap_Perc=L_Price_Gap_Percentage) %>% 
    #mutate(Mileage = format(Mileage, nsmall = 2, big.mark = ",")) %>% 
    #head(20) %>% 
    knitr::kable()
L_Recent_Table

##############################################
## Method - Random Forest 
##############################################
# 
# Based on the content and knowledge gained from chapter 31.11 in this 
# link^[https://rafalab.dfci.harvard.edu/dsbook/examples-of-algorithms.html#random-forests]
# I have selected this algorithm to be the most suitable with the best accuracy for my project.

# I have first tried knn, however I faced errors (too many ties in knn) as there 
# were many near by neighbor even when increasing value of k.

# I have also experimented with many parameters for random forest like mtry, ntree, etc.
# However the results were not good documented in Train Trials.xlsx.

# Finally I have used the default parameters for random forest and kept the model
# training for around 20 hours and the results were great, an RMSE of 13,500 vs 
# 20,000 using my custom linear regression.

# I also used cross validation k =3, I could have used a higher value but that will
# take longer to train. 


# Below are optional code, just saving previous data.
# save.image(file = "Data_Load.RData") # was used to save the loaded data.
# load("Data_Load.RData")
# if (!require(dplyr)) install.packages("dplyr")
# library(dplyr)
# library(caret)

### Data Preparation 
# We are excluding car column as its empty
set.seed(123)  
data_rfr <- data
data_rfr <- data_rfr[, -which(names(data_rfr) == "car")]

# Creating year2 column
data_rfr<- data_rfr %>% mutate (year2 = substr(year, nchar(year) - 3, nchar(year)))

# checking if there is any NA in the data
colSums(is.na(data_rfr))

# I had first started with subset data to test the random forest, but then used full data
#data_small <- data_rfr[sample(nrow(data_rfr), 1000), ]
data_small <- data_rfr

# Selecting the columns we need to optimize the data needed for training,
data_small <- data_small%>% ungroup() %>% select(price, make, model, trim, year2, mileage)

# Used factor for the columns to be used by random forest
data_small$make <- as.factor(data_small$make)
data_small$model <- as.factor(data_small$model)
data_small$trim <- as.factor(data_small$trim)
str(data_small)

### Hyper Parameter
# We tried different combination of knn and random parameters but results were 
# not satisfactory as documented in Train Trials.xlsx
#mt = "knn"
#tg = expand.grid(.k = seq(from = 1, to = 5, by = 1))
mt ="rf"
#tg = data.frame(.mtry = 5)
#tg <- expand.grid(.mtry = 5)
#tg <- expand.grid(.mtry = seq(from = 2, to = 5, by = 1), .ntree = c(500, 1000, 1500))
ctrl <- trainControl(method = "repeatedcv", number = 3, verboseIter = TRUE)  # x-fold cross-validation

# The below random forest training will take 20 hours on i7 laptop, 
# alternatively you can load the saved trained model rf01.RData in the next command.
# rf_model <- train(price ~ make + model + trim + year2 + mileage, data = data_small, method = mt , trControl = ctrl )
# print(rf_model$bestTune)
# predictions <- predict(rf_model, data_small)
# accuracy <- postResample(predictions, data_small$price)
# print(accuracy)

# we have previously saved the Random Forest Model, so you can load it here.
#save.image(file = "rf01.RData")
load("rf01.RData")

### Validation Results
# Benchmarking the results and comparing with previous linear regression.
# As you can see the results have vastly improved from an RMSE 20,000 to 13,500.
rmse_results <- bind_rows(
  rmse_results,
  data_frame(Method_on_validation="Method 02 - Random Forest",
  RMSE = accuracy[[1]] ,
  Improvement = model_6_rmse - accuracy[[1]]))
rmse_results %>% 
mutate(RMSE = format(RMSE, nsmall = 2, big.mark = ",")) %>% 
mutate(Improvement = format(Improvement, nsmall = 2, big.mark = ",")) %>% 
mutate(RMSE = round(as.numeric((gsub(",", "", RMSE))), digits = 0)) %>% 
mutate(Improvement = round(as.numeric((gsub(",", "", Improvement))), digits = 0))  %>%
mutate(RMSE = format(RMSE, nsmall = 0, big.mark = ",")) %>%
mutate(Improvement = format(Improvement, nsmall = 0, big.mark = ","))%>%
knitr::kable()


########################################
### Predict Car Price - User input 
########################################
# In this section, we can predict the price of a car based on the below user input.
# 
# This has been used to test the model manually.
# 
# Create a list of a few car and its features, you can change this as you like.
########################################

# Create a list of a few car and its features

new_data <- data.frame(make = "Toyota", model = "Land Cruiser", trim = "GXR",
                       personal = "", year = 2015,  gear_type = "Automatic", cylinder = 8, mileage = 10000)
new_row <- data.frame(make = "Genesis", model = "G70", trim = "Standard", 
                      personal = "", year = 2019, gear_type = "Automatic", cylinder = 6, mileage = 90000)
new_data <- rbind(new_data, new_row)
new_row <- data.frame(make = "Nissan", model = "Patrol", trim = "SE",
                      personal = "", year = 2014, gear_type = "Automatic", cylinder = 8, mileage = 125000)
new_data <- rbind(new_data, new_row)
new_row <- data.frame(make = "Toyota", model = "Land Cruiser", trim = "GXR",
                      personal = "", year = 2013, gear_type = "Automatic", cylinder = 6, mileage = 175000)
new_data <- rbind(new_data, new_row)
new_row <- data.frame(make = "Chevrolet", model = "Camaro", trim = "ZL1",
                      personal = "", year = 2014, gear_type = "Automatic", cylinder = 8, mileage = 10000)
new_data <- rbind(new_data, new_row)
new_row <- data.frame(make = "Alfa Romeo", model = "4 C", trim = "Standard",
                      personal = "", year = 2019, gear_type = "Automatic", cylinder = 4, mileage = 10000)
new_data <- rbind(new_data, new_row)
new_row <- data.frame(make = "Audi", model = "A8", trim = "4.0",
                      personal = "", year = 2019, gear_type = "Automatic", cylinder = 8, mileage = 100000)
new_data <- rbind(new_data, new_row)
new_data

# Update mileage_range, year, model and trim
new_data$mileage_range <- ifelse(new_data$mileage == 0, 0, ceiling(new_data$mileage / 10000) * 10000)
new_data$year <- paste(new_data$make, new_data$make, new_data$model, new_data$trim, new_data$year, sep = "-")
new_data$model <- paste(new_data$make, new_data$model, sep = "-")
new_data$trim <- paste( new_data$make, new_data$model, new_data$trim,new_data$cylinder, new_data$gear_type, sep = "-")
new_data

# predict using above linear regression model first then random forest for comparison
predicted_price_rf <- new_data %>% 
  left_join(make_avgs, by='make') %>%
  left_join(model_avgs, by='model') %>%
  left_join(trim_avgs, by='trim') %>%  
  left_join(year_avgs_uu, by='trim') %>%
  left_join(data_price_pmmt, by='trim') %>%
  left_join(data_avg_ymmt, by='trim') %>%  
  left_join(data_avg_mileage_range, by='year') %>% 
  left_join(damruu, by='trim') %>%
  mutate(year_updated = substr(year, nchar(year) - 3, nchar(year))) %>%
  mutate(mileage_range_updated = case_when( mileage == 0 ~ 0,   mileage > 0 ~ ceiling(mileage / 10000) * 10000)) %>%
  mutate(price_year_value = ifelse (b==0, (as.numeric(price_pmmt)* as.numeric(year_v) * (as.numeric(year_updated) - as.numeric(avg_ymmt))),0) ) %>%
  mutate(avg_mileage_range_updated = (ifelse ((aa + bb* as.numeric(year_updated))>0,aa + bb* as.numeric(year_updated),0 ))  +mileage_v2) %>% # nolint
  mutate(pred1 = mu + b_make + b_model + b_trim + (as.numeric(a) + as.numeric(b)*as.numeric(year_updated) ) 
         + price_year_value
  ) %>%
  mutate(pred2 = pred1 + (mileage_v) * pred1 * (as.numeric(avg_mileage_range_updated ) - as.numeric(mileage_range_updated))/as.numeric(mileage_v3*10000)  )     %>%  
  mutate(pred3 = ifelse((pred2) >= (price_pmmt *price_lower_boundary), 
                        pred2, 
                        price_pmmt *price_lower_boundary))

    
# add Year and mileage effects to the table
predicted_price_rf<- predicted_price_rf %>% mutate(Year_Effect = pred1- price_pmmt)
predicted_price_rf<- predicted_price_rf %>% mutate(Mileage_Effect = pred2- pred1) 
# add some other parameters used for debugging
predicted_price_rf <- predicted_price_rf %>%
  group_by(trim) %>%
  mutate(pred3_pmmt = mean(pred3))
predicted_price_rf <- predicted_price_rf %>%
  group_by(trim) %>%
  mutate(pmmt_difference = (price_pmmt - pred3_pmmt))
predicted_price_rf <- predicted_price_rf %>%
  group_by(trim) %>%
  mutate(pmmt_absolute = (price_pmmt - pred3_pmmt)/price_pmmt*100)


# Create and update Columns ( years, make, model, trim)
predicted_price_rf_2<- predicted_price_rf %>% mutate (year2 = substr(year, nchar(year) - 3, nchar(year)))
predicted_price_rf_2$make <- as.factor(predicted_price_rf$make)
predicted_price_rf_2$model <- as.factor(predicted_price_rf$model)
predicted_price_rf_2$trim <- as.factor(predicted_price_rf$trim)


# Checking which record we need to remove,
# as random forest need to have been trained on the data
data_temp <- data
data_temp<- data_temp %>% mutate (year2 = substr(year, nchar(year) - 3, nchar(year))) %>% 
  mutate (year2 = as.numeric(year2))

# check trims not trained
missing_trims_rf <- setdiff(predicted_price_rf_2$trim, data_temp$trim)
head(missing_trims_rf)

# remove years not trained
missing_years <- setdiff(predicted_price_rf_2$year2, data_temp$year2)
head(missing_years)

# Remove the new untrained data
nrow(predicted_price_rf_2)
predicted_price_rf_2 <- predicted_price_rf_2 %>% 
  filter(trim %in% data$trim)
predicted_price_rf_2 <- predicted_price_rf_2 %>% 
  filter(year2 %in% data_temp$year2)
nrow(predicted_price_rf_2)

# Prepare the data that needs to be predicted
new_rf_data <- predicted_price_rf_2%>% ungroup() %>% select( make, model, trim, year2, mileage)

# Run Random forest model on new small data.
predicted_price_rf_2 <- predicted_price_rf_2 %>% 
  ungroup() %>%
  mutate(predicted_price_rf_2 = predict(rf_model, newdata = new_rf_data))

# Print out the prediction
predicted_price_rf_2 %>% select(trim, year, mileage, 
                                L_Pred_Price = pred3,
                                R_Pred_Price = predicted_price_rf_2
                                ) %>%
mutate(L_Pred_Price = format(L_Pred_Price, nsmall = 2, big.mark = ",")) %>%
mutate(R_Pred_Price = format(R_Pred_Price, nsmall = 2, big.mark = ",")) %>%
mutate(mileage = format(mileage, nsmall = 2, big.mark = ","))%>%
knitr::kable()
########################################
### Export Recommendation on Full Data
########################################
# 
# In this section, we will use the model above to predict all the prices of
# the full dataset (train, test, validate) and export files that provide the best
# recommendation car deals.
# 
# A Random Forest Full data export folder will be generated and
# a subfolder will be created based on run time time stamp.
# A few files will be created used for debugging, however there are 2 export files
# that are of interest.
#
# _Export_Formated_rf.csv contains full data along with predicted price and comparison 
# againsed actual price. You can filter by column Price_Gap_Percentage with the
# the highest positive value to get the best deals.
#
# _Export_Formated_Filtered_rf.csv is the same as the earlier file, however has been 
# sorted and filtered with the best deals per trim, with Price_Gap_Percentage_RF & 
# Price_Gap_Percentage_RF > 20. Also having mileage less than 80,000 km 
# and is still available for sale (Not sold).


# create folder for Random Forest Folder and automatic timestamp subfolder
version_folder <- "03. Random Forest Model - Full Data Export"

# Check if the folder already exists
if (!dir.exists(version_folder)) {
    # Create the folder
    dir.create(version_folder)
}
# Get the current date and time
current_datetime <- Sys.time()
# Create a folder name based on date and time
folder_name <- format(current_datetime, "%Y%b%d_%H-%M-%S")
# Create a folder with current_datetime inside the version folder
folder_path <- paste0(version_folder, "/", folder_name)
dir.create(folder_path)
folder_path
R_Full_path<-folder_path

# First run Linear regression prediction which will be used for comparison
# It is also used for analysis and interpretability.

data_rf_2 <- data %>% 
    left_join(make_avgs, by='make') %>%
    left_join(model_avgs, by='model') %>%
    left_join(trim_avgs, by='trim') %>%  
    left_join(year_avgs_uu, by='trim') %>%
    left_join(damruu, by='trim') %>%
    mutate(year_updated = substr(year, nchar(year) - 3, nchar(year))) %>%
    mutate(mileage_range_updated = case_when( mileage == 0 ~ 0,   mileage > 0 ~ ceiling(mileage / 10000) * 10000)) %>% # nolint
    mutate(price_year_value = ifelse (b==0, (as.numeric(price_pmmt)* as.numeric(year_v) * (as.numeric(year_updated) - as.numeric(avg_ymmt))),0) ) %>%
    mutate(avg_mileage_range_updated = (ifelse ((aa + bb* as.numeric(year_updated))>0,aa + bb* as.numeric(year_updated),0 ))  +mileage_v2) %>% # nolint
    mutate(pred1 = mu + b_make + b_model + b_trim + (as.numeric(a) + as.numeric(b)*as.numeric(year_updated) ) 
    + price_year_value
    ) %>%
    mutate(pred2 = pred1 + (mileage_v) * pred1 * (as.numeric(avg_mileage_range_updated ) - as.numeric(mileage_range_updated))/as.numeric(mileage_v3*10000)  )     %>%
    mutate(pred3 = ifelse((pred2) >= (price_pmmt *price_lower_boundary), 
                       pred2, 
                       price_pmmt *price_lower_boundary)) 
#str(data_rf_2)

# Create and update Columns ( years, make, model, trim)
data_rf_2<- data_rf_2 %>% mutate (year2 = substr(year, nchar(year) - 3, nchar(year)))
data_rf_2$make <- as.factor(data_rf_2$make)
data_rf_2$model <- as.factor(data_rf_2$model)
data_rf_2$trim <- as.factor(data_rf_2$trim)
new_rf_data <- data_rf_2%>% ungroup() %>% select(price, make, model, trim, year2, mileage)

# Run Random forest model on all the data.
data_rf_2 <- data_rf_2 %>% 
  ungroup() %>%
  mutate(predicted_price_rf = predict(rf_model, newdata = new_rf_data))

# Add columns to compare actual price vs linear regression vs random forest
data_rf_2<- data_rf_2 %>% mutate(price_difference = pred3 - price) 
# Calculate the percentage difference based on the price for linear regression
data_rf_2<- data_rf_2 %>% mutate(percentage_difference = (price_difference / price) * 100)    
data_rf_2<- data_rf_2 %>% mutate(percentage_difference_updated = ifelse (percentage_difference<=0.01 & percentage_difference>= -0.01 ,0,percentage_difference))  
data_rf_2<- data_rf_2 %>% mutate(price_difference_rf = predicted_price_rf - price) 
# Calculate the percentage difference based on the price for random forest
data_rf_2<- data_rf_2 %>% mutate(percentage_difference_rf = (price_difference_rf / price) * 100)    
data_rf_2<- data_rf_2 %>% mutate(percentage_difference_updated_rf = ifelse (percentage_difference_rf<=0.01 & percentage_difference_rf>= -0.01 ,0,percentage_difference_rf))
# Add Year and Mileage effects based on Linear Regression model for interpretability
data_rf_2<- data_rf_2 %>% mutate(absolut_difference = abs(price_difference))
data_rf_2<- data_rf_2 %>% mutate(Year_Effect = pred1- price_pmmt)
data_rf_2<- data_rf_2 %>% mutate(Mileage_Effect = pred2- pred1) 
# Other paramaters used for debugging
data_rf_2 <- data_rf_2 %>%
    group_by(trim) %>%
    mutate(pred3_pmmt = mean(pred3))
data_rf_2 <- data_rf_2 %>%
    group_by(trim) %>%
    mutate(pmmt_difference = (price_pmmt - pred3_pmmt))
data_rf_2 <- data_rf_2 %>%
    group_by(trim) %>%
    mutate(pmmt_absolute = (price_pmmt - pred3_pmmt)/price_pmmt*100)
# Export the full data with prediction which is still not formatted
write.csv(data_rf_2, file = paste0(folder_path, "/_Export_full_rf.csv"), row.names = FALSE)
nrow(data_rf_2)

# Rename and format the columns and export file
data_rf_3<- data_rf_2 %>% select(
Id = web.scraper.order,
Link = car.href,
Make = make,
Model = model,
Trim= trim,
Year = year_updated,
Price= price,
Mileage =mileage,
Date_sold= date_sold,
Average_Year_per_Trim=avg_ymmt,
#Average_Milerange_per_Trim_Year= avg_mileage_range,
Average_Milerange_per_Trim_Year_LR= avg_mileage_range_updated,  
Average_Price_per_Trim =price_pmmt,
Year_Effect,
Mileage_Effect,
L_Predicted_Price = pred3,  
L_Price_Gap_Percentage =percentage_difference_updated,
R_Predicted_Price = predicted_price_rf ,
R_Price_Gap_Percentage =percentage_difference_updated_rf
)
data_rf_3 <- data_rf_3 %>%
   mutate(Price = format(Price, nsmall = 0, big.mark = ",")) %>%
   mutate(Mileage = format(Mileage, nsmall = 0, big.mark = ",")) %>%
   mutate(Average_Year_per_Trim = format(Average_Year_per_Trim, nsmall = 2, big.mark = ",")) %>%
   mutate(Average_Price_per_Trim = format(Average_Price_per_Trim, nsmall = 2, big.mark = ",")) %>%
   #mutate(Average_Milerange_per_Trim_Year = format(Average_Milerange_per_Trim_Year, nsmall = 2, big.mark = ",")) %>%
   mutate(Average_Milerange_per_Trim_Year_LR = format(Average_Milerange_per_Trim_Year_LR, nsmall = 2, big.mark = ",")) %>%
   mutate(Year_Effect = format(Year_Effect, nsmall = 0, big.mark = ",")) %>%
   mutate(Mileage_Effect = format(Mileage_Effect, nsmall = 0, big.mark = ",")) %>%
   mutate(L_Predicted_Price = format(L_Predicted_Price, nsmall = 0, big.mark = ",")) %>%
   mutate(R_Predicted_Price  = format(R_Predicted_Price , nsmall = 0, big.mark = ",")) 
data_rf_3 <- data_rf_3 %>%
    arrange(Trim, desc(R_Price_Gap_Percentage)) %>%
    group_by(Trim) %>%
    mutate(Count = n()) 
write.csv(data_rf_3, file = paste0(folder_path, "/_Export_Formated_rf.csv"), row.names = FALSE)
nrow(data_rf_3)

# Export recommendation of best car deals
data_rf_4 <- data_rf_3 %>%
    arrange(Trim, desc(R_Price_Gap_Percentage)) %>%
    filter(Count > 10) %>%
    filter(R_Price_Gap_Percentage > 20) %>%
    filter(L_Price_Gap_Percentage > 20) %>% # added
    filter(Date_sold == "") %>%
    mutate( mileage2 = (gsub(",", "", Mileage))) %>%
    mutate( mileage2 = as.numeric(mileage2)) %>%
    filter((mileage2) < 80000)
 
data_rf_4 <- data_rf_4 %>%
    group_by(Trim) %>% 
    #arrange(Trim, desc(Price_Gap_Pecentage)) %>%
    #arrange(desc(Price_Gap_Pecentage)) %>%
    top_n(2, R_Price_Gap_Percentage)
    #top_n(if (Count/10<1,1,Count/10), Price_Gap_Pecentage)
data_rf_4 <- data_rf_4 %>%
select(
Id,
Link,
Make,
Model,
Trim,
Year,
Price,
Mileage,
L_Predicted_Price,
L_Price_Gap_Percentage,
R_Predicted_Price,
R_Price_Gap_Percentage,
Average_Year_per_Trim,
#Average_Milerange_per_Trim_Year,
Average_Milerange_per_Trim_Year_LR,
Average_Price_per_Trim,
Year_Effect,
Mileage_Effect
)
write.csv(data_rf_4, file = paste0(folder_path, "/_Export_Formated_Filtered_rf.csv"), row.names = FALSE)
nrow(data_rf_4)


# Navigate to "03. Random Forest Model - Full Data Export" and select the most recent folder, you
# will find _Export_Formated_Filtered.csv which will provide you with the best car
# deals. 

# You can open the file _Export_Formated_Filtered.csv and view the car deals, all
# of which have much lower actual price compared to our prediction. Upon verification,
# we realize many of those of are great deals.

# If you open via excel Columns A to H are the original data, where G is the actual price 
# and I is the linear predicted price and k is the Random forest predicted price.

# The 13560 listing have been shortlisted to around 20 deals where their actual price is way lower
# than predicted (Random forest). This is being recommended by the system indicating a possible good deal.

# The last 5 columns provides some interpretation from linear regression side. 
# These columns show the average year, mileage and price for this trim, then year 
# and mileage price effects. This helps us check if randmon forest is not suitable in certain
# cases.

# For wider selection, you can refer to the _Export_Formated.csv where you have the full analysis.

# The below table has a sample list of best deals. As you can see the actually price is way
# lower (at least 20%) than the predicted. These are a great deals. If you check one by one like I did,
# when you compare the same or similar trim and year you'll notice that these are the lowest prices.
data_rf_4<-data_rf_4 %>%
  mutate(L_Predicted_Price = round(as.numeric((gsub(",", "", L_Predicted_Price))), digits = 0)) %>% 
  mutate(R_Predicted_Price = round(as.numeric((gsub(",", "", R_Predicted_Price))), digits = 0)) %>% 
  mutate(R_Price_Gap_Percentage = round(as.numeric((gsub(",", "", R_Price_Gap_Percentage))), digits = 0)) %>% 
  mutate(L_Predicted_Price = format(L_Predicted_Price, nsmall = 0, big.mark = ",")) %>%
  mutate(R_Predicted_Price = format(R_Predicted_Price, nsmall = 0, big.mark = ",")) %>%
  mutate(R_Price_Gap_Percentage = format(R_Price_Gap_Percentage, nsmall = 0, big.mark = ",")) 

R_Full_Table<-data_rf_4 %>% 
    #group_by(Trim) %>% 
    #slice(1) %>% 
    select(Trim, Year, Mileage, Price = Price,L_Price= L_Predicted_Price,R_Price= R_Predicted_Price,R_Gap=R_Price_Gap_Percentage) %>% 
    #mutate(Mileage = format(Mileage, nsmall = 2, big.mark = ",")) %>% 
    #head(20) %>% 
    knitr::kable()
R_Full_Table

##############################################
### Export Recommendation on Recent Data
##############################################

# In this section we will run the same model above on fresh new data web scrapped.
# The system will then automatically provide recommendation of which cars are the best deals. 
# This is a powerful and useful tool.

# Optional to save old data in case needed
data_rf_2_old<-data_rf_2
data_p_old<- data_p
data_p<- data_p %>% mutate (year2 = substr(year, nchar(year) - 3, nchar(year)))

# We are using the same dataset data_p extracted earlier from "pred09Apr202411am.csv"
nrow(data_p)

# Checking which record we need to remove,
# as random forest need to have been trained on the data
# You might need to remove more columns if rf is failing, like year in future.
# Here missing was new types of trims which are exported so that use is aware.

missing_trims <- setdiff(data_p$trim, data$trim)
head(missing_trims)

# Remove the new untrained data
data_p <- data_p %>% 
    filter(trim %in% data$trim)
nrow(data_p)
# Optional to remove un acceptable data like 2013 although filtered
# data_temp <- data
# data_temp<- data_temp %>% mutate (year2 = substr(year, nchar(year) - 3, nchar(year))) %>%
#   mutate (year2 = as.numeric(year2))
# data_p <- data_p %>% 
#   filter(year2 %in% data_temp$year2)
# nrow(data_p)

# Predict using Linear Regression & Random for comparison
data_rf_2 <- data_p %>% 
    left_join(make_avgs, by='make') %>%
    left_join(model_avgs, by='model') %>%
    left_join(trim_avgs, by='trim') %>%  
    left_join(year_avgs_uu, by='trim') %>%
    left_join(damruu, by='trim') %>%
    left_join(data_price_pmmt, by='trim') %>% # added
    left_join(data_avg_ymmt, by='trim') %>% # added
    left_join(data_avg_mileage_range, by='year') %>% # added
    mutate(year_updated = substr(year, nchar(year) - 3, nchar(year))) %>%
    mutate(mileage_range_updated = case_when( mileage == 0 ~ 0,   mileage > 0 ~ ceiling(mileage / 10000) * 10000)) %>% # nolint
    mutate(price_year_value = ifelse (b==0, (as.numeric(price_pmmt)* as.numeric(year_v) * (as.numeric(year_updated) - as.numeric(avg_ymmt))),0) ) %>%
    mutate(avg_mileage_range_updated = (ifelse ((aa + bb* as.numeric(year_updated))>0,aa + bb* as.numeric(year_updated),0 ))  +mileage_v2) %>% # nolint
    mutate(pred1 = mu + b_make + b_model + b_trim + (as.numeric(a) + as.numeric(b)*as.numeric(year_updated) ) 
    + price_year_value
    ) %>%
    mutate(pred2 = pred1 + (mileage_v) * pred1 * (as.numeric(avg_mileage_range_updated ) - as.numeric(mileage_range_updated))/as.numeric(mileage_v3*10000)  )     %>%
    mutate(pred3 = ifelse((pred2) >= (price_pmmt *price_lower_boundary), 
                       pred2, 
                       price_pmmt *price_lower_boundary)) 

# Update columns year2, make, model, and trim
data_rf_2<- data_rf_2 %>% mutate (year2 = substr(year, nchar(year) - 3, nchar(year)))
data_rf_2$make <- as.factor(data_rf_2$make)
data_rf_2$model <- as.factor(data_rf_2$model)
data_rf_2$trim <- as.factor(data_rf_2$trim)
new_rf_data <- data_rf_2%>% ungroup() %>% select(price, make, model, trim, year2, mileage)
#head(new_rf_data )

# Run Random Forest Prediction
data_rf_2 <- data_rf_2 %>% 
  ungroup() %>%
  mutate(predicted_price_rf = predict(rf_model, newdata = new_rf_data))

# Calculate gap between actual price, linear regression and random forest price prediction
data_rf_2<- data_rf_2 %>% mutate(price_difference = pred3 - price) 
# Calculate the percentage difference based on the price Linear Regression
data_rf_2<- data_rf_2 %>% mutate(percentage_difference = (price_difference / price) * 100)    
data_rf_2<- data_rf_2 %>% mutate(percentage_difference_updated = ifelse (percentage_difference<=0.01 & percentage_difference>= -0.01 ,0,percentage_difference))  
data_rf_2<- data_rf_2 %>% mutate(price_difference_rf = predicted_price_rf - price) 
# Calculate the percentage difference based on the price Random Forest
data_rf_2<- data_rf_2 %>% mutate(percentage_difference_rf = (price_difference_rf / price) * 100)    
data_rf_2<- data_rf_2 %>% mutate(percentage_difference_updated_rf = ifelse (percentage_difference_rf<=0.01 & percentage_difference_rf>= -0.01 ,0,percentage_difference_rf))
# Add Year and Mileage effect for interpretability
data_rf_2<- data_rf_2 %>% mutate(absolut_difference = abs(price_difference))
data_rf_2<- data_rf_2 %>% mutate(Year_Effect = pred1- price_pmmt)
data_rf_2<- data_rf_2 %>% mutate(Mileage_Effect = pred2- pred1) 
# Other parameters for debugging
data_rf_2 <- data_rf_2 %>%
    group_by(trim) %>%
    mutate(pred3_pmmt = mean(pred3))
data_rf_2 <- data_rf_2 %>%
    group_by(trim) %>%
    mutate(pmmt_difference = (price_pmmt - pred3_pmmt))
data_rf_2 <- data_rf_2 %>%
    group_by(trim) %>%
    mutate(pmmt_absolute = (price_pmmt - pred3_pmmt)/price_pmmt*100)

# Creating output folder 
version_folder <- "04. Random Forest Model - Recent Data Export"
# Check if the folder already exists
if (!dir.exists(version_folder)) {
    # Create the folder
    dir.create(version_folder)
}
# Get the current date and time
current_datetime <- Sys.time()
# Create a folder name based on date and time
folder_name <- format(current_datetime, "%Y%b%d_%H-%M-%S")
# Create a folder with current_datetime inside the version folder
folder_path <- paste0(version_folder, "/", folder_name)
dir.create(folder_path)
folder_path
R_Recent_path<-folder_path

# Export full data un formatted with random forest prediction
write.csv(data_rf_2, file = paste0(folder_path, "/_Export_full_rf.csv"), row.names = FALSE)
nrow(data_rf_2)
# Export Car listing trims that were not inferred as the model was not trained on it.
write.csv(missing_trims, file = paste0(folder_path, "/_Export_missing.csv"), row.names = FALSE)
length(missing_trims)

# Format and Select Columns for the full data predicted
data_rf_3<- data_rf_2 %>% select(
Id = web.scraper.order,
Link = car.href,
Make = make,
Model = model,
Trim= trim,
Year = year_updated,
Price= price,
Mileage =mileage,
Date_sold= date_sold,
Average_Year_per_Trim=avg_ymmt,
#Average_Milerange_per_Trim_Year= avg_mileage_range,
Average_Milerange_per_Trim_Year_LR= avg_mileage_range_updated,  
Average_Price_per_Trim =price_pmmt,
Year_Effect,
Mileage_Effect,
L_Predicted_Price = pred3,  
L_Price_Gap_Percentage =percentage_difference_updated,
R_Predicted_Price = predicted_price_rf ,
R_Price_Gap_Percentage =percentage_difference_updated_rf
)
data_rf_3 <- data_rf_3 %>%
   mutate(Price = format(Price, nsmall = 0, big.mark = ",")) %>%
   mutate(Mileage = format(Mileage, nsmall = 0, big.mark = ",")) %>%
   mutate(Average_Year_per_Trim = format(Average_Year_per_Trim, nsmall = 2, big.mark = ",")) %>%
   mutate(Average_Price_per_Trim = format(Average_Price_per_Trim, nsmall = 2, big.mark = ",")) %>%
   #mutate(Average_Milerange_per_Trim_Year = format(Average_Milerange_per_Trim_Year, nsmall = 2, big.mark = ",")) %>%
   mutate(Average_Milerange_per_Trim_Year_LR = format(Average_Milerange_per_Trim_Year_LR, nsmall = 2, big.mark = ",")) %>%
   mutate(Year_Effect = format(Year_Effect, nsmall = 0, big.mark = ",")) %>%
   mutate(Mileage_Effect = format(Mileage_Effect, nsmall = 0, big.mark = ",")) %>%
   mutate(L_Predicted_Price = format(L_Predicted_Price, nsmall = 0, big.mark = ",")) %>%
   mutate(R_Predicted_Price  = format(R_Predicted_Price , nsmall = 0, big.mark = ",")) 
data_rf_3 <- data_rf_3 %>%
    arrange(Trim, desc(R_Price_Gap_Percentage)) %>%
    group_by(Trim) %>%
    mutate(Count = n()) 
write.csv(data_rf_3, file = paste0(folder_path, "/_Export_Formated_rf.csv"), row.names = FALSE)
nrow(data_rf_3)

# Export the best deal recommendations, where random forest prediction is 10%
# higher than actual and linear regression is 10% higher than actual price.
data_rf_4 <- data_rf_3 %>%
    arrange(Trim, desc(R_Price_Gap_Percentage)) %>%
    #filter(Count > 10) %>%
    filter(R_Price_Gap_Percentage > 10) %>%
    filter(L_Price_Gap_Percentage > 10) #%>%
    # filter(Date_sold == "") %>%
    # mutate( mileage2 = (gsub(",", "", Mileage))) %>%
    # mutate( mileage2 = as.numeric(mileage2)) %>%
    # filter((mileage2) < 80000)
 
# data_rf_4 <- data_rf_4 %>%
#     group_by(Trim) %>% 
#     #arrange(Trim, desc(Price_Gap_Pecentage)) %>%
#     #arrange(desc(Price_Gap_Pecentage)) %>%
#     top_n(2, Price_Gap_Percentage_RF)
#     #top_n(if (Count/10<1,1,Count/10), Price_Gap_Pecentage)
data_rf_4 <- data_rf_4 %>%
select(
Id,
Link,
Make,
Model,
Trim,
Year,
Price,
Mileage,
L_Predicted_Price,
L_Price_Gap_Percentage,
R_Predicted_Price,
R_Price_Gap_Percentage,
Average_Year_per_Trim,
#Average_Milerange_per_Trim_Year,
Average_Milerange_per_Trim_Year_LR,
Average_Price_per_Trim,
Year_Effect,
Mileage_Effect
)
write.csv(data_rf_4, file = paste0(folder_path, "/_Export_Formated_Filtered_rf.csv"), row.names = FALSE)
nrow(data_rf_4)

# Navigate to "04. Random Forest Model - Recent Data Export" and find most recent folder, you
# will find _Export_Formated_Filtered.csv which will provide you with the best car
# deals. 
# You can open the file _Export_Formated_Filtered.csv and view the car deals, all
# of which have much lower actual price compared to our prediction. Upon verification,
# we realize many of those are great deals. For wider selection, you can
# refer to the _Export_Formated.csv where you can finetune your search results.


# The below table has a sample list of best deals. As you can see the actually price is way
# lower (at least 10%) than the predicted. These are a great deals. If you check one by one like I did,
# when you compare the same or similar trim and year you'll notice that these are the lowest prices.

data_rf_4<-data_rf_4 %>%
  mutate(L_Predicted_Price = round(as.numeric((gsub(",", "", L_Predicted_Price))), digits = 0)) %>% 
  mutate(R_Predicted_Price = round(as.numeric((gsub(",", "", R_Predicted_Price))), digits = 0)) %>% 
  mutate(R_Price_Gap_Percentage = round(as.numeric((gsub(",", "", R_Price_Gap_Percentage))), digits = 0)) %>% 
  mutate(L_Predicted_Price = format(L_Predicted_Price, nsmall = 0, big.mark = ",")) %>%
  mutate(R_Predicted_Price = format(R_Predicted_Price, nsmall = 0, big.mark = ",")) %>%
  mutate(R_Price_Gap_Percentage = format(R_Price_Gap_Percentage, nsmall = 0, big.mark = ",")) 

R_Recent_Table<- data_rf_4 %>% 
    #group_by(Trim) %>% 
    #slice(1) %>% 
    select(Trim, Year, Mileage, Price = Price,L_Price= L_Predicted_Price,R_Price= R_Predicted_Price,R_Gap=R_Price_Gap_Percentage) %>% 
    #mutate(Mileage = format(Mileage, nsmall = 2, big.mark = ",")) %>% 
    #head(20) %>% 
    knitr::kable()
R_Recent_Table







