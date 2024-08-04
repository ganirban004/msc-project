
#Loading Libraries
library(tidyverse)
library(readxl)

#Importing the dataset
df = read_excel(file.choose(), header = TRUE)



df =df %>% 
  select(-c('LAT','LON','TYPE of Observatory','Count of days with data'))

#Making Monthly data

# Convert the data to long format
df = df %>%
  gather(Day, Rainfall, -c('DISTRICT','STATION','YEAR')) 


# Create a Date variable
df$Date = as.Date(paste(df$YEAR, df$Day), format="%Y %j")

# Extract Month, Year
df$MONTH = format(df$Date, "%m")
df$YEAR = format(df$Date, "%Y")
df$DATE = format(df$Date, "%d")

df$MONTH = month.abb[as.integer(df$MONTH)]

#Final Data

rf_data = df %>% 
  select(-c('Day','Date'))


# Summarize Monthly Total Rainfall
daily_rainfall_summary = rf_data %>%
  group_by(DISTRICT,YEAR, MONTH, DATE) %>%
  summarise(
    No_of_Station = sum(!is.na(Rainfall)),
    Rainfall_on_day = sum(Rainfall, na.rm = TRUE) / No_of_Station
  )


#Replacing missing data


# Convert MONTH to a factor with proper ordering
month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
daily_rainfall_summary <- daily_rainfall_summary %>%
  mutate(MONTH = factor(MONTH, levels = month_order))

daily_rainfall_summary$YEAR <- as.numeric(as.character(daily_rainfall_summary$YEAR))
daily_rainfall_summary$DATE <- as.numeric(as.character(daily_rainfall_summary$DATE))


# Function to calculate the mean rainfall for a given district and month for the previous and next 10 years
calculate_mean_rainfall <- function(district, month, year) {
  year<-as.numeric(year)
  ten_years_before <- year - 10
  ten_years_after <- year + 10
  
  mean_rainfall <- daily_rainfall_summary %>%
    ungroup() %>%
    filter(DISTRICT == first(district), MONTH == month, YEAR >= ten_years_before, YEAR <= ten_years_after) %>%
    summarise(mean_rainfall = mean(Rainfall_on_day, na.rm = TRUE)) %>%
    pull(mean_rainfall)
  
  return(mean_rainfall)
}

# Replace missing data with the mean rainfall for the corresponding district, month, and year for the previous and next 10 years
daily_rainfall <- daily_rainfall_summary %>%
  group_by(DISTRICT, MONTH, DATE, YEAR) %>%
  mutate(Rainfall_on_day = ifelse(is.na(Rainfall_on_day), calculate_mean_rainfall(unique(DISTRICT), MONTH, YEAR), Rainfall_on_day))

daily_rainfall=na.omit(daily_rainfall)


df = daily_rainfall %>% 
  filter(YEAR != 2021)


# Remove the 'No_of_Station' column
df <- df[, !(names(df) %in% c("No_of_Station"))]







# Step 1: Identify Missing Districts and Years

# Create a dataframe with all possible combinations of district and year
all_combinations <- expand.grid(DISTRICT = unique(df$DISTRICT), YEAR = unique(df$YEAR))

# Find the missing combinations
missing_combinations <- anti_join(all_combinations, df, by = c("DISTRICT", "YEAR"))



# Function to check leap year
is_leap_year <- function(year) {
  (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
}

# Expand the dataset
expanded_dataset <- data.frame()

for (i in 1:nrow(missing_combinations)) {
  district <- missing_combinations$DISTRICT[i]
  year <- missing_combinations$YEAR[i]
  leap_year <- is_leap_year(year)
  
  # February has 29 days in a leap year
  if (leap_year) {
    days_in_month <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  } else {
    days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  }
  
  for (month in 1:12) {
    for (day in 1:days_in_month[month]) {
      new_row <- data.frame(
        DISTRICT = district,
        YEAR = year,
        MONTH = month,
        DATE = day,
        Rainfall_on_day = NA
      )
      expanded_dataset <- rbind(expanded_dataset, new_row)
    }
  }
}

# Show first few rows of the expanded dataset
head(expanded_dataset)


# First, convert the numeric month column to a factor
expanded_dataset$MONTH <- factor(expanded_dataset$MONTH)

# Then, use the levels of the factor to replace numeric values with short month names
levels(expanded_dataset$MONTH) <- month.abb

# Now, your MONTH column should contain short month names
head(expanded_dataset)



df <- rbind(df, expanded_dataset)


#Export the data then import it again. This way the code will take less time to run.
write.csv(df, file = "daily_rainfall_new.csv", row.names = FALSE, sep = ",")


df= read.csv(file.choose(), header = TRUE)



calculate_mean_rainfall <- function(df,district, date, month, year) {
  ten_years_before <- year - 10
  ten_years_after <- year + 10
  
  filtered_data <- df %>%
    filter(DISTRICT == district, DATE == date, MONTH == month, YEAR >= ten_years_before, YEAR <= ten_years_after)
  
  # Check if any data is filtered
  if (nrow(filtered_data) == 0) {
    cat("No data found for the given criteria.")
    return(NA)
  }
  
  mean_rainfall <- mean(filtered_data$Rainfall_on_day, na.rm = TRUE)
  
  return(mean_rainfall)
}

# Replace missing data with the mean rainfall for the corresponding district, month, and year for the previous and next 10 years
daily_rainfall <- df %>%
  group_by(DISTRICT, MONTH, DATE, YEAR) %>%
  mutate(Rainfall_on_day = ifelse(is.na(Rainfall_on_day), calculate_mean_rainfall(df, DISTRICT, DATE, MONTH, YEAR), Rainfall_on_day))

