# IMPORTING DATASET AND LIBRARIES
library(tidyverse)
library(modifiedmk)
library(forecast)
daily_rainfall_s24pgs <- read.csv(file.choose(), header = TRUE)   # extract district data from the final wb data
daily_rainfall_s24pgs$DISTRICT <- as.factor(daily_rainfall_s24pgs$DISTRICT)
daily_rainfall_s24pgs$MONTH <- factor(daily_rainfall_s24pgs$MONTH, levels = c("Jan", "Feb", "Mar",
                                                                              "Apr", "May", "Jun", "Jul", "Aug", 
                                                                              "Sep", "Oct", "Nov", "Dec"))
daily_rainfall_s24pgs <- arrange(daily_rainfall_s24pgs, YEAR, MONTH)

# STANDARD DEVIATION FUNCTION
sdp <- function(x) {
  sqrt(mean((x-mean(x))^2))
}

# YEARLY PLOT
s24pgs_yearly <- daily_rainfall_s24pgs %>% 
  group_by(YEAR) %>% summarise(TOTAL.RAINFALL=round(sum(RAINFALL.ON.DAY), 4))
ggplot(s24pgs_yearly, aes(x = YEAR, y = TOTAL.RAINFALL)) + geom_boxplot(color = "red", fill="orange", alpha=0.3) + 
  geom_line(color = "blue") + geom_point(color = "black")  + 
  labs(title = "YEARWISE TOTAL RAINFALL AT SOUTH 24 PARGANAS", x = "Year", y = "Total Rainfall (in mm)") + theme_minimal()
mean(s24pgs_yearly$TOTAL.RAINFALL)
sdp(s24pgs_yearly$TOTAL.RAINFALL)
view(s24pgs_yearly[s24pgs_yearly$TOTAL.RAINFALL==max(s24pgs_yearly$TOTAL.RAINFALL), ])
view(s24pgs_yearly[s24pgs_yearly$TOTAL.RAINFALL==min(s24pgs_yearly$TOTAL.RAINFALL), ])

# MONTHLY PLOT
s24pgs_monthly <- daily_rainfall_s24pgs %>% 
  group_by(YEAR, MONTH) %>% summarise(TOTAL.RAINFALL=sum(RAINFALL.ON.DAY)) %>% 
  group_by(MONTH) %>% summarise(MEAN=round(mean(TOTAL.RAINFALL), 4), SD=round(sdp(TOTAL.RAINFALL), 4), 
                                MAXIMUM=round(max(TOTAL.RAINFALL), 4), MINIMUM=round(min(TOTAL.RAINFALL), 4))
ggplot(s24pgs_monthly, aes(x=" ", y=MEAN*100/sum(MEAN),RAINFALL, fill=MONTH)) + 
  geom_col(color="black") + geom_text(aes(label=round(MEAN*100/sum(MEAN), 2)), 
                                      position = position_stack(vjust = 0.5)) + coord_polar(theta="y") + 
  labs(title="DISTRIBUTION OF RAINFALL OVER MONTHS", x=" ", y=" ") + theme_minimal()
write.table(s24pgs_monthly, file="s24pgs_monthly_stat.csv", row.names=FALSE, sep=',')
#export the data to make table in overleaf

s24pgs_monthly_1 <- daily_rainfall_s24pgs[daily_rainfall_s24pgs$MONTH=="Dec", ] %>%
  group_by(YEAR, MONTH) %>% summarise(TOTAL.RAINFALL=sum(RAINFALL.ON.DAY))
#change the month one by one to get images and other data for different months
ggplot(s24pgs_monthly_1, aes(x = YEAR, y = TOTAL.RAINFALL)) + geom_boxplot(color = "red", fill="orange", alpha=0.4) + 
  geom_line(color = "blue") + geom_point(color = "black")  + 
  labs(title = "DECEMBER", x = "Year", y = "Total Rainfall (in mm)")
view(s24pgs_monthly_1[s24pgs_monthly_1$TOTAL.RAINFALL==max(s24pgs_monthly_1$TOTAL.RAINFALL), ])
view(s24pgs_monthly_1[s24pgs_monthly_1$TOTAL.RAINFALL==min(s24pgs_monthly_1$TOTAL.RAINFALL), ])

# SEASONAL PLOT
s24pgs_seas <- transform(daily_rainfall_s24pgs, 
                             SEASON = case_when(
                               MONTH %in% c("Mar", "Apr", "May") ~ "Pre-Monsoon",
                               MONTH %in% c("Jun", "Jul", "Aug", "Sep") ~ "Monsoon",
                               MONTH %in% c("Oct", "Nov") ~ "Post-Monsoon",
                               MONTH %in% c("Dec", "Jan", "Feb") ~ "Winter"))
s24pgs_seas$SEASON <- factor(s24pgs_seas$SEASON, 
                             levels=c("Pre-Monsoon", "Monsoon", "Post-Monsoon", "Winter"))
arrange(s24pgs_seas, YEAR, SEASON)
s24pgs_seasonal <- s24pgs_seas %>% 
  group_by(YEAR, SEASON) %>% summarise(TOTAL.RAINFALL=sum(RAINFALL.ON.DAY)) %>% 
  group_by(SEASON) %>% summarise(MEAN=round(mean(TOTAL.RAINFALL), 4), SD=round(sdp(TOTAL.RAINFALL), 4), 
                                 MAXIMUM=round(max(TOTAL.RAINFALL), 4), MINIMUM=round(min(TOTAL.RAINFALL), 4))
ggplot(s24pgs_seasonal, aes(x=" ", y=MEAN*100/sum(MEAN), RAINFALL, fill=SEASON)) + 
  geom_col(color="black") + geom_text(aes(label=round(MEAN*100/sum(MEAN), 2)), 
                                      position = position_stack(vjust = 0.5)) + coord_polar(theta="y") + 
  labs(title="DISTRIBUTION OF RAINFALL OVER SEASONS", x=" ", y=" ") + theme_minimal()
write.table(s24pgs_seasonal, file="s24pgs_seasonal_stat.csv", row.names=FALSE, sep=',')
#export the data to make table in overleaf

s24pgs_seasonal_1 <- s24pgs_seas[s24pgs_seas$SEASON=="Winter", ] %>%
  group_by(YEAR, SEASON) %>% summarise(TOTAL.RAINFALL=sum(RAINFALL.ON.DAY))
# change the season one by one to get images and other data for different seasons
ggplot(s24pgs_seasonal_1, aes(x = YEAR, y = TOTAL.RAINFALL)) + geom_boxplot(color = "red", fill="orange", alpha=0.4) + 
  geom_line(color = "blue") + geom_point(color = "black")  + 
  labs(title = "WINTER", x = "Year", y = "Total Rainfall (in mm)")
view(s24pgs_seasonal_1[s24pgs_seasonal_1$TOTAL.RAINFALL==max(s24pgs_seasonal_1$TOTAL.RAINFALL), ])
view(s24pgs_seasonal_1[s24pgs_seasonal_1$TOTAL.RAINFALL==min(s24pgs_seasonal_1$TOTAL.RAINFALL), ])

# MAKING COUNT DATA
count_s24pgs <- daily_rainfall_s24pgs
count_s24pgs$NO.RAIN <- count_s24pgs$RAINFALL.ON.DAY <= 2.5
count_s24pgs$LIGHT.RAIN <- count_s24pgs$RAINFALL.ON.DAY > 2.5 & count_s24pgs$RAINFALL.ON.DAY <= 15.5
count_s24pgs$MODERATE.RAIN <- count_s24pgs$RAINFALL.ON.DAY > 15.5 & count_s24pgs$RAINFALL.ON.DAY <= 64.5
count_s24pgs$HEAVY.RAIN <- count_s24pgs$RAINFALL.ON.DAY > 64.5

# YEARLY PLOT
count_s24pgs_yearly <- count_s24pgs %>% 
  group_by(YEAR) %>% summarise(No=sum(NO.RAIN), 
                               Light=sum(LIGHT.RAIN), 
                               Moderate=sum(MODERATE.RAIN), 
                               Heavy=sum(HEAVY.RAIN))
count_s24pgs_yearly_1 <- count_s24pgs_yearly %>% 
  gather(Category, COUNT, -(1))
count_s24pgs_yearly_1$Category <- factor(count_s24pgs_yearly_1$Category, levels=c("No", "Light", "Moderate", "Heavy"))
count_s24pgs_yearly_1 <- arrange(count_s24pgs_yearly_1, YEAR, Category)
ggplot(count_s24pgs_yearly_1, aes(x=YEAR, y=COUNT)) + geom_line(aes(color=Category)) + 
  scale_color_manual(name="Category", labels=c("No", "Light", "Moderate", "Heavy"), 
                     values=c("yellow3", "green4", "blue", "red")) + theme_minimal()
count_s24pgs_yearly_stat <- count_s24pgs_yearly_1 %>% 
  group_by(Category) %>% summarise(MEAN=round(mean(COUNT), 4), SD=round(sdp(COUNT), 4), MAXIMUM=max(COUNT), MINIMUM=min(COUNT))
write.table(count_s24pgs_yearly_stat, file = "count_s24pgs_yearly_stat.csv", row.names = FALSE, sep = ',')
view(count_s24pgs_yearly[count_s24pgs_yearly$Heavy==min(count_s24pgs_yearly$Heavy), ])

# MONTHLY PLOT
count_s24pgs_monthly_no <- count_s24pgs %>% 
  group_by(YEAR, MONTH) %>% summarise(NO.RAIN=sum(NO.RAIN)) %>% 
  group_by(MONTH) %>% summarise(MEAN=round(mean(NO.RAIN), 4), SD=round(sdp(NO.RAIN), 4), 
                                MAXIMUM=max(NO.RAIN), MINIMUM=min(NO.RAIN))
ggplot(count_s24pgs_monthly_no, aes(x=" ", y=MEAN*100/sum(MEAN), RAINFALL, fill=MONTH)) + 
  geom_col(color="black") + geom_text(aes(label=round(MEAN*100/sum(MEAN), 2)), 
                                      position = position_stack(vjust = 0.5)) + coord_polar(theta="y") + 
  labs(title="DISTRIBUTION OF NO RAINFALL OVER MONTHS", x=" ", y=" ") + theme_minimal()

count_s24pgs_monthly_light <- count_s24pgs %>% 
  group_by(YEAR, MONTH) %>% summarise(LIGHT.RAIN=sum(LIGHT.RAIN)) %>% 
  group_by(MONTH) %>% summarise(MEAN=round(mean(LIGHT.RAIN), 4), SD=round(sdp(LIGHT.RAIN), 4), 
                                MAXIMUM=max(LIGHT.RAIN), MINIMUM=min(LIGHT.RAIN))
ggplot(count_s24pgs_monthly_light, aes(x=" ", y=MEAN*100/sum(MEAN), RAINFALL, fill=MONTH)) + 
  geom_col(color="black") + geom_text(aes(label=round(MEAN*100/sum(MEAN), 2)), 
                                      position = position_stack(vjust = 0.5)) + coord_polar(theta="y") + 
  labs(title="DISTRIBUTION OF LIGHT RAINFALL OVER MONTHS", x=" ", y=" ") + theme_minimal()

count_s24pgs_monthly_moderate <- count_s24pgs %>% 
  group_by(YEAR, MONTH) %>% summarise(MODERATE.RAIN=sum(MODERATE.RAIN)) %>% 
  group_by(MONTH) %>% summarise(MEAN=round(mean(MODERATE.RAIN), 4), SD=round(sdp(MODERATE.RAIN), 4), 
                                MAXIMUM=max(MODERATE.RAIN), MINIMUM=min(MODERATE.RAIN))
ggplot(count_s24pgs_monthly_moderate, aes(x=" ", y=MEAN*100/sum(MEAN), RAINFALL, fill=MONTH)) + 
  geom_col(color="black") + geom_text(aes(label=round(MEAN*100/sum(MEAN), 2)), 
                                      position = position_stack(vjust = 0.5)) + coord_polar(theta="y") + 
  labs(title="DISTRIBUTION OF MODERATE RAINFALL OVER MONTHS", x=" ", y=" ") + theme_minimal()

count_s24pgs_monthly_heavy <- count_s24pgs %>% 
  group_by(YEAR, MONTH) %>% summarise(HEAVY.RAIN=sum(HEAVY.RAIN)) %>% 
  group_by(MONTH) %>% summarise(MEAN=round(mean(HEAVY.RAIN), 4), SD=round(sdp(HEAVY.RAIN), 4), 
                                MAXIMUM=max(HEAVY.RAIN), MINIMUM=min(HEAVY.RAIN))
ggplot(count_s24pgs_monthly_heavy, aes(x=" ", y=MEAN*100/sum(MEAN), RAINFALL, fill=MONTH)) + 
  geom_col(color="black") + geom_text(aes(label=round(MEAN*100/sum(MEAN), 2)), 
                                      position = position_stack(vjust = 0.5)) + coord_polar(theta="y") + 
  labs(title="DISTRIBUTION OF HEAVY RAINFALL OVER MONTHS", x=" ", y=" ") + theme_minimal()

count_s24pgs_monthly <- count_s24pgs %>% 
  group_by(YEAR, MONTH) %>% summarise(No=sum(NO.RAIN), 
                                      Light=sum(LIGHT.RAIN), 
                                      Moderate=sum(MODERATE.RAIN), 
                                      Heavy=sum(HEAVY.RAIN))
count_s24pgs_monthly_1 <- count_s24pgs_monthly %>% 
  gather(Category, COUNT, -(1:2))
count_s24pgs_monthly_1$Category <- factor(count_s24pgs_monthly_1$Category, levels=c("No", "Light", "Moderate", "Heavy"))
count_s24pgs_monthly_1 <- arrange(count_s24pgs_monthly_1, YEAR, MONTH, Category)
ggplot(count_s24pgs_monthly_1[count_s24pgs_monthly_1$MONTH=="Apr", ], aes(x=YEAR, y=COUNT)) + 
  geom_line(aes(color=Category)) + scale_color_manual(name="Category", labels=c("No", "Light", "Moderate", "Heavy"), 
                     values=c("yellow3", "green4", "blue", "red"))
#keep changing months to get plots for different months

# SEASONAL PLOT
count_s24pgs_seas <- transform(count_s24pgs, 
                         SEASON = case_when(
                           MONTH %in% c("Mar", "Apr", "May") ~ "Pre-Monsoon",
                           MONTH %in% c("Jun", "Jul", "Aug", "Sep") ~ "Monsoon",
                           MONTH %in% c("Oct", "Nov") ~ "Post-Monsoon",
                           MONTH %in% c("Dec", "Jan", "Feb") ~ "Winter"))
count_s24pgs_seas$SEASON <- factor(count_s24pgs_seas$SEASON, 
                             levels=c("Pre-Monsoon", "Monsoon", "Post-Monsoon", "Winter"))
count_s24pgs_seas <- arrange(count_s24pgs_seas, YEAR, SEASON)
count_s24pgs_seasonal_no <- count_s24pgs_seas %>% 
  group_by(YEAR, SEASON) %>% summarise(NO.RAIN=sum(NO.RAIN)) %>% 
  group_by(SEASON) %>% summarise(MEAN=round(mean(NO.RAIN), 4), SD=round(sdp(NO.RAIN), 4), 
                                MAXIMUM=max(NO.RAIN), MINIMUM=min(NO.RAIN))
ggplot(count_s24pgs_seasonal_no, aes(x=" ", y=MEAN*100/sum(MEAN), RAINFALL, fill=SEASON)) + 
  geom_col(color="black") + geom_text(aes(label=round(MEAN*100/sum(MEAN), 2)), 
                                      position = position_stack(vjust = 0.5)) + coord_polar(theta="y") + 
  labs(title="DISTRIBUTION OF NO RAINFALL OVER SEASONS", x=" ", y=" ") + theme_minimal()
count_s24pgs_seasonal_light <- count_s24pgs_seas %>% 
  group_by(YEAR, SEASON) %>% summarise(LIGHT.RAIN=sum(LIGHT.RAIN)) %>% 
  group_by(SEASON) %>% summarise(MEAN=round(mean(LIGHT.RAIN), 4), SD=round(sdp(LIGHT.RAIN), 4), 
                                 MAXIMUM=max(LIGHT.RAIN), MINIMUM=min(LIGHT.RAIN))
ggplot(count_s24pgs_seasonal_light, aes(x=" ", y=MEAN*100/sum(MEAN), RAINFALL, fill=SEASON)) + 
  geom_col(color="black") + geom_text(aes(label=round(MEAN*100/sum(MEAN), 2)), 
                                      position = position_stack(vjust = 0.5)) + coord_polar(theta="y") + 
  labs(title="DISTRIBUTION OF LIGHT RAINFALL OVER SEASONS", x=" ", y=" ") + theme_minimal()
count_s24pgs_seasonal_moderate <- count_s24pgs_seas %>% 
  group_by(YEAR, SEASON) %>% summarise(MODERATE.RAIN=sum(MODERATE.RAIN)) %>% 
  group_by(SEASON) %>% summarise(MEAN=round(mean(MODERATE.RAIN), 4), SD=round(sdp(MODERATE.RAIN), 4), 
                                 MAXIMUM=max(MODERATE.RAIN), MINIMUM=min(MODERATE.RAIN))
ggplot(count_s24pgs_seasonal_moderate, aes(x=" ", y=MEAN*100/sum(MEAN), RAINFALL, fill=SEASON)) + 
  geom_col(color="black") + geom_text(aes(label=round(MEAN*100/sum(MEAN), 2)), 
                                      position = position_stack(vjust = 0.5)) + coord_polar(theta="y") + 
  labs(title="DISTRIBUTION OF MODERATE RAINFALL OVER SEASONS", x=" ", y=" ") + theme_minimal()

count_s24pgs_seasonal_heavy <- count_s24pgs_seas %>% 
  group_by(YEAR, SEASON) %>% summarise(HEAVY.RAIN=sum(HEAVY.RAIN)) %>% 
  group_by(SEASON) %>% summarise(MEAN=round(mean(HEAVY.RAIN), 4), SD=round(sdp(HEAVY.RAIN), 4), 
                                 MAXIMUM=max(HEAVY.RAIN), MINIMUM=min(HEAVY.RAIN))
ggplot(count_s24pgs_seasonal_heavy, aes(x=" ", y=MEAN*100/sum(MEAN), RAINFALL, fill=SEASON)) + 
  geom_col(color="black") + geom_text(aes(label=round(MEAN*100/sum(MEAN), 2)), 
                                      position = position_stack(vjust = 0.5)) + coord_polar(theta="y") + 
  labs(title="DISTRIBUTION OF HEAVY RAINFALL OVER SEASONS", x=" ", y=" ") + theme_minimal()
count_s24pgs_seasonal <- count_s24pgs_seas %>% 
  group_by(YEAR, SEASON) %>% summarise(No=sum(NO.RAIN), 
                                      Light=sum(LIGHT.RAIN), 
                                      Moderate=sum(MODERATE.RAIN), 
                                      Heavy=sum(HEAVY.RAIN))
count_s24pgs_seasonal_1 <- count_s24pgs_seasonal %>% 
  gather(Category, COUNT, -(1:2))
count_s24pgs_seasonal_1$Category <- factor(count_s24pgs_seasonal_1$Category, levels=c("No", "Light", "Moderate", "Heavy"))
count_s24pgs_seasonal_1 <- arrange(count_s24pgs_seasonal_1, YEAR, Category, SEASON)
ggplot(count_s24pgs_seasonal_1[count_s24pgs_seasonal_1$SEASON=="Winter", ], aes(x=YEAR, y=COUNT)) + 
  geom_line(aes(color=Category)) + scale_color_manual(name="Category", labels=c("No", "Light", "Moderate", "Heavy"), 
                                                      values=c("yellow3", "green4", "blue", "red"))
#keep changing months to get plots for different months

# REGRESSION
s24pgs_var <- s24pgs_seas[s24pgs_seas$SEASON=="Monsoon", ] %>%  
  group_by(YEAR, SEASON) %>% summarise(VARIANCE=(sdp(RAINFALL.ON.DAY))^2)
# change the season one by one to get images and other data for different seasons
reg_model <- glm(VARIANCE~YEAR, data = s24pgs_var)
summary(reg_model)
ggplot(s24pgs_var, aes(x = YEAR, y = VARIANCE)) + geom_point(color = "black") + 
  geom_abline(intercept = reg_model$coefficients[1], slope = reg_model$coefficients[2], color = "red", size = 1) + 
  labs(title = "SOUTH 24 PARGANAS", x = "Year", y = "Variance") + theme_minimal() + 
  theme(
    panel.grid.major = element_line(color = "grey50", size = 0.5),
    panel.grid.minor = element_line(color = "grey50", size = 0.25))
t_stat <- reg_model$coefficients[2]/summary(reg_model)$coefficients["YEAR", "Std. Error"]
p_value_greater <- 1 - pt(t_stat, df = length(s24pgs_var$YEAR) - 2)

# MANN-KENDALL TEST
mkttest(s24pgs_var$VARIANCE)

# ARIMA FORECASTING
june_rainfall_s24pgs <- daily_rainfall_s24pgs[daily_rainfall_s24pgs$MONTH %in% c("Jun", "Jul", "Aug", "Sep"), ]
june_yearly_s24pgs <- june_rainfall_s24pgs %>% 
  group_by(YEAR) %>% summarise(TOTAL.RAINFALL = sum(RAINFALL.ON.DAY))
arima_model <- auto.arima(june_yearly_s24pgs$TOTAL.RAINFALL, xreg = june_yearly_s24pgs$YEAR)
arima_forecast <- forecast(arima_model, xreg = c(2021:2030))
autoplot(arima_forecast, flwd = 1) + labs(title = "MALDA", x = "Time Point", y = "Total Rainfall (in mm)")+ theme_minimal() + 
  theme(
    panel.grid.major = element_line(color = "grey50", size = 0.5),
    panel.grid.minor = element_line(color = "grey50", size = 0.25))

