# Unemployment project
# 1. How has the unemployment rate changed over time (by date)?
# 2. Are there any seasonal patterns in unemployment? (Analyze based on the frequency column.)
# 3. Which years or months had the highest and lowest unemployment rates?
# 4. Which Indian states have the highest and lowest unemployment rates?
# 5. Can you identify any regional disparities in employment and unemployment?
# 6. Is there a correlation between the estimated labor participation rate and the unemployment rate across different states?
# 7. What is the estimated employed percentage in each state?
# 8. Are there any states where the employed percentage significantly differs from the national average?
# 9. How does the employed percentage correlate with the unemployment rate?
# 10.What is the estimated labor participation rate in each state?
# 11.Is there any relationship between labor participation and unemployment?

# Solution
summary(Unemployment_Data$Date)

# Activate packages for data analysis and visualization
library("dplyr")
library("ggplot2")

# Import the Unemployment dataset.
Unemployment_Data <- readxl::read_xlsx("Unemployment dataset.xlsx")
View(Unemployment_Data)

# Question 1
Changes_Overtime <- Unemployment_Data %>% group_by(Date) %>% summarise(Total_Rate = sum(`Estimated Unemployment Rate (%)`))%>% 
  arrange(Date)
View(Changes_Overtime_2020)

# Per year
Changes_Overtime_2020 <- select(Unemployment_Data, Date, `Estimated Unemployment Rate (%)`)
Changes_Overtime_2020 <- Changes_Overtime_2020 %>% mutate(DATE = dmy(Date)) %>% 
  filter(substr(Date,7,10) == 2020) %>%
  group_by(Date) %>% summarise(Total_Rate = sum(`Estimated Unemployment Rate (%)`))


Changes_Overtime_2020$month <- as.integer(substr(Changes_Overtime_2020$Date,4,5))
Changes_Overtime_2020$year <- as.integer(substr(Changes_Overtime_2020$Date,7,10))

Changes_Overtime_2020 <- Changes_Overtime_2020[order(Changes_Overtime_2020$month),]
View(Changes_Overtime_2020)
ggplot(data = Changes_Overtime, mapping = aes(x = Date, y = Total_Rate)) + geom_line()
# Visuals
ggplot()

# Question 2 
Seasonal_pattern <- Unemployment_Data %>% group_by(Month = substr(Date,4,5)) %>% 
  summarise(Total = sum(`Estimated Unemployment Rate (%)`))
View(Seasonal_pattern)

# Question 3
# In terms of years 
# For the highest year
Highest_Years <- Unemployment_Data %>% group_by(Years = substr(Date,7,10)) %>% 
  summarise(Unempoyment_Rate = sum(`Estimated Unemployment Rate (%)`)) %>% 
  arrange(desc(Unempoyment_Rate)) %>% head(1)
View(Highest_Years)

# For the lowest year
Lowest_Years <- Unemployment_Data %>% group_by(Years = substr(Date,7,10)) %>% 
  summarise(Unempoyment_Rate = sum(`Estimated Unemployment Rate (%)`)) %>% 
  arrange(Unempoyment_Rate) %>% head(1)
View(Lowest_Years)

# For Highest month
Highest_Month <- Unemployment_Data %>% group_by(Month = substr(Date,4,5)) %>% 
  summarise(Unemployment_Rate = sum(`Estimated Unemployment Rate (%)`)) %>% 
  arrange(desc(Unemployment_Rate)) %>% arrange(desc(Unemployment_Rate)) %>% head(1)
View(Highest_Month)

# For the lowest month
Lowest_Month <- Unemployment_Data %>% group_by(substr(Date,4,5)) %>%
  summarise(Unemployment_Rate = sum(`Estimated Unemployment Rate (%)`)) %>% arrange(Unemployment_Rate) %>%
  head(1)

View(Lowest_Month)

# Question 4
# Region with the highest unemployment rate
Highest_Unemployment_Rate <- Unemployment_Data %>% group_by(Region) %>% summarise(Total = sum(`Estimated Unemployment Rate (%)`)) %>% arrange(desc(Total)) %>% head(1)
View(Highest_Unemployment_Rate)

# Region with the lowest unemployment rate
Lowest_Unemployment_Rate <- Unemployment_Data %>% group_by(Region) %>% 
  summarise(Total = sum(`Estimated Unemployment Rate (%)`)) %>% arrange(Total) %>% head(1)
View(Lowest_Unemployment_Rate)

# Question 5
Disperities <- Unemployment_Data %>% group_by(Region) %>% summarise(Total_Unemployed = sum(`Estimated Unemployment Rate (%)`),
                                                                    Total_Employed = sum(`Estimated Employed Rate (%)`))
View(Disperities)

# Question 6
cor(Unemployment_Data$`Estimated Labour Participation Rate (%)`, Unemployment_Data$`Estimated Unemployment Rate (%)`)

# Since the correlation is negative, there is no correlation between estimated labor participation rate and the unemployment rate

# Question 7 
Percentage <- Unemployment_Data %>% group_by(Region) %>% summarise(Sum_Employed = sum(`Estimated Employed`)) %>%
  mutate(Employed_Percentage = Sum_Employed/sum(Unemployment_Data$`Estimated Employed`) * 100)
View(Percentage)

# Question 8
States <- Unemployment_Data %>% group_by(Region) %>% summarise(Average = mean(`Estimated Employed`))

# Question 9
Employed_VS_Unemployed <- cor(Unemployment_Data$`Estimated Unemployment Rate (%)`,Unemployment_Data$`Estimated Employed Rate (%)`)
print(Employed_VS_Unemployed)
# Hence the correlation between the unemployment rate and the employed rate is negative.

# Question 10
Labour_Rate <- Unemployment_Data %>% group_by(Region) %>% summarise(Total = sum(`Estimated Labour Participation Rate (%)`))
View (Labour_Rate)
 
# Question 11
Labor_Unemployment <- cor(Unemployment_Data$`Estimated Labour Participation Rate (%)`,
                          Unemployment_Data$`Estimated Unemployment Rate (%)`)
 ggplot(data = Unemployment_Data, mapping = aes(x = `Estimated Unemployment Rate (%)`,
                        y = `Estimated Labour Participation Rate (%)`)) + 
   geom_point(color = "navy", alpha = 0.8, size = 2, pch = 10)
View(Labor_Visuals)