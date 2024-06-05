library(ggplot2)
library(scales)
library(ggpubr)
library(leaps)
library(readxl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(car)
library(gridExtra)
library(hms)
library(ggpubr)

file_path <- "C:/Users/sebru/OneDrive/Documents/Oxford Undergraduate/Biology/2nd Year/Borneo/Borneo Butterfly Data_Copy.xlsx"
data <- read_excel(file_path,  sheet = 2)

View(data)

data$Count_per_100meter <- (data$`Butterfly Count` / data$Distance)*100

#dummy variable trail
data <- data %>%
  mutate(Trail_dummy = ifelse(Trail == "Covered", 1, 0))


# Convert 'Time_Start' and 'Time_End' to hms format
data$Time_Start <- hms::as_hms(format(data$Time_Start, "%H:%M:%S"))
data$Time_End <- hms::as_hms(format(data$Time_End, "%H:%M:%S"))

data$Time_Start_numeric <- hour(data$Time_Start) * 3600 + minute(data$Time_Start) * 60 + second(data$Time_Start)

Count_v_Temp <- lm(Count_per_100meter ~ Average_Temp, data = data)
Count_v_Humid <- lm(Count_per_100meter ~ Average_Humidity, data = data)
Count_v_Trail <- lm(Count_per_100meter ~ Trail_dummy, data = data)
Count_v_Time_poly <- lm(Count_per_100meter ~ poly(Time_Start_numeric, 2), data = data)



summary(Count_v_Temp)
#p = 0.105 for regression/model

summary(Count_v_Humid)
#p = 0.124

summary(Count_v_Trail)
#p = 0.177


summary(Count_v_Time_poly)
#p = 7.05e-05
#linear component isn't significant but, as with an ANOVA with an interaction significant but main effect not, it is customary to keep linear component in model.


#select polynomial time and look at subsequent. Remove humidity due to multicollinearity .
Count_v_Time_poly_Temp <- lm(Count_per_100meter ~ poly(Time_Start_numeric,2) + Average_Temp, data = data)
Count_v_Time_poly_Trail <- lm(Count_per_100meter ~ poly(Time_Start_numeric,2) + Trail_dummy, data = data)
Count_v_Time_poly_linear <- lm(Count_per_100meter ~ poly(Time_Start_numeric,2), data = data)

#F-test for feature selection 
#model_comparison <- anova(reduced_model, full_model)

comparison <- anova(Count_v_Time_poly, Count_v_Time_poly_Temp)
print(comparison)
#p = 0.4522

comparison2 <- anova(Count_v_Time_poly, Count_v_Time_poly_Trail)
print(comparison2)
#p = 0.03662 *


#Trail qualifies 
Count_v_Time_poly_Trail_Temp <- lm(Count_per_100meter ~ poly(Time_Start_numeric, 2) + Trail_dummy + Average_Temp, data = data)


comparison4 <- anova(Count_v_Time_poly_Trail, Count_v_Time_poly_Trail_Temp)
print(comparison4)
# p = 0.5437
#tempreature doesn't qualify for the model 



#Model consists of Count = Constant + Beta1 Time + Beta2 Time^2 + Beta3 Trail



#Faceted scatter plots 
f1 <- ggplot(data, aes(x = Time_Start, y = Count_per_100meter, color = Trail)) +
  geom_point() +
  geom_smooth(data = data, aes(x = Time_Start_numeric, y = Count_per_100meter), method = "lm", formula = y ~ poly(x, 2), se = TRUE) +
  labs(title = "Butterfly Count vs. Time (Quadratic)",
       x = "Time Start",
       y = "Butterfly Count per 100m") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_color_manual(values = c("Covered" = "blue", "Exposed" = "red")) +
  scale_x_time(labels = scales::time_format("%H"), breaks = scales::breaks_width("1 hour")) +
  facet_wrap(~ Trail, scales = "fixed")


f2 <- ggplot(data, aes(x = Average_Temp, y = Count_per_100meter, color = Trail)) +
  geom_point() +
  geom_smooth(data = data, aes(x = Average_Temp, y = Count_per_100meter), method = "lm", se = TRUE) +
  labs(title = "Butterfly Count vs. Average Temperature",
       x = "Average Temperature (°C)",
       y = "Butterfly Count per 100m") +
  theme_minimal() +
  scale_color_manual(values = c("Covered" = "blue", "Exposed" = "red")) +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top")

f3 <- ggplot(data, aes(x = Average_Humidity, y = Count_per_100meter, color = Trail)) +
  geom_point() +
  geom_smooth(data = data, aes(x = Average_Humidity, y = Count_per_100meter), method = "lm", se = TRUE, level = 0.95) +
  labs(title = "Butterfly Count vs. Humidity",
       x = "Average Humidity (%)",
       y = "Butterfly Count per 100m") +
  theme_minimal() +
  scale_color_manual(values = c("Covered" = "blue", "Exposed" = "red")) +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top")

f4 <- ggplot(data, aes(x = Average_Light, y = Count_per_100meter, color = Trail)) +
  geom_point() +
  geom_smooth(data = data, aes(x = Average_Light, y = Count_per_100meter), method = "lm", se = TRUE, level = 0.95) +
  labs(title = "Butterfly Count vs. Light",
       x = "Average Light (ISO)",
       y = "Butterfly Count per 100m") +
  theme_minimal() +
  scale_color_manual(values = c("Covered" = "blue", "Exposed" = "red")) +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top")

cor_result <- cor.test(data$Trail_dummy, data$Count_per_100meter, method = "pearson")


f5 <- ggplot(data, aes(x = Trail, y = Count_per_100meter, color = Trail)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level = 0.95) +
  labs(title = "Butterfly Count vs. Trail",
       x = "Trail",
       y = "Butterfly Count per 100m") +
  theme_minimal() +
  scale_color_manual(values = c("Covered" = "blue", "Exposed" = "red")) +
  scale_x_discrete(limits = c("Exposed", "Covered")) +  # Customizing the order of the x-axis
  annotate("text", x = 1, y = max(data$Count_per_100meter), 
           label = paste("R = ", round(cor_result$estimate, 2), 
                         ", p = ", round(cor_result$p.value, 4)), 
           hjust = -0.2, vjust = 0.5, size = 3)

summary_data <- data %>%
  group_by(Trail) %>%
  summarise(mean_count = mean(Count_per_100meter), se = sd(Count_per_100meter) / sqrt(n()))

f6 <- ggplot(summary_data, aes(x = Trail, y = mean_count, fill = Trail)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_count - 2 * se, ymax = mean_count + 2 * se), width = 0.2) +
  labs(title = "Mean Count per 100 Meters by Trail",
       x = "Trail",
       y = "Mean Count per 100 Meters") +
  scale_fill_manual(values = c("Covered" = "blue", "Exposed" = "red")) + 
  scale_x_discrete(limits = c("Exposed", "Covered")) +  # Customizing the order of the x-axis
  theme_minimal()

# Combine the faceted plots into a 3x3 grid
grid.arrange(f1, f2, f3, f4, f5, f6, nrow = 3, ncol = 2)


#Unfaceted scatter plots 
p1 <- ggplot(data, aes(x = Time_Start, y = Count_per_100meter)) +
  geom_point() +
  geom_smooth(data = data, aes(x = Time_Start_numeric, y = Count_per_100meter), method = "lm", formula = y ~ poly(x, 2), se = TRUE) +
  labs(title = "Butterfly Count vs. Time (Quadratic)",
       x = "Time Start",
       y = "Butterfly Count per 100m") +
  theme_minimal() +
  facet_wrap(~ Trail) +
  scale_x_time(labels = scales::time_format("%H:%M"), breaks = scales::pretty_breaks(n = 6))


p2 <- ggplot(data, aes(x = Average_Temp, y = Count_per_100meter)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level = 0.95) +
  labs(title = "Butterfly Count vs. Average Temperature",
       x = "Average Temperature (°C)",
       y = "Butterfly Count per 100m") +
  theme_minimal() +
  stat_cor(method = "pearson", label.x = , label.y = max(data$Count_per_100meter))

p3 <- ggplot(data, aes(x = Average_Humidity, y = Count_per_100meter)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level = 0.95) +
  labs(title = "Butterfly Count vs. Humidity",
       x = "Average Humidity (%)",
       y = "Butterfly Count per 100m") +
  theme_minimal() +
  stat_cor(method = "pearson", label.x = , label.y = max(data$Count_per_100meter))

p4 <- ggplot(data, aes(x = Average_Light, y = Count_per_100meter)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level = 0.95) +
  labs(title = "Butterfly Count vs. Light",
       x = "Average Light",
       y = "Butterfly Count per 100m") +
  theme_minimal() +
  stat_cor(method = "pearson", label.x = , label.y = max(data$Count_per_100meter))

p5 <- ggplot(data, aes(x = Trail_dummy, y = Count_per_100meter)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", na.rm = TRUE) +
  labs(title = "Butterfly Count vs Trail",
       x = "Trail",
       y = "Butterfly Count per 100m") +
  theme_minimal() +
  stat_cor(method = "pearson", label.x = , label.y = max(data$Count_per_100meter))

p6 <- ggplot(data, aes(x = Trail, y = Count_per_100meter)) +
  geom_boxplot() +
  labs(title = "Box Plot of Trail vs Count_per_meter",
       x = "Trail",
       y = "Count per 100 Meters") +
  theme_minimal()

# Combine the plots into grid
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2)




#none of the independent variables have a very strong correlation with count. 
#remove light


#look for collinearity 
m1 <- ggplot(data, aes(x = Trail_dummy, y = Average_Temp)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", na.rm = TRUE) +
  labs(title = "Temperature vs Trail",
       x = "Trail",
       y = "Temperature (°C)") +
  theme_minimal() +
  stat_cor(method = "pearson", label.x = , label.y = max(data$Count_per_100meter))

#some correlation between trail and temperature 


m2 <- ggplot(data, aes(x = Trail_dummy, y = Average_Humidity)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", na.rm = TRUE) +
  labs(title = "Humidity vs Trail",
       x = "Trail",
       y = "Humidity (%)") +
  theme_minimal() +
  stat_cor(method = "pearson", label.x = , label.y = max(data$Count_per_100meter))

#another correlation between independent variables 
m3 <- ggplot(data, aes(x = Average_Temp, y = Average_Humidity)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", na.rm = TRUE) +
  labs(
       x = "Temperature(°C)",
       y = "Relative Humidity (%)") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
  ) +
  stat_cor(method = "pearson", label.x = 30, label.y = 88) +
  scale_x_continuous(expand = c(0, 0), limits = c(25, 40)) +
  scale_y_continuous(expand = c(0, 0), limits = c(40, 90)) +
  coord_cartesian(clip = 'off')



#Super strong negative correlation between temperature and humidity 
#temperature and humidity are certainly redundant. Cannot use both in the multiple regression. 

grid.arrange(m1, m2, m3, nrow = 2, ncol = 2)




full_model <- lm(Count_per_100meter ~ Average_Temp + Average_Humidity + Trail_dummy + poly(Time_Start_numeric,2), data = data)
vif(full_model)

#We can see this with VIF which essentially informs us that Temperature and Humidity are highly collinear. 
#remove humidity

full_model2 <- lm(Count_per_100meter ~ Average_Temp + Trail_dummy + poly(Time_Start_numeric,2), data = data)
vif(full_model2)

model3 <- lm(Count_per_100meter ~ Average_Temp + poly(Time_Start_numeric,2), data = data)
vif(model3)

model4 <- lm(Count_per_100meter ~ Trail_dummy + poly(Time_Start_numeric,2), data = data)
vif(model4)

model5 <- lm(Count_per_100meter ~ Trail_dummy + Average_Temp, data = data)
vif(model5)

#this tells us that there is some collinearity between temperature and time, but not so much that they cannot both be included
#we can now begin forward selection 

Count_v_Temp <- lm(Count_per_100meter ~ Average_Temp, data = data)
Count_v_Humid <- lm(Count_per_100meter ~ Average_Humidity, data = data)
Count_v_Trail <- lm(Count_per_100meter ~ Trail_dummy, data = data)
Count_v_Time <- lm(Count_per_100meter ~ poly(Time_Start_numeric, 2), data = data)

summary(Count_v_Temp)
#p = 0.105 for regression/model

summary(Count_v_Humid)
#p = 0.124

summary(Count_v_Trail)
#p = 0.177

summary(Count_v_Time)
#p = 7.05e-05

#time has by far the lowest p value, so it enters the model. 
Count_v_Time_Temp <- lm(Count_per_100meter ~ poly(Time_Start_numeric, 2) + Average_Temp, data = data)
Count_v_Time_Humidity <- lm(Count_per_100meter ~ poly(Time_Start_numeric, 2) + Average_Humidity, data = data)
Count_v_Time_Trail <- lm(Count_per_100meter ~ poly(Time_Start_numeric, 2) + Trail_dummy, data = data)

summary(Count_v_Time_Temp)
#p-value: 0.0002276
summary(Count_v_Time_Trail)
#p-value: 3.491e-05
summary(Count_v_Time_Humidity)
#p-value: 0.0002328
#trail is the lowest p-value so qualifies. 

Count_v_Time_Trail_Temp <- lm(Count_per_100meter ~ poly(Time_Start_numeric, 2) + Trail_dummy + Average_Temp, data = data)
Count_v_Time_Trail_Humidity <- lm(Count_per_100meter ~ poly(Time_Start_numeric, 2) + Trail_dummy + Average_Humidity, data = data)

summary(Count_v_Time_Trail_Temp)
#p-value: 0.0001109
summary(Count_v_Time_Trail_Humidity)
#p-value: 0.0001144

#temp is slightly better and we cannot include both Temp and Humidity
# So the model becomes Count_v_Time_Trail_Temp

#compare models with F-test for feature selection 

model_comparison <- anova(Count_v_Time, Count_v_Time_Trail)
print(model_comparison)
#p = 0.03662 *, significantly better

model_comparison2 <- anova(Count_v_Time_Trail, Count_v_Time_Trail_Temp)
print(model_comparison2)
# p= 0.5437, not significantly better. 

#Therefore, best model is Count_v_Time_Trail
summary(Count_v_Time_Trail)

#Comparison to non-dummy 
comp <- lm(Count_per_100meter ~ poly(Time_Start_numeric, 2) + Trail, data = data)
summary(comp)
#exactly the same, R is just doing it on my behalf