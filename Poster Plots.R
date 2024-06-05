
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

m3

ggsave('myplot_new.png', m3, bg='transparent')





#Faceted Scatter Plots 
f1 <- ggplot(data, aes(x = Time_Start, y = Count_per_100meter, color = Trail)) +
  geom_point() +
  geom_smooth(data = data, aes(x = Time_Start_numeric, y = Count_per_100meter), method = "lm", formula = y ~ poly(x, 2), se = TRUE) +
  labs(
       x = "Time Start",
       y = "Butterfly Count per 100m") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),   
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
  ) +
  scale_color_manual(values = c("Covered" = "blue", "Exposed" = "red")) +
  scale_x_time(labels = scales::time_format("%H"), breaks = scales::breaks_width("1 hour")) +
  facet_wrap(~ Trail, scales = "fixed")

ggsave('myplot2.png', f1, bg='transparent')

f1

f2 <- ggplot(data, aes(x = Average_Temp, y = Count_per_100meter, color = Trail)) +
  geom_point() +
  geom_smooth(data = data, aes(x = Average_Temp, y = Count_per_100meter), method = "lm", se = TRUE) +
  labs(
       x = "Average Temperature (°C)",
       y = "Butterfly Count per 100m") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),   
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),#transparent plot bg
    legend.position = "none"
  ) +
  scale_color_manual(values = c("Covered" = "blue", "Exposed" = "red")) +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top")

f2

ggsave('myplot3_new.png', f2, bg='transparent')
