
# Add dates to the dataframe

dt$Date <- dates

# Reshape the data for ggplot2
dt_long <- tidyr::gather(dt, key = "Portfolio", value = "Retorno", -Date)

# Filter data for January 2023 onwards
dt_long <- dt_long[dt_long$Date >= as.Date("2023-01-01"), ]

# Plot using ggplot2
ggplot(dt_long, aes(x = Date, y = Retorno, color = Portfolio, linetype = Portfolio)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Comparação dos retornos mensalmente de 2023",
       x = "Mês",
       y = "Retorno (%)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 12),
        panel.grid.major = element_line(color = "lightgray", size = 0.5),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "bottom") +
  scale_color_manual(values = c("blue", "red", "green")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month")



dt_long_cum <- dt_long %>%
  group_by(Portfolio) %>%
  mutate(Cumulative_Return = cumsum(Retorno))

# Plot using ggplot2 with enhanced aesthetics
ggplot(dt_long_cum, aes(x = Date, y = Cumulative_Return, color = Portfolio, linetype = Portfolio)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Comparação dos retornos acumulados de 2023",
       x = "Mês",
       y = "Retorno Acumulado (%)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 12),
        panel.grid.major = element_line(color = "lightgray", size = 0.5),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "bottom") +
  scale_color_manual(values = c("blue", "red", "green")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")
