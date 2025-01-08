# Load required libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(tidyr)
library(grid)
library(rstatix)


# Function to process rainfall data files
process_file_total <- function(file_path) {
  data <- read.csv(file_path, sep = ";", dec = ",", stringsAsFactors = FALSE)
  colnames(data)[c(1, 19)] <- c("Data", "Chuva")
  data <- data[ , c(1, 19)]  # Keep only Date and Rainfall columns
  data$Data <- as.Date(data$Data, format = "%d/%m/%Y")
  aggregated_data <- aggregate(data$Chuva, by = list(data$Data), FUN = sum)
  colnames(aggregated_data)[2] <- "Chuva"
  total_rainfall <- sum(aggregated_data$Chuva)
  return(total_rainfall)
}

# Define file paths
dry_files <- list(
  "7" = "7-DRY_PAMPULHA-20_09_2019--26_09_2019.csv",
  "15" = "15-DRY_PAMPULHA-12_09_2019--26_09_2019.csv",
  "30" = "30-DRY_PAMPULHA-28_08_2019--26_09_2019.csv"
)

wet_files <- list(
  "7" = "7-WET_PAMPULHA-19_01_2020--25_01_2020.csv",
  "15" = "15-WET_PAMPULHA-11_01_2020--25_01_2020.csv",
  "30" = "30-WET_PAMPULHA-27_12_2019--25_01_2020.csv"
)

# Calculate total rainfall
total_dry <- sapply(dry_files, process_file_total)
total_wet <- sapply(wet_files, process_file_total)

# Prepare data for plotting
data_plot <- data.frame(
  Day_Span = factor(c('7', '15', '30'), levels = c('7', '15', '30')),
  Dry = total_dry,
  Wet = total_wet
)
data_melt_total <- melt(data_plot, id.vars = 'Day_Span')

# Plot total rainfall
pdf("total_rainfall.pdf", width=3, height=4)
ggplot(data_melt_total, aes(x = as.factor(Day_Span), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.5) +
  labs(x = 'Days Prior to Sampling', 
       y = 'Accumulated Rainfall (mm)', 
       fill = "Season") +
  scale_fill_manual(values = c("#FF0000", "#0000FF")) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 800),
                     breaks = seq(0, 800, by = 100)) +
  theme_pubr() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = c(0.03, 0.99),
    legend.box = "vertical",
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.key.size = unit(0.3, 'cm'),
    legend.text = element_text(size = 8, margin = margin(l = 2)),
    legend.key.height = unit(0.4, "cm"),
    legend.title = element_text(size = 9, hjust = 0.5, vjust= -2),
    legend.spacing.x = unit(0.15, 'cm'),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0)
  ) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
dev.off()

# Analysis of average daily rainfall
average_rainfall <- data.frame(
  Day_Span = factor(c('7', '15', '30'), levels = c('7', '15', '30')),
  Dry = c(1.829, 0.853, 0.427),
  Wet = c(45.2, 30.9, 24.2)
)
data_melt_avg <- melt(average_rainfall, id.vars = 'Day_Span')

# Plot average rainfall
pdf("average_rainfall.pdf", width=3, height=4)
ggplot(data_melt_avg, aes(x = as.factor(Day_Span), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.5) +
  labs(x = 'Days Prior to Sampling', 
       y = 'Average Daily Rainfall (mm)', 
       fill = "Season") +
  scale_fill_manual(values = c("#FF0000", "#0000FF")) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 60),
                     breaks = seq(0, 60, by = 15)) +
  theme_pubr() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = c(0.03, 0.99),
    legend.box = "vertical",
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.key.size = unit(0.3, 'cm'),
    legend.text = element_text(size = 8, margin = margin(l = 2)),
    legend.key.height = unit(0.4, "cm"),
    legend.title = element_text(size = 9, hjust = 0.5, vjust= -2),
    legend.spacing.x = unit(0.15, 'cm'),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0)
  ) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
dev.off()

# Light radiation analysis
# Calculate light hours per day (threshold > 3 KJ/m2)
wet_light_hours_per_day <- data_wet_10 %>%
  filter(`Radiação KJ/m2` > 3) %>%
  group_by(Data) %>%
  summarise(light_hours = n())

# Calculate mean radiation during light hours
wet_mean_radiation_per_hour <- data_wet_10 %>%
  filter(`Radiação KJ/m2` > 3) %>%
  group_by(Data) %>%
  summarise(Radiação_média = mean(`Radiação KJ/m2`, na.rm = TRUE))

# Combine data
wet_combined_data <- left_join(wet_mean_radiation_per_hour, wet_light_hours_per_day, by = "Data") %>%
  mutate(Season = "Wet")

# Repeat for dry season
dry_light_hours_per_day <- data_dry_10 %>%
  filter(`Radiação KJ/m2` > 3) %>%
  group_by(Data) %>%
  summarise(light_hours = n())

dry_mean_radiation_per_hour <- data_dry_10 %>%
  filter(`Radiação KJ/m2` > 3) %>%
  group_by(Data) %>%
  summarise(Radiação_média = mean(`Radiação KJ/m2`, na.rm = TRUE))

dry_combined_data <- left_join(dry_mean_radiation_per_hour, dry_light_hours_per_day, by = "Data") %>%
  mutate(Season = "Dry")

combined_data <- bind_rows(wet_combined_data, dry_combined_data)

# Calculate average light hours
average_light_hours <- combined_data %>%
  group_by(Season) %>%
  summarise(avg_light_hours = mean(light_hours, na.rm = TRUE))

# Calculate effect size for radiation
effect_size_avg_hour_radiation <- wilcox_effsize(Radiação_média ~ Season, data = combined_data)
effect_size_value_avg_hour_radiation <- round(effect_size_avg_hour_radiation$effsize, 3)

# Create legend for light hours
light_hours_legend <- grobTree(
  rectGrob(gp = gpar(fill = NA, col = NA)),
  textGrob(
    label = "Avg. Light\n(Hrs/Day)",
    x = 3.1,
    y = 4000,
    hjust = 0.5,
    gp = gpar(col = "black", fontsize = 7, fontface = "bold")
  ),
  textGrob(
    label = paste0("Dry: ", round(average_light_hours$avg_light_hours[average_light_hours$Season == "Dry"], 2), "\n",
                   "Wet: ", round(average_light_hours$avg_light_hours[average_light_hours$Season == "Wet"], 2)),
    x = 3.1,
    y = 3700,
    hjust = 0.5,
    gp = gpar(col = "black", fontsize = 6)
  )
)

# Plot mean light radiation
pdf("mean_light_radiation.pdf", width=3, height=4)
ggplot(combined_data, aes(x = Season, y = Radiação_média, fill = Season)) +
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.2) +
  geom_jitter(shape = 24, position = position_jitter(0.05)) +
  scale_fill_manual(values = season_colors) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 2500),
    breaks = seq(0, 2500, by = 250)
  ) +
  ylab(expression("Daily Mean Light Radiation (KJ/m"^2*"/h)")) +
  theme_pubr() +
  theme(legend.position = "none") +
  annotation_custom(light_hours_legend, xmin = 0.26, xmax = 0.41, ymin = 2, ymax = 2.6) +
  annotate("text", x = 1.5, y = 2000, 
           label = paste0("Effect Size: ", 
                          round(effect_size_value_avg_hour_radiation, 2)), 
           size = 2.75) +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", paired = FALSE)
dev.off()