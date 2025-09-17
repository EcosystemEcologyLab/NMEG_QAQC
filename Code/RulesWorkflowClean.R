library(dplyr)
library(ggplot2)
library(tidyr)

#===============================================================================
#Load data- mature ppine subset
#===============================================================================
ppine_obs <- read.csv("./Data/CirclePlotObs.csv") %>%
  filter(
    Site == "PPINE",
    Species == "PIPO",
    !is.na(Height),
    !is.na(Tree_Tag_Number)
  )
circleplotdat <- ppine_obs %>%
  group_by(Site, Transect, Expr1003, Species, Tree_Tag_Number) %>%
  filter(all(Height >= 3)) %>% # 3m arbitrary threshold for mature trees atm
  mutate(LindseyID = cur_group_id()) %>%
  ungroup() %>%
  arrange(LindseyID) %>%
  relocate(LindseyID, .before = 1)

#===============================================================================
#Apply (load functions before)
#===============================================================================
dat_qc <- qa_qc_heights_final(circleplotdat)
# 
# # check single tree
# plot_qaqc_single(dat_qc, tree_id = 58)
# dat_qc %>% filter(Tree_Tag_Number == 601)  #one LindseyID

#===============================================================================
# Stats/plots
#===============================================================================
raw_data <- dat_qc
clean_data <- dat_qc %>% filter(is.na(Flag))

site_stats_raw <- raw_data %>%
  group_by(Year) %>%
  summarise(mean_height = mean(Height, na.rm = TRUE),
            n_trees = n(), .groups = "drop")
site_stats_clean <- clean_data %>%
  group_by(Year) %>%
  summarise(mean_height = mean(Height, na.rm = TRUE),
            n_trees = n(), .groups = "drop")

#annual mean difference bt cleaned and raw data
ggplot() +
  geom_line(data = site_stats_raw, aes(x = Year, y = mean_height, color = "Raw"), size = 1, linetype = "dashed") +
  geom_line(data = site_stats_clean, aes(x = Year, y = mean_height, color = "Cleaned"), size = 1.2) +
  scale_color_manual(values = c("Raw" = "pink", "Cleaned" = "skyblue")) +
  labs(title = "", y = "Mean Height (m)", x = "Year", color = "Dataset") +
  theme_minimal(base_size = 14)

#percent flagged
flag_summary <- dat_qc %>%
  group_by(Year) %>%
  summarise(
    n_total = n(),
    n_flagged = sum(!is.na(Flag)),
    percent_flagged = 100 * n_flagged / n_total,
    .groups = "drop"
  )
p1 <- ggplot(flag_summary, aes(x = Year, y = percent_flagged)) +
  geom_col(fill = "orange") +
  geom_text(aes(label = n_flagged), vjust = -0.5, size = 3) +  # Use raw counts for labels
  theme_minimal(base_size = 14) +
  labs(
    title = "",
    y = "Percent Flagged (%)",
    x = ""
  )


#flagged per individual
flag_per_tree <- dat_qc %>%
  group_by(Tree_Tag_Number) %>%
  summarise(
    n_total = n(),
    n_flagged = sum(!is.na(Flag)),
    percent_flagged = 100 * n_flagged / n_total,
    .groups = "drop"
  )
ggplot(flag_per_tree, aes(x = reorder(Tree_Tag_Number, -percent_flagged), y = percent_flagged)) +
  geom_col(fill = "orange") +
  geom_text(aes(label = round(percent_flagged, 1)), vjust = -0.5, size = 3) +
  theme_minimal(base_size = 12) +
  labs(
    title = "",
    x = "Tree_Tag_Number",
    y = "Percent Flagged (%)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#annual boxplots
height_comparison <- bind_rows(
  raw_data %>% mutate(Dataset = "Raw"),
  clean_data %>% mutate(Dataset = "Cleaned")
)

ggplot(height_comparison, aes(x = factor(Year), y = Height, fill = Dataset)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.color = "red", outlier.shape = 1) +
  scale_fill_manual(values = c("Raw" = "pink", "Cleaned" = "skyblue")) +
  labs(x = "Year", y = "Height (m)", title = "") +
  theme_minimal(base_size = 14)

# #annual CV (all trees by year)
# cv_stats <- height_comparison %>%
#   group_by(Year, Dataset) %>%
#   summarise(
#     mean_height = mean(Height, na.rm = TRUE),
#     sd_height = sd(Height, na.rm = TRUE),
#     cv = sd_height / mean_height,
#     .groups = "drop"
#   )
# 
# ggplot(cv_stats, aes(x = Year, y = cv, color = Dataset)) +
#   geom_line(size = 1) +
#   geom_point(size = 2) +
#   scale_color_manual(values = c("Raw" = "pink", "Cleaned" = "skyblue")) +
#   labs(title = "", y = "CV", x = "Year") +
#   theme_minimal(base_size = 14)

# 
# ggplot(height_comparison, aes(x = Height, fill = Dataset)) +
#   geom_density(alpha = 0.4) +
#   scale_fill_manual(values = c("Raw" = "pink", "Cleaned" = "skyblue")) +
#   labs(title = "", x = "Height (m)", y = "Density") +
#   theme_minimal(base_size = 14)


mean_stats <- height_comparison %>%
  group_by(Year, Dataset) %>%
  summarise(mean_height = mean(Height, na.rm = TRUE), .groups = "drop")

ggplot(mean_stats, aes(x = Year, y = mean_height, color = Dataset)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Raw" = "pink", "Cleaned" = "skyblue")) +
  labs(title = "", y = "Mean Height (m)", x = "Year") +
  theme_minimal(base_size = 14)


#individual CV (ie, per time series)
cv_per_tree <- bind_rows(
  raw_data %>%
    group_by(LindseyID) %>%
    summarise(cv = sd(Height, na.rm = TRUE) / mean(Height, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(Dataset = "Raw"),
  clean_data %>%
    group_by(LindseyID) %>%
    summarise(cv = sd(Height, na.rm = TRUE) / mean(Height, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(Dataset = "Cleaned")
)

ggplot(cv_per_tree, aes(x = Dataset, y = cv, fill = Dataset)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.7) +
  scale_fill_manual(values = c("Raw" = "pink", "Cleaned" = "skyblue")) +
  labs(title = "Distribution of Height CV per Tree Record",
       y = "Coefficient of Variation (CV)",
       x = "") +
  theme_minimal(base_size = 14)



flag_summary_safe <- flag_summary %>%
  rename(total_trees = n_total,
         total_flags = n_flagged)

flag_percent <- flag_counts_simplified %>%
  left_join(flag_summary_safe %>% select(Year, total_trees, total_flags), by = "Year") %>%
  mutate(percent_of_total = 100 * n_flagged / total_trees)

top_of_bar <- flag_percent %>%
  group_by(Year) %>%
  summarise(top_y = sum(percent_of_total), total_flags = first(total_flags), .groups = "drop")

ggplot(flag_percent, aes(x = factor(Year), y = percent_of_total, fill = Flag_simple)) +
  geom_col() +
  geom_text(aes(label = n_flagged),
            position = position_stack(vjust = 0.5), size = 3, color = "white") +
  geom_text(data = top_of_bar,
            aes(x = factor(Year), y = top_y + 2, label = total_flags),  # 2% above bar
            inherit.aes = FALSE, size = 3.5, color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "",
    x = "Year",
    y = "Percent of Total Trees (%)",
    fill = "Flag Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
