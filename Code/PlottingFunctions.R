
plot_qaqc_all_together <- function(df) {
  ggplot(df %>% arrange(Tree_Tag_Number, Year),
         aes(x = Year, y = Height, group = Tree_Tag_Number)) +
    geom_line(color = "gray40") +
    geom_point(aes(color = is.na(Flag)), size = 2) +
    geom_point(data = subset(df, !is.na(Flag)), aes(x = Year, y = Height), color = "red", size = 3) +
    geom_text(data = subset(df, !is.na(Flag)), aes(label = Flag), vjust = -1, size = 2.5, color = "red") +
    scale_color_manual(values = c("TRUE" = "black", "FALSE" = "red"), guide = "none") +
    labs(title = "QA/QC Height Trajectories", y = "Height (m)", x = "Year") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_qaqc_all_faceted <- function(df) {
  plot_qaqc_all_together(df) +
    facet_wrap(~ Tree_Tag_Number, scales = "free_y")
}

plot_qaqc_single <- function(df, tree_id) {
  tree_data <- df %>% filter(LindseyID == tree_id) %>% arrange(Year)
  ggplot(tree_data, aes(x = Year, y = Height)) +
    geom_line(color = "gray40") +
    geom_point(aes(color = is.na(Flag)), size = 3) +
    geom_point(data = subset(tree_data, !is.na(Flag)), color = "red", size = 4) +
    geom_text(data = subset(tree_data, !is.na(Flag)), aes(label = Flag), vjust = -1, size = 3, color = "red") +
    scale_color_manual(values = c("TRUE" = "black", "FALSE" = "red"), guide = "none") +
    labs(title = paste("Tree", tree_id), x = "", y = "Height (m)") +
    theme_minimal(base_size = 12)
}
