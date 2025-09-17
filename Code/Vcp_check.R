#QA/QC height, DBH, and biomass values for Vcp

library(dplyr)
library(ggplot2)

ppine_obs <- read.csv("./Data/CirclePlotObs.csv")%>%
  filter(Site == "PPINE",
         Species == "PIPO",
         Expr1003 == 1)
# %>%
#   select(Tree_Tag_Number, Species, Year, Height, DBH_1, Percent_Dead)

ggplot(ppine_obs, aes(Year, Height, color = Tree_Tag_Number))+
  geom_line()+
  theme_minimal()+
  theme(
    legend.position = "none"
  )


#check fields for (discretely) impossible values 
anyDuplicated(ppine_obs[, c("Tree_Tag_Number", "Year")])
colSums(is.na(ppine_obs))

ppine_obs %>%
  group_by(Tree_Tag_Number) %>%
  arrange(Year) %>%
  mutate(dHeight = Height - lag(Height),
         dDBH = DBH_1 - lag(DBH_1)) %>%
  filter(dHeight < -1 | dDBH < -1) # thresholds can be adjusted

#Base Checks

ppine_obs <- ppine_obs %>%
  mutate(
    data_flag = ifelse(
      Percent_Dead < 0 | Percent_Dead > 100 |
        Height < 0 | Height > 40 |
        DBH_1 < 0 | DBH_1 > 80,
      1, 0
    )
  )

flagged_rows <- ppine_obs %>% filter(data_flag == 1)
print(flagged_rows)

#===============================================================================

ppine_obs <- read.csv("./Data/CirclePlotObs.csv")%>%
 filter(Site == "PPINE")
#%>%
#   select(Tree_Tag_Number, Species, Year, Height, DBH_1, DBH_2, Percent_Dead)

minmax <- ppine_obs%>%
  group_by(Tree_Tag_Number, Species, Plot_Distance)%>%
  reframe(min_height = min(Height, na.rm = T),
            max_height = max(Height, na.rm = T),
          Height = Height,
          DBH_1 = DBH_1,
          Year = Year,
          Transect = Transect)%>%
  mutate(diff_height = max_height - min_height)%>%
  arrange(Tree_Tag_Number, Year)%>%
  filter(Species == "PIPO",
         diff_height > 0)


library(dplyr)

multi_transects <- minmax %>%
  group_by(Tree_Tag_Number) %>%
  summarise(n_transects = n_distinct(Transect), .groups = "drop") %>%
  filter(n_transects > 1)

n_multi_transects <- nrow(multi_transects)

n_multi_transects


#unique ID: tag number, year, transect, distance, (species?) >?


ggplot(suspect_trees, aes(Year, Height, color = Tree_Tag_Number))+
  geom_line()+
  theme_minimal()+
  theme(
    legend.position = "none"
  )



#dplyr filter join

suspect_trees <- semi_join(ppine_obs, minmax, join_by(Tree_Tag_Number))%>%
  arrange(Tree_Tag_Number, Year)


ggplot(suspect_trees, aes(Year, Height, color = Tree_Tag_Number))+
  geom_line()+
  theme_minimal()+
  theme(
    legend.position = "none"
  )

#==========================

ppine_obs <- read.csv("./Data/CirclePlotObs.csv")%>%
  filter(Site == "PPINE",
         Species == "PIPO",
         Expr1003 == 1)%>%
  filter(!is.na(Tree_Tag_Number))%>%
select(Tree_Tag_Number, Species, Year, Height, DBH_1, Percent_Dead)

ggplot(ppine_obs, aes(Year, Height, color = Tree_Tag_Number))+
  geom_line()+
  theme_minimal()+
  theme(
    legend.position = "none"
  )

test <- ppine_obs%>%
  group_by(Tree_Tag_Number)%>%
  mutate(n_obs = n_distinct(Year), .groups = "drop") %>%
  filter(n_obs > 5,
         Year %in% 2007:2015)%>%
  arrange(Tree_Tag_Number)%>%
  select(Tree_Tag_Number, Year, Height, DBH_1, Height_to_Canopy, Percent_Dead, Notes)

ggplot(test, aes(Year, DBH_1))+
  facet_wrap("Tree_Tag_Number")+
  geom_line()+
  theme_minimal()+
  theme(
    legend.position = "right"
  )
