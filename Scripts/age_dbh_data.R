# RWI_sept4 is too large to import.. r warns it will lag behind. So, that data
# is required separately

DBH_data <- #RWI_sept4 %>%
  select(year, plot_id_needle, tree_num, lat, long, dbh_cm, ht_m) %>%
  rename(plot = plot_id_needle)
DBH_data$year <- as.character(DBH_data$year)


DBH_data <- full_join(DBH_data, rwi_00s_plot_filtered) %>%
  unite("uniquetreeID", 2:3, sep = ".", remove = FALSE) %>%
  select(uniquetreeID, plot, tree_num, dbh_cm, ht_m, lat, long)

rwi_dat_toage <- rwi_dat %>%
  na.omit(rwi)%>%
  group_by(plot, tree_num) %>%
  summarise(age = n())

DBH_data_age <- full_join(DBH_data, rwi_dat_toage) 
DBH_data_age <- distinct(DBH_data_age, uniquetreeID, .keep_all = TRUE)
DBH_data_age <- DBH_data_age %>%
  filter(age >= 120)

write.csv(DBH_data_age, file = "/Users/kaitlynmcknight/Documents/GitHub/Tree-ring-synchrony/Data/DBH_age_data.csv")
