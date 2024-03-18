# load data
source(here::here("updated_cleaning_code.R"))
DBH_age_data <- read.csv(here("Data/DBH_age_data.csv"))

# clean data
DBH_age_data <- DBH_age_data %>%
  select(-X)

# prep joining dataset
rwi_00s_tree_filtered <- rwi_00s_tree_filtered %>%
  unite("uniquetreeID", 1:2, sep = ".", remove = FALSE)

# combine with RWI data
age_rwi <- left_join(DBH_age_data, rwi_00s_tree_filtered) %>%
  select(uniquetreeID, plot, tree_num, age, 9:127)

age_rwi<- age_rwi %>% pivot_longer(5:123, names_to = "year", values_to="rwi")

# investigate age/DBH classes 
A <- hist(DBH_age_data$age, plot = TRUE)
hist(DBH_age_data$dbh_cm)
median(age_rwi$age) #173.5
median(DBH_age_data$dbh_cm, na.rm=TRUE) #29.1

# lets try separating into two age classes: >200 and <200
avg_age_199 <- age_rwi %>%
  filter(age < 300) #%>%
#   group_by(plot) %>%
#   summarise(m=mean(rwi),
#             sd = sd(rwi, na.rm=FALSE))
avg_age_199$age_class <- "younger than 300 yrs"
# avg_age_199<- na.omit(avg_age_199)


avg_age_200 <- age_rwi %>%
  filter(age > 300) #%>%
#   group_by(plot) %>%
#   summarise(m=mean(rwi),
#             sd = sd(rwi, na.rm=FALSE)) 
avg_age_200$age_class <- "older than 300 yrs"  

avg_age_classes <- rbind(avg_age_199, avg_age_200)


var_age <- ggplot(avg_age_classes, aes(x = age_class, y = rwi))+
  geom_boxplot()




