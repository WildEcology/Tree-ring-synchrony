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

age_rwi <- na.omit(age_rwi)

var_age <- ggplot(avg_age_classes, aes(x = age_class, y = rwi))+
  geom_boxplot()

growth_variability_tree <- age_rwi %>%
  group_by(uniquetreeID)%>%
  summarise(var_grow = var(rwi))

growth_variability_site <- age_rwi %>%
  group_by(plot)%>%
  summarise(var_grow = var(rwi))


age_rwi$year <- as.factor(age_rwi$year)
growth_variability_year <- age_rwi %>%
  group_by(year)%>%
  summarise(var_grow = var(rwi))

age_data <- age_rwi %>%
  select(uniquetreeID, age)
age_var_grow <- inner_join(growth_variability_tree, age_data)





ggplot(data = age_var_grow, aes(x=age, y=var_grow))+
  geom_point()
#### DBH analysis ####

growth_variability <- age_rwi %>%
  group_by(uniquetreeID)%>%
  summarise(var_grow = var(rwi))

# join w/ DBH data
DBH_data <- DBH_age_data %>%
  select(uniquetreeID, dbh_cm)

DBH_grow_var_tree <- full_join(growth_variability_tree, DBH_data)
DBH_grow_var_site <- cross_join(growth_variability_site, DBH_data)
DBH_grow_var_year <- cross_join(growth_variability_year, DBH_data)
DBH_grow_var_year$dbh_cm <- as.numeric(DBH_grow_var_year$dbh_cm)
DBH_grow_var_year <- na.omit(DBH_grow_var_year)
# create DBH classes
DBH_grow_var_year <- DBH_grow_var_year %>%
  mutate(class = case_when(dbh_cm > 0 & dbh_cm <= 20 ~ "0-20",
                           dbh_cm > 20 & dbh_cm <= 50 ~ "20-50",
                           dbh_cm > 50 ~ "50+"))

ggplot(data = DBH_grow_var_year, aes(x=dbh_cm, y=var_grow, color = class))+
  geom_violin()

DBH_grow_var_tree$dbh_cm <- as.numeric(DBH_grow_var_tree$dbh_cm)
DBH_grow_var_tree <- na.omit(DBH_grow_var_tree)
# create DBH classes
DBH_grow_var_tree <- DBH_grow_var_tree %>%
  mutate(class = case_when(dbh_cm > 0 & dbh_cm <= 20 ~ "0-20",
                           dbh_cm > 20 & dbh_cm <= 50 ~ "20-50",
                           dbh_cm > 50 ~ "50+"))

ggplot(data = DBH_grow_var_tree, aes(x=dbh_cm, y=var_grow, color = class))+
  geom_violin()

