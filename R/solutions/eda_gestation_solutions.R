#       _/_/    _/  _/      _/_/      _/   
#    _/    _/  _/  _/    _/    _/  _/_/    
#       _/    _/_/_/_/    _/_/_/    _/     
#    _/          _/          _/    _/      
# _/_/_/_/      _/    _/_/_/    _/_/_/     

# Exploratory Data Analysis of Gestation data
{
	library(tidyverse)
	library(mosaicData)
}
# if you don't have mosaicData, install it

data(Gestation)
slice(Gestation, 1:5)

# Activity 1 - Quick look at the data

# number of observations
count(Gestation)

# number of observations per racial group
count(Gestation, race)

# number of observations by racial group and level of mother's education
Gestation_n_race_ed <- Gestation %>% 
	group_by(race, ed) %>% 
	count() %>% 
	arrange(-n)

# Activity 2 - Further summary statistics

# mean age of mothers across all births
# ensure you use a human friendly name for the value you're creating
Gestation %>% 
	summarise(mean_age = mean(age, na.rm = TRUE))

# calculate both mothers' mean age and babies' mean weight
summarise(Gestation, 
          mean_age = mean(age, na.rm = T),
          mean_wt  = mean(wt, na.rm = T))

# an alternative method if you prefer working in long format: 
# if you want to change from wide to long format 
gest_long <- Gestation %>% 
	pivot_longer(
		cols = c(age, wt), 
		names_to = 'variable',
		values_to = "value"
	)
gest_long %>% 
	group_by(variable) %>% 
	summarise(mean = mean(value, na.rm = T))
# but this is more complicated than necessary for this example

# Activity 3 - Grouped summaries

# make a new data frame containing only id, age and race variables
gest_simple <- Gestation %>% 
	dplyr::select(id, age, race)
gest_simple %>% slice(1:5)

# calculate the mean age & sd by race
gest_simple %>% 
	group_by(race) %>% 
	summarise(mean_age = mean(age, na.rm = T),
						sd_age = sd(age, na.rm = T))
# could also use summarise_at 
gest_simple %>% 
	group_by(race) %>% 
	summarise_at(
		.vars = vars(age), 
		.funs = list(mean, sd),
		na.rm = T
		)

# Activity 4 - Extensions


# Activity 4a - Correlation

# Calculate the correlation between age and weight across all births
Gestation %>% 
	summarise(cor = cor(wt, age))

# there are NA in the data so we have two options: 
# 1 - ?cor() shows that the option `use = "complete.obs"`
Gestation %>% 
	dplyr::select(wt, age) %>% 
	summarise(cor = cor(wt, age, use = "complete.obs"))
# 2 - Or we can drop them in the dataset in the first place
Gestation %>% 
	dplyr::select(wt, age) %>% 
	drop_na() %>% 
	summarise(cor = cor(wt, age))

# Calculate the correlation between age and weight for each race group
Gestation %>% 
	group_by(race) %>% 
	summarise(cor = cor(wt, age, use = "complete.obs"))

# Activity 4b - Multiple summary statistics

# Calculate the sample mean of the ages and weights of the mothers in each race group
Gestation %>% 
	group_by(race) %>% 
	summarise_at(
		.vars = vars(wt, age),
		.funs = mean, na.rm = T
	)

# Activity 4c - Pivoting wider

# Make a wide table from the summary data frame calculated in Activity 1 that has the number of observations for each combination of mother's education level and race. Make each row is an education level and each column a race group.
# Hint: Look at the help file for `pivot_wider` for what to do with missing cells (where there is no combination of these variables) and set the argument to be 0.
gestation_n_race_ed_wide <- Gestation_n_race_ed %>% 
	pivot_wider(
		names_from = race,
		values_from = n,
		values_fill = 0
	)
gestation_n_race_ed_wide

# Activity 4d - Multiple summary statistics

# Calculate the mean, standard deviation, minimum, maximum and proportion of values missing for the mothers' ages for each race group.
# Hint: you *can* use summarise_at() for this but you could also just summarise()
# simplest way with just summarise() --------------------------------------
Gestation %>% 
	group_by(race) %>% 
	summarise(
		mean_age = mean(age, na.rm = T),
		min_age = min(age, na.rm = T),
		max_age = max(age, na.rm = T),
		na_per_group = sum(is.na(age)),
		n = n()
	) %>% 
	mutate(prop_missing = na_per_group / n)


# alternative methods ------------------------------------------------------
# an extension of previous summarise_at work to add multiple functions 
mean_min_max <- Gestation %>% 
	group_by(race) %>% 
	summarise_at(
		.vars = vars(age),
		.funs = list(mean = mean,
								 min = min,
								 max = max),
		na.rm = T
	) 
## it is a bit more complicated for the proportion missing because we do not want na.rm = TRUE
# So do it in a two stage process
prop_miss_v2 <- Gestation %>% 
	group_by(race) %>% 
	summarise(na_per_group = sum(is.na(age)),
						n = n())
#and calculate the proportion
prop_miss_v2 <- prop_miss_v2 %>% 
	mutate(prop_missing = na_per_group / n)

# bind the datasets back together
mean_min_max %>% 
	left_join(prop_miss_v2, by = "race")

# or you can do the whole thing in one step with mapping (using ~ and .)
Gestation %>% 
	group_by(race) %>% 
	summarise_at(
		.vars = vars(age),
		.funs = list(mean = ~mean(., na.rm = T),
								 min = ~min(., na.rm = T),
								 max = ~max(., na.rm = T),
								 prop_missing = ~sum(is.na(.)/length(.))
		)
	) 

