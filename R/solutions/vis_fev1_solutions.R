#       _/_/    _/  _/      _/_/      _/   
#    _/    _/  _/  _/    _/    _/  _/_/    
#       _/    _/_/_/_/    _/_/_/    _/     
#    _/          _/          _/    _/      
# _/_/_/_/      _/    _/_/_/    _/_/_/     

# Exploratory Data Analysis of FEV1 data

library(tidyverse)
library(here)

# read the data in
fev1 <- read_csv(here("data/fev1.csv"), col_types = list('id' = 'f'))

# sample the data so that we have 20 patients with more than 6 observations
set.seed(10)

fev1_sampled <- fev1 %>% 
    count(id) %>%
    filter(n > 6) %>%
    slice_sample(n = 20) %>%
    select(id) %>%
    inner_join(fev1)

fev1_sampled

# Activity 5 - A simple scatter plot

# Calculate the correlation between age and FEV1
# (yes, this isn't strictly correct because there's repeated measures)
summarise(fev1, r = cor(age, FEV1))

# Answer: looks like a linear model may be appropriate here


# Build a plot that shows the relationship between FEV1 and age

fev1_plot <- ggplot(data = fev1, aes(x = age,
																		 y = FEV1)) +
	geom_point()

fev1_plot

# Answer: looks like a linear model may not be appropriate here after all! 

# Activity 6 - Improving the plot

# Add meaningful labels for the $x$ and $y$ axes, including units, and change the plot's colour theme from the default.

# Add a smooth line of best fit to the plot. 
fev1_plot_improved <- ggplot(data = fev1, aes(x = age,
												y = FEV1)) +
	geom_point(col = "gray60", alpha = 0.5) +
	xlab("Age (years)") +
	ylab("FEV1 (L)") +
	ggtitle("Spirometry of 300 girls in Topeka, KS") + 
	theme_bw()

fev1_plot_improved

fev1_plot_smooth <- fev1_plot_improved + 
	geom_smooth(color = "red", method = "loess", lty = 2, se=FALSE) 

fev1_plot_smooth

# Activity 7

# Activity 7a - Showing further structure
# Take a subset of the data 
# Determine a way to highlight which observations belong to the same individual in your plot
fev1_plot_id <- ggplot(data = fev1_sampled, aes(x = age,
																							y = FEV1,
																							group = id, 
																							colour = id)) +
	geom_point(alpha = 0.5) +
	xlab("Age (years)") +
	ylab("FEV1 (L)") +
	ggtitle("Spirometry of 300 girls in Topeka, KS") + 
	theme_bw() +
	theme(legend.position = "none")

fev1_plot_id
# So now observations from the same id are in the same colour, but it's a bit of a mess so this needs more 
# thought...

# Activity 7b - How many observations per individual?
## BACK TO THE FULL DATASET 

# Count the number of times that each `id` is measured and make a bar plot 
# count how many times each id appears
counts <- count(fev1, id) 

# could pre-calculate and do a geom_col() but geom_bar() does a count for us
ggplot(data = counts, aes(x = factor(n))) +  # factor(n) helps labels on x
	geom_bar() +
	xlab("Number of observations per individual")


# Activity 7c - Incorporating height

# Make a plot that shows both FEV1 and age but also includes height

# show by colour (the best?)
ggplot(data = fev1, aes(x = age, y = FEV1)) +
	geom_point(aes(col = height), alpha = 0.5) +
	xlab("Age (years)") +
	ylab("FEV1 (L)") +
	ggtitle("Spirometry of 300 girls in Topeka, KS") + 
	theme_bw()

# by size 
ggplot(data = fev1, aes(x = age, y = FEV1)) +
	geom_point(aes(size = height), pch = 13, alpha = 0.5) +
	xlab("Age (years)") +
	ylab("FEV1 (L)") +
	ggtitle("Spirometry of 300 girls in Topeka, KS") + 
	theme_bw()

# dichotomise the data (generally not a good idea if you have the raw data)
fev1 %>% 
	mutate(tall_people = as.numeric(height > 1.5)) %>% 
	ggplot(aes(x = age, y = FEV1)) +
	geom_point(col = "gray60", alpha = 0.5) +
	facet_wrap(~tall_people) + 
	xlab("Age (years)") +
	ylab("FEV1 (L)") +
	ggtitle("Spirometry of 300 girls in Topeka, KS") + 
	theme_bw()

# Activity 7d - skimr

# Use skimr::skim() to generate a summary table of the data.
# You'll need to install skimr if you don't already have it
#install.packages("skimr")
library(skimr)
skim(fev1)

# can also do this by group - here we summarise the age at recruitment for each ID 
summ_age <- fev1 %>% 
	group_by(id) %>%
	mutate(age0 = floor(min(age))) %>%
	ungroup()

skim_age <- summ_age %>% 
	group_by(age0) %>% 
	skimr::skim()

# can manipulate the skim output like a data frame
skim_age %>% 
	filter(skim_variable == "FEV1") 

# Activity 7e - GGally

#install.packages("GGally")
library(GGally)

# Generate a pairs plot with GGally::ggpairs(), for all columns except id
# You'll need to install GGally if you don't already have it
ggpairs(fev1, columns = c("age","height","FEV1"))

# change attributes of points to make a little clearer
ggpairs(fev1, columns = c("FEV1","height","age"), 
				upper = list(continuous = wrap("points", alpha = 0.3, size=0.1)),
				lower = list(continuous = "cor"), 
				diag = list(continuous = wrap('barDiag', bins = 10, fill = "lightblue", col = "blue")))


# Activity 7f - Accounting for repeat measurement

# Build a regression model to look at how FEV1 varies with age, accounting for the
# structure by including a random effect mean for each id and a spline curve for
# the effect of age

# first let's have a look at the trajectories of each id (using the group variable)
ggplot(data = fev1, aes(x = age, y = FEV1, group = id)) +
	geom_line(alpha = 0.4, col = "gray50") +
	xlab("Age (years)") +
	ylab("FEV1 (L)") +
	ggtitle("Spirometry of 300 girls in Topeka, KS") 
# generally people's lung function increases as they get older (not too surprising)
# but what is the effect of age on this development, taking into account they're 
# spirometry at baseline

#install.packages("mgcv")
library(mgcv)

model <- gamm(data = fev1, FEV1 ~ s(age), random = list(id = ~1))

pred <- data.frame(age = seq(min(fev1$age),
														 max(fev1$age),
														 length.out = 101)) %>%
	mutate(FEV1 = predict(model$gam, .))

# the use of aes(color = ...) here is non-standard
ggplot(data = fev1, aes(x = age, y = FEV1)) +
	geom_line(alpha = 0.05, aes(group = id, color = "Data")) +
	geom_smooth(aes(color = "Loess"), se=FALSE, method = "loess") +
	theme_bw() +
	geom_line(data = pred, aes(color = "GAMM")) +
	xlab("Age (years)") +
	ylab("FEV1 (L)") +
	ggtitle("Average spirometry of 300 girls in Topeka, KS") +
	scale_color_manual(values = c("Data" = "black",
																"Loess" = "red",
																"GAMM" = "blue"),
										 name = "Source")
