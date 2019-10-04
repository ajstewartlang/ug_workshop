library(tidyverse)
library(viridis)
library(ggbeeswarm)
library(ggthemes)

# Data tidying and wrangling ####
# Create data for 10,000 people - each with measures of Working Memory (WM), IQ, and 
# reading Comprehension (Comp)

data1 <- read_csv("data_files/data1.csv")

# Create data for 48 participants (all present in data) taking part in an experiment

dataRT <- read_csv("data_files/dataRT.csv")

# Combine the data1 and dataRT datasets using the inner_join() function
# Map this onto a new variable we're calling dataRT_all
dataRT_all <- inner_join(data1, dataRT, by = "id")

data_transformed <- mutate(dataRT_all, log_simple = log(simple_sentence), 
                            log_complex = log(complex_sentence))

filtered_data <- filter(data_transformed, id != 2006)

data_long <- gather(dataRT, "condition", "rt", c("simple_sentence", "complex_sentence"))
View(data_long)

data_wide <- spread(data_long, "condition", "rt")
View(data_wide)

# Use dplyr to get some summary statistics from the RT dataset using the pipe operator
data_long %>% 
  group_by(condition) %>% 
  summarise(Mean = mean(rt), Min = min(rt), Max = max(rt), SD = sd(rt))

# Recode one column capturing 2x2 and then splitting
# First create the data set - 24 items each with one RT measure for each of 4 conditions

my_data <- read_csv("data_files/my_data.csv")
my_data

# Recode condition columns follows:
# Condition 1 = prime A, target A
# Condition 2 = prime A, target B
# Condition 3 = prime B, target A
# Condition 4 = prime B, target B

my_data <- my_data %>% 
  mutate(condition = recode(condition, 
                            "1" = "primeA_targetA",
                            "2" = "primeA_targetB", 
                            "3" = "primeB_targetA", 
                            "4" = "primeB_targetB"))

# now separate the Condition column using "_" as our separator
my_data <- separate(my_data, col = "condition", into = c("prime", "target"), sep = "_")
my_data

# combine again
my_data <- unite(my_data, col = "condition", c("prime", "target"), sep = "_")
wide_data <- spread(my_data, key = "condition", value = "rt")
wide_data

# or using the pipe %>%
my_data %>% 
  unite(col = "condition", c("prime", "target"), sep = "_") %>%
  spread(key = "condition", value = "rt")

# Visualisation ####
# Bar Graph
data_summ <- data_long %>% group_by(condition) %>% summarise(Mean = mean(rt), sd = sd(rt))

ggplot(data_summ, aes (x = condition, y = Mean, group = condition, 
                       fill = condition, ymin = Mean - sd, ymax = Mean + sd)) + 
  geom_bar(stat = "identity", width = .5) + 
  geom_errorbar(width = .25) +  
  ggtitle("Bar chart with Error Bars") + 
  guides(fill = FALSE) 

# When boxplots can mislead

data2 <- read_csv("data_files/data2.csv")

ggplot(data2, aes(x = group, y = rt)) + geom_boxplot()
ggplot(data2, aes(x = group, y = rt)) + geom_jitter(size = 2, width = .1, alpha = .25)
ggplot(data2, aes(x = group, y = rt)) + geom_boxplot() + geom_jitter(size = 2, width = .1, alpha = .25)
ggplot(data2, aes(x = group, y = rt)) + geom_violin() + geom_jitter(width = .1, alpha = .5)

# Violin Plot
ggplot(data_long, aes(x = condition, y = rt, group = condition, fill = condition)) + 
  geom_violin() + 
  geom_jitter(alpha = .25, position = position_jitter(0.05)) + 
  guides(colour = FALSE, fill = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 1) 

# Beeswarm plot
ggplot(data_long, aes(x = condition, y = rt, group = condition, fill = condition)) + 
  geom_beeswarm(alpha = .25) + 
  guides(colour = FALSE, fill = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 1) 

data_long %>%
  mutate(Condition = recode(condition, 
                            "complex_sentence" = "Complex Sentence",
                            "simple_sentence" = "Simple Sentence")) %>%
  ggplot(aes(x = Condition, y = rt, group = Condition, fill = Condition)) + 
  geom_beeswarm(alpha = .25) + 
  guides(colour = FALSE, fill = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 1) +
  labs(title = "Plot of Reaction Time (ms.) by Condition",
       x = "Condition", 
       y = "Reaction Time (ms.)") +
  theme(text = element_text(size = 15))

# Five Thirty Eight theme
data_long %>%
  mutate(Condition = recode(condition, 
                            "complex_sentence" = "Complex Sentence",
                            "simple_sentence" = "Simple Sentence")) %>%
  ggplot(aes(x = Condition, y = rt, group = Condition, fill = Condition)) + 
  geom_beeswarm(alpha = .25) + 
  guides(colour = FALSE, fill = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 1) +
  labs(title = "Plot of Reaction Time (ms.) by Condition",
       x = "Condition", 
       y = "Reaction Time (ms.)") +
  theme_fivethirtyeight()

# Economist theme
data_long %>%
  mutate(Condition = recode(condition, 
                            "complex_sentence" = "Complex Sentence",
                            "simple_sentence" = "Simple Sentence")) %>%
  ggplot(aes(x = Condition, y = rt, group = Condition, fill = Condition)) + 
  geom_beeswarm(alpha = .25) + 
  guides(colour = FALSE, fill = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 1) +
  labs(title = "Plot of Reaction Time (ms.) by Condition",
       x = "Condition", 
       y = "Reaction Time (ms.)") +
  theme_economist()

my_plot <- data_long %>%
  mutate(Condition = recode(condition, 
                            "complex_sentence" = "Complex Sentence",
                            "simple_sentence" = "Simple Sentence")) %>%
  ggplot(aes(x = Condition, y = rt, group = Condition, fill = Condition)) + 
  geom_beeswarm(alpha = .25) + 
  guides(colour = FALSE, fill = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 1) +
  labs(title = "Plot of Reaction Time (ms.) by Condition",
       x = "Condition", 
       y = "Reaction Time (ms.)") +
  theme_economist()
  
ggsave("my_plot.png", my_plot, height = 8, width = 15, units = "cm")



# Raincloud plot on data 
library(RColorBrewer)
source("https://gist.githubusercontent.com/ajstewartlang/6c4cd8ab9e0c27747424acdfb3b4cff6/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

set.seed(1234)
id <- sample(id, 48)
simple_sentence <- as.integer(rnorm(48, mean = 2000, sd = 140))
complex_sentence <- as.integer(rnorm(48, mean = 2400, sd = 160))

dataRT <- tibble(id, simple_sentence, complex_sentence)

dataRT <- gather(dataRT, key = "condition", value = "rt", c("simple_sentence", "complex_sentence"))

raincloud_theme = theme(
  text = element_text(size = 12),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  axis.text = element_text(size = 12),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=12),
  legend.text=element_text(size=12),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)

sumld <- plyr::ddply(dataRT, ~ condition, summarise, mean = mean(rt), median = median(rt), 
               lower = lb(rt), upper = ub(rt))

ggplot(data = dataRT, aes(y = rt, x = condition, fill = condition)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, trim=FALSE) +
  geom_point(aes(y = rt, color = condition), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1,  outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 3) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Accent") +
  scale_fill_brewer(palette = "Accent") +
  coord_flip() +
  theme_bw() +
  raincloud_theme +
  labs(x=NULL) +
  scale_y_continuous(breaks = seq(1500,3000,by = 200))


# find out about the dataset and generate some descriptives
head(mpg)
unique(mpg$manufacturer)
length(unique (mpg$manufacturer))
mpg %>% group_by(class) %>% summarise(Mean = mean(hwy))
mpg %>% group_by(cyl) %>% summarise(Mean = mean(hwy))

# build a violin plot with added descriptives
ggplot(mpg, aes(x = factor(cyl), y = cty, fill = factor(cyl))) + 
  geom_violin() +
  guides(colour = FALSE, fill = FALSE) +
  stat_summary(fun.data = mean_cl_boot, colour = "black", size = .5) +
  labs(title = "City Fuel Consumption by Number of Cylinders", 
       x = "Number of Cylinders", y = "City Fuel Consumption (mpg)")

# facet wrap by vehicle class with displacement instead of cylinder number
ggplot(mpg, aes(x = displ, y = hwy, colour = class)) + 
  geom_point() +
  facet_wrap(~ class) +
  guides(colour = FALSE) + 
  labs(title = "Highway Fuel Consumption by Cylinder Displacement \nfor Each Vehicle Class",
       x = "Displacement (litres)",
       y = "Highway Fuel Consumption (mpg)")

# now add a linear function to each
ggplot(mpg, aes(x = displ, y = hwy, colour = class)) + 
  geom_point() + 
  facet_wrap(~ class) +
  guides(colour = FALSE) + 
  geom_smooth(method = "lm") + 
  labs(title = "Highway Fuel Consumption by Cylinder Displacement \nfor Each Vehicle Class",
       x = "Displacement (litres)",
       y = "Highway Fuel Consumption (mpg)")

# plot basic histogram
ggplot(mpg, aes(x = displ)) + 
  geom_histogram(binwidth = .5) +
  guides(fill = FALSE) +
  labs(title = "Histogram of Cylinder Displacement",
       x = "Displacement (litres)",
       y = "Count")

ggplot(mpg, aes(x = displ)) + 
  geom_histogram(binwidth = .5) +
  geom_histogram(data = filter(mpg, class == "suv"), fill = "grey", binwidth = .5) +
  guides(fill = FALSE) +
  labs(title = "Histogram of Cylinder Displacement",
       subtitle = "SUVs highlighted",
       x = "Displacement (litres)",
       y = "Count")

ggplot(mpg, aes(x = displ, y = hwy, colour = class)) + 
  geom_jitter(width = 0.05, alpha = .5, size = 4) + 
  labs(title = "Scatterplot of Highway Fuel Consumption against \nEngine Displacement Grouped by Class",
       x = "Displacement (litres)",
       y = "Highway Fuel Consumption (mpg)")

# 2d histogram with density heatmap
ggplot(mpg, aes(x = displ, y = hwy)) +
  stat_bin2d(bins = 10, colour = "black") + 
  scale_fill_viridis() + 
  labs(title = "Density heatmap of Highway Fuel Consumption against Engine Displacement",
       x = "Displacement (litres)",
       y = "Highway Fuel Consumption (mpg)")

# Using gganimate
library(gganimate)

# Animated plot using the 'gapminder' dataset
library(gapminder)

# gganimate code of life expectancy by GDP for each contintent over time
p <- ggplot(gapminder, aes(gdpPercap, lifeExp, colour = continent)) +
  geom_point(size = 2, alpha = 0.7, show.legend = TRUE) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(title = "Gapminder dataset", subtitle = 'Year: {frame_time}', 
       x = 'GDP per capita', y = 'Life expectancy (years)') +
  transition_time(year) +
  ease_aes('linear') +
  theme(text = element_text(size = 20))

animate(p, height = 500, width = 900)

# Now vary each point according to population size
p <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, size = pop, colour = continent)) +
  geom_point(alpha = 0.7, show.legend = TRUE) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(title = "Gapminder dataset", subtitle = 'Year: {frame_time}', 
       x = 'GDP per capita', y = 'Life expectancy (years)') +  
  transition_time(year) +
  ease_aes('linear') +
  theme(text = element_text(size = 20))

animate(p, height = 500, width = 900)

# gganimate code faceted by continent
p <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~ continent) +
  labs(title = "Gapminder dataset", subtitle = 'Year: {frame_time}', 
       x = 'GDP per capita', y = 'Life expectancy (years)') +   transition_time(year) +
  ease_aes('linear') +
  theme(text = element_text(size = 20))

animate(p, height = 500, width = 900)


# Dot plot on life expectancy data
# Static faceted by year
df_Americas <- gapminder %>% filter(continent == "Americas")

ggplot(df_Americas, aes(x = lifeExp, y = fct_reorder(country, lifeExp))) +
  geom_point(color = "#0072B2", size = 3) +
  scale_x_continuous(name = "life expectancy (years)",
                     limits = c(30, 85),
                     expand = c(0, 0)) +
  scale_y_discrete(name = NULL, expand = c(0, 0.5)) + 
  facet_wrap(~ year) + 
  theme(text = element_text(size=8))

# Dynamic - separate frame per year
df_Americas <- gapminder %>% filter(continent == "Americas")
p <- ggplot(df_Americas, aes(x = lifeExp, y = fct_reorder(country, lifeExp))) +
  geom_point(color = "#0072B2", size = 3) +
  scale_x_continuous(name = "Life expectancy (years)",
                     limits = c(30, 85),
                     expand = c(0, 0)) +
  scale_y_discrete(name = NULL, expand = c(0, 0.5)) +
  labs(title = 'Year: {frame_time}') + 
  theme(text = element_text(size = 14)) +
  transition_time(year) +
  ease_aes('linear')

animate(p, height = 500, width = 900)

library(ggforce)
library(concaveman)
library(nycflights13)

# First part  ####
# Based on this great review here: https://rviews.rstudio.com/2019/09/19/intro-to-ggforce/

str(airports)
head(airports)

my_plot <- airports %>%
  filter(lon < 0, lat > 23, tzone != "\\N") %>%
  ggplot(aes(lon, lat, color = tzone)) + 
  geom_point(show.legend = FALSE)  

my_plot 

my_plot +
  geom_mark_rect(aes(label = tzone, fill = tzone), show.legend = FALSE) +
  labs(title = "Plot of continental US airports grouped by IANA time zone") +
  theme_void() 

my_plot +
  geom_mark_hull(aes(label = tzone, fill = tzone), show.legend = FALSE) +
  labs(title = "Plot of continental US airports grouped by IANA time zone") +
  theme_void() 

my_plot +
  geom_mark_hull(aes(label = tzone, fill = tzone), show.legend = FALSE, expand = unit(3, "mm")) +
  labs(title = "Plot of continental US airports grouped by IANA time zone") +
  facet_zoom(xy = (tzone == "America/Anchorage")) +
  theme_no_axes() 

# Example of alluvial diagran

prep_planes <- planes %>%
  filter(year > 1998, year < 2005) %>%
  filter(engine != "Turbo-shaft") %>%
  select(manufacturer, engine) %>%
  mutate(manufacturer = str_to_title(manufacturer))

prep_planes %>%
  gather_set_data(1:2) %>%
  ggplot(aes(x = x, id = id, split = y, value = 1))  +
  geom_parallel_sets(aes(fill = engine), show.legend = FALSE, alpha = 0.3) +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgrey", fill = "white") +
  geom_parallel_sets_labels(angle = 0) +
  theme_no_axes()

prep_planes <- planes %>%
  filter(year > 1960) %>%
  filter(engines != 2) %>%
  select(manufacturer, engines) %>%
  mutate(manufacturer = str_to_title(manufacturer))

prep_planes %>%
  gather_set_data(1:2) %>%
  ggplot(aes(x = x, id = id, split = y, value = 1))  +
  geom_parallel_sets(aes(fill = engines), show.legend = FALSE, alpha = 0.5) +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgrey", fill = "white") +
  geom_parallel_sets_labels(angle = 0) +
  theme_no_axes()

prep_planes %>%
  group_by(manufacturer, engines) %>%
  summarise(n())

# Another example with wind farm data ####

wind_farms <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-11-06/us_wind.csv")

my_filtered_data <- wind_farms %>%
  filter(ylat > 24 & t_state != "AK") 

my_plot <- my_filtered_data %>%
  ggplot(aes(xlong, ylat, color = t_state)) + 
  geom_point(show.legend = FALSE)  

my_plot +
  labs(title = "Plot of Windfarms in Continental US") +
  theme_void() 

my_filtered_data <- wind_farms %>%
  filter(t_state == "CA") 

my_plot <- my_filtered_data %>%
  ggplot(aes(xlong, ylat, color = t_state)) + 
  geom_point(show.legend = FALSE, alpha = .5)  

my_plot

my_plot +
  geom_mark_rect(aes(label = t_county, fill = t_county), show.legend = FALSE) +
  labs(title = "Plot of Windfarms in California Grouped by County") +
  theme_void() 

prep_farms <- wind_farms %>%
  filter(t_state == "CA" & t_manu != "missing") %>%
  select(t_county, t_manu) 

prep_farms %>%
  gather_set_data(1:2) %>%
  ggplot(aes(x = x, id = id, split = y, value = 1))  +
  geom_parallel_sets(aes(fill = t_manu), show.legend = FALSE, alpha = 0.5) +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgrey", fill = "white") +
  geom_parallel_sets_labels(angle = 0) +
  theme_no_axes()

# Star Wars dataset ####

star_wars_tally <- starwars %>%
  filter(!is.na (homeworld) & !is.na(species)) %>% 
  group_by(species) %>%
  tally() 

prep_star_wars <- left_join(starwars, star_wars_tally, by = "species") %>%
  filter(!is.na (homeworld) & !is.na(species)) %>%
  filter(n > 1) %>%
  select(homeworld, species) 

prep_star_wars %>%
  mutate(homeworld = factor(homeworld), species = factor(species)) %>%
  group_by(species) %>%
  summarise(n())

prep_star_wars %>%
  gather_set_data(1:2) %>%
  ggplot(aes(x = x, id = id, split = y, value = 1))  +
  geom_parallel_sets(aes(fill = homeworld), show.legend = FALSE, alpha = 0.5) +
  geom_parallel_sets_axes(color = "white", fill = "white", size = 25) +
  geom_parallel_sets_labels(angle = 0, size = 4) +
  theme_no_axes() +
  labs(title = "Mapping of Homeworlds to Species in the Star Wars Universe")

# Ridgeline plot
library(ggridges)

mpg %>%
  mutate(manufacturer = str_to_title(manufacturer)) %>%
  ggplot(aes(x = cty, y = fct_reorder(manufacturer, cty), group = manufacturer)) +
  geom_density_ridges(scale = 5, size = 0.5, rel_min_height = 0.01) +
  labs(title = "Ridge Plot of City Fuel Consumption for Different Car Manufacturers", 
       x = "City Fuel Consumption (MPG)",
       y = NULL) +
  theme_ridges() 


# Animations ####
library(gganimate)
library(NHANES)

# Boxplot of BMI by Race and AgeDecade 
my_plot <- NHANES %>% 
  distinct(ID, .keep_all = TRUE) %>%
  ggplot(aes(x = Race1, y = BMI, colour = Race1)) + 
  geom_boxplot() +
  guides(colour = FALSE) +
  labs(x = "Race", title = "Age = {closest_state}") +
  transition_states(AgeDecade) +
  theme(text = element_text(size = 20)) 

animate(my_plot, height = 500, width = 800)
anim_save("NHANES_plot.gif")

NHANES_tidy <- NHANES %>%
  filter(Race1 != "Other") %>%
  filter(as.character(AgeDecade) != " 0-9")

my_plot <- NHANES_tidy %>%
  mutate(AgeDecade = fct_drop(AgeDecade," 0-9")) %>%
  group_by(AgeDecade, Race1) %>%
  summarise(median_BMI = median(BMI, na.rm = TRUE)) %>%
  ggplot(aes(x = median_BMI, y = reorder(Race1, median_BMI), colour = Race1)) +
  geom_point(size = 3) +
  labs(x = "Median BMI", y = "Race", title = "Median BMI by Race and by Age Group", 
       subtitle = "Age = {closest_state}") +
  transition_states(AgeDecade) +
  theme(text = element_text(size = 20)) +
  guides(colour = FALSE)

animate(my_plot, height = 300, width = 800)


library(HH)

# Create the data
my_data <- as.data.frame(matrix(c(34,7,13,1,4,13,7,84,24,7,2,27,51,3,23), nrow = 3))
colnames(data) <- c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree")
rownames(data) <- c("Question 1", "Question 2", "Question 3")

# Plot the data as divergent stacked bar chart
likert(my_data, 
       main = "Diverging Stacked Bar Chart\nfor Likert Scale Data",
       sub = "Response")

# Scraping Twitter and Visualising Text Data ####
# Doing a quick sentiment analysis on tweets mentioning suicide
# created between midnight and 6AM 

library(rtweet)
library(lubridate)

tweets <- search_tweets(q = "suicide", n = 1000, include_rts = FALSE, 
                        retryonratelimit = FALSE)

time <- tibble(Time = hour(tweets$created_at))

time %>%
  filter(!is.na(Time)) %>%
  group_by(Time) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) 

time %>%
  filter(!is.na(Time)) %>%
  ggplot(aes(x = Time)) + 
  geom_histogram(binwidth = 1)

time_more <- mutate(tweets, Time = hour(created_at))

text <- time_more

rt1 <- text %>%
  unnest_tokens(word, text)

sent <- get_sentiments("bing")   #nrc for details - bing for positive vs negatives sentiment

word_counts <- rt1 %>%
  inner_join(sent) %>%
  count(word, sentiment, sort = TRUE)

word_counts %>% 
  filter(n > 25 & word != "suicide") %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")  

# Creating a wordcloud ####
library(wordcloud)

word_counts_tidy <- filter(word_counts, word != "suicide")

set.seed(1234)

wordcloud(words = word_counts_tidy$word, 
          freq = word_counts_tidy$n, 
          min.freq = 5,
          max.words = 200, 
          random.order = FALSE, 
          rot.per = 0.35, 
          colors=brewer.pal(8, "Dark2"))

# More Twitter scraping - Opeth ####
tweets <- search_tweets("Opeth", n = 2000, include_rts = FALSE, retryonratelimit = FALSE) 

tweets <- tweets %>% separate(col = created_at, into = c("date", "time"), sep = " ") 

ggplot(tweets, aes (x = date)) + 
  geom_bar(fill = "black", alpha = .5) + 
  labs(x = "Date", y = "Number of Tweets") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = paste("Tweets Mentioning Opeth Scraped On", Sys.Date())) 

# plotting Tweets on map ####
library(leaflet)
my_map <- lat_lng(tweets)
to_plot <- leaflet(my_map) %>% 
  addTiles()

to_plot %>% addCircles(lng = ~lng, lat = ~lat, weight = 8, radius = 40, 
                       color = "#fb3004", stroke = TRUE, fillOpacity = 0.8)

text <- tweets

rt1 <- text %>%
  unnest_tokens(word, text)

sent <- get_sentiments("bing")   # bing for positive vs negatives sentiment

word_counts <- rt1 %>%
  inner_join(sent) %>%
  count(word, sentiment, sort = TRUE)

word_counts %>% 
  filter(n > 25 & word != "suicide") %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")  

# get tweets from two journals - Science and Nature ####
tmls <- get_timelines(c("Nature", "sciencemagazine"), n = 1000)
tmls %>%
  filter(created_at > "2018-12-1") %>%
  group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  geom_point() +
  theme_minimal() +
  theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by the journals Nature and Science",
    subtitle = "Twitter status (tweet) counts aggregated by day",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# get tweets from Trump ####
tmls <- get_timelines(user = "@realDonaldTrump", n = 1000)

text <- tmls

rt1 <- text %>%
  unnest_tokens(word, text)

sent <- get_sentiments("bing")   #nrc for details - bing for positive vs negatives sentiment

word_counts <- rt1 %>%
  inner_join(sent) %>%
  count(word, sentiment, sort = TRUE)

word_counts %>% 
  filter(n > 1 & word != "trump") %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment", 
       title = "Sentiment Analysis of Tweets by @realDonaldTrump over Christmas")  

word_counts_tidy <- word_counts
set.seed(1234)
wordcloud(words = word_counts_tidy$word, 
          freq = word_counts_tidy$n, 
          min.freq = 1,
          max.words = 200, 
          random.order = FALSE, 
          rot.per = 0.35, 
          colors=brewer.pal(8, "Dark2"))

tmls %>%
  ts_plot("days", trim = 1L) +
  geom_point() +
  theme_minimal() +
  theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter posts by @realDonaldTrump",
    subtitle = "Twitter status (tweet) counts aggregated by day",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# More tidytext ####
library(gutenbergr)

# Get 2 HG Wells books ####
titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds")
books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

text_waroftheworlds <- books %>%
  filter(title == "The War of the Worlds") %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) 

text_underthesea <- books %>%
  filter(title == "Twenty Thousand Leagues under the Sea") %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) 

text_waroftheworlds %>%
  count(word) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = word)) +
  geom_col() +
  coord_flip() +
  guides(fill = FALSE) +
  labs(title = "Top 10 word in The War of the Worlds")

text_underthesea_count <- text_underthesea %>%
  count(word) %>%
  top_n(200)

wordcloud(words = text_underthesea_count$word, 
          freq = text_underthesea_count$n, 
          min.freq = 1,
          scale = c(3, 1), 
          max.words = 200, 
          random.order = FALSE, 
          rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))

# BBC plots ####
# Note, you need to install.packages("devtools") if you don't have it
devtools::install_github('bbc/bbplot')

library(bbplot)

my_plot <- mpg %>%
  count(class) %>%
  mutate(class = fct_reorder(class, n)) %>%
  ggplot(aes(x = class, y = n, fill = class)) +
  geom_col() +
  coord_flip() +
  guides(fill = FALSE) 

my_plot <- mpg %>%
  count(class) %>%
  mutate(class = fct_reorder(class, n)) %>%
  ggplot(aes(x = class, y = n, fill = class)) +
  geom_col() +
  coord_flip() +
  guides(fill = FALSE)  +
  bbc_style() +
  labs(title="Number of each type of car",
       subtitle = "Data collected in the US, 1999-2008")

finalise_plot(plot_name = my_plot,
              source = "ONS",
              save_filepath = "my_plot.png",
              width_pixels = 640,
              height_pixels = 550)


