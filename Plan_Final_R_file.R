## Installing packages needed
library(readr)
library(dplyr)
library(ggplot2)
library(maps)
library(sf)
library(stringr)



#Filter the dataset to include the colums that I need 
#TOTAL_INDIVIDUAL_VICTIM has missing values as well as a few other so i will take them out 
main_hate_crime_csv = read_csv("hate_crime.csv")
hate_crime_data = main_hate_crime_csv %>% 
  select(DATA_YEAR, STATE_NAME, OFFENSE_NAME, LOCATION_NAME, BIAS_DESC, VICTIM_TYPES, LOCATION_NAME)
hate_crime_data


# I created a bar graph that displays by goruping all the biases and counting the number of crimes  to then show the top 10 hate crimes from all the years 
#inspo form https://www.kaggle.com/datasets/louissebye/united-states-hate-crimes-19912017?resourc e=download&select=hate_crime.csv.
top_ten = hate_crime_data %>% select(BIAS_DESC) %>%
  group_by(BIAS_DESC) %>%
  summarize(count = sum(n()))%>%
  arrange(desc(count))%>%
  head(10)
ggplot(top_ten, aes(reorder(BIAS_DESC, (count)), count, fill = BIAS_DESC))+
  geom_bar(stat = 'identity')+
  xlab(label = 'Hate Crime')+
  labs(title = "Top Ten US Hate Crimes from 1991 to 2018")+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())+
  theme(legend.key.size = unit(x = 2, units = 'line'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15))


# count the number of hate crimes for all years
year_counts = hate_crime_data %>%
  group_by(DATA_YEAR) %>%
  summarize(count = n())

# create a line graph showing the trend of hate crime 
ggplot(year_counts, aes(x = DATA_YEAR, y = count)) +
  geom_line(color = "steelblue") +
  labs(title = "Hate Crimes Reported per Year", x = "Year", y = "Number of Hate Crimes")

#count highest number of hate crimes every year 
hate_crime_count_by_year = hate_crime_data %>% 
  count(DATA_YEAR, sort = TRUE) %>% 
  arrange(desc(n))
#I see that 2001 had the highest 

#Now I want to see what crime was the most in in the top one  

data_2001 = hate_crime_data[hate_crime_data$DATA_YEAR == 2001, "BIAS_DESC"]
bias_desc_table = sort(table(data_2001), decreasing = TRUE)
top_categories = head(names(bias_desc_table), 1)
print(top_categories)
#"Anti-Black or African American" hatred was the highest in 2001 

data_1996 = hate_crime_data[hate_crime_data$DATA_YEAR == 1996, "BIAS_DESC"]
bias_desc_table = sort(table(data_2001), decreasing = TRUE)
top = head(names(bias_desc_table), 1)
print(top)
#"Anti-Black or African American" was also the highest in 1996

data_2000 = hate_crime_data[hate_crime_data$DATA_YEAR == 2000, "BIAS_DESC"]
bias_desc_table = sort(table(data_2001), decreasing = TRUE)
top_c = head(names(bias_desc_table), 1)
print(top_c)
#"Anti-Black or African American" was also the highest in 2000


## I want to see the trend of hate crimes towards black people on a grpah 
# Filter  by "Anti-Black or African American"
black_hate_crime_data = hate_crime_data %>%
  filter(BIAS_DESC == "Anti-Black or African American")
# Count the number of hate crimes for all years
year_counts = black_hate_crime_data %>%
  group_by(DATA_YEAR) %>%
  summarize(count = n())
#  line graph showing the trend of hate crime
ggplot(year_counts, aes(x = DATA_YEAR, y = count)) +
  geom_line(color = "steelblue") +
  labs(title = "Anti-Black or African American hate crimes over 1991 to 2018", x = "Year", y = "Number of Hate Crimes")


### See how Muslim hate crime rates compare in 2001
muslim_hate_crime_data = hate_crime_data %>%
  filter(BIAS_DESC == "Anti-Islamic (Muslim)")
# Count the number of hate crimes for all years
mus_counts = muslim_hate_crime_data %>%
  group_by(DATA_YEAR) %>%
  summarize(count = n())
#  line graph showing the trend of hate crime
ggplot(mus_counts, aes(x = DATA_YEAR, y = count)) +
  geom_line(color = "steelblue") +
  labs(title = "Muslim hate crimes over 1991 to 2018", x = "Year", y = "Number of Hate Crimes")

###i want to see that in the years of 1995-1997 what group was hate crimed the most. 
# Creare frequency table and sort it 
data_1995 = hate_crime_data[hate_crime_data$DATA_YEAR == 1995, "BIAS_DESC"]
bias_desc_table = sort(table(data_2001), decreasing = TRUE)
top_categories = head(names(bias_desc_table), 1)
print(top_categories)
data_1997 = hate_crime_data[hate_crime_data$DATA_YEAR == 1997, "BIAS_DESC"]
bias_desc_table = sort(table(data_2001), decreasing = TRUE)
top_categories = head(names(bias_desc_table), 1)
print(top_categories)
#########
## States include guam and other fedreal territory 
# Count the number of hate crimes per state
state_counts = hate_crime_data %>%
  group_by(STATE_NAME) %>%
  summarize(count = n())
state_counts

# Calculate the hate crime rates for each state
state_rates = state_counts
state_rates$rate = state_rates$count / sum(state_rates$count) * 100

# Sort the state_rates data frame by hate crime rate
sorted_rates = state_rates[order(state_rates$rate, decreasing = TRUE), ]

# Find the states with the highest and lowest hate crime rates
highest_rates = head(sorted_rates, n = 5)
lowest_rates = tail(sorted_rates, n = 5) 

###
#Now I want to find out what percentage from the total hate crimes in each state are a towards "Anti-Black or African American"
# filter  for only "Anti-Black or African American" hate crimes
black_hate_crime_data = black_hate_crime_data %>% filter(BIAS_DESC == "Anti-Black or African American")
# Calculate the total number of hate crimes for each state
state_counts_for_black_data = black_hate_crime_data %>% count(STATE_NAME)
# Calculate the percentage of hate crimes that are Anti-Black or African American for each state
state_percentages_for_black = state_counts_for_black_data %>%
  mutate(percent = n / sum(n) * 100) %>%
  arrange(desc(percent))
state_percentages_for_black

#####
#I want to know what is the most common hate crime in each state 

# Group the data by state and hate crime and count the number of them 
state_bias_counts = hate_crime_data %>%
  group_by(STATE_NAME, BIAS_DESC) %>%
  summarize(count = n()) %>%
  ungroup()

# Find the hate crime with the highest number for each state
most_common_bias_by_state = state_bias_counts %>%
  group_by(STATE_NAME) %>%
  slice_max(count) %>%
  ungroup() %>% 
  arrange(desc(count))
most_common_bias_by_state
## this will tell me from all the states how many of them had "Anti-Black or African American" as top crime 
anti_black_states = most_common_bias_by_state[most_common_bias_by_state$BIAS_DESC == "Anti-Black or African American", ]
#out of the 53, 42 had "Anti-Black or African American"

### 
#I want to find the most common offense type 
# create a  table of OFFENSE_TYPE
offense_freq = data.frame(hate_crime_data$OFFENSE_NAME)
# sort the table to get  the top three
#Group offense type and count it 
top_three_offenses = offense_freq %>%
  group_by(hate_crime_data.OFFENSE_NAME) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
head(top_three_offenses, 3)
# Often through "Destruction/Damage/Vandalism of Property" "Intimidation"  and "Simple Assault" 

#used https://sparkbyexamples.com/r-programming/r-subset-function-usage/
# create a subset of the data with the offenses
subset_data = subset(hate_crime_data, OFFENSE_NAME %in% c("Destruction/Damage/Vandalism of Property", "Intimidation", "Simple Assault"))

# create  tables for each offense and bias
property_damage_freq = table(subset_data$BIAS_DESC[subset_data$OFFENSE_NAME == "Destruction/Damage/Vandalism of Property"])
intimidation_freq = table(subset_data$BIAS_DESC[subset_data$OFFENSE_NAME == "Intimidation"])
simple_assault_freq = table(subset_data$BIAS_DESC[subset_data$OFFENSE_NAME == "Simple Assault"])
# results are 
cat("Bias for Destruction/Damage/Vandalism of Property: ", names(property_damage_freq)[which.max(property_damage_freq)], "\n")
cat("Bias for Intimidation: ", names(intimidation_freq)[which.max(intimidation_freq)], "\n")
cat("Bias for Simple Assault: ", names(simple_assault_freq)[which.max(simple_assault_freq)], "\n")
## For all three offenses Anti-Black or African American had the most 

#I want to find the most common targets  
# create a  table of victim type 
victim_req = table(hate_crime_data$VICTIM_TYPES)
# sort the table to get  the top three
top_three_vic = names(sort(victim_req, decreasing = TRUE)[1:3])
top_three_vic
# Often through "Individual" "Other" "Business"  

## create a  table of where incidents occur 
loc_name = table(hate_crime_data$LOCATION_NAME)
# sort the table to get  the top three
top_loc = names(sort(loc_name, decreasing = TRUE)[1:3])
top_loc
# often in "Residence/Home" "Highway/Road/Alley/Street/Sidewalk" "Other/Unknown"   

###
#Filitered the state_percentages to not include Guam and Federal on the map. 
state_data_for_black = state_percentages_for_black %>% 
  filter(STATE_NAME != "Federal" & STATE_NAME != "Guam") %>%
  mutate(STATE_NAME = tolower(STATE_NAME))

colnames(state_data_for_black) = c('state', 'count', 'percent')

########
##Used this to help me https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/ 

# Load map data
map_data = map_data("state")
colnames(map_data) = c('long', 'lat', 'group', 'order', 'state', 'subregion')


# Merge crime data with map data
crime_map_data_for_blacks = left_join(map_data, state_data_for_black, by = 'state')

# Create  us map for hate crimes against blacks
crime_map_for_blacks = ggplot(crime_map_data_for_blacks, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = percent), color = "black")+
  labs(title = "Hate Crimes on African Americans by State")
crime_map_for_blacks

# Create  us map for all hate crimes 

all_crime_data = main_hate_crime_csv %>% 
  filter(STATE_NAME != "Federal" & STATE_NAME != "Guam") %>%
  mutate(STATE_NAME = tolower(STATE_NAME)) %>%
  group_by(STATE_NAME) %>%
  summarize(count = n())
all_crime_data = all_crime_data %>%
  group_by(STATE_NAME) %>%
  mutate(percent = count / sum(all_crime_data$count) * 100)

colnames(all_crime_data) = c('state', 'count', 'percent')

all_crime_map_data = left_join(map_data, all_crime_data, by = 'state')

crime_map_for_all = ggplot(all_crime_map_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = percent), color = "black")+
  labs(title = "All Hate Crimes by State")
crime_map_for_all


#######
## Ignore this
#"Anti-Black or African American", "Anti-White",  "Anti-Asian", "Anti-Arab", "Anti-Hispanic or Latino", "Anti-American Indian or Alaska Native", "Anti-Other Race/Ethnicity/Ancestry", "Anti-Multiple Races, Group"            
#"Anti-Jewish", "Anti-Protestant", "Anti-Other Religion", "Anti-Islamic (Muslim)", "Anti-Multiple Religions, Group", "Anti-Catholic", "Anti-Jehovah's Witness","Anti-Mormon" , "Anti-Buddhist",  "Anti-Sikh" , "Anti-Other Christian", "Anti-Hindu",  

###For this part I wanted to create a pie chart showing the distributions of different hate crimes overall 
# categorizes the biases into broader groups such as "Race/Ethnicity/Ancestry", "Religion", "Sexual Orientation", and "Disability".
# Checks if the BIAS_DESC column contains certain keywords using the str_detect() function
#If a keyword found it will be assigned to a the corresponding category 
hate_crime_counts = hate_crime_data %>%
  count(BIAS_DESC) %>%
  mutate(Category = case_when(
    str_detect(BIAS_DESC, "American Indian|Alaska Native") ~ "Race/Ethnicity/Ancestry",
    str_detect(BIAS_DESC, "Arab") ~ "Race/Ethnicity/Ancestry",
    str_detect(BIAS_DESC, "Asian") ~ "Race/Ethnicity/Ancestry",
    str_detect(BIAS_DESC, "Black|African American") ~ "Race/Ethnicity/Ancestry",
    str_detect(BIAS_DESC, "Hispanic|Latino") ~ "Race/Ethnicity/Ancestry",
    str_detect(BIAS_DESC, "Pacific Islander") ~ "Race/Ethnicity/Ancestry",
    str_detect(BIAS_DESC, "Other Race") ~ "Race/Ethnicity/Ancestry",
    str_detect(BIAS_DESC, "White") ~ "Race/Ethnicity/Ancestry",
    str_detect(BIAS_DESC, "Buddhist") ~ "Religion",
    str_detect(BIAS_DESC, "Catholic") ~ "Religion",
    str_detect(BIAS_DESC, "Eastern Orthodox") ~ "Religion",
    str_detect(BIAS_DESC, "Hindu") ~ "Religion",
    str_detect(BIAS_DESC, "Islamic") ~ "Religion",
    str_detect(BIAS_DESC, "Jehovah") ~ "Religion",
    str_detect(BIAS_DESC, "Jewish") ~ "Religion",
    str_detect(BIAS_DESC, "Mormon") ~ "Religion",
    str_detect(BIAS_DESC, "Multiple Religions") ~ "Religion",
    str_detect(BIAS_DESC, "Other Christian") ~ "Religion",
    str_detect(BIAS_DESC, "Other Religion") ~ "Religion",
    str_detect(BIAS_DESC, "Protestant") ~ "Religion",
    str_detect(BIAS_DESC, "Sikh") ~ "Religion",
    str_detect(BIAS_DESC, "Atheism|Agnosticism") ~ "Religion",
    str_detect(BIAS_DESC, "Bisexual") ~ "Sexual Orientation",
    str_detect(BIAS_DESC, "Gay (Male)") ~ "Sexual Orientation",
    str_detect(BIAS_DESC, "Male") ~ "Sexual Orientation",
    str_detect(BIAS_DESC, "Female") ~ "Sexual Orientation",
    str_detect(BIAS_DESC, "Heterosexual") ~ "Sexual Orientation",
    str_detect(BIAS_DESC, "Lesbian, Gay, Bisexual, or Transgender") ~ "Sexual Orientation",
    str_detect(BIAS_DESC, "Lesbian") ~ "Sexual Orientation",
    str_detect(BIAS_DESC, "Mental Disability") ~ "Disability",
    str_detect(BIAS_DESC, "Physical Disability") ~ "Disability"
  ))

## Create a a pie chart 
pie_chart = ggplot(hate_crime_counts, aes(x = "", y = n, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(fill = "Category") +
  ggtitle("Distribution of Hate Crimes by Category")
print(pie_chart)


