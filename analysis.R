# Global Terrorism Database Collaborative Analysis

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyverse)

# Load the dataset
gtd_data <- read.csv("globalterrorismdb_0718dist.csv", 
                     stringsAsFactors = FALSE)

# Basic data exploration
glimpse(gtd_data)

cleaned_data <- gtd_data %>% 
  select(country_txt,nkill) %>%
  na.omit()

glimpse(cleaned_data)

# Group by region (or country) and calculate total fatalities
fatalities_per_region <- cleaned_data %>%
  group_by(country_txt) %>%
  summarise(total_fatalities = sum(nkill, na.rm = TRUE))

# View the data for graphing
glimpse(fatalities_per_region)

# Create a bar plot for fatalities per region
ggplot(fatalities_per_region, aes(x = reorder(country_txt, -total_fatalities), y = total_fatalities)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Total Fatalities per Region",
       x = "Region",
       y = "Total Fatalities") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Select top 10 countries based on total fatalities
top_20_countries <- fatalities_per_region %>%
  top_n(20, total_fatalities) %>%  # Select top 10
  arrange(desc(total_fatalities))  # Arrange in descending order

# Create a horizontal bar plot for top 10 countries
ggplot(top_20_countries, aes(x = total_fatalities, y = reorder(country_txt, total_fatalities))) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +  # Adjust bar width if needed
  theme_minimal() +  # Minimal theme
  labs(title = "Top 20 Countries by Total Fatalities",
       x = "Total Fatalities",
       y = "Country") +
  theme(axis.text.y = element_text(size = 8),  # Adjust country label size if necessary
        panel.grid.minor = element_blank(),    # Remove minor grid lines
        panel.grid.major.y = element_blank())  # Remove horizontal grid lines

# 
# Install countrycode package if not already installed
install.packages("countrycode")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(countrycode)

# Add continent column to the dataset based on country names
cleaned_data <- cleaned_data %>%
  mutate(continent = countrycode(country_txt, "country.name", "continent"))

# Group by continent and calculate total fatalities
fatalities_per_continent <- cleaned_data %>%
  group_by(continent) %>%
  summarise(total_fatalities = sum(nkill, na.rm = TRUE))

# Plot fatalities per continent
ggplot(fatalities_per_continent, aes(x = total_fatalities, y = reorder(continent, total_fatalities))) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
  theme_minimal() +
  labs(title = "Total Fatalities per Continent",
       x = "Total Fatalities",
       y = "Continent") +
  theme(axis.text.y = element_text(size = 10),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())


 
# # Select top 10 countries based on total fatalities
# top_10_countries <- fatalities_per_region %>%
#   top_n(10, total_fatalities) %>%  # Select top 10
#   arrange(desc(total_fatalities))  # Arrange in descending order
# 
# # Create a horizontal bar plot for top 10 countries
# ggplot(top_10_countries, aes(x = total_fatalities, y = reorder(country_txt, total_fatalities))) +
#   geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +  # Adjust bar width if needed
#   theme_minimal() +  # Minimal theme
#   labs(title = "Top 10 Countries by Total Fatalities",
#        x = "Total Fatalities",
#        y = "Country") +
#   theme(axis.text.y = element_text(size = 8),  # Adjust country label size if necessary
#         panel.grid.minor = element_blank(),    # Remove minor grid lines
#         panel.grid.major.y = element_blank())  # Remove horizontal grid lines
# 
# 
# 
# 
# 
# 
# 

# 
# # Load ggplot2 if not already loaded
# library(ggplot2)
# 
# # Create a jitter plot for fatalities per region
# ggplot(cleaned_data, aes(x = nkill, y = reorder(country_txt, nkill))) +
#   geom_jitter(width = 0.2, height = 0.1, color = "steelblue", alpha = 0.6) +
#   theme_minimal() +  # Minimal theme
#   labs(title = "Fatalities per Incident by Region",
#        x = "Number of Fatalities",
#        y = "Country") +
#   theme(axis.text.y = element_text(size = 8),  # Adjust country label size if necessary
#         panel.grid.minor = element_blank(),    # Remove minor grid lines
#         panel.grid.major.y = element_blank())  # Remove horizontal grid lines
