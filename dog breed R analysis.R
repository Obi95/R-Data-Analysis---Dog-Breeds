library(dplyr)
library(ggplot2)
library(tidyr)

file_path <- "C:/Users/Sushi/Downloads/Dog_breeds.csv"

dog_breeds <- read.csv(file_path)

# 1. Which dog breeds originated from the same country?
breeds_by_country <- dog_breeds %>%
  group_by(Country.of.Origin) %>%
  summarize(Breeds = paste(Breed, collapse = ", ")) %>%
  filter(nchar(Breeds) > 0)

print("Dog breeds from the same country:")
print(breeds_by_country, n = Inf)

# Graph for Question 1
plot_country_breeds <- ggplot(breeds_by_country, aes(x = reorder(Country.of.Origin, -nchar(Breeds)), y = nchar(Breeds), fill = Country.of.Origin)) +
  geom_bar(stat = "identity") +
  labs(title = "Dog Breeds Originated from the Same Country",
       x = "Country of Origin",
       y = "Number of Breeds",
       fill = "Country of Origin") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +  # Remove legend to avoid redundancy
  geom_text(aes(label = nchar(Breeds)), vjust = -0.5, size = 3, color = "black")  # Add data labels

# Print the plot
print(plot_country_breeds)

# 2. What are the most common fur colors among the different breeds?
fur_colors <- dog_breeds %>%
  mutate(Fur.Color = ifelse(Fur.Color == "", NA, Fur.Color)) %>% # Handle empty strings
  drop_na(Fur.Color) %>%
  separate_rows(Fur.Color, sep = ", ") %>%
  group_by(Fur.Color) %>%
  summarize(Breeds = paste(Breed, collapse = ", "), Count = n()) %>%
  arrange(desc(Count))

print("Most common fur colors and breeds sharing these colors:")
print(fur_colors, n = Inf)

# 3. What is the range of heights among different dog breeds?
height_range <- dog_breeds %>%
  summarize(Min.Height = min(Height..in., na.rm = TRUE), Max.Height = max(Height..in., na.rm = TRUE))

print("Range of heights among different dog breeds:")
print(height_range)

# 4. Ranking breeds by the least common health problems
health_problems <- dog_breeds %>%
  select(Breed, Common.Health.Problems) %>%
  separate_rows(Common.Health.Problems, sep = ", ") %>%
  filter(Common.Health.Problems != "") %>%
  group_by(Breed, Common.Health.Problems) %>%
  summarise(Count = n()) %>%
  ungroup()

breeds_least_health_problems <- health_problems %>%
  group_by(Breed) %>%
  summarize(Total_Health_Problems = sum(Count))

least_common_health_breeds <- breeds_least_health_problems %>%
  filter(Total_Health_Problems == min(Total_Health_Problems))

print("Breeds with the least common health problems:")
print(least_common_health_breeds)


# HYPOTHESIS

# We can see a lot of the Breeds found in our dataset coming from England. This possibly 
# could have been attributed to the rich class in England wanting to have unique dog breeds
# mostly for aesthetics and to display a "social class"
# among their circles.
# Affenpinscher, Akita, and Afrikanis had the least common health issuess. These breeds might 
# possess genetic or physiological traits that confer resilience against common health problems 
# observed in other breeds. Factors such as genetic diversity, historical breeding practices, 
# or environmental adaptations could contribute to their health resilience. According to the 
# average height found in our dataset,
# Breeders or natural selection may favor dogs of this height range for practical reasons such 
# as agility, adaptability to living conditions. 