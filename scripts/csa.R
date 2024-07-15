# CHINESE STUDENTS ALLIANCE DIRECTORIES

save.image("csa.1912.RData")

library(tidyverse)

# 1912 directory 

## Chinese names 

### import list of Chinese names from 1912 directory

library(readr)
csa_1912_zh <- read_delim("data/csa_1912_zh.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)


# reverse order of characters (reading direction from right to left -> left to right)

library(stringi)

# create function 

# Function to reverse the order of characters in a string
reverse_string <- function(input_string) {
  # Split the string into individual characters
  char_vector <- stri_split_boundaries(input_string, type = "character")[[1]]
  
  # Reverse the order of the characters
  reversed_char_vector <- rev(char_vector)
  
  # Combine the reversed characters back into a single string
  reversed_string <- stri_paste(reversed_char_vector, collapse = "")
  
  return(reversed_string)
}


# Apply the reverse_string function to the Name_zh column
csa_1912_zh <- csa_1912_zh %>%
  mutate(Name_zh_reversed = sapply(Name_zh, reverse_string)) 

# count length 

csa_1912_zh <- csa_1912_zh %>% mutate(length = nchar(Name_zh))


# extract patronym 

# extract given name

# save and export 

write.csv(csa_1912_zh, "~/data curation/output/csa_1912-1932/csa_1912_zh_reversed.csv")


# load curated data in English 

library(readr)
csa1912 <- read_delim("data/CSA1912/csa1912.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

# load curated names in Chinese 

library(readr)
csa1912_ZhName <- read_delim("data/CSA1912/csa1912_ZhName.csv", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

# join English and Chinese 

csa1912_biling <- left_join(csa1912, csa1912_ZhName)

# separate city and state from address

# Create a function to extract city and state
extract_city_state <- function(address) {
  parts <- strsplit(address, ",")[[1]]
  n <- length(parts)
  state <- trimws(parts[n])
  city <- trimws(parts[n-1])
  return(c(city, state))
}

# Apply the function to the data
csa1912_biling <- csa1912_biling %>%
  rowwise() %>%
  mutate(city = extract_city_state(Address)[1],
         state = extract_city_state(Address)[2])

# export -> expand states and join with coordinates

write.csv(csa1912_biling, "~/data curation/output/csa_1912-1932/csa1912_with_citystate.csv")

# re-import clean data 

library(readr)
csa1912_city_state_clean <- read_delim("data/CSA1912/csa1912_city_state_clean.csv", 
                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

# separate surname and given name in English 

csa1912_clean <- csa1912_city_state_clean  %>% mutate(Name_eng,
                 FirstName=str_replace(Name_eng,"[^,]+, ",""),
                 LastName=str_replace(Name_eng,",.*",""))

# extract gender 

csa1912_clean <- csa1912_clean %>% mutate(gender = str_extract(FirstName, "Miss|Mrs.")) %>% 
  mutate(gender = str_squish(gender)) 

csa1912_clean <- csa1912_clean %>% mutate(FirstName = str_replace(FirstName, "Miss, ", "")) %>% 
                                            mutate(FirstName = str_replace(FirstName, "Miss ", "")) %>% 
                                                     mutate(FirstName = str_replace(FirstName, "Mrs. ", "")) %>% 
                                                              mutate(FirstName = str_squish(FirstName)) 

csa1912_clean <- csa1912_clean %>% rename(Title = gender)

csa1912_clean$Gender <- ifelse(csa1912_clean$Title == "Miss" | csa1912_clean$Title == "Mrs.", "Female", 
                               ifelse(csa1912_clean$Title == "NA", "Male"))

csa1912_clean <- csa1912_clean %>%
  mutate(Gender = case_when(
    Title == "Miss" ~ "Female",
    Title == "Mrs." ~ "Female",
    is.na(Title) ~ "Male",
    TRUE ~ "Male"
  ))


# add geo coordinates

# load list of us cities with lat and long

View(us_geo_data)
uscities <- read.csv("~/data curation/data/uscities.csv")

# create variable for join 

uscities_to_join <- uscities %>% select(city, state_name, lat, lng) %>% mutate(city_state = paste(city, state_name, sep = "_"))
uscities_to_join <- uscities_to_join %>% select(city_state, lat, lng)

csa1912_clean <- csa1912_clean %>% mutate(city_state = paste(City, State, sep = "_"))

csa1912_clean <- left_join(csa1912_clean, uscities_to_join)

# reload data with complete geo coordinates 

library(readr)
csa1912_latlgn_complete <- read_delim("data/CSA1912/csa1912_latlgn_complete.csv", 
                                      delim = ";", escape_double = FALSE, trim_ws = TRUE) 

### map cities

library(ggmap)

us_states <- map_data("state")
states <- us_map(regions = "states")

csa_city_count <- csa1912_latlgn_complete %>% group_by(State, City, lat, lng) %>% count() 

csa_city_count %>% 
  ggplot() +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group), fill = "grey", color = "darkgrey", lwd = 0.2) +
  geom_point(aes(x = lng, y = lat, size = csa_city_count$n),
             label = "City",
             alpha = .8, color = "red") +
  labs(title = "Members of the Chinese Students Alliance in the USA (1912)",
       subtitle = "Distribution by city",
       size = "Number of students",
       caption = "Based on 'The Directory of Chinese Students in the United States, 1911-1912'")

hist(csa_city_count$n)

# map with leaflet

library(leaflet)

csa_city_count %>% rename(long = lng) %>% drop_na(City) %>% 
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers( radius = ~log(n)*3,
                    label = ~City,
                    color = "white",
                    weight = 2,
                    opacity = 0.6,
                    fill = TRUE,
                    fillColor = "red",
                    fillOpacity = 0.9,
                    stroke = TRUE,
                    popup = ~paste( "City:", City ,
                                    "",
                                    "Number of students", n))

# Define breaks for the legend
breaks <- c(10, 20, 50, 100)

# Create the map
map <- csa_city_count %>% 
  rename(long = lng) %>%
  drop_na(City) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    radius = ~log(n) * 3,
    label = ~City,
    color = "white",
    weight = 2,
    opacity = 0.6,
    fill = TRUE,
    fillColor = "red",
    fillOpacity = 0.9,
    stroke = TRUE,
    popup = ~paste("City:", City, "<br>", "Number of students:", n)
  ) %>%
  addLegend(
    position = "bottomright",
    title = "Number of Students",
    labels = as.character(breaks),
    colors = rep("red", length(breaks)),
    opacity = 0.9,
    labFormat = labelFormat(suffix = ""),
    values = breaks
  )


# Define breaks and corresponding radii for the legend
breaks <- c(10, 20, 50, 100)
radii <- log(breaks) * 3

# Create a function to generate legend labels
generate_legend_labels <- function(breaks, radii) {
  paste0("<div style='display: inline-block; width: 30px; height: ", 
         radii, "px; background-color: red; border-radius: 50%; margin-bottom: 5px;'></div> ", 
         breaks)
}

# Create the map


map <- csa_city_count %>% 
  rename(long = lng) %>%
  drop_na(City) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    radius = ~log(n) * 3,
    label = ~City,
    color = "white",
    weight = 2,
    opacity = 0.6,
    fill = TRUE,
    fillColor = "red",
    fillOpacity = 0.9,
    stroke = TRUE,
    popup = ~paste("City:", City, "<br>", "Number of students:", n)
  )


legend_html <- "
<div style='background-color: white; padding: 10px;'>
  <h4>Number of Students</h4>
  <div style='display: flex; align-items: center;'>
    <div style='background-color: red; width: 20px; height: 20px; border-radius: 50%; margin-right: 5px;'></div>
    <span>10</span>
  </div>
  <div style='display: flex; align-items: center;'>
    <div style='background-color: red; width: 25px; height: 25px; border-radius: 50%; margin-right: 5px;'></div>
    <span>20</span>
  </div>
  <div style='display: flex; align-items: center;'>
    <div style='background-color: red; width: 30px; height: 30px; border-radius: 50%; margin-right: 5px;'></div>
    <span>50</span>
  </div>
  <div style='display: flex; align-items: center;'>
    <div style='background-color: red; width: 35px; height: 35px; border-radius: 50%; margin-right: 5px;'></div>
    <span>100</span>
  </div>
</div>
"

# Add the custom legend to the map
map <- map %>% 
  addControl(html = legend_html, position = "bottomright")

map


## BASIC STATS

csa1912_latlgn_complete %>% group_by(Gender) %>% count()
