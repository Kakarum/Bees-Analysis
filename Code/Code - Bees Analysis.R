#Master Course "Data Science for Economics" - December 2024
#Advanced Multivariate Statistics - Exam Project


#THE IMPACT OF EXTERNAL FACTORS ON HONEY BEES PRODUCTIVITY IN USA

#Gallo Luigi, Longo Angelica, Rizzi Melissa

################################################################################
#LIST OF USED LIBRARIES 

library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(maps)
library(scales)
library(lubridate)
library(tidyr)
library(states)
library(rnaturalearth)
library(sf)
library(lme4)
library(tidyverse)
library(insight)
library(lattice)
library(corrplot)
library(car)
library(psych)
library(DescTools)
library(MASS)
library(robustbase)
library(MuMIn) 
library(performance)
library(sjPlot)
library(plotly)
library(cluster)
library(factoextra)
library(tclust)



################################################################################
#DATA PREPARATION AND DATA VISUALIZATION - GENERAL DATASET

#IMPORT DATASETS
df_pesticides <- read_csv("data/vHoneyNeonic_v03.csv")  
df_honey <- read_csv("data/US_honey_production_dataset.csv")[-1]

#DATASET ADJUSTMENTS
  #Replace concatenated state names with correct formatting in df_honey
df_honey <- df_honey %>%
  mutate(state = str_replace_all(state, c("NorthDakota" = "North Dakota",
                                          "SouthDakota" = "South Dakota",
                                          "SouthCarolina" = "South Carolina",
                                          "NewYork" = "New York",
                                          "NewJersey" = "New Jersey",
                                          "NewMexico" = "New Mexico",
                                          "NorthCarolina" = "North Carolina",
                                          "WestVirginia" = "West Virginia")))

df_pesticides$state <- NULL  #Remove the state column from df_pesticides
df_pesticides <- df_pesticides %>%
  rename(state = StateName) # Rename "StateName" column to "state" for consistency

df_honey2_renamed <- df_honey %>%
  rename(
    numcol = colony_number,     
    priceperlb = average_price,  
    yieldpercol = yield_per_colony,
    totalprod = productions
  ) 

df_honey2_renamed <- df_honey2_renamed %>%
  filter(year > 2017)

df_honey2_renamed <- df_honey2_renamed %>%
  dplyr::select(year, state, numcol, yieldpercol, totalprod, priceperlb)

#COMBINE DATA OVER TIME
pesticides_filtered <- df_pesticides %>%
  dplyr::select(year, state, numcol, yieldpercol, totalprod, priceperlb)   

all_years_data <- rbind(pesticides_filtered, df_honey2_renamed)    

#PLOT: number of colonies per year
numcol_per_year <- all_years_data %>%
  group_by(year) %>%
  summarise(total_numcol = sum(numcol, na.rm = TRUE))

ggplot(numcol_per_year, aes(x = year, y = total_numcol)) +
  geom_line(color = "#ffa93f", size = 0.7) +
  geom_point(color = "#FF7F00", size = 2) +
  labs(title = "Number of Colonies per Year", x = "Year", y = "Number of Colonies") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5)          
  )

#PLOT: number of colonies per state over time
ggplot(all_years_data, aes(x = year, y = numcol, color = state)) +
  geom_line() +
  labs(title = "Number of Colonies per State Over Time", x = "Year", y = "Number of Colonies") +
  theme_minimal() +
  facet_wrap(~ state, scales = "free_y") +
  theme(legend.position = "none")+
  theme(
    plot.title = element_text(hjust = 0.5),          
  )+
  scale_y_continuous(labels = label_number(accuracy = 1))

#PLOT: average yield per state over time
ggplot(all_years_data, aes(x = year, y = yieldpercol, color = state)) +
  geom_line() +
  labs(title = "Average Yield per State Over Time", x = "Year", y = "Average Yield per colony") +
  theme_minimal() +
  facet_wrap(~ state, scales = "free_y") +
  theme(legend.position = "none")+
  theme(
    plot.title = element_text(hjust = 0.5)          
  )

#PLOT: average price per year
price_per_year <- all_years_data %>%
  group_by(year) %>%
  summarise(mean_priceperlb = mean(priceperlb, na.rm = TRUE))

ggplot(price_per_year, aes(x = year, y = mean_priceperlb)) +
  geom_line(color = "#ffa93f", size = 0.7) +
  geom_point(color = "#FF7F00", size = 2) +
  labs(title = "Average Price per Year", x = "Year", y = "Average Price ($ per lb)") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5),          
  )

#PLOT: visualize differences in number of colonies in the first (1991) and last (2021) year of the dataset
usa_map <- map_data("state")  # Load map data for US states

  #Filter data for the first year (1991)
first_year <- min(all_years_data$year)
first_year #1991
data_first_year <- all_years_data %>%
  filter(year == first_year)
data_first_year$state <- tolower(data_first_year$state)

  #Merge map and data for the first year
map_data_first_year <- merge(usa_map, data_first_year, by.x = "region", by.y = "state", all.x = TRUE)
map_data_first_year <- map_data_first_year[order(map_data_first_year$order), ]  # Order by 'order' column

  #Filter data for the most recent year (2021)
last_year <- max(all_years_data$year)
last_year #2021
data_last_year <- all_years_data %>%
  filter(year == last_year)
data_last_year$state <- tolower(data_last_year$state)  # Convert state names to lowercase

  #Merge map and data for the last year
map_data_last_year <- merge(usa_map, data_last_year, by.x = "region", by.y = "state", all.x = TRUE)
map_data_last_year <- map_data_last_year[order(map_data_last_year$order), ]  #Order by 'order' column for proper map display

state_centroids <- map_data_first_year %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat), group = mean(group), totalprod = mean(totalprod)) #Calculate centroids for state

  #Plot number of colonies per state in the first year
ggplot(map_data_first_year, aes(x = long, y = lat, group = group, fill = totalprod)) +
  geom_polygon(color = "white") +
  geom_text(data = state_centroids, aes(x = long, y = lat, label = region), color = "black", size = 3) +
  scale_fill_gradient(low = "#fff68f", high = "#ff4500", 
                      na.value = "#FFF5EE", 
                      name = "Total Production",
                      labels = label_number(accuracy = 1)) +
  labs(title = paste("Distribution of Honey Production in the United States (", first_year, ")", sep = ""),
       fill = "Total Production") +
  coord_fixed(1.3) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())+
  theme(
    plot.title = element_text(hjust = 0.5),          
  )

  #Plot number of colonies per state in the last year
ggplot(map_data_last_year, aes(x = long, y = lat, group = group, fill = totalprod)) +
  geom_polygon(color = "white") +
  geom_text(data = state_centroids, aes(x = long, y = lat, label = region), color = "black", size = 3) +
  scale_fill_gradient(low = "#fff68f", high = "#ff4500", 
                      na.value = "#FFF5EE", 
                      name = "Total Production",
                      labels = label_number(accuracy = 1)) +
  labs(title = paste("Distribution of Honey Production in the United States (", last_year, ")", sep = ""),
       fill = "Total Production") +
  coord_fixed(1.3) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())+
  theme(
    plot.title = element_text(hjust = 0.5),          
  )



################################################################################
#DATA PREPARATION ADDING POLLUTION AND TEMPERATURE VARIABLES - FINAL DATASET

df_pesticides <- na.omit(df_pesticides)

sort(unique(df_pesticides$year))

annual_aqi_list <- list()   #Create an empty list to collect annual dataset

  #Union of dataset from 1994 to 2016
for (year in 1994:2016) {
  file_path <- paste0("data/aqi/annual_aqi_by_county_", year, ".csv")
  annual_aqi_list[[as.character(year)]] <- read_csv(file_path)  
}
annual_aqi_all_years <- bind_rows(annual_aqi_list)    #Merge all datasets in one

  #Apply transformations and aggregation to the unified dataset
annual_aqi_state_summary <- annual_aqi_all_years %>%
  dplyr::select(
    'State', "County", "Year", "Days with AQI", "Good Days", "Unhealthy Days",
    "Very Unhealthy Days", "Hazardous Days", "Max AQI", "Days CO",
    "Days NO2", "Days Ozone", "Days PM2.5", "Days PM10"
  ) %>%
  mutate(
    Percent_Good_Days = (`Good Days` / `Days with AQI`) * 100,
    Percent_Unhealthy_Days = ((`Unhealthy Days` +
                                 `Very Unhealthy Days` +
                                 `Hazardous Days`) / `Days with AQI`) * 100 
  ) %>%
  group_by(State, Year) %>%
  summarise(
    Max_AQI = max(`Max AQI`, na.rm = TRUE),
    Days_CO = max(`Days CO`, na.rm = TRUE),
    Days_NO2 = max(`Days NO2`, na.rm = TRUE),
    Days_Ozone = max(`Days Ozone`, na.rm = TRUE),
    Percent_Good_Days = mean(Percent_Good_Days, na.rm = TRUE),
    Percent_Unhealthy_Days = mean(Percent_Unhealthy_Days, na.rm = TRUE),
    Days_PM2.5 = max(`Days PM2.5`, na.rm = TRUE),
    Days_PM10 = max(`Days PM10`, na.rm = TRUE)
  ) %>%  ungroup()

#COMBINED DATASET (honey production + pesticides grouper by year, state)
df_combined <- df_pesticides %>%
  left_join(annual_aqi_state_summary, by = c("state" = "State", "year" = "Year"))


#IMPORT NATURAL DISTASTERS DATASET 
us_disaster_declarations <- read_csv("data/us_disaster_declarations.csv")

us_disaster <- us_disaster_declarations %>%
  rename(year = fy_declared) %>%
  filter(year >= 1994 & year <= 2016) %>%
  dplyr::select(disaster_number, state, year, incident_type)

us_disaster <- us_disaster %>% 
  distinct(disaster_number, .keep_all = TRUE)

state_names <- data.frame(
  state = state.abb,       
  state_name = state.name 
)
us_disaster <- us_disaster %>%
  left_join(state_names, by = "state") %>%
  rename(state = state_name, state_code = state) %>%
  dplyr::select(disaster_number, state, year, incident_type)

us_disaster_summary <- us_disaster %>%
  mutate(
    Flood = ifelse(incident_type == "Flood", 1, 0),
    Fire = ifelse(incident_type == "Fire", 1, 0),
    Storm_Group = ifelse(incident_type %in% c("Severe Storm", "Tornado", "Hurricane"), 1, 0)
  ) %>%
  group_by(year, state) %>%
  summarise(
    Flood = sum(Flood),
    Fire = sum(Fire),
    Storm_Group = sum(Storm_Group)
  )

#COMBINED DATASET (honey + pesticides + natural disasters)
df_combined2 <- df_combined %>%
  left_join(us_disaster_summary, by = c("state" = "state", "year" = "year"))
df_combined2 <- df_combined2 %>%
  mutate_all(~ ifelse(is.na(.), 0, .))


#DATASET ADJUSTMENT: CONSIDERED STATE AREA FOR KM2
  #Create a dataframe with state name and area in km²
us_states <- ne_states(country = "United States of America", returnclass = "sf")

us_states_area <- us_states %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%  #Convert area in m² and then in km²
  dplyr::select(name, area_km2) %>%
  st_set_geometry(NULL)

df_combined2 <- df_combined2 %>%
  left_join(us_states_area, by = c("state" = "name"))

df_combined2 <- df_combined2 %>%
  mutate(
    nCLOTHIANIDIN_km2 = nCLOTHIANIDIN / area_km2,
    nIMIDACLOPRID_km2 = nIMIDACLOPRID / area_km2,
    nTHIAMETHOXAM_km2 = nTHIAMETHOXAM / area_km2,
    nACETAMIPRID_km2 = nACETAMIPRID / area_km2,
    nTHIACLOPRID_km2 = nTHIACLOPRID / area_km2,
    nAllNeonic_km2 = nAllNeonic / area_km2
  )

df_combined2 <- df_combined2 %>%
  mutate(
    nCLOTHIANIDIN_km2 = format(nCLOTHIANIDIN_km2, scientific = FALSE),
    nIMIDACLOPRID_km2 = format(nIMIDACLOPRID_km2, scientific = FALSE),
    nTHIAMETHOXAM_km2 = format(nTHIAMETHOXAM_km2, scientific = FALSE),
    nACETAMIPRID_km2 = format(nACETAMIPRID_km2, scientific = FALSE),
    nTHIACLOPRID_km2 = format(nTHIACLOPRID_km2, scientific = FALSE),
    nAllNeonic_km2 = format(nAllNeonic_km2, scientific = FALSE)
  )


#IMPORT PRECIPITATION DATASET
folder_path <- "data/precipitation/"  

file_list <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

read_and_clean <- function(file_path, skip_rows = 4) {
  state_name <- tools::file_path_sans_ext(basename(file_path)) 
  data <- read_csv(file_path, skip = skip_rows)
  data <- data %>% mutate(state = state_name)   #Function to read and clean every file
  
  return(data)
}  

all_states_prec <- bind_rows(lapply(file_list, function(file) read_and_clean(file, skip_rows = 4)))
all_states_prec$year <- substr(all_states_prec$Date, 1, 4)
all_states_prec$year <- as.numeric(all_states_prec$year)
all_states_prec$Date <- NULL  #Delete column with dates

names(all_states_prec)[names(all_states_prec) == "Value"] <- "Precipitation"    #Rename columns
names(all_states_prec)[names(all_states_prec) == "Anomaly"] <- "Anomaly_prec"   #Rename columns

#COMBINED DATASET (honey + pesticides + natural disasters + precipitation)
df_combined3 <- df_combined2 %>%
  left_join(all_states_prec, by = c("state" = "state", "year" = "year"))


#PALMER INDEX DATASET
folder_path2 <- "data/Palmer/"  
file_list2 <- list.files(path = folder_path2, pattern = "*.csv", full.names = TRUE)
all_states_palmer <- bind_rows(lapply(file_list2, function(file) read_and_clean(file, skip_rows = 2)))
all_states_palmer$year <- substr(all_states_palmer$Date, 1, 4)
all_states_palmer$year <- as.numeric(all_states_palmer$year)
all_states_palmer$Date <- NULL
names(all_states_palmer)[names(all_states_palmer) == "Value"] <- "Palmer_index"

all_states_palmer <- all_states_palmer %>%
  group_by(state, year) %>%
  summarize(palmer_index = mean(Palmer_index))

#COMBINED DATASET (honey + pesticides + natural disasters + precipitation + palmer index)
df_combined3 <- df_combined3 %>%
  left_join(all_states_palmer, by = c("state" = "state", "year" = "year"))


#IMPORT MAX/MIN TEMPERATURE DATASET
  #max
states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
            "Connecticut", "Delaware", "Florida", "Georgia", "Idaho", "Illinois", 
            "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", 
            "Maryland", "Massachussets", "Michigan", "Minnesota", "Mississippi", 
            "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
            "New Jersey", "New York", "New Mexico", "North Dakota", 
            "North Carolina", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
            "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
            "Texas", "Utah", "Vermont", "Virginia", "Washington", 
            "West Virginia", "Wisconsin", "Wyoming")

states_list_max <- map(states, ~ read_csv(paste0("data/Max temp/", .x, ".csv"), skip = 3) %>%
                         mutate(State = .x))

all_states_max <- bind_rows(states_list_max)

all_states_max <- all_states_max %>%
  rename(Temperature = Value)

  #Fahrenheit to Celsius
all_states_max <- all_states_max %>%
  mutate(Temperature = (Temperature - 32) * 5 / 9) 

all_states_max <- all_states_max %>%
  mutate(
    Date = as.character(Date),  #Be sure that 'Date' is in character Date
    Year = substr(Date, 1, 4),  #Extract year
    Year = as.numeric(Year),  #Convert year in numeric
  )

max_temp_per_year <- all_states_max %>%
  group_by(State, Year) %>%
  filter(Temperature == max(Temperature, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(State, Year, Temperature) #Select max temp for every year and state

max_temp_per_year <- max_temp_per_year %>%
  rename(Max_Temperature = Temperature)

max_temp_per_year <- max_temp_per_year %>%
  mutate(Max_Temperature = round(Max_Temperature, 2)) %>%
  arrange(State) %>%  # Alphabetic order
  distinct(State, Year, .keep_all = TRUE)  # Keep only one record per State and Year

df_combined3 <- df_combined3 %>%
  left_join(max_temp_per_year, by = c("state" = "State", "year" = "Year"))

  #min
states_list_min <- map(states, ~ read_csv(paste0("data/Min temp/", .x, " min.csv"), skip = 3) %>%
                         mutate(State = .x))

all_states_min <- bind_rows(states_list_min)

all_states_min <- all_states_min %>%
  rename(Temperature = Value)

all_states_min <- all_states_min %>%
  mutate(Temperature = (Temperature - 32) * 5 / 9)

all_states_min <- all_states_min %>%
  mutate(
    Date = as.character(Date),  
    Year = substr(Date, 1, 4),
    Year = as.numeric(Year),
  )

min_temp_per_year <- all_states_min %>%
  group_by(State, Year) %>%
  filter(Temperature == min(Temperature, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(State, Year, Temperature)

min_temp_per_year <- min_temp_per_year %>%
  rename(Min_Temperature = Temperature)

min_temp_per_year <- min_temp_per_year %>%
  mutate(Min_Temperature = round(Min_Temperature, 2)) %>%  
  arrange(State)%>%  # Alphabetic order
  distinct(State, Year, .keep_all = TRUE)

#COMBINED DATASET (honey + pesticides + natural disasters + precipitation + 
#                   palmer index + max/min temperature)
df_combined3 <- df_combined3 %>%
  left_join(min_temp_per_year, by = c("state" = "State", "year" = "Year"))


#df_combined3 = FINAL DATASET

  #Check missing values
missing_values <- colSums(is.na(df_combined3))
missing_values

  #check duplicates
df_combined3 %>%
  group_by(state, year) %>%
  summarise(count = n()) %>%
  filter(count > 1)



################################################################################
#CLUSTERING

us_states <- ne_states(country = "United States of America", returnclass = "sf")
us_states_area <- us_states %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%  #Convert area in m² and then in km²
  dplyr::select(name, area_km2) %>%
  st_set_geometry(NULL)

all_years_data <- all_years_data %>%
  left_join(us_states_area, by = c("state" = "name"))

all_years_data$colperkm2 <- all_years_data$numcol/all_years_data$area_km2

  #Select relevant variables
all_years_data <- all_years_data %>%
  dplyr::select(state, year, priceperlb, yieldpercol, colperkm2)

  #Analysis of Last available year
Last_year <- all_years_data %>%
  summarise(last_year = max(year))
last_year

df_2021 <- all_years_data %>%
  filter(year == 2021) %>%
  dplyr::select(-year)

states_count <- df_2021 %>%
  summarise(num_states = n_distinct(state))
states_count

  #Selected year: 2021
df_2021_clustering <- df_2021 %>%
  dplyr::select(priceperlb, yieldpercol, colperkm2) %>%
  na.omit()
df_2021_scaled <- scale(df_2021_clustering)

plot_ly(
  data = df_2021,
  x = ~priceperlb,
  y = ~yieldpercol,
  z = ~colperkm2,
  text = ~state, # Add state names as text
  hoverinfo = "text", # Display only the text on hover
  colors = c("#FFD700")  # Set the color of markers to black
) %>%
  add_markers(
    marker = list(
      color = "#FFD700",      # Set the color to black
      size = 6,             # Adjust the size of the markers if needed
      opacity = 1,             # Set the opacity (optional)
      line = list(
        color = "#CD6600",       # Set the border color
        width = 1.5           # Set the border width
      )           # Set the opacity (optional)
    )
  ) %>%
  layout(
    scene = list(
      xaxis = list(title = "Price per lb"),
      yaxis = list(title = "Yield per col"),
      zaxis = list(title = "Col per km2")
    ),
    title = "Data visualization 2021"
  )                  # -> Plot suggest presence of outliers


#K-MEAN
  #Choose the optimal number of clusters
set.seed(123) #For reproducibility
  
  #Elbow Method
wss <- sapply(1:6, function(k) {
  kmeans(df_2021_scaled, centers = k, nstart = 25)$tot.withinss
})
  #Plot Elbow Method
plot(1:6, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters", 
     ylab = "Total Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Method",
     font.main = 1)

  #Silhouette Score
silhouette_scores <- sapply(2:6, function(k) {
  km <- kmeans(df_2021_scaled, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(df_2021_scaled))
  mean(ss[, 3]) # Average silhouette width
})

  #Plot Silhouette Scores
plot(2:6, silhouette_scores, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters", 
     ylab = "Average Silhouette Score",
     main = "Silhouette Method",
     font.main = 1)

# -> Optimal number = 4

kmeans_result <- kmeans(df_2021_scaled, centers = 4)
df_2021 <- df_2021 %>%
  filter(complete.cases(priceperlb, yieldpercol, colperkm2)) %>%
  mutate(cluster = kmeans_result$cluster)

sil2 <- silhouette(kmeans_result$cluster, dist(df_2021_scaled))
  
  #Plot silhouette scores
fviz_silhouette(sil2) +
  labs(title = "Silhouette Plot (K-Means)") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5)
    )+
  scale_fill_manual(values = c("#FF7F00", "#FFD700", "#556B2F", "#BCEE68"))+
  scale_color_manual(values = c("#FF7F00", "#FFD700", "#556B2F", "#BCEE68"))

# -> misclassified units

plot_ly(
  data = df_2021,
  x = ~priceperlb,
  y = ~yieldpercol,
  z = ~colperkm2,
  color = ~factor(cluster),
  text = ~state, 
  hoverinfo = "text", 
  colors = c("#FF7F00", "#FFD700", "#556B2F", "#BCEE68") 
) %>%
  add_markers() %>%
  layout(
    scene = list(
      xaxis = list(title = "Price per lb"),
      yaxis = list(title = "Yield per col"),
      zaxis = list(title = "Col per km2")
    ),
    title = "Clusters 2021"
  )

centroids_kmeans <- kmeans_result$centers %>%
  as.data.frame() %>%
  mutate(
    priceperlb = (priceperlb * sd(df_2021_clustering$priceperlb)) + mean(df_2021_clustering$priceperlb),
    yieldpercol = (yieldpercol * sd(df_2021_clustering$yieldpercol)) + mean(df_2021_clustering$yieldpercol),
    colperkm2 = (colperkm2 * sd(df_2021_clustering$colperkm2)) + mean(df_2021_clustering$colperkm2)
  )

centroids_kmeans


#CHECK FOR THE OUTLIERS
cov_matrix <- cov(df_2021_scaled)
mean_vector <- colMeans(df_2021_scaled)
mahalanobis_dist <- mahalanobis(df_2021_scaled, mean_vector, cov_matrix)

  #Define a threshold for outliers (e.g., > 95th percentile)
threshold <- quantile(mahalanobis_dist, 0.95)

  #Identify outliers
outliers <- mahalanobis_dist > threshold

  #Plot Mahalanobis distances
plot(mahalanobis_dist, 
     main = "Mahalanobis Distance", 
     xlab = "Observations", 
     ylab = "Mahalanobis Distance", 
     pch = 19, 
     col = ifelse(outliers, "#ffa93f", "black"),
     font.main = 1)
abline(h = threshold, col = "#ffa93f", lwd = 2, lty = 2) # Add threshold line
legend("topleft", legend = c("Outlier", "Threshold (95th percentile)"), # Add legend
       col = c("#ffa93f", "#ffa93f"), pch = c(19, NA), lty = c(NA, 2))
text(     # Add state names to the outliers
  x = which(outliers), 
  y = mahalanobis_dist[outliers], 
  labels = df_2021$state[outliers], # State names for outliers
  pos = 4, 
  cex = 0.8,
  col = "black"
)

  #Robust Mahalanobis distances
cov_robust <- cov.mcd(df_2021_scaled)
mean_robust <- cov_robust$center
cov_robust_matrix <- cov_robust$cov

mahalanobis_robust_dist <- mahalanobis(df_2021_scaled, mean_robust, cov_robust_matrix)
threshold_robust <- quantile(mahalanobis_robust_dist, 0.95)
outliers_robust <- mahalanobis_robust_dist > threshold_robust

  #Plot robust Mahalanobis distances
plot(mahalanobis_robust_dist, 
     main = "Robust Mahalanobis Distance", 
     xlab = "Observations", 
     ylab = "Robust Mahalanobis Distance", 
     pch = 19, 
     col = ifelse(outliers_robust, "#FF7F00", "black"),
     font.main = 1)
abline(h = threshold_robust, col = "#FF7F00", lwd = 2, lty = 2) # Add threshold line
legend("topleft", legend = c("Outlier", "Threshold (95th percentile)"), # Add legend
       col = c("#FF7F00", "#FF7F00"), pch = c(19, NA), lty = c(NA, 2))
text(
  x = which(outliers_robust), 
  y = mahalanobis_robust_dist[outliers_robust], 
  labels = df_2021$state[outliers_robust], 
  pos = 4, 
  cex = 0.8, 
  col = "black"
)

  #Distance-distance plot
outliers_combined <- (mahalanobis_dist > threshold) | (mahalanobis_robust_dist > threshold_robust)

plot(
  mahalanobis_dist, mahalanobis_robust_dist, 
  xlab = "Mahalanobis Distance", 
  ylab = "Robust Mahalanobis Distance", 
  main = "Distance-Distance Plot",
  pch = 21, bg = "#FFD700", font.main = 1
)
abline(h = threshold_robust, col = "#FF7F00", lwd = 2, lty = 2)
abline(v = threshold, col = "#ffa93f", lwd = 2, lty = 2)
text(
  x = mahalanobis_dist[outliers_combined], # X-coordinates of outliers
  y = mahalanobis_robust_dist[outliers_combined], # Y-coordinates of outliers
  labels = df_2021$state[outliers_combined], # State names for outliers
  pos = 2, 
  cex = 0.8, 
  col = "black"
)


#ROBUST CLUSTERING
alpha <- 0.05 
set.seed(123) 

  #Trying different k
k_values <- 2:5

  #Robust clustering for each value of k and add the cluster to the dataframe
for (k in k_values) {
  robust_kmeans <- tkmeans(df_2021_scaled, k = k, alpha = alpha, nstart = 25)
  df_2021[[paste0("cluster", k)]] <- robust_kmeans$cluster
}

  #Plot silhouette scores
silhouette_plots <- list()

# Cycle for every value of k
for (k in k_values) {
  robust_kmeans <- tkmeans(df_2021_scaled, k = k, alpha = alpha, nstart = 25)
  
  # Silhouette score
  sil <- silhouette(robust_kmeans$cluster, dist(df_2021_scaled))
  
  # Silhouette plot
  silhouette_plots[[paste0("k_", k)]] <- 
    fviz_silhouette(sil) +
    labs(title = paste("Silhouette Plot (Trimmed K-Means, k =", k, ")")) +
    theme_minimal() +
    scale_fill_manual(values = c('black',"#FF7F00", "#FFD700", "#556B2F", "#BCEE68", "red"))+
    scale_color_manual(values = c('black',"#FF7F00", "#FFD700", "#556B2F", "#BCEE68", "red")) # Cambia colore qui
}

# Visualize plots for every value of k
for (plot_name in names(silhouette_plots)) {
  print(silhouette_plots[[plot_name]])
}

centroids_trimmed_2021 <- df_2021 %>%
  group_by(cluster4) %>%
  summarise(
    priceperlb = mean(priceperlb),
    yieldpercol = mean(yieldpercol),
    colperkm2 = mean(colperkm2)
  )

centroids_trimmed_2021

  #k=3
plot_ly(
  data = df_2021,
  x = ~priceperlb,
  y = ~yieldpercol,
  z = ~colperkm2,
  color = ~factor(cluster3),
  text = ~state, # Add state names as text
  hoverinfo = "text", # Display only the text on hover
  colors = c("black", "#FF7F00", "#FFD700", "#BCEE68") # Adjust colors as needed
) %>%
  add_markers() %>%
  layout(
    scene = list(
      xaxis = list(title = "Price per lb"),
      yaxis = list(title = "Yield per col"),
      zaxis = list(title = "Col per km2")
    ),
    title = "Robust Clusters 2021 (k = 3)"
  )

  #k=4 
plot_ly(
  data = df_2021,
  x = ~priceperlb,
  y = ~yieldpercol,
  z = ~colperkm2,
  color = ~factor(cluster4),
  text = ~state, # Add state names as text
  hoverinfo = "text", # Display only the text on hover
  colors = c('black',"#FF7F00", "#FFD700", "#556B2F", "#BCEE68") # Adjust colors as needed
) %>%
  add_markers() %>%
  layout(
    scene = list(
      xaxis = list(title = "Price per lb"),
      yaxis = list(title = "Yield per col"),
      zaxis = list(title = "Col per km2")
    ),
    title = "Robust Clusters 2021 (k = 4)"
  )

First_year <- all_years_data %>%
  summarise(first_year = min(year))
first_year

df_1991 <- all_years_data %>%
  filter(year == 1991) %>%
  dplyr::select(-year)

states_count <- df_1991 %>%
  summarise(num_states = n_distinct(state))
states_count

#Selected year: 1991
df_1991_clustering <- df_1991 %>%
  dplyr::select(priceperlb, yieldpercol, colperkm2) %>%
  na.omit()
df_1991_scaled <- scale(df_1991_clustering)

plot_ly(
  data = df_1991,
  x = ~priceperlb,
  y = ~yieldpercol,
  z = ~colperkm2,
  text = ~state, # Add state names as text
  hoverinfo = "text", # Display only the text on hover
  colors = c("#FFD700")  # Set the color of markers to black
) %>%
  add_markers(
    marker = list(
      color = "#FFD700",      # Set the color to black
      size = 6,             # Adjust the size of the markers if needed
      opacity = 1,             # Set the opacity (optional)
      line = list(
        color = "#FF7F00",       # Set the border color
        width = 1.5           # Set the border width
      )           # Set the opacity (optional)
    )
  ) %>%
  layout(
    scene = list(
      xaxis = list(title = "Price per lb"),
      yaxis = list(title = "Yield per col"),
      zaxis = list(title = "Col per km2")
    ),
    title = "Data visualization 1991"
  )                  # -> Plot suggest presence of outliers


robust_kmeans4 <- tkmeans(df_1991_scaled, k = 4, alpha = alpha, nstart = 25)
df_1991 <- df_1991 %>%
  filter(complete.cases(priceperlb, yieldpercol, colperkm2)) %>%
  mutate(cluster4 = robust_kmeans4$cluster)

sil <- silhouette(robust_kmeans4$cluster, dist(df_1991_scaled))
  #Plot silhouette scores
fviz_silhouette(sil) +
  labs(title = "Silhouette Plot (Trimmed K-Means)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "plain"), # Centra il titolo
    legend.position = "bottom" # Sposta la legenda in basso (opzionale)
  ) +
  scale_fill_manual(values = c("#FF7F00", "#ffa93f", "#FFD700", "#556B2F", "#BCEE68")) +  # Colori per le barre
  scale_color_manual(values = c("#FF7F00", "#ffa93f", "#FFD700", "#556B2F", "#BCEE68"))   # Colori per i bordi delle barre

centroids_trimmed_1991 <- df_1991 %>%
  group_by(cluster4) %>%
  summarise(
    priceperlb = mean(priceperlb),
    yieldpercol = mean(yieldpercol),
    colperkm2 = mean(colperkm2)
  )

centroids_trimmed_1991

plot_ly(
  data = df_1991,
  x = ~priceperlb,
  y = ~yieldpercol,
  z = ~colperkm2,
  color = ~factor(cluster4),
  text = ~state, # Add state names as text
  hoverinfo = "text", # Display only the text on hover
  colors = c('black',"#FF7F00","#556B2F", "#FFD700","#BCEE68") # Adjust colors as needed
) %>%
  add_markers() %>%
  layout(
    scene = list(
      xaxis = list(title = "Price per lb"),
      yaxis = list(title = "Yield per col"),
      zaxis = list(title = "Col per km2")
    ),
    title = "Robust Clusters 1991 (k = 4)"
  )


#MAP OF CLUSTER-STATE
  #1991
df_map <- df_1991 %>%
  mutate(state_lower = tolower(state))

states_map <- map_data("state")

plot_data <- left_join(states_map, df_map, by = c("region" = "state_lower"))

state_labels <- data.frame(
  state = state.name,
  long = state.center$x,
  lat = state.center$y
) %>%
  mutate(state_lower = tolower(state)) %>%
  left_join(df_map, by = "state_lower")

  #Plot the map with clusters
ggplot(plot_data, aes(x = long, y = lat, group = group, fill = factor(cluster4))) +
  geom_polygon(color = "white") + 
  coord_fixed(1.3) +
  theme_void() +
  scale_fill_manual(values = c("#2F4F4F","#FF7F00","#556B2F", "#FFD700", "#BCEE68"),
                    na.value = "#C1CDC1",
                    labels = c("Outliers", "Low price - High production", "Low price - Low production", "High price - Low production", "High colony per km2")
                    )+
  labs(title = "Clusters by State (1991)", fill = "Cluster") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "plain"),
    legend.position = c(0.81, 0.2),
    legend.justification = "left")+
  geom_text(data = state_labels, aes(x = long, y = lat, label = state.x), 
            color = "black", size = 3, inherit.aes = FALSE)

  #2021
df_map_2021 <- df_2021 %>%
  mutate(state_lower = tolower(state))

states_map <- map_data("state")

plot_data_2021 <- left_join(states_map, df_map_2021, by = c("region" = "state_lower"))

state_labels_2021 <- data.frame(
  state = state.name,
  long = state.center$x,
  lat = state.center$y
) %>%
  mutate(state_lower = tolower(state)) %>%
  left_join(df_map_2021, by = "state_lower")

  #Plot the map with clusters for 2021
ggplot(plot_data_2021, aes(x = long, y = lat, group = group, fill = factor(cluster4))) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  theme_void() +
  scale_fill_manual(
    values = c("#2F4F4F", "#FF7F00", "#FFD700", "#556B2F", "#BCEE68"),
    na.value = "#C1CDC1",
    labels = c("Outliers", "Low price - High production", "Low price - Low production", "High price - Low production", "High colony per km2")
  ) +
  labs(
    title = "Clusters by State (2021)", 
    fill = "Cluster"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "plain"),
    legend.position = c(0.81, 0.2),
    legend.justification = "left"
  ) +
  geom_text(
    data = state_labels_2021, 
    aes(x = long, y = lat, label = state.x), 
    color = "black", 
    size = 3, 
    inherit.aes = FALSE
  )



################################################################################
#YEAR-STATE REGRESSION

  #General decreasing trend in honey production
ggplot(df_combined3) +
  geom_point(aes(year, yieldpercol)) +
  scale_y_continuous(labels = scales::label_number()) + 
  facet_wrap(~ state, scales = "free_y") +  
  labs(title = "Production per Colony Over Time by State", x = "Year", y = "Production per Colony") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5)          
  )


# 1) FULL POOLING - Standard LM (fixed-effect model)
fit_lm <- lm(yieldpercol~year,data = df_combined3)
summary(fit_lm)

ggplot(df_combined3, aes(x = year, y = yieldpercol)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "#FFA500") + 
  scale_y_continuous(labels = scales::label_number()) + 
  labs(title = "Average Yield Over Time", 
       x = "Year", 
       y = "Production") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5)          
  )

  #MSE
df_combined3$pred_fullpooling <- predict(fit_lm, re.form = NULL)
df_combined3$residuals_full <- df_combined3$yieldpercol - df_combined3$pred_fullpooling
mse1 <- mean(df_combined3$residuals_full^2)
rmse1 <- sqrt(mse1)
print(rmse1)


# 2) NO POOLING - Separate model for each state
fit_lm_no_pooling <- lm(yieldpercol~year*state,data = df_combined3)
summary(fit_lm_no_pooling)

ggplot(df_combined3, aes(x = year, y = yieldpercol, col = state)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  
  scale_y_continuous(labels = scales::label_number()) +
  labs(title = "Production Over Time", 
       x = "Year", 
       y = "Production") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5))

  #MSE
df_combined3$pred_nopooling <- predict(fit_lm_no_pooling, re.form = NULL)
df_combined3$residuals_no <- df_combined3$yieldpercol - df_combined3$pred_nopooling
rmse2 <- sqrt(mean(df_combined3$residuals_no^2))
print(rmse2)


# 3) MIXED-EFFECTS MODEL - Year as fixed and state as variable
fit_mixed <- lmer(yieldpercol ~ year + (1 | state), data = df_combined3)
summary(fit_mixed)

df_combined3$pred_mixed <- predict(fit_mixed, re.form = NULL)  # Predict with random effects

ggplot(df_combined3, aes(x = year, y = yieldpercol, col = state)) +
  geom_point(size = 1) +
  geom_line(aes(y = pred_mixed), linewidth = 0.7) +
  scale_y_continuous(labels = scales::label_number()) +  
  labs(title = "Average Yield Over Time with Mixed-Effects Model", 
       x = "Year", 
       y = "Production") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5))

  #MSE
df_combined3$residuals <- df_combined3$yieldpercol - df_combined3$pred_mixed
rmse3 <- sqrt(mean(df_combined3$residuals^2))
print(rmse3)


            ##############################
      #DOES IT MAKE SENSE TO USE A MIXED MODEL?

# 1) To verify if residuals are consistent with the model assumptions
model <- lmer(yieldpercol ~ year + (1 | state), data = df_combined3)
par(mfrow = c(1, 2))
plot(resid(model) ~ fitted(model), main = "Residuals vs Predicted Values",
     font.main = 1,
     xlab = "Fitted Values",
     ylab = "Residuals")
qqnorm(resid(model),
       font.main = 1)
qqline(resid(model))

# 2) Does it make sense to consider the year as fixed?
model_simple <- lmer(yieldpercol ~ (1 | state), data = df_combined3)
model_full <- lmer(yieldpercol ~ year + (1 | state), data = df_combined3)
anova(model_simple, model_full)

# 3) Does it make sense to consider the state as variable?
icc <- performance::icc(model_full)
print(icc)

# 4) Relation between predicted and observed values
par(mfrow = c(1, 1))

df_combined3$predicted <- predict(model)
ggplot(df_combined3, aes(x = predicted, y = yieldpercol)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "#FFA500", size = 1) +
  labs(title = "Predicted vs observed values", x = "Predicted values", y = "Observed values") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5))

# 5) Dotplot
dotplot(ranef(model_full, condVar = TRUE))

            ##############################



################################################################################
#VARIABLES ANALYSIS - MULTIPLE CORRELATION AND OUTLIERS

  #Keep only useful variables
df_clean <- df_combined3[c('yieldpercol','year','state', 'nCLOTHIANIDIN_km2','nIMIDACLOPRID_km2',
                           'nTHIAMETHOXAM_km2','nACETAMIPRID_km2','nTHIACLOPRID_km2','Max_AQI',
                           'Days_CO','Days_NO2','Days_Ozone','Percent_Good_Days','Percent_Unhealthy_Days',
                           'Days_PM2.5', 'Days_PM10',
                           'Flood','Fire','Storm_Group','Precipitation','Anomaly_prec','palmer_index',
                           'Max_Temperature','Min_Temperature')]

df_clean_subset <- df_clean[ , -(1:3)]  
str(df_clean_subset) #check if all the variables are numerical
df_clean_subset[] <- lapply(df_clean_subset, function(x) {
  if (is.character(x)) {
    as.numeric(as.character(x))
  } else {
    x
  }
})

str(df_clean_subset)


#CORRELATION MATRIX AND HEATMAP
cor_matrix <- cor(df_clean_subset)
print(cor_matrix)

zero_sd_columns <- sapply(df_clean_subset, sd) == 0
names(zero_sd_columns[zero_sd_columns])

corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         tl.cex = 0.8, 
         tl.col = "black", 
         col = colorRampPalette(c("#FF7F00", "white", "#556B2F"))(200)) 

df_clean <- df_clean[ , -(2:3)]

str(df_clean) 

df_clean[] <- lapply(df_clean, function(x) {
  if (is.character(x)) {
    as.numeric(as.character(x))
  } else {
    x
  }
})


#VIF - Variance inflation factor
df_clean_standardized <- df_clean %>%
  mutate(across(where(is.numeric), ~ scale(.) %>% as.vector()))

lm_fixed_all_var <- lm(yieldpercol ~ . , data = df_clean_standardized)
summary(lm_fixed_all_var)

vif_values <- vif(lm_fixed_all_var)
print(vif_values)

vif_values <- as.data.frame(vif_values)
sqrt(vif_values)>1.5

      #Pesticides columns are correlated: nacet, nthis: high percentage of 0 -> keep only the sum column
      #Temp max and min are correlated -> keep just max
      #Delete Percent_Good_Days -> keep just unhealthy days

df_clean1 <- df_combined3[c('yieldpercol','year','state', 'nAllNeonic_km2','Max_AQI',
                            'Days_CO','Days_NO2','Days_Ozone','Percent_Unhealthy_Days',
                            'Days_PM2.5', 'Days_PM10',
                            'Flood','Fire','Storm_Group','Precipitation','Anomaly_prec','palmer_index',
                            'Max_Temperature')]

str(df_clean1)

df_clean1 <- df_clean1 %>%
  mutate(across(.cols = -state, .fns = as.numeric))

df_clean_standardized1 <- df_clean1 %>%
  mutate(across(where(is.numeric), ~ scale(.) %>% as.vector()))

#Check correlations for the clean dataset
  #VIF
lm_fixed_all_var <- lm(yieldpercol ~ . -state -year , data = df_clean_standardized1)  
summary(lm_fixed_all_var)

vif_values <- vif(lm_fixed_all_var)
vif_values <- as.data.frame(vif_values)
sqrt(vif_values)>1.5  # -> all FALSE

df_clean_subset1 <- df_clean1[ , -(1:3)]

  #Correlation and heatmap
cor_matrix <- cor(df_clean_subset1)
print(cor_matrix)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8,tl.col = "black", col = colorRampPalette(c("#FF7F00", "white", "#556B2F"))(200)) 
  # -> all uncorrelated


#DATAFRAME WITH USEFUL STATISTICS OF THE CHOOSEN VARIABLES
summary(df_clean1)
str(df_clean1)
options(scipen = 10)

numeriche <- df_clean1[sapply(df_clean1, is.numeric)]

  #Statistics
media <- sapply(numeriche, mean, na.rm = TRUE)
mediana <- sapply(numeriche, median, na.rm = TRUE)
varianza <- sapply(numeriche, var, na.rm = TRUE)
trimmed_media <- sapply(numeriche, mean,trim = 0.1, na.rm = TRUE)
winsorized_media <- sapply(numeriche, function(x) winsor.mean(x, trim = 0.1, na.rm = TRUE))
mad_values <- sapply(numeriche, mad, constant = 1, na.rm = TRUE)
madmean_values <- sapply(numeriche, MeanAD, na.rm = TRUE)

  #Creation of a table with robust statistics
statistiche_tabella <- data.frame(
  Mean = media,
  Trimmed_mean = trimmed_media,
  Winsorized_media = winsorized_media,
  Median = mediana,
  Variance = varianza,
  Std_error = sqrt(varianza),
  Mean_abs_deviation = madmean_values,
  Median_abs_deviation = mad_values
)


#VISUALIZATION OF USEFUL VARIABLES
  #Boxplot
par(font.main = 1)
boxplot(numeriche, main = "Boxplot of numerical variables", col = "#FFA500", las = 2)

  #Boxplot for standardized variables
numeriche_std <- numeriche %>%
  mutate(across(where(is.numeric), ~ scale(.)[, 1]))

par(font.main = 1)
boxplot(numeriche_std, 
        main = "Boxplot of standardized numerical variables", 
        col = "#FFA500", 
        las = 2, 
        font.main = 1, 
        xaxt = "n")  
axis(1, at = 1:ncol(numeriche_std), labels = FALSE)  
text(x = 1:ncol(numeriche_std), 
     y = par("usr")[3] - 0.5, 
     labels = colnames(numeriche_std),  
     srt = 45,       
     adj = 1,        
     xpd = TRUE,     
     cex = 0.8)    

numeriche_std <- numeriche_std[, !names(numeriche_std) %in% "yieldpercol"]
numeriche_std <- numeriche_std[, !names(numeriche_std) %in% "year"]


#OUTLIERS DETECTION
#MAHALANOBIS DISTANCE
cov_matrix <- cov(numeriche_std)
mean_vector <- colMeans(numeriche_std)

  #Mahalanobis distance for every observation
mahalanobis_dist <- mahalanobis(numeriche_std, mean_vector, cov_matrix)

  #Threshold for outliers identification (distances > 95° percentile)
threshold <- quantile(mahalanobis_dist, 0.95)

  #Outliers identification
outliers <- numeriche_std[mahalanobis_dist > threshold, ]

  #Number and percentage of outliers
num_outliers <- nrow(outliers)
cat("Number of outliers:", num_outliers, "\n")
num_total <- nrow(numeriche_std)
percentage_outliers <- (num_outliers / num_total) * 100
cat("Percentage of outliers:", round(percentage_outliers, 2), "%\n")

  #Mahalanobis distance plot
plot(mahalanobis_dist, 
     main = "Mahalanobis Distance", 
     xlab = "Observation", 
     ylab = "Mahalanobis Distance", 
     pch = 19, 
     col = ifelse(mahalanobis_dist > threshold, "#ffa93f", "black"))

abline(h = threshold, col = "#ffa93f", lwd = 2, lty = 2)

legend("top", legend = c("Outlier", "Threshold (95th percentile)"), 
       col = c("#ffa93f", "#ffa93f"), pch = c(19, NA), lty = c(NA, 2))

#ROBUST MAHALANOBIS DISTANCE
  #Check variables with IQR = 0 and remove them -> fire and flood
apply(numeriche_std, 2, IQR)
numeriche_std <- numeriche_std[, apply(numeriche_std, 2, IQR) > 0]

  #Robust estimation of covariance using MCD
cov_robust <- cov.mcd(numeriche_std)
  #Robust mean using MCD
mean_robust <- cov_robust$center

mahalanobis_robust_dist <- mahalanobis(numeriche_std, mean_robust, cov_robust$cov)
threshold_robust <- quantile(mahalanobis_robust_dist, 0.95)
outliers_robust <- numeriche_std[mahalanobis_robust_dist > threshold_robust, ]

num_outliers_robust <- nrow(outliers_robust)
num_total <- nrow(numeriche_std)
percentage_outliers_robust <- (num_outliers_robust / num_total) * 100
cat("Number of robust outliers:", num_outliers_robust, "\n")
cat("Percentage of robust outliers:", round(percentage_outliers_robust, 2), "%\n")

  #Robust Mahalanobis Distance Plot
plot(mahalanobis_robust_dist, 
     main = "Robust Mahalanobis Distance", 
     xlab = "Observation", 
     ylab = "Robust Mahalanobis Distance", 
     pch = 19, 
     col = ifelse(mahalanobis_robust_dist > threshold_robust, "#FF7F00", "black"))

abline(h = threshold_robust, col = "#FF7F00", lwd = 2, lty = 2)

legend("top", legend = c("Outlier", "Threshold (95th percentile)"), 
       col = c("#FF7F00", "#FF7F00"), pch = c(19, NA), lty = c(NA, 2))


#OUTLIERS REMOVAL
threshold_estremi <- quantile(mahalanobis_robust_dist, 0.98)

data_noout <- numeriche_std[mahalanobis_robust_dist < threshold_estremi, ]
df_model <- df_clean_standardized1[mahalanobis_robust_dist < threshold_estremi, ]

  #Extreme outliers rows
indici_estremi <- which(mahalanobis_robust_dist >= threshold_estremi)
deleted_outliers <- df_clean1[indici_estremi, ]
  # -> All data about California


#MAHALNOBIS DISTANCE AND PLOTS WITHOUT OUTLIERS
#Mahalanobis distance
cov_matrix <- cov(data_noout)
mean_vector <- colMeans(data_noout)

mahalanobis_dist <- mahalanobis(data_noout, mean_vector, cov_matrix)
threshold <- quantile(mahalanobis_dist, 0.95)

outliers <- data_noout[mahalanobis_dist > threshold, ]

plot(mahalanobis_dist, 
     main = "Mahalanobis Distance", 
     xlab = "Observation", 
     ylab = "Mahalanobis Distance", 
     pch = 19, 
     col = ifelse(mahalanobis_dist > threshold, "#ffa93f", "black"))

abline(h = threshold, col = "#ffa93f", lwd = 2, lty = 2)

legend("topright", legend = c("Outlier", "Threshold (95th percentile)"), 
       col = c("#ffa93f", "#ffa93f"), pch = c(19, NA), lty = c(NA, 2))

#Robust Mahalanobis Distance
cov_robust <- cov.mcd(data_noout)
mean_robust <- cov_robust$center

mahalanobis_robust_dist <- mahalanobis(data_noout, mean_robust, cov_robust$cov)
threshold_robust <- quantile(mahalanobis_robust_dist, 0.95)

outliers_robust <- data_noout[mahalanobis_robust_dist > threshold_robust, ]

plot(mahalanobis_robust_dist, 
     main = "Robust Mahalanobis Distance", 
     xlab = "Observation", 
     ylab = "Robust Mahalanobis Distance", 
     pch = 19, 
     col = ifelse(mahalanobis_robust_dist > threshold_robust, "#FF7F00", "black"))

abline(h = threshold_robust, col = "#FF7F00", lwd = 2, lty = 2)

legend("topright", legend = c("Outlier", "Threshold (95th percentile)"), 
       col = c("#FF7F00", "#FF7F00"), pch = c(19, NA), lty = c(NA, 2))


#DISTANCE-DISTANCE PLOT
plot(
  mahalanobis_dist, mahalanobis_robust_dist, 
  xlab = "Mahalanobis distance", 
  ylab = "Robust distance", 
  main = "Distance-Distance Plot",
  pch = 21, bg = "#FFD700"
)

abline(h = threshold_robust, col = "#FF7F00", lwd = 2, lty = 2)
abline(v = threshold, col = "#ffa93f", lwd = 2, lty = 2)



################################################################################
#BEST SUBSET SELECTION

  #Df without extreme outliers and "year" variable, not useful for the analysis
df_model <- df_model[, !names(df_model) %in% "year"]

#FORWARD SELECTION
  #Starts with no predictors and adds one predictor at a time,
  #choosing the predictor that improves the model fit the most at each step.
  #For mixed-effects models, this process involves evaluating both the fixed 
  #effects and their interactions with random effects.
lm_mixed <- lmer(yieldpercol ~ . -state +(1 | state), data = df_model)
summary(lm_mixed)

null_model <- lmer(yieldpercol ~ (1 | state), data = df_model)
summary(null_model)

names(df_model)

predictors <- setdiff(names(df_model), c("yieldpercol", "state"))
predictors

# 1) AIC
  #AIC based on loglikelihood function, penalizes more complex models
forward_search <- function(response, predictors, random_effect, data) {
  selected_predictors <- c() #Selected predictors
  remaining_predictors <- predictors #Remaining predictors
  best_aic <- AIC(null_model) #AIC of null model
  current_model <- null_model #Current model
  
  #Track the steps
  step_results <- data.frame(
    step = integer(),
    predictor_added = character(),
    AIC_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  step_count <- 0
  
  repeat {
    aic_values <- sapply(remaining_predictors, function(predictor) {
      formula <- as.formula(paste(response, "~", 
                                  paste(c(selected_predictors, predictor), collapse = " + "),
                                  "+ (1|", random_effect, ")"))
      model <- lmer(formula, data = data)
      return(AIC(model))
    })
    
    best_new_predictor <- names(which.min(aic_values))
    new_aic <- min(aic_values)
    
    if (new_aic < best_aic) {
      best_aic <- new_aic
      selected_predictors <- c(selected_predictors, best_new_predictor)
      remaining_predictors <- setdiff(remaining_predictors, best_new_predictor)
      
      current_model <- lmer(as.formula(paste(response, "~", 
                                             paste(selected_predictors, collapse = " + "),
                                             "+ (1|", random_effect, ")")), 
                            data = data)
      step_count <- step_count + 1
      #Save results
      step_results <- rbind(step_results, data.frame(
        step = step_count,
        predictor_added = best_new_predictor,
        AIC_value = best_aic,
        stringsAsFactors = FALSE
      ))
      
    } else {
      break
    }
  }
  
  return(list(model = current_model, predictors = selected_predictors, step_results = step_results))
}

  #Forward research
result_aic <- forward_search(response = "yieldpercol", 
                             predictors = predictors, 
                             random_effect = "state", 
                             data = df_model)

final_model_aic <- result_aic$model
selected_predictors_aic <- result_aic$predictors
steps_aic <- result_aic$step_results
steps_aic

selected_predictors_aic


# 2) MALLOW CP INDEX
  #Considers residual error and the number of estimated parameters
calculate_cp <- function(model, full_model, n) {
  rss <- sum(resid(model)^2)                  # RSS of current model
  sigma2 <- as.numeric(attr(VarCorr(full_model), "sc")^2)  # Residual variance of the full model
  p <- length(fixef(model)) + 1               # Number of estimated parameters
  cp <- (rss / sigma2) - (n - 2 * p)          # Cp formula
  return(cp)
}

full_model <- lmer(yieldpercol ~ . - state + (1 | state), data = df_model)
n <- nrow(df_model)

forward_search_with_cp <- function(response, predictors, random_effect, data, full_model, n) {
  selected_predictors <- c()
  remaining_predictors <- predictors
  best_cp <- Inf
  current_model <- null_model
  
  step_results_cp <- data.frame(
    step = integer(),
    predictor_added = character(),
    Cp_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  step_count <- 0
  
  repeat {
    cp_values <- sapply(remaining_predictors, function(predictor) {
      formula <- as.formula(paste(response, "~", 
                                  paste(c(selected_predictors, predictor), collapse = " + "),
                                  "+ (1|", random_effect, ")"))
      model <- lmer(formula, data = data)
      cp <- calculate_cp(model, full_model, n)
      return(cp)
    })
    
    best_new_predictor <- names(which.min(cp_values))
    new_cp <- min(cp_values)
    
    if (new_cp < best_cp) {
      best_cp <- new_cp
      selected_predictors <- c(selected_predictors, best_new_predictor)
      remaining_predictors <- setdiff(remaining_predictors, best_new_predictor)
      current_model <- lmer(as.formula(paste(response, "~", 
                                             paste(selected_predictors, collapse = " + "),
                                             "+ (1|", random_effect, ")")), 
                            data = data)
      
      step_count <- step_count + 1
      step_results_cp <- rbind(step_results_cp, data.frame(
        step = step_count,
        predictor_added = best_new_predictor,
        Cp_value = best_cp,
        stringsAsFactors = FALSE
      ))
      
    } else {
      break
    }
  }
  
  return(list(model = current_model, predictors = selected_predictors, cp = best_cp, step_results = step_results_cp))
}

  #Forward research
result_cp <- forward_search_with_cp(response = "yieldpercol", 
                                    predictors = predictors, 
                                    random_effect = "state", 
                                    data = df_model, 
                                    full_model = full_model, 
                                    n = n)

final_model_cp <- result_cp$model
selected_predictors_cp <- result_cp$predictors
final_cp <- result_cp$cp
steps_cp <- result_cp$step_results
steps_cp

selected_predictors_cp

  #Mallow CP Index Plot
ggplot(steps_cp, aes(x = step, y = Cp_value, label = predictor_added)) +
  geom_line() + 
  geom_point() +
  geom_text(aes(y = Cp_value - 1.5), size = 3, angle = 360, hjust = 0.5) +
  labs(title = "Evolution of Cp in Forward Selection", x = "Step", y = "Cp") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5)         
  )


#NEW MODEL WITH BEST SUBSET SELECTION
df_model1 <- df_model %>%
  dplyr::select(Days_PM2.5, nAllNeonic_km2, Days_NO2, palmer_index, Days_CO)

  #Robust Mahalanobis Distance
cov_robust <- cov.mcd(df_model1)
mean_robust <- cov_robust$center

mahalanobis_robust_dist <- mahalanobis(df_model1, mean_robust, cov_robust$cov)
threshold_robust <- quantile(mahalanobis_robust_dist, 0.95)

outliers_robust <- df_model1[mahalanobis_robust_dist > threshold_robust, ]

plot(mahalanobis_robust_dist, 
     main = "Robust Mahalanobis Distance", 
     xlab = "Observation", 
     ylab = "Robust Mahalanobis Distance", 
     pch = 19, 
     col = ifelse(mahalanobis_robust_dist > threshold_robust, "#FF7F00", "black"))

abline(h = threshold_robust, col = "#FF7F00", lwd = 2, lty = 2)

legend("topleft", legend = c("Outlier", "Threshold (95th percentile)"), 
       col = c("#FF7F00", "#FF7F00"), pch = c(19, NA), lty = c(NA, 2))


# -> Improvement in outliers detection



################################################################################
#MIXED-EFFECT MODEL

  #Linear Regression - Fixed Model
lm_fixed <- lm(yieldpercol ~ . -state , data = df_model)
summary(lm_fixed)

  #Mixed Model with all variables
lm_mixed_all <- lmer(yieldpercol ~ . -state +(1 | state), data = df_model)
summary(lm_mixed_all)

  #Mixed Model with best subset selection variables
lm_mixed <- lmer(yieldpercol ~ Days_PM2.5+nAllNeonic_km2+Days_NO2+
                   palmer_index + Days_CO +(1 | state), data = df_model)
summary(lm_mixed)

lm_mixed2 <- lmer(yieldpercol ~ Days_PM2.5+nAllNeonic_km2 +(1 | state), data = df_model)


            ##############################
                  #MODELS EVALUATION

# 1) R Squared
r2(lm_fixed)
r2(lm_mixed_all)
r2(lm_mixed)  
r2(lm_mixed2)

# 2) Q-Q plot of residuals
qqmath(lm_mixed, 
       main = "QQ Plot of Residuals",
       col = "black",
       par.settings = list(
         par.main.text = list(fontface = "plain") 
       ))

# 3) Residuals vs Fitted Plot
plot(resid(lm_mixed) ~ fitted(lm_mixed), 
     main = "Residuals vs Fitted Values",  
     font.main = 1,
     xlab = "Fitted Values",
     ylab = "Residuals")                        
abline(h = 0, col = "#FFA500", lwd = 2)  

# 4) Actual vs Fitted Plot
plot(predict(lm_mixed), df_model$yieldpercol,
     main = "Actual vs Fitted Values",  
     font.main = 1,
     xlab = "Fitted Values",
     ylab = "Actual Values")
abline(0, 1, col = "#FFA500", lwd = 2)

# 5) Comparing RMSE
  #Fixed model
pred_fixed <- predict(lm_fixed)
obs_fixed <- df_model$yieldpercol
rmse_fixed <- sqrt(mean((obs_fixed - pred_fixed)^2))
rmse_fixed

  #Mixed model BSS
pred <- predict(lm_mixed)
obs <- df_model$yieldpercol

rmse_mixed <- sqrt(mean((obs - pred)^2))
rmse_mixed

  #Mixed model all variables
pred_all <- predict(lm_mixed_all)
obs_all <- df_model$yieldpercol

rmse_mixed_all <- sqrt(mean((obs_all - pred_all)^2))
rmse_mixed_all

# 6) Dotplot
dotplot(ranef(lm_mixed, condVar = TRUE))

# 7)
plot_model(lm_mixed, type = "est", show.values = TRUE, show.p = FALSE)+
  ggtitle("Coefficients Confidence Intervals")+
  theme(plot.title = element_text(hjust = 0.5))
plot_model(lm_mixed_all, type = "est", show.values = TRUE, show.p = FALSE)+
  ggtitle("Coefficients Confidence Intervals")+
  theme(plot.title = element_text(hjust = 0.5))

# 8)
model_stats <- data.frame(
  model = c( "lm_fixed", "lm_mixed_all", "lm_mixed"),
  AIC = c(AIC(lm_fixed), AIC(lm_mixed_all), AIC(lm_mixed))
)

ggplot(model_stats, aes(x = model, y = AIC, fill = model)) +
  geom_col() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "AIC Comparison of Models", y = "AIC", x = "Model") +
  scale_fill_manual(values = c("#FF7F00", "#FFA500", "#FFD700"))

            ##############################



################################################################################
#BOOTSTRAP

  #Fit your mixed model
lm_mixed <- lmer(yieldpercol ~ Days_PM2.5 + nAllNeonic_km2 + Days_NO2  + palmer_index + Days_CO + (1 | state), 
                 data = df_model)

  #Define a function to extract parameters of interest (e.g., fixed effects)
extract_fun <- function(fit) {
  fixef(fit)
}

  #Perform a parametric bootstrap with 1000 simulations
set.seed(123)
boot_res <- bootMer(
  lm_mixed, 
  FUN = extract_fun, 
  nsim = 1000, 
  re.form = NA,        #Simulate new random effects
  type = "parametric"  #Parametric bootstrap
)

  #boot_res$t now contains the bootstrapped estimates
head(boot_res$t)

  #Get confidence intervals using quantiles
original_est <- fixef(lm_mixed)
conf_intervals <- apply(boot_res$t, 2, quantile, probs = c(0.025, 0.975))

  #Combine results into a dataframe
results_df <- data.frame(
  Parameter = names(original_est),
  Estimate = original_est,
  CI_lower = conf_intervals[1, ],
  CI_upper = conf_intervals[2, ]
)

results_df

ggplot(results_df, aes(x = Estimate, y = reorder(Parameter, Estimate))) +
  geom_point(size = 3, color = "#FF7F00") +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2, color = "#FFA500") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Bootstrap Confidence Intervals for Model Parameters",
    x = "Bootstrap Estimate (with 95% CI)",
    y = "Parameter"
  ) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))



################################################################################
