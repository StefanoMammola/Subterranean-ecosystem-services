rm(list = ls())

setwd("C:/Users/Adri/OneDrive_personal/OneDrive/Post_Doc_Verbania/Maps_groundwater")

# Package loading ---------------------------------------------------------

if(!require("pacman")) {install.packages("pacman")}
pacman::p_load("raster", "ggplot2", "sf", "dplyr", "rnaturalearth", "rnaturalearthdata",
               "rnaturalearthhires", "cowplot", "rio", "tidyr")

# ==========================================
# ==========================================
#     Irrigation/Population Plot      
# ==========================================
# ==========================================


# Load and merge Groundwater rasters --------------------------------------

ugLL <- raster("water_table_2015_1.tif")
ugUL <- raster("water_table_2015_2.tif")
ugUR <- raster("water_table_2015_3.tif")
ugLR <- raster("water_table_2015_4.tif")


ugL <- raster::merge(ugLL, ugUL, tolerance=0.05,  overlap=F, ext=NULL)
ugR <- raster::merge(ugLR, ugUR, tolerance=0.05,  overlap=F, ext=NULL)
ug <- raster::merge(ugL, ugR, tolerance=0.05,  overlap=F, ext=NULL)


# Load Irrigation rasters and unify crs -----------------------------------

raster2 <- raster("irrigation.tif")
raster1 <- resample(ug, raster2)

EqualEarthProj <- "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
raster1 <- projectRaster(raster1, crs = EqualEarthProj)
raster2 <- projectRaster(raster2, crs = EqualEarthProj)


# Load country borders ----------------------------------------------------

countries <- ne_countries(scale = "large", returnclass = "sf") 
`%ni%` <- Negate(`%in%`)
countries <- countries[countries$sovereignt%ni%c("Antarctica"),]

countries <- countries %>%
  st_transform(EqualEarthProj)


# Crop rasters ------------------------------------------------------------

raster1 <- crop(raster1, extent(countries))
raster2 <- crop(raster2, extent(countries))

raster1 <- mask(x = raster1, mask = countries)
raster2 <- mask(x = raster2, mask = countries)


# Classify each raster into 3 categories (Low, Medium, High) --------------

breaks1 <- seq((min(raster1[], na.rm = TRUE)), (max(raster1[], na.rm = TRUE)), length.out = 4)
breaks2 <- seq((min(raster2[], na.rm = TRUE)), (max(raster2[], na.rm = TRUE)), length.out = 4)

breaks1 <- quantile(raster1[], probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
breaks2 <- quantile(raster2[], probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

raster1_class <- cut(raster1[], breaks = breaks1, labels = c(1, 2, 3))  
raster2_class <- cut(raster2[], breaks = breaks2, labels = c(1, 2, 3))  


# Combine rasters and define pallete --------------------------------------

combined_raster <- as.numeric(raster1_class) * 10 + as.numeric(raster2_class) 
combined_raster <- raster(matrix(combined_raster, nrow = nrow(raster1), ncol = ncol(raster1), byrow = TRUE))

extent(combined_raster) <- extent(raster1)
crs(combined_raster) <- crs(raster1)
bivariate_colors <- c(
  "Low_Low" = "#e8e8e8", "Low_Medium" = "#ace4e4", "Low_High" = "#5ac8c8",  # Low values of Raster1
  "Medium_Low" = "#dfb0d6", "Medium_Medium" = "#a5add3", "Medium_High" = "#5698b9",  # Medium values of Raster1
  "High_Low" = "#be64ac", "High_Medium" = "#8c62aa", "High_High" = "#3b4994"   # High values of Raster1
)


# Convert the raster to a data frame for plotting -------------------------

raster_df <- as.data.frame(combined_raster, xy = TRUE)
raster_df$layer <- as.character(raster_df$layer)

library(plyr)
raster_df$layer <- revalue(raster_df$layer, c("11" = "Low_Low", "12" = "Low_Medium", "13" ="Low_High",
                                              "21" = "Medium_Low", "22" = "Medium_Medium", "23" = "Medium_High",
                                              "31" = "High_Low", "32" = "High_Medium", "33"= "High_High"))
detach("package:plyr", unload=TRUE)

raster_df$color <- bivariate_colors[as.character(raster_df$layer)]  # Map colors using the palette


# Create plot -------------------------------------------------------------

Irrigation_Groundwater_plot <- ggplot() +
  geom_raster(data = raster_df, aes(x = x, y = y, fill = color)) +
  geom_sf(data = countries, fill = NA, color = "black", size = 0.1) +
  scale_fill_identity() +
  theme_void()

legend_df <- expand.grid(
  Irrigation = c("Low", "Medium", "High"),  # Categories for Raster 1
  Groundwater = c("Low", "Medium", "High")  # Categories for Raster 2
)
legend_df$color <- as.vector(bivariate_colors)  # Add the colors to the data frame

map_legend <- ggplot() +
  geom_tile(data = legend_df, aes(x = Irrigation, y = Groundwater, fill = color)) +
  scale_fill_identity() +
  labs(x = "Irrigation →", y = "Groundwater →") +
  theme_minimal() +
  theme(axis.title = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1))

Irrigation_Groundwater_plot <- ggdraw() +
  draw_plot(Irrigation_Groundwater_plot) +  
  draw_plot(map_legend,x = 0, y = 0.07, width = 0.23, height = 0.39)  # Inset in bottom-left


# Clean environment and save the plot -------------------------------------

rm(list=setdiff(ls(), c("Irrigation_Groundwater_plot")))


# ==========================================
# ==========================================
#     Mines/Extractions Plot      
# ==========================================
# ==========================================

setwd("C:/Users/Adri/OneDrive_personal/OneDrive/Post_Doc_Verbania/Maps_groundwater/74548 mine polygons")


# Load mines shape files and country borders ------------------------------

mines <- st_read("74548_projected.shp")
countries <- ne_countries(scale = "large", returnclass = "sf") 
`%ni%` <- Negate(`%in%`)
countries <- countries[countries$sovereignt%ni%c("Antarctica"),]

# Ensure CRS is the same
mines <- st_transform(mines, crs = st_crs(countries))

countries <- st_transform(countries, 3395)
mines <- st_transform(mines, 3395)

invalid_mines <- mines[!st_is_valid(mines), ]
invalid_countries <- countries[!st_is_valid(countries), ]

# Repair invalid geometries
mines <- st_make_valid(mines)
countries <- st_make_valid(countries)


# Calculate mines and countries area --------------------------------------

mines_by_country <- st_intersection(mines, countries)

mines_by_country <- mines_by_country %>%
  mutate(mine_area = st_area(geometry))

countries <- countries %>%
  mutate(country_area = st_area(geometry))

mine_area_by_country <- mines_by_country %>%
  group_by(sovereignt) %>% 
  summarize(total_mine_area = sum(mine_area))


# Calculate of % of area occupied by mines by country ---------------------

countries_area <- as.data.frame(cbind(countries$sovereignt, countries$country_area))
countries_area$V2 <- as.numeric(countries_area$V2)
colnames(countries_area) <- c("sovereignt", "country_area")

countries_area <- countries_area %>% 
  group_by(sovereignt) %>%
  summarize(total_area = sum(country_area))


merged_df <- merge(mine_area_by_country, countries_area, by = "sovereignt", all.x = TRUE)

merged_df <- merged_df %>%
  mutate(percent_mine_area = (total_mine_area / total_area) * 100)

merged_df2 <- as.data.frame(cbind(merged_df$sovereignt, merged_df$percent_mine_area))
colnames(merged_df2) <- c("sovereignt", "percent_mine_area")

result <- merge(countries, merged_df2, by = "sovereignt", all.x = TRUE)
result$percent_mine_area <- as.numeric(result$percent_mine_area)


# Convert data to equal earth projection ----------------------------------

equal_area_projection <- c("+proj=eqearth")
result_mines <- result %>%
  st_transform(equal_area_projection)



# Load minerals extraction ------------------------------------------------

setwd("C:/Users/Adri/OneDrive_personal/OneDrive/Post_Doc_Verbania/Maps_groundwater/Minerals")

temp <- list.files(pattern="\\.csv$")
mineral <- lapply(temp, read.delim, sep =",")

for (i in 1:length(mineral)) {
  
  al2 <- mineral[[i]] %>%
    select(which(sapply(., is.numeric)))
  al2 <- al2 %>% select_if(is.numeric) %>% replace(is.na(.), 0)
  al2$total <- rowSums(al2[,c(1:length(colnames(al2)))])
  al2$country <- mineral[[i]][,1]
  
  mineral[[i]] <- al2
}

result <- bind_rows(lapply(mineral, function(df) {
  df %>% select(country, total)  
})) %>%
  group_by(country) %>%
  summarize(Total_Value = sum(total, na.rm = TRUE))

`%ni%` <- Negate(`%in%`)

result0 <- result[result$Total_Value%ni%0,]


# Rename unmatched countries ----------------------------------------------

library(plyr)

countries <- ne_countries(scale = "large", returnclass = "sf") 

result0$names_notfound <-   result0$country %in%countries$sovereignt
result0$country <- revalue(result0$country, c("Bosnia & Herzegovina" = "Bosnia and Herzegovina",
                                              "Congo" = "Republic of the Congo",
                                              "Congo, Democratic Republic" = "Democratic Republic of the Congo",
                                              "Czech Republic" = "Czechia",
                                              "Eswatini (Swaziland)" = "eSwatini",
                                              "France (French Guiana)" = "France",
                                              "Ireland, Republic of" = "Ireland",
                                              "Korea (Rep. of)" = "South Korea",
                                              "Korea, Dem. P.R. of" = "North Korea",
                                              "New Caledonia" = "France",
                                              "Serbia" = "Republic of Serbia",
                                              "Tanzania" = "United Republic of Tanzania",
                                              "Trinidad & Tobago" = "Trinidad and Tobago",
                                              "USA" = "United States of America",
                                              "Yemen, Republic of" = "Yemen"
))
detach("package:plyr", unload=TRUE)


# Classify each raster into 3 categories (Low, Medium, High) --------------

result_mines$mineral <- result0$Total_Value [match(result_mines$sovereignt, result0$country)]
result_mines <- result_mines %>%
  mutate(mining_intensity = cut(percent_mine_area, breaks = quantile(percent_mine_area, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                                labels = c("Low", "Medium", "High"), include.lowest = TRUE),
         mineral_intensity = cut(mineral, breaks = quantile(mineral, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                                labels = c("Low", "Medium", "High"), include.lowest = TRUE),
         bivariate_class = paste(as.character(mining_intensity), as.character(mineral_intensity), sep="_")
)


# Define pallette ---------------------------------------------------------

bivariate_colors <- c(
  "Low_Low" = "#fee5d9", "Low_Medium" = "#fcae91", "Low_High" = "#fb6a4a",  
  "Medium_Low" = "#fcbba1", "Medium_Medium" = "#fc9272", "Medium_High" = "#ef3b2c",  
  "High_Low" = "#de2d26", "High_Medium" = "#a50f15", "High_High" = "#67000d"   
)


# Create plot -------------------------------------------------------------

result_mines <- result_mines %>%
  mutate(color = bivariate_colors[bivariate_class])

map <- ggplot(data = result_mines) +
  geom_sf(aes(fill = color), color = "black", size = 0.1) +
  scale_fill_identity() +  # Use the predefined colors
  theme_minimal()+
  
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

legend_data <- expand.grid(mining_intensity = c("Low", "Medium", "High"), mineral_intensity = c("Low", "Medium", "High")) %>%
  mutate(
    bivariate_class = paste(mining_intensity, mineral_intensity, sep = "_"),
    color = bivariate_colors[bivariate_class]
  )

map_legend <- ggplot() +
  geom_tile(data = legend_data, aes(x = mining_intensity, y = mineral_intensity, fill = color)) +
  scale_fill_identity() +
  labs(x = "Mining Intensity →", y = "Mineral Extraction →") +
  theme_minimal() +
  theme(axis.title = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1))

Mines_minerals_plot <- ggdraw() +
  draw_plot(map) +  
  draw_plot(map_legend, x = 0, y = 0.07, width = 0.23, height = 0.39)  # Inset in bottom-left


# Clean environment and save the plots ------------------------------------

rm(list=setdiff(ls(), c("Irrigation_Groundwater_plot", "Mines_minerals_plot")))


# ==========================================
# ==========================================
#     Tourism/Income Plot      
# ==========================================
# ==========================================

setwd("C:/Users/Adri/OneDrive_personal/OneDrive/Post_Doc_Verbania/Maps_groundwater")


# Load world country borders ----------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")

# We are going to remove some countries that are problematic for the merge of
# the different shape files. Then we are going to incorporate Russia again
`%ni%` <- Negate(`%in%`)
world2 <- world[world$sovereignt%ni%c("Russia", "Fiji", "Antarctica"),]

world_combined <- world2 %>%
  group_by(sovereignt) %>%  
  summarize(
    geometry = st_union(geometry) 
  )

world_combined[max(length(rownames(world_combined)))+1,] <- world[world$sovereignt%in%"Russia",c("sovereignt", "geometry")]

world_combined$visitors <- NA ; world_combined$income <- NA


# Load cave data ----------------------------------------------------------

data <- read.csv("data_caves2.csv", header = T, sep = ",")

country_list <- unique(data$Country)
for (i in 1:length(unique(data$Country))) {
  data_sub <- data[data$Country%in%country_list[i],]
  rownames(world_combined) <- world_combined$sovereignt
  world_combined[country_list[i],3] <- sum(data_sub$Visitors.year..2019.)
  rownames(world_combined) <- world_combined$sovereignt
  world_combined[country_list[i],4] <- sum(data_sub$Estimated.total.income.year.Euros)
}

equal_area_projection <- c("+proj=eqearth")
world_combined <- world_combined %>%
  st_transform(equal_area_projection)


# Classify each raster into 3 categories (Low, Medium, High) --------------

world_combined <- world_combined %>%
  mutate(
    visitors_cat = cut(visitors, breaks = quantile(visitors, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                       labels = c("Low", "Medium", "High"), include.lowest = TRUE),
    income_cat = cut(income, breaks = quantile(income, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                     labels = c("Low", "Medium", "High"), include.lowest = TRUE),
    bivariate_class = paste(as.character(visitors_cat), as.character(income_cat), sep = "_")
  )


# Define pallette ---------------------------------------------------------

bivariate_colors <- c(
  "Low_Low" = "#deebf7", "Low_Medium" = "#c6dbef", "Low_High" = "#4292c6",
  "Medium_Low" = "#9ecae1", "Medium_Medium" = "#6baed6", "Medium_High" = "#0571b0",
  "High_Low" = "#2171b5", "High_Medium" = "#08519c", "High_High" = "#08306b"
)

world_combined <- world_combined %>%
  mutate(color = bivariate_colors[bivariate_class])

world_combined$color <- replace_na(world_combined$color, "white")


# Create plot -------------------------------------------------------------

map <- ggplot(data = world_combined) +
  geom_sf(aes(fill = color), color = "black", size = 0.1) +
  scale_fill_identity() +  # Use the predefined colors
  theme_minimal() +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

legend_data <- expand.grid(visitors_cat = c("Low", "Medium", "High"), income_cat = c("Low", "Medium", "High")) %>%
  mutate(
    bivariate_class = paste(visitors_cat, income_cat, sep = "_"),
    color = bivariate_colors[bivariate_class]
  )

map_legend <- ggplot() +
  geom_tile(data = legend_data, aes(x = visitors_cat, y = income_cat, fill = color)) +
  scale_fill_identity() +
  labs(x = "Visitors →", y = "Income →") +
  theme_minimal() +
  theme(axis.title = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1))

Turism_income_plot <- ggdraw() +
  draw_plot(map) +  
  draw_plot(map_legend, x = 0, y = 0.07, width = 0.23, height = 0.39)  # Inset in bottom-left

# Clean environment and save the plots ------------------------------------

rm(list=setdiff(ls(), c("Irrigation_Groundwater_plot", "Mines_minerals_plot", "Turism_income_plot")))


# ==========================================
# ==========================================
#     Groundwater/Population Plot      
# ==========================================
# ==========================================


# Load and merge Groundwater rasters --------------------------------------

ugLL <- raster::raster("water_table_1958_LL.tif")
ugUL <- raster::raster("water_table_1958_UL.tif")
ugUR <- raster::raster("water_table_1958_UR.tif")
ugLR <- raster::raster("water_table_1958_LR.tif")

ugL <- raster::merge(ugLL, ugUL, tolerance=0.05,  overlap=F, ext=NULL)
ugR <- raster::merge(ugLR, ugUR, tolerance=0.05,  overlap=F, ext=NULL)
ug <- raster::merge(ugL, ugR, tolerance=0.05,  overlap=F, ext=NULL)

# Load Population raster and unify crs ------------------------------------

population <- raster::raster("population.tif")

ugRS <- resample(ug, population)

EqualEarthProj <- "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
ugRS <- projectRaster(ugRS, crs = EqualEarthProj)
population <- projectRaster(population, crs = EqualEarthProj)

# Load country borders ----------------------------------------------------

countries <- ne_countries(scale = "large", returnclass = "sf") 
`%ni%` <- Negate(`%in%`)
countries <- countries[countries$sovereignt%ni%c("Antarctica"),]

countries <- countries %>%
  st_transform(EqualEarthProj)


# Crop rasters ------------------------------------------------------------

ugRS <- crop(ugRS, extent(countries))
population <- crop(population, extent(countries))

ugRS <- mask(x = ugRS, mask = countries)
population <- mask(x = population, mask = countries)


# Classify each raster into 3 categories (Low, Medium, High) --------------

breaks1 <- quantile(ugRS[], probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
breaks2 <- quantile(population[], probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

ug_class <- cut(ugRS[], breaks = breaks2, labels = c(1, 2, 3))  
population_class <- cut(population[], breaks = breaks2, labels = c(1, 2, 3))  


# Combine rasters and define palette --------------------------------------

combined_raster <- as.numeric(ug_class) * 10 + as.numeric(population_class)  # Encode combinations as 11, 12, ..., 33
combined_raster <- raster(matrix(combined_raster, nrow = nrow(ugRS), ncol = ncol(ugRS), byrow = TRUE))

extent(combined_raster) <- extent(ugRS)
crs(combined_raster) <- crs(ugRS)

bivariate_colors <- c(
  "Low_Low" = "#e8e8e8", "Low_Medium" = "#ace4e4", "Low_High" = "#5ac8c8",  
  "Medium_Low" = "#e4c2a0", "Medium_Medium" = "#b8d98f", "Medium_High" = "#7dbd3a",  
  "High_Low" = "#f0a5a0", "High_Medium" = "#f28a2b", "High_High" = "#c14d0d"   
)

# Convert the raster to a data frame for plotting -------------------------

raster_df <- as.data.frame(combined_raster, xy = TRUE)
raster_df$layer <- as.character(raster_df$layer)

library("plyr")
raster_df$layer <- revalue(raster_df$layer, c("11" = "Low_Low", "12" = "Low_Medium", "13" ="Low_High",
                                              "21" = "Medium_Low", "22" = "Medium_Medium", "23" = "Medium_High",
                                              "31" = "High_Low", "32" = "High_Medium", "33"= "High_High"))
detach("package:plyr", unload=TRUE)

raster_df$color <- bivariate_colors[as.character(raster_df$layer)]  # Map colors using the palette

# Create plot -------------------------------------------------------------

GroundWater_population_plot <- ggplot() +
  geom_raster(data = raster_df, aes(x = x, y = y, fill = color)) +
  geom_sf(data = countries, fill = NA, color = "black", size = 0.1) +
  scale_fill_identity() +
  theme_void()

legend_df <- expand.grid(
  population = c("Low", "Medium", "High"),  
  ug = c("Low", "Medium", "High")  
)

legend_df$color <- as.vector(bivariate_colors)  

map_legend <- ggplot() +
  geom_tile(data = legend_df, aes(x = population, y = ug, fill = color)) +
  scale_fill_identity() +
  labs(x = "Population →", y = "Groundwater →") +
  theme_minimal() +
  theme(axis.title = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1))

GroundWater_population_plot <- ggdraw() +
  draw_plot(GroundWater_population_plot) +  
  draw_plot(map_legend,x = 0, y = 0.07, width = 0.23, height = 0.39)  

# Clean environment and save the plots ------------------------------------

rm(list=setdiff(ls(), c("Irrigation_Groundwater_plot", "Mines_minerals_plot", "Turism_income_plot", "GroundWater_population_plot")))

# ==========================================
# ==========================================
#     Plot Figure 4   
# ==========================================
# ==========================================

plot_grid(GroundWater_population_plot, Irrigation_Groundwater_plot, Mines_minerals_plot, Turism_income_plot, labels = "AUTO")
