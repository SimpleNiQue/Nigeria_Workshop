###  Working with spatial data in R

## We can do GIS in R 
#  Most spatial processing can be done with these packages 
#We need to install the needed packages for spatial data

#install.packages("sf")
#install.packages("tmap")
#install.packages("leaflet")
#install.packages("mapview")
#install.packages("units")
#install.packages("exactextractr")
#install.packages("RColorBrewer")


#After installing packages we load the packages using the library function

library(sf)
library(raster)
library(tmap)
library(leaflet)
library(mapview)
library(units)
library(exactextractr)
library(RColorBrewer)



# But there is a bunch of more spatial packages for specific needs. This tutorial will focus on the above listed packages for handling 
# spatial data in bottom-up population modelling framework.

# Also tidyverse functions are useful to manipulate the attribute tables of the spatial data (merge, subset, extract)
# So, let's add the 'tidyverse' to the list
library(tidyverse)
tmap_options(check.and.fix = TRUE) # also this function to fix potential issues during the processing


# Set the working directory for your data

#Specify Drive Path
drive_path <- "C:/Users/oy1r22/OneDrive - University of Southampton/Desktop/Day 3/"
input_path <- paste0(drive_path, "Spatial Data/")


# Spatial data can be in a vector ... (objects of type points, lines or polygons)

health_facilities <- st_read(paste0(input_path, "GRID3_Nigeria_-_Health_Care_Facilities.shp"))
road_network <- st_read(paste0(input_path, "nga_rds_1m_dcw.shp"))
states <- st_read(paste0(input_path, "Admin2_states.shp"))
country <- st_read(paste0(input_path, "nga_polbnda_adm0_1m.shp"))
pop_raster <- raster(paste0(input_path, "NGA_population_v1_2_gridded.tif"))


# Static maps using tmap -------------------------------------------------------------
#Plot country shapefile

tm_shape(country) + 
  tm_fill("Terr_Name")

#We will plot health facilities over the country

tm_shape(country) + 
  tm_fill("Terr_Name")+
  tm_shape(health_facilities)+
  tm_dots(size = 0.2, col = "type", title = "Health facilities")

#We will plot the road network over the country 
  
tm_shape(country) + 
  tm_fill("Terr_Name")+
  tm_shape(road_network)+
  tm_lines(size = 0.5)

#We will plot the states

tm_shape(states) + 
  tm_fill("statename")+
  tm_layout(legend.show = TRUE)


#We can also plot a raster data
#It takes a while to plot raster dataset because of the massive size
#tm_shape(country) + 
  #tm_borders()+
  #tm_shape(pop_raster) +
  #tm_raster()


#Alternatively we can simply use the plot() function to display spatial objects
## plot() is very useful when it comes to display spatial data
## but, tmap package offers many more possibilities in terms of mapping, and allows both static and interactive views
## plot() reads both for vector and raster data, but you will also need to load the following packages


## With vector data, the default is to plot the attributes
plot(states)


## However you can plot a single attribute
plot(states['region'], main = "region",
     pal = rev(heat.colors(10)))


## For a quick look at a vector, you can also extract the geometry with st_geometry() and then call the function plot()
## Like this...
st_geometry(states) %>% plot()

## ... or this
plot(st_geometry(states))



## plot() can combine layers (using the argument add=TRUE)
plot(st_geometry(country))
plot(st_geometry(states), add = TRUE,
     pch = 16, col = "green", cex = 0.5)


## Displaying a raster with plot() is very simple
plot(pop_raster)


## Displaying raster and vector at once: again very easy with plot()
plot(pop_raster)
plot(st_geometry(states), add = T,
     pch = 16, cex = 0.5)


## tmap_arrange can combine multiple maps in one image 

# Create two maps
m1 <- tm_shape(country) + tm_polygons()
m2 <- tm_shape(states) + tm_polygons()
m3 <- tm_shape(health_facilities) + tm_dots()

# Arrange them in one image 
tmap_arrange(m1, m2, m3, nrow = 1)

# Interactive Map ---------------------------------------------------------

#The maps we have produced so far are static map. tmap allows us to also do an interactive map where we can zoom-in and out
#to see the spatial distribution of objects 
#we use the tmap_mode() function to toggle between interactive and static maps.
#The default option: tmap_mode("plot") is a static mapping mode and tmap_mode("view") is used for interactive map

#Here we are plotting the states
tmap_mode("view")


#We will plot the states
tm_shape(states)+
  tm_borders(col='orange', lwd=5)+
  tm_shape(states)+
  tm_borders()+
  tm_basemap('OpenStreetMap')


#We will plot healthcare facilities in each state interactively
tm_shape(health_facilities)+
  tm_dots(col='type', size=0.1)+
  tm_basemap('OpenStreetMap')+
  tm_shape(states)+
  tm_borders(lwd=1)

#We will plot road interactively
tm_shape(road_network)+
  tm_lines(size = 0.5)+
  tm_basemap('OpenStreetMap')+
  tm_shape(states)+
  tm_borders(lwd=1)

# Some basic geoprocessing ------------------------------------------------

# We can also join a vector layer with tabular data (using a common identifier)
#We have a csv file of predicted population for all the states. We are going to join the csv file to the 
#states shapefile and map it
#We need to load the csv file

pop_estimate <- read.csv(paste0(input_path, "Population.csv"))

#View population estimates
#view(pop_estimate)

#We have to join the population estimate file to the states vector data based on a common field

#let view the states shapefile to see its variables(attributes)
#view(states)

#You would realize we have a number of unique field common to both dataset that we can join the two datasets
#We are going to join the two datasets using the "statename" variable

pop_states <- states %>% 
  inner_join(pop_estimate, by = "statename")

#We are going to visualize population distribution per states 
tm_shape(pop_states)+
  tm_polygons(col='total', lwd=5)+
  tm_shape(states)+
  tm_borders()+
  tm_basemap('OpenStreetMap')

              
# We can subset a vector layer using a simple query
pop_kano <- pop_states %>%
  filter(statename=='Kano') # extracting Kano states
qtm(pop_kano)


#Use Kano State and clip population raster
# Clip the raster using the polygon extent
r_clipped <- crop(pop_raster, pop_kano)

#map the population of Kano State

tm_shape(r_clipped)+
  tm_raster()+
  tm_shape(pop_kano)+
  tm_basemap('OpenStreetMap')+
  tm_borders(lwd=4)

#Subset all states with a population of more than 5 million people

state_5million <- pop_states %>% 
  filter(total > 5000000)

state_5million$statename

#Let plot those states

tm_shape(state_5million)+
  tm_polygons(col = "total", title = "Population")+
  tm_basemap('OpenStreetMap')+
  tm_borders(lwd=4)
  

# Buffer analysis is an incredibly important step in spatial statistics, as it enables to aggregate the output data, 
#based on another layer and within a specific area  (e.g. the population within a 1 km radius around a health post, or 
#sum of pixel values at subdistrict administrative level)

#We are going to select only health facilities in Abuja Local Government Area(Municipal Area Council)

lga <- st_read(paste0(input_path,"GRID3_Nigeria_-_Local_Government_Area_Boundaries.shp"))

Abuja_lga <- lga %>% 
  filter(lga_name_x == "Municipal Area Council")

#Finding the number of health facilities in Abuja Municipality

health_facilities_Abuja <- health_facilities %>% 
  filter(lga_name=='Municipal Area Council')

# EXERCISE: How many health facilities in Abuja?
tally(health_facilities_Abuja)

#Counting and visualizing the types of health facilities in Abuja

health_facilities_Abuja %>% 
  group_by(type) %>% 
  summarise(count= n()) %>% 
  ggplot(aes(x = type, y = count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5, size = 4) +
  xlab("Type of Health Facility") +
  ylab("Number") +
  ggtitle("Number of Health Facilities in Abuja")+
  scale_fill_brewer(type = "qual", palette = "Greens")


#Plot health facilities
tm_shape(health_facilities_Abuja)+
  tm_dots(col='type', size=0.07, id='primary_na', popup.vars=c('category','functional','source'))+
  tm_basemap('OpenStreetMap')+
  tm_shape(Abuja_lga)+
  tm_borders(lwd=4)


# EXERCISE: How many tertiary health care facilities are in Abuja?

table(health_facilities_Abuja$type)

# Discovering the gridded population dataset ------------------------------

pop_Abuja <- crop(pop_raster, Abuja_lga)
plot(pop_Abuja)

tm_shape(health_facilities_Abuja)+
  tm_dots(col='type', size=0.07, id='primary_na', popup.vars=c('category','functional','source'))+
  tm_basemap('OpenStreetMap')+
  tm_shape(pop_Abuja)+
  tm_raster()+
  tm_shape(Abuja_lga)+
  tm_borders(lwd=4)


# Buffering points --------------------------------------------------------

health_facilities_Abuja_buffered <- st_buffer(health_facilities_Abuja, dist=set_units(1, km))

tm_shape(pop_Abuja)+
  tm_raster()+
  tm_shape(health_facilities_Abuja_buffered)+
  tm_borders()+
  tm_shape(health_facilities_Abuja)+
  tm_dots( size=0.08, id='primary_na')+
  tm_shape(Abuja_lga)+
  tm_borders(lwd=4)+
  tm_basemap('OpenStreetMap')

# convert the merged buffers to a SpatialPolygonsDataFrame object
health_facilities_Abuja_buffered <- as(health_facilities_Abuja_buffered, "Spatial")


# Computing the population ------------------------------------------------


health_facilities_Abuja_pop <- raster::extract(pop_Abuja, health_facilities_Abuja_buffered, fun=sum, na.rm=T,df=T)

#Rename extracted population
health_facilities_Abuja_pop <- health_facilities_Abuja_pop %>%  
  rename(pop = NGA_population_v1_2_gridded)


#EXERCISE: How many people are living in 1km radius of a Health Center in Abuja?
sum(health_facilities_Abuja_pop$pop)


# Exercises----------------------------
# Create a map of Nigeria that combines the raster layer called "pop_raster" and the vector layer "states"
#a. calculate population density using the pop_state object
#b. make an interactive map of population density per states
#c. select all state with a population less than 1million. Use the selected state to crop pop_raster
#d. map the cropped raster interactively


### End of spatial data analysis
#####If you want to learn more on spatial data analysis visit this website
#https://bookdown.org/nicohahn/making_maps_with_r5/docs/introduction.html 
#https://learn.grid3.org/







