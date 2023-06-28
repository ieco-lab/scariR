#Impervious Surface of Individual Snares
#Anna Carlson 
#March 2023 

#load libraries 
library(raster)
library(sf)
library(tidyverse)
library(dplyr)
library(tigris)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyr)
library(rgdal)
library(sp)
library(ggrepel)
library(knitr)
library(rgeos)
library(mapview)
library(rpart)
library(terra)


#load in raster of impervious surface for PA
imp_surf <- raster("/Users/annacarlson/Desktop/MS Docs/PA_NCLD.tif")

#check to make sure imp_surf raster loaded correctly
plot(imp_surf)

#load in snare latitude and longitude
snare_loc <- read_csv("/Users/annacarlson/Desktop/MS Docs/snare_lat_long.csv")
snare_loc<- snare_loc %>%
  mutate(LONG = LONG*(-1))

#transform lat and long values into a spatial object
snare_location <- st_as_sf(snare_loc, coords = c("LONG", "LAT"),  crs = 4326)
snare_location <- st_transform(snare_location, st_crs(imp_surf))
snare_imp_surf <- raster::extract(imp_surf, # the raster that you wish to extract values from
                                snare_location, # a point, or polygon spatial object
                                buffer = 5000, # specify a 5000m radius (for FOX, RACCOON, & OPOSSUM)
                                fun = mean, # extract the MEAN value from each plot
                                sp = TRUE) # create spatial object

#turn the object into a dataframe
fivekm_snare_imp_surf_df <- as.data.frame(snare_imp_surf)

#pull PA counties from tigris package
pa_counties <- counties("Pennsylvania", cb = TRUE)

#list of counties my potential sites are located in 
site_counties <- species_dis %>% distinct(NAME)
site_counties <- data.frame(NAME = c('Philadelphia', 'Montgomery', 'Delaware', 'Bucks', 'Chester'))

#join site_counties with pa_counties to subset the shape file 
sub_site <- left_join(site_counties, pa_counties, by = "NAME")

#convert sub_site file to sf
counties <- st_as_sf(sub_site)

#transform projection of counties so it is the same as the raster 
proj_counties <- counties %>% 
  st_transform(crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

counts <- as(proj_counties, 'Spatial')

#crop raster to county data set
crop_imp_surf <- crop(imp_surf, counts)
plot(crop_imp_surf)

#mask cropped raster so that all data outside county boundaries is removed
mask_crop_imp_surf <- mask(crop_imp_surf, counts)
df_imp_surf<-as.data.frame(mask_crop_imp_surf, xy = TRUE)

#import site boundaries 
sites_shp <- st_read("/Users/annacarlson/Downloads/with_pennypack_trust")

#transform site boundaries to the correct projection
sites_shp <- sites_shp %>% 
  st_transform(crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

#update Names for sites to letter code 
sites_shp<- sites_shp %>% 
  mutate_at("Name", str_replace, "Wissahickon Valley Park", "WVP") %>% 
  mutate_at("Name", str_replace, "West Fairmount Park", "FPW") %>% 
  mutate_at("Name", str_replace, "Pennypack Park", "PPP") %>% 
  mutate_at("Name", str_replace, "East Fairmount Park", "FPE") %>% 
  mutate_at("Name", str_replace, "Tacony Creek Park", "TCP") %>% 
  mutate_at("Name", str_replace, "Morris Park", "MP") %>%
  mutate_at("Name", str_replace, "Wisters Woods Park", "WSW") %>%
  mutate_at("Name", str_replace, "Temple University Ambler", "TUA") %>%
  mutate_at("Name", str_replace, "Silver Lake Nature Center", "SLV") %>%
  mutate_at("Name", str_replace, "Churchville Nature Center", "CVN") %>%
  mutate_at("Name", str_replace, "ChesLen Preserve", "CLP") %>%
  mutate_at("Name", str_replace, "Airdrie Forest Preserve", "ARD") %>% 
  mutate_at("Name", str_replace, "Pennypack Ecological Trust", "PPT")

#code in BaseR - was constructed with guidance from Jason Gleditsch

#creating function to give me the standard error of x 
st.err <- function(x) {sd(x, na.rm = TRUE) / sqrt(length(x))}
#make a vector of the sites (this holds all the site names)
sites <- levels(as.factor(sites_shp$Name))
#creating an empty object because we need somewhere to store the results of the iteration
out <- NULL
#this is where we do the iterations
imp_check <- vector("list", length(sites))
names(imp_check) <- sites
for (i in 1:length(sites)) {
  tmp <- sites_shp[which(sites_shp$Name == sites[i]), ] #which statement pulls information for the specified row (information comes from the columns which are listed after the , (because no columns are listed, all columns are pulled))
#creating the buffer around the park boundaries
  buff <- st_buffer(tmp, dist = 3000)
#mask the raster data outside the buffer 
  clip <- crop(mask_crop_imp_surf, buff)
  clip <- mask(clip, buff)
  clip <- mask(clip, st_zm(tmp), inverse = TRUE) #mask the shape of the park on the raster
  
  clip_df <- as.data.frame(clip, xy = TRUE)
  
  out.tmp <- data.frame(site = sites[i],
                        imp_surv = mean(clip_df$PA_NCLD, na.rm = TRUE),
                        imp_surv_se = st.err(clip_df$PA_NCLD))
  
  out <- rbind(out, out.tmp)
  
  imp_check[[sites[i]]] <- list(clip = clip_df, buff = buff, park = tmp)
  
  rm(out.tmp, tmp, buff, clip, clip_df)
}


#create a binary reading of impervious surface (if 0 then remains 0, if >0 then becomes 1)
#df_imp_surf <- na.omit(df_imp_surf)
#df_binary <- mutate(df_imp_surf, binary = ifelse(x>0, '1', '0'))



#create reclassification matrix
reclass_df <- c(0, 0, 0,
                1, Inf, 1)

#reshape the object into a matrix with columns and rows
reclass_m <- matrix(reclass_df,
                    ncol = 3,
                    byrow = TRUE)

# reclassify the raster using the reclass object - reclass_m
imp_surf_classified <- reclassify(mask_crop_imp_surf,
                             reclass_m)

#code in BaseR - was constructed with guidance from Jason Gleditsch

#creating function to give me the standard error of x 
st.err <- function(x) {sd(x, na.rm = TRUE) / sqrt(length(x))}
#make a vector of the sites (this holds all the site names)
sites <- levels(as.factor(sites_shp$Name))
#creating an empty object because we need somewhere to store the results of the iteration
binary_imp_surf <- NULL
#this is where we do the iterations
imp_check <- vector("list", length(sites))
names(imp_check) <- sites
for (i in 1:length(sites)) {
  tmp <- sites_shp[which(sites_shp$Name == sites[i]), ] #which statement pulls information for the specified row (information comes from the columns which are listed after the , (because no columns are listed, all columns are pulled))
  #creating the buffer around the park boundaries
  buff <- st_buffer(tmp, dist = 3000)
  #mask the raster data outside the buffer 
  clip <- crop(imp_surf_classified, buff)
  clip <- mask(clip, buff)
  clip <- mask(clip, st_zm(tmp), inverse = TRUE) #mask the shape of the park on the raster
  
  clip_df <- as.data.frame(clip, xy = TRUE)
  
  out.tmp <- data.frame(site = sites[i],
                        imp_surv = mean(clip_df$PA_NCLD, na.rm = TRUE),
                        imp_surv_se = st.err(clip_df$PA_NCLD))
  
  binary_imp_surf <- rbind(out, out.tmp)
  
  imp_check[[sites[i]]] <- list(clip = clip_df, buff = buff, park = tmp)
  
  rm(out.tmp, tmp, buff, clip, clip_df)
}

binary_imp_surf <- binary_imp_surf[-(1:38),]






#make raster into a data frame for plotting
df_imp_surf<-as.data.frame(imp_surf, xy = TRUE)


#set the boundaries of my projection using the minimum and maximum from the imp_surf dataframe
xbounds <- c(min(snare_imp_surf_df$coords.x1) - 1000, max(snare_imp_surf_df$coords.x1) + 1000)
ybounds <- c(min(snare_imp_surf_df$coords.x2) - 1000, max(snare_imp_surf_df$coords.x2) + 1000)

ggplot() +
  #plot cropped and masked raster turned df
  geom_raster(data = df_binary, aes(x = x, y = y, fill = PA_NCLD)) +
  #overlay snare location points
  geom_sf(data = snare_location, fill = NA, color = "white") +
  #setting the boundaries of the projection
  coord_sf(xlim = xbounds, ylim = ybounds) +
  # Center the title
  theme(plot.title = element_text(hjust = 0.5)) +
  # Give it a title
  labs(title = "Projected Counties with NCLD Impervious Surface Data")

#for just one site at a time (to check them)
ggplot() +
  #plot cropped and masked raster turned df
  geom_raster(data = imp_check[["PPT"]][["clip"]], aes(x = x, y = y, fill = PA_NCLD)) +
  #overlay snare location points
  geom_sf(data = imp_check[["PPT"]][["park"]], fill = NA, color = "green") +
  geom_sf(data = imp_check[["PPT"]][["buff"]], fill = NA, color = "white") +
  # Center the title
  theme(plot.title = element_text(hjust = 0.5)) +
  # Give it a title
  labs(title = "Projected Counties with NCLD Impervious Surface Data")



