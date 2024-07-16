
### Made by Simon Oiry, the 16th of July 2024
### The script is designed to map seagrasses over time on two mudflats, Kerouarc'h and Fort Espagnol, in the Morbihan Gulf.

# Load necessary libraries
library(terra)      # For spatial data handling
library(tidyverse)  # For data manipulation and visualization

# List all .tif files in the "Data/ICECREAMS/" directory and create a data frame
img_list <- list.files("Data/ICECREAMS/", pattern = ".tif", recursive = TRUE, full.names = TRUE) %>% 
  as_tibble() %>% 
  rename(path = "value") %>% 
  mutate(name = gsub(".*/","",path),                 # Extract the filename
         year = substr(name,12,15),                  # Extract the year from the filename
         date = as.POSIXct(substr(name,12,19), format = "%Y%m%d"))  # Extract and format the date

# Load the intertidal mask shapefile
mask <- "Data/mask/Intertidal_mask_Auray.shp" %>% 
  vect()



# Loop through each image and each site to process the data
for(img_i in 1:nrow(img_list)){
  for(site_i in 1:length(unique(mask$Site_Name))){
    
    # Extract the mask for the current site
    mask_site <- mask[which(mask$Site_Name == unique(mask$Site_Name)[site_i])]
    
    # Process the current image
    df <- img_list %>% 
      slice(img_i) %>% 
      pull(path) %>% 
      rast() %>% 
      crop(mask_site) %>%                    # Crop the image to the site mask
      as.data.frame(xy = TRUE) %>%           # Convert the raster to a data frame
      mutate(site = unique(mask$Site_Name)[site_i],  # Add site information
             date = img_list %>% 
               slice(img_i) %>% 
               pull(date),                   # Add date information
             year = img_list %>% 
               slice(img_i) %>% 
               pull(year)) %>%               # Add year information
      dplyr::filter(out_class == 4 & 
                      SPC20Unknown != 0 )   %>%        # Filter the data for a specific class
      as_tibble()
    # Combine the data frames
    if(img_i == 1 & site_i == 1){
      output <- df
    }else{
      output <- rbind(output,df)
    }
    
  }
  rm(df)
  rm(mask_site)
}

cols <- c("Kerouarc'h" = "#399E5A", "Fort Espagnol" = "#5ABCB9")


output %>% 
  ggplot() + 
  geom_boxplot(aes(x = year, y = SPC20Unknown, color = site)) +
  ylab("Seagrass Cover (%)") +
  xlab("Year") +
  scale_colour_manual(values = cols) +
  labs(color = "Sites :") + 
  theme_bw() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 17),
    legend.position = "top"
  )



output %>% 
  group_by(year, site) %>% 
  reframe(extent = sum(SPC20Unknown*n())*(10^-6)) %>% 
  ggplot(aes(x = year, y = extent, color = site))+
  geom_point()+
  geom_smooth(method = "glm")+
  scale_y_log10() +
  ylab("Seagrass Extent (km²)") +
  xlab("Year") +
  scale_colour_manual(values = cols) +
  labs(color = "Sites :") + 
  theme_bw() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 17),
    legend.position = "top"
  )

