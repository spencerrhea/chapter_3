# This script is to anlyize the data from the EXCHANGE program 
library(tidyverse)
library(sf)


# Read in data 
meta_data <- read_csv('data/EXCHANGE/raw/data/ec1_metadata_v3/ec1_metadata_igsn_samples.csv',
                      skip = 1)

meta_data_lat_long <- meta_data %>%
    select(igsn = IGSN, longitude = Longitude, latitude = Latitude, 
           local_dec = `Locality Description`,
           local = Locality,
           location = `Location Description`,
           biome = `Biome`)


site_local <- meta_data %>%
    # mutate(igsn =  substr(IGSN, 1, 8)) %>%
    select(longitude = Longitude, latitude = Latitude, biome = Biome, 
           locality = Locality, location_description = `Location Description`, 
           locality_description = `Locality Description`,
           collection_date = `Collection date`) 
        

site_local_sf <- site_local %>%
    filter(!is.na(longitude)) %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) 

mapview::mapview(site_local_sf)

soil_files <- list.files('data/EXCHANGE/raw/data/ec1_soil_v3/', full.names = T)

for(i in 1:length(soil_files)) {
    
    this_soil_file <- read_csv(soil_files[i]) %>%
        left_join(., meta_data_lat_long, by = 'igsn')
    
    if(i == 1) {
        soil_data <- this_soil_file
        next
    }
    
    soil_data <- left_join(soil_data, this_soil_file,
                           by = c('latitude', 'longitude', 'campaign', 'kit_id', 
                                  'transect_location', 'local', 'local_dec',
                                  'biome', 'location'))
}

# combine all igsn codes into one column. It is annlying how they do this, 
# becuase diffrent codes correspond to the same locaiton but differnt data products 
soil_data <- soil_data %>%
    unite(igsn.x, igsn.y, igsn.x.x, igsn.y.y, igsn.x.x.x, igsn.y.y.y, igsn.x.x.x.x,
          igsn.y.y.y.y, igsn.x.x.x.x.x, igsn.y.y.y.y.y, igsn.x.x.x.x.x.x,
          igsn.y.y.y.y.y.y, sep = ',') %>%
    rename(igsn = igsn.x) %>%
    filter(local_dec == 'Chesapeake Bay')

# check for complete sites
complete_sites_soils <- soil_data %>%
    group_by(local, location) %>%
    summarise(n = n())

## Pull out contionus data for pca 
# skip minerals for now because idk what they are 
# skip texture for now becuase a lot are missing 
continous_vars_interest <- c('Ca_meq_100g', 'Mg_meq_100g', 'Na_meq_100g', 'K_meq_100g',
                             'CEC_meq_100g', 
                             # 'Al_meq_100g', # Skip for now becuase lot of missing data 
                             'specific_conductance_us_cm',
                             'moisturecontent_perc_drywtbasis', # Maybe not 
                             'Fe_ug_g', 'ph', 'carbon_weight_perc', 'nitrogen_weight_perc',
                             'bulk_density_g_cm3', 'th_s', 'alpha', 'n',
                             'albite', 'kaolinite', 'microcline', 'quartz')

soil_data_cont <- soil_data %>%
    select(!!continous_vars_interest, transect_location, kit_id, latitude, 
           longitude, local)

soil_data_cont <- soil_data_cont %>%
    mutate(transect_location = factor(transect_location, c('wetland', 'transition', 'upland')))


#### minerals 
# 'albite', 'kaolinite', 'microcline', 'quartz'
# albite = feldspar mineral 
# Microcline = ignious rock forming 
# quartz = primate mineral 
# kaolinite = 1;1 clay, alumnim slicate 

#### Water retention 
# th_s = Saturated water content in cubic meter per cubic meter
# n = Shape parameter
# alpha = Scale parameter inversely proportional to mean pore diameter

#### PCA ####
pairs.panels(soil_data_cont[,c(-20, -21, -22, -23, -24)],
             gap = 0,
             bg = c("#065C00", "#C27900", "#EDCA00")[soil_data_cont$transect_location],
             pch=21)

soil_data_cont_complet <- soil_data_cont[complete.cases(soil_data_cont),]

pc <- prcomp(soil_data_cont_complet[,c(-20, -21, -22, -23, -24)],
             center = TRUE,
             scale. = TRUE)
attributes(pc)

library(devtools)
library(ggbiplot)
g <- ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              var.axes = T,
              groups = soil_data_cont_complet$transect_location)
g <- g + scale_color_discrete(name = '')
g <- g + ggthemes::theme_few() +
    theme(legend.direction = 'horizontal',
               legend.position = 'bottom') +
    scale_color_manual(values = c("#EDCA00", "#C27900", "#065C00"))
print(g)

g_line <- ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              var.axes = FALSE,
              groups = soil_data_cont_complet$transect_location) +
    geom_line(aes(group = as.factor(soil_data_cont_complet$kit_id)))
g_line <- g_line + scale_color_discrete(name = '')
g_line <- g_line + ggthemes::theme_few() +
    theme(legend.direction = 'horizontal',
          legend.position = 'bottom') +
    scale_color_manual(values = c("#EDCA00", "#C27900", "#065C00"))
print(g_line)

look <- soil_data_cont_complet %>%
    group_by(kit_id) %>%
    summarise(n = n())
#### Old ####
soil_data_ <- soil_data %>%
    left_join(., site_local)

soil_data_check <- soil_data_ %>%
    group_by(latitude, longitude) %>%
    summarise(n = n())

# filter for chespeak site 
meta_data_chespeak <- site_local %>%
    filter(locality_description == 'Chesapeake Bay')

names_igsn <- grep('igsn', names(soil_data), value = T)

soil_data_lok <- soil_data %>%
    select(names_igsn)


soil_data %>%
    filter(kit_id)


site_local_sf %>%
    filter(igsn == 'IEEXG00Q') %>%
    mapview::mapview()

meta_look_1 <- meta_data %>%
    filter(IGSN == 'IEEXG00O0')
meta_look_2 <- meta_data %>%
    filter(IGSN == 'IEEXG00NY')
