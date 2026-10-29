# This script is to compare the range of soil conditions exposed to sea level rise
# We looked at locations in NC below 10 m in elevation and forested. WE
# downloaded data from gSSURGO for soil , NLCD for forests, and Yueng et al 2026 
# For ghost forest location 

library('sf')
library('tidyverse')
library('terra')
library('ggthemes')


#### Get NC Elevation  ####
coasl_aoi <- st_read('data/coastal_nc.kml')
nc_boundary <- USAboundaries::us_states() %>%
    filter(name == 'North Carolina')

elevation <- elevatr::get_elev_raster(coasl_aoi, z = 9)
elevation <- terra::rast(elevation)
elevation <- mask(elevation, st_transform(st_zm(coasl_aoi), st_crs(elevation)))

elevation_mask <- ifel(elevation <= 3, 1, NA)

#### Retrive gSSURGO ####
# Make grid to extract from
# nc <- USAboundaries::us_states() %>%
#     filter(name == 'North Carolina')
# 
# # Project to a CRS with meter units
# # NAD83 / Conus Albers (meters)
# nc_proj <- st_transform(nc, 5070)
# 
# # Create 500 km x 500 km grid
# grid_500km <- st_make_grid(
#     nc_proj,
#     cellsize = c(50000, 50000),   # 500 km = 500,000 meters
#     square = TRUE
# )
# 
# # Convert to sf object
# grid_sf <- st_sf(
#     grid_id = seq_along(grid_500km),
#     geometry = grid_500km
# )
# 
# # Clip to North Carolina
# grid_nc <- st_intersection(st_transform(coasl_aoi, st_crs(grid_sf)), grid_sf)
# 
# # Optional: transform back to WGS84
# grid_nc_ll <- st_transform(grid_nc, 4326)
# 
# rownames(grid_nc_ll) <- 1:length(grid_nc_ll$geometry)
# for(i in 1:nrow(grid_nc_ll)) {
#     this_grid <- grid_nc_ll[i,]
# 
#     soil <- soilDB::mukey.wcs(aoi = this_grid,
#                               db = 'gssurgo',
#                               quiet = TRUE)
# 
#     if(i == 1) {
#         all_gssurgo <- soil
#     } else {
#         all_gssurgo <- raster::merge(all_gssurgo, soil)
#     }
# 
#     print(i)
# }
# 
# raster::writeRaster(all_gssurgo, 'data/sssurgo_coastal_nc.tif',
#                     overwrite=TRUE)
all_gssurgo <- terra::rast('data/sssurgo_coastal_nc.tif')

st_bbox(all_gssurgo)

# Get soil data 
mukey_values <- unique(values(all_gssurgo$mukey))[,'mukey']

ssurgo <- st_layers('data/gSSURGO_NC.gdb')
ssurgo_componet <- st_read('data/gSSURGO_NC.gdb', layer = 'component')
ssurgo_chorizon <- st_read('data/gSSURGO_NC.gdb', layer = 'chorizon')

soil_join <- full_join(ssurgo_componet, ssurgo_chorizon)

# Soil vars from Macrosheds
nrcs_var_name = c(
    # percent
    'soil_org' = 'om_r',
    # percent
    'soil_sand' = 'sandtotal_r',
    # percent
    'soil_silt' = 'silttotal_r',
    # percent
    'soil_clay' = 'claytotal_r',
    # mass/volume
    # 'soil_partical_density' = 'partdensity',
    # micrometers per second
    # 'soil_ksat' = 'ksat_r',
    # centimeters of water per centimeter of soil,
    # quantity of water that the soil is capable
    # of storing for use by plants
    'soil_awc' = 'awc_r',
    # Water content, 1/10 bar, is the amount of soil
    # water retained at a tension of 15 bars, expressed
    # as a volumetric percentage of the whole
    # soil material.
    'soil_water_0.1bar' = 'wtenthbar_r',
    # Water content, 1/3 bar, is the amount of soil
    # water retained at a tension of 15 bars, expressed
    # as a volumetric percentage of the whole
    # soil material. 15 bar = wilting point
    'soil_water_0.33bar' = 'wthirdbar_r',
    # Water content, 15 bar, is the amount of soil
    # water retained at a tension of 15 bars, expressed
    # as a volumetric percentage of the whole
    # soil material. 15 bar = field capacity
    'soil_water_15bar' = 'wfifteenbar_r',
    # Water content, 0 bar, is the amount of soil
    # water retained at a tension of 15 bars, expressed
    # as a volumetric percentage of the whole
    # soil material.
    'soil_water_0bar' = 'wsatiated_r',
    # percent of carbonates, by weight, in the
    # fraction of the soil less than 2 millimeters
    # in size.
    # 'pf_soil_carbonate' = 'caco3_r',
    # percent, by weight hydrated calcium sulfates
    # in the fraction of the soil less than 20
    # millimeters in size
    # 'pf_soil_gypsum' = 'gypsum_r',
    # Cation-exchange capacity (CEC-7) is the
    # total amount of extractable cations that can
    # be held by the soil, expressed in terms of
    # milliequivalents per 100 grams of soil at
    # neutrality (pH 7.0)
    'soil_cat_exchange_7' = 'cec7_r',
    # Effective cation-exchange capacity refers to
    # the sum of extractable cations plus aluminum
    # expressed in terms of milliequivalents per
    # 100 grams of soil
    # 'pf_soil_cat_exchange_eff' = 'ecec_r',
    # Electrical conductivity (EC) is the electrolytic
    # conductivity of an extract from saturated
    # soil paste, expressed as decisiemens per
    # meter at 25 degrees C.
    'pf_soil_elec_cond' = 'ec_r',
    # Sodium adsorption ratio is a measure of the
    # amount of sodium (Na) relative to calcium (Ca)
    # and magnesium (Mg) in the water extract from
    # saturated soil paste. It is the ratio of the
    # Na concentration divided by the square root
    # of one-half of the Ca + Mg concentration.
    # Soils that have SAR values of 13 or more may
    # be characterized by an increased dispersion
    # of organic matter and clay particles, reduced
    # saturated hydraulic conductivity (Ksat) and
    # aeration, and a general degradation of soil
    # structure.
    'pf_soil_SAR' = 'sar_r',
    # pH is the 1:1 water method. A crushed soil
    # sample is mixed with an equal amount of water,
    # and a measurement is made of the suspension.
    'soil_ph' = 'ph1to1h2o_r',
    'extractable_fe' = 'feoxalate_r')
# Bulk density, one-third bar, is the ovendry
# weight of the soil material less than 2
# millimeters in size per unit volume of soil
# at water tension of 1/3 bar, expressed in
# grams per cubic centimeter
# 'pf_soil_bulk_density' = 'dbthirdbar_r'),
#Linear extensibility refers to the change in
# length of an unconfined clod as moisture
# content is decreased from a moist to a dry state.
# It is an expression of the volume change
# between the water content of the clod at
# 1/3- or 1/10-bar tension (33kPa or 10kPa tension)
# and oven dryness. The volume change is reported
# as percent change for the whole soil
# 'soil_linear_extend' = 'lep_r',
# Liquid limit (LL) is one of the standard
# Atterberg limits used to indicate the plasticity
# characteristics of a soil. It is the water
# content, on a percent by weight basis, of
# the soil (passing #40 sieve) at which the
# soil changes from a plastic to a liquid state
# 'soil_liquid_limit' = 'll_r',
# Plasticity index (PI) is one of the standard
# Atterberg limits used to indicate the plasticity
# characteristics of a soil. It is defined as
# the numerical difference between the liquid
# limit and plastic limit of the soil. It is
# the range of water content in which a soil
# exhibits the characteristics of a plastic solid.
# 'soil_plasticity_index' = 'pi_r'
#### done ####

all_data <- tibble()
for(i in 1:length(nrcs_var_name)) {
    
    this_var <- nrcs_var_name[i]
    
    soil_data_one_var <- soil_join %>%
        select(cokey, chkey, hzname, desgnmaster, hzdept_r, hzdepb_r,
               !!this_var, mukey, compname, comppct_r, majcompflag)
    
    cokey_size <- soil_data_one_var %>%
        filter(!is.na(.data[[names(this_var)]])) %>%
        mutate(layer_size = hzdepb_r-hzdept_r) %>%
        group_by(cokey) %>%
        summarise(cokey_size = sum(layer_size)) %>%
        ungroup()
    
    cokey_weighted_av <- soil_data_one_var %>%
        filter(!is.na(.data[[names(this_var)]])) %>%
        mutate(layer_size = hzdepb_r-hzdept_r) %>%
        left_join(., cokey_size, by = 'cokey') %>%
        mutate(layer_prop = layer_size/cokey_size) %>%
        mutate(value_mat_weith = .data[[names(this_var)]] * layer_prop)  %>%
        group_by(mukey, cokey) %>%
        summarise(value_comp = sum(value_mat_weith),
                  comppct_r = unique(comppct_r)) %>%
        ungroup()
    
    # mukey weighted average of all compenets
    mukey_weighted_av <- cokey_weighted_av %>%
        group_by(mukey) %>%
        mutate(comppct_r_sum = sum(comppct_r)) %>%
        summarise(value_mukey = sum(value_comp*(comppct_r/comppct_r_sum)))

    mukey_weighted_av[,'name'] <- names(this_var)
    
    all_data <- rbind(all_data, mukey_weighted_av)
    
}

soil_name <- soil_join %>%
    select(mukey, taxorder) %>%
    distinct(mukey, .keep_all = T)

all_data_wide <- all_data %>%
    pivot_wider(names_from = 'name', values_from = 'value_mukey') %>%
    left_join(., soil_name)

#### NLCD ####
# nlcd <- rast('data/Annual_NLCD_LndCov_2025_CU_C1V2/Annual_NLCD_LndCov_2025_CU_C1V2.tif')
# 
# nlcd_nc_coast <- terra::crop(nlcd, st_transform(coasl_aoi, st_crs(nlcd)))
# nlcd_nc_coast <- terra::mask(nlcd_nc_coast, st_transform(coasl_aoi, st_crs(nlcd)))
# terra::writeRaster(nlcd_nc_coast, 'data/nlcd_nc_coast.tif')

nlcd_nc_coast <- terra::rast('data/nlcd_nc_coast.tif') %>%
    project(., project(., all_gssurgo))

terra::cats(nlcd_nc_coast)
terra::levels(nlcd_nc_coast)

elevation_mask <- elevation_mask %>%
    project(., all_gssurgo)

# Including emergent wetlands seems to no do much to the distribution, probably 
# because they make up a small area 
forest_classes <- c(41, 42, 43, 90) #95)
nlcd_forest_wet <- ifel(nlcd_nc_coast %in% forest_classes, 1, NA)

nlcd_forest_wet_exposed <- mask(nlcd_forest_wet, elevation_mask)
#### Ghost forest ####
ghost_forest <- rast('data/HSR_1/HSR_1.tif') %>%
    project(., all_gssurgo)

gf_elevation_dist <- mask(project(elevation, ghost_forest), ghost_forest)

hist(values(gf_elevation_dist))

#### Subset soil data ####

# Get rasters of soil for each class 
surgo_forested <- mask(all_gssurgo, nlcd_forest_wet_exposed)
surgo_gf <- mask(all_gssurgo, ghost_forest)

# get tabular data for frequency
surgo_forested_mukeys <- surgo_forested %>%
    values() %>%
    as.tibble() %>%
    filter(!is.na(mukey)) %>%
    # group_by(mukey) %>%
    # summarise(n = n()) %>%
    mutate(mukey = as.character(mukey)) %>%
    left_join(all_data_wide) %>%
    mutate(type = 'Forest')

surgo_gf_mukeys <- surgo_gf %>%
    values() %>%
    as.tibble() %>%
    filter(!is.na(mukey)) %>%
    # group_by(mukey) %>%
    # summarise(n = n()) %>%
    mutate(mukey = as.character(mukey)) %>%
    left_join(all_data_wide) %>%
    mutate(type = 'GF')

# Get tabular data summarized 
surgo_forested_mukeys_sum <- surgo_forested %>%
    values() %>%
    as.tibble() %>%
    filter(!is.na(mukey)) %>%
    group_by(mukey) %>%
    summarise(n = n()) %>%
    mutate(mukey = as.character(mukey)) %>%
    left_join(all_data_wide) %>%
    mutate(type = 'Forest')

surgo_gf_mukeys_sum <- surgo_gf %>%
    values() %>%
    as.tibble() %>%
    filter(!is.na(mukey)) %>%
    group_by(mukey) %>%
    summarise(n = n()) %>%
    mutate(mukey = as.character(mukey)) %>%
    left_join(all_data_wide) %>%
    mutate(type = 'GF')

#### Plots ####
#### Frequency of organic matter 
all_class_ssurgo <- rbind(surgo_gf_mukeys, surgo_forested_mukeys)

surrgo_with_no_emergent_wetland <- ggplot(all_class_ssurgo) +
    geom_density(aes(x = soil_org, color = type),
                 linewidth = 2) +
    scale_color_manual(values = c('#009C00', '#C76408'),
                       name = element_blank()) +
    theme_few() +
    labs(x = 'Soil Organic Matter (%)',
         y = 'Frequency') +
    theme(legend.position = c(0.85, 0.85))
    
png('plots/ssurgo/density_soil_om_with_no_emergent.png', units = 'in', res = 200,
    width = 6, height = 4)
print(surrgo_with_no_emergent_wetland)
dev.off()

#### Soil type 
## Total area 
surgo_forested_mukeys_sum_taxa <- surgo_forested_mukeys_sum %>%
    group_by(taxorder) %>%
    summarise(n = sum(n, na.rm = T)) %>%
    mutate(area_km2 = (n * 30*30)/1e+6) %>%
    mutate(taxorder = factor(taxorder, c('Entisols', 'Inceptisols', 'Alfisols', 
                                         'Spodosols', 'Ultisols', 'Histosols', NA))) %>%
    filter(!is.na(taxorder))

surgo_gf_mukeys_sum_taxa <- surgo_gf_mukeys_sum %>%
    group_by(taxorder) %>%
    summarise(n = sum(n, na.rm = T)) %>%
    mutate(area_km2 = (n * 30*30)/1e+6) %>%
    mutate(taxorder = factor(taxorder, c('Entisols', 'Inceptisols', 'Alfisols', 
                                         'Spodosols', 'Ultisols', 'Histosols', NA))) %>%
    filter(!is.na(taxorder))

ggplot(surgo_forested_mukeys_sum_taxa) +
    geom_bar(stat = 'identity', aes(x = taxorder, y = area_km2)) +
    theme_few() +
    labs(x = 'Soil Type',
         y = 'Area (km2)',
         title = 'Coastal forests')

ggplot(surgo_gf_mukeys_sum_taxa) +
    geom_bar(stat = 'identity', aes(x = taxorder, y = area_km2)) +
    theme_few() +
    labs(x = 'Soil Type',
         y = 'Area (km2)',
         title = 'Coastal forests')

#### Percent area 
surgo_forested_mukeys_sum_taxa_percent <- surgo_forested_mukeys_sum_taxa %>%
    mutate(percent_area = area_km2/sum(surgo_forested_mukeys_sum_taxa$area_km2)) %>%
    mutate(type = 'Forest')

surgo_gf_mukeys_sum_taxa_percent <- surgo_gf_mukeys_sum_taxa %>%
    mutate(percent_area = area_km2/sum(surgo_gf_mukeys_sum_taxa$area_km2)) %>%
    mutate(type = 'GF')

percent_area_soils <- rbind(surgo_forested_mukeys_sum_taxa_percent, surgo_gf_mukeys_sum_taxa_percent)

soil_type_area <- ggplot(percent_area_soils) +
    geom_bar(stat = 'identity', position='dodge',
             aes(x = taxorder, y = percent_area, fill = type,
                 color = 'black')) +
    theme_few() +
    scale_fill_manual(values = c('#009C00', '#C76408'),
                       name = element_blank()) +
    scale_color_manual(values = 'black',
                       guide = 'none') +
    labs(x = 'Soil Type',
         y = 'Percent of area in each class (%)',
         title = 'Coastal NC below 3 m') +
    theme(legend.position = c(0.1, 0.85))

png('plots/ssurgo/soil_type_by_area.png', units = 'in', res = 200,
    width = 8, height = 4)
print(soil_type_area)
dev.off()


#### Soil Scatter
surgo_forested_mukeys_sum %>%
    filter(soil_org >= 0.1) %>%
    ggplot(aes(soil_org, soil_sand, color = soil_ph)) +
    geom_point() +
    viridis::scale_color_viridis() +
    theme_few() +
    scale_x_log10() 

soil_scatter <- rbind(surgo_forested_mukeys_sum, surgo_gf_mukeys_sum) %>%
    filter(soil_org >= 0.1) %>%
    ggplot(aes(soil_org, soil_sand, color = type)) +
    geom_point() +
    scale_color_manual(values = c('#009C00', '#C76408'),
                      name = element_blank()) +
    theme_few() +
    scale_x_log10() +
    labs(x = 'Soil Organic Carbon (%)',
         y = 'Soil Sand Content (%)') +
    theme(legend.position = 'none')

png('plots/ssurgo/soil_om_san_scatter.png', units = 'in', res = 200,
    width = 4, height = 4)
print(soil_scatter)
dev.off()

#### Look at some sites ####
sites <- read_csv('data/Sites1_I_E_Soil - Sites1_E_I_Soil.csv')

sites_sf <- sites %>%
    st_as_sf(coords = c('x', 'y'), crs = 4326)

mapview::mapview(sites_sf, zcol = 'taxorder')

sites_sf_GC <- sites_sf %>%
    filter(site_name == 'GR_1') %>%
    st_buffer(2800)

surgo_crop <- terra::mask(all_gssurgo, st_transform(sites_sf_GC, st_crs(all_gssurgo)))
surgo_crop <- trim(surgo_crop)
surgo_crop <- droplevels(surgo_crop)

#### Organic matter 
look_up_om <- all_data_wide %>%
    select(mukey, soil_org)

r2 <- classify(surgo_crop, look_up_om)
r2 <- ifel(r2 >= 110, 0, r2)
plot(as.numeric(r2))
mapview::mapview(r2)

#### Soil type 
soil_type_class <- all_data_wide %>%
    select(mukey, taxorder)

surgo_names <- data.frame(mukey = unique(values(surgo_crop))[,'mukey']) %>%
    filter(!is.na(mukey)) %>%
    mutate(mukey = as.character(mukey))

these_soil_names <- surgo_names %>%
    left_join(all_data_wide) %>%
    select(mukey, taxorder) %>%
    mutate(mukey = as.numeric(mukey))

levels(surgo_crop) <- these_soil_names
plot(surgo_crop, col = c('#B87300', '#C7A56D', '#FA2A9C', '#2317C2', '#F5FA2A'))
plot(surgo_crop, col = c('#2317C2', '#8727F5', '#C7A56D'))


soil_types <- unique(these_soil_names$taxorder)

soil_types_tib <- tibble(value = 1:6, taxorder=soil_types) %>%
    full_join(these_soil_names) %>%
    select(mukey, value)

soil_class <- classify(surgo_crop, soil_types_tib)
mapview::mapview(soil_class)

terra::writeRaster(soil_class, 'test_soil.tif', overwrite = T)


