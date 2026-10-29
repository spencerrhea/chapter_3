library('sf')
library('tidyverse')
library('terra')
library('ggthemes')



#### Get NC Elevation  ####
# goose_creek <- st_read('data/goose_soils.shp')

goose_creek <- st_read('data/soil_sampling_locations_august.shp')

add_point <- tibble(id = c(7, 8, 9, 10), site_name = c('ULT_x', 'HIST_1', 'HIST_2', 'HIST_3'),
                    soil_type = c('Ultisol', 'Histosol', 'Histosol', 'Histosol'),
                    x = c(35.477395, 35.577750, 35.592210, 35.592980),
                    y = c(-76.909647, -75.913520, -75.907570, -75.910610)) %>%
    st_as_sf(coords = c('y', 'x'), crs = 4326)

goose_creek <- goose_creek %>%
    rbind(add_point)
# goose_creek <- st_read('data/goose_soils')

elevation_points <- elevatr::get_elev_point(goose_creek)

sf::st_write(elevation_points, 'data/august_soil_sampling_sites',
             driver = 'GeoJSON',
             delete_dsn = T)

mapview::mapview(goose_creek)

sites <- read_csv('data/Sites1_I_E_Soil - Sites1_E_I_Soil.csv')
sites_save <- sites %>%
    st_as_sf(coords = c('x', 'y'), crs = 4326) %>%
    mutate(title = site_name) %>%
    select(title)

sf::write_sf(sites_save, 'data/THELORACS_sites',
             driver = 'GeoJSON',
             delete_dsn = T)

