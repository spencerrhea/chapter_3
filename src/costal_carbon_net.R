library(tidyverse)

depthseries_data <- read_csv('data/CCN_data/CCN_depthseries.csv')

# Check if any data is missing 
missing_om_frac <- depthseries_data %>%
    filter(is.na(dry_bulk_density)) %>%
    filter(is.na(fraction_organic_matter) | is.na(fraction_carbon)) %>%
    distinct(study_id, site_id, core_id)

depthseries_data_sum_by_core <- depthseries_data %>%
    inner_join(., missing_om_frac) %>%
    mutate(depth_int = depth_max - depth_min) %>%
    group_by(core_id) %>%
    mutate(total_depth = max(depth_max))


depthseries_data_sum_by_core %>%
    filter(core_id == 'savannah_high_1') %>%
    ggplot(aes(x = fraction_organic_matter, y = depth_max)) +
    geom_point() +
    geom_line()

depthseries_data_sum_by_core %>%
    filter(core_id == '703') %>%
    ggplot(aes(x = dry_bulk_density, y = depth_max)) +
    geom_point() +
    geom_line()
