# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("tidyverse")
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source(files = "src/functions.R")

# Replace the target list below with your own:
list(
  
  # 1. Downloads ------------------------------------------------------------
  
  # Long-term we'll want to transition these downloads to tar_download() but
  # I haven't had luck making that method work
  
  # In-situ raw data with methods
  tar_target(aq_situ_download,
             {
               download.file('https://figshare.com/ndownloader/files/15475154',
                             mode = 'wb',# Needs to be written in binary for some reason 
                             destfile = 'data/in/aq_situ.zip')
               unzip('data/in/aq_situ.zip', exdir = 'data/in/aq_situ')
             }),
  
  tar_file_read(in_vis,
                # Detect the file path from aq_situ_download since there are two
                # that are returned
                aq_situ_download %>%
                  grep(pattern = "unity", value = TRUE),
                read_csv(file = !!.x)),
  
  
  # Site Inventory with type, because it's not in the other inventory
  tar_target(inv_type_download,
             {
               download.file('https://figshare.com/ndownloader/files/24720434',
                             mode = 'wb',
                             destfile = 'data/in/inv.feather')
               
               "data/in/inv.feather"
             }),
  
  tar_file_read(inv_type,
                inv_type_download,
                read_feather(path = !!.x),
                packages = "feather",
                format = "feather"),
  
  
  # Unique site inventory 
  tar_target(site_download,
             {
               download.file('https://figshare.com/ndownloader/files/24720437',
                             mode = 'wb',
                             destfile = 'data/in/unq_site.feather')
               
               "data/in/unq_site.feather"
             }),
  
  tar_file_read(site,
                site_download,
                read_feather(path = !!.x),
                packages = "feather",
                format = "feather"),
  
  
  # Ecoregion data
  tar_target(ecoregion_download,
             {
               download.file('https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na/na_cec_eco_l2.zip',
                             destfile = 'data/in/eco2.zip')
               
               unzip('data/in/eco2.zip', exdir = 'data/in/ecoregion')
             }),
  
  tar_file_read(ecoregion,
                ecoregion_download %>%
                  grep(pattern = "shp$", value = TRUE),
                st_read(dsn = !!.x),
                packages = "sf"),
  
  tar_file_read(simul_methods,
                "data/in/aq_situ/in-situ/wqp_long_with_methods.csv",
                read_csv(!!.x)),
  
  
  
  # 2. Data management and eval ---------------------------------------------
  
  tar_target(inv_type_edit,
             inv_type %>%
               select(SiteID = MonitoringLocationIdentifier,
                      type = ResolvedMonitoringLocationTypeName) %>%
               mutate(type = ifelse(grepl('Lake',type),'Lake',type))),
  
  tar_target(site_vis,
             site %>%
               inner_join(inv_type_edit) %>%
               distinct(SiteID,lat,long,type)),
  
  # Select only simultaneous observations
  tar_target(simul,
             # Only bother with data that has complete simultaneous observations of
             # chl_a, doc, etc...
             in_vis %>%
               select(-p_sand) %>%
               filter(if_all(c(chl_a,doc,tss,secchi), ~!is.na(.))) %>% 
               inner_join(site_vis) %>%
               filter(type != 'Facility') %>%
               #Set some reasonable thresholds, AquaSat is too generous
               filter(secchi < 15,
                      chl_a < 1000,## ug/L
                      tss < 1000, ## mg/L
                      doc < 50),
             format = "feather"),
  
  tar_target(no_secchi,
             in_vis %>%
               select(-p_sand) %>%
               filter(across(c(chl_a,doc,tss), ~!is.na(.))) %>% 
               inner_join(site_vis) %>%
               filter(type != 'Facility') %>%
               #Set some reasonable thresholds, AquaSat is too generous
               filter(
                 chl_a < 1000,## ug/L
                 tss < 1000, ## mg/L
                 doc < 50),
             format = "feather"),
  
  tar_target(unique_simul,
             simul %>%
               distinct(SiteID, lat, long, type) %>%
               st_as_sf(.,coords = c('long','lat'), crs = 4326),
             packages = c("tidyverse", "sf")),
  
  # Where are sites with simultaneous observations of clarity constituents?
  # Issues with the mapview package here right now
  tar_target(simul_vis,
             {
               # Remove flat geobuff which breaks display
               mapviewOptions(fgb = FALSE)
               mapview(unique_simul, zcol = 'type')
             },
             packages = c("tidyverse", "sf", "mapview")),
  
  # What is the general relationship between variables in log log space
  tar_target(log_simul_vis,
             {
               log_simul <- simul %>%
                 dplyr::mutate(across(c(secchi,chl_a,tss,doc,tis),
                                      log10)) %>%
                 dplyr::filter(across(c(chl_a,doc,secchi,tss),
                                      ~!is.na(.) & . < Inf & . > -Inf)) 
               
               log_simul %>%
                 sample_frac(0.2) %>%
                 ungroup() %>%
                 select(secchi,chl_a,tss,doc,type) %>%
                 ggpairs(lower = list(continuous = wrap('points',shape = 1)),
                         diag = list(continuous = wrap('densityDiag', alpha = 0.5)),
                         mapping = ggplot2::aes(color = type),
                         columns = c('secchi','chl_a','tss','doc')) +
                 ggthemes::theme_few() + 
                 scale_color_manual(values = c('seagreen3','skyblue3','saddlebrown'))
             },
             packages = c("tidyverse", "GGally", "ggthemes")
  ),
  
  tar_target(nap_test,
             {
               range <- c(50, 100, 200, 234)
               
               # Couldn't think of a more clever way to multiply
               # chl_a by the range of values, so just made
               # dataframe 4 times bigger with new column called ratio. 
               nap_test <- expand_grid(no_secchi, chl_ratio = range) %>%
                 mutate(power = ifelse(chl_ratio == 234, 0.57, 1),
                        chl_a_biomass = exp(log(chl_ratio / 1000) + log(chl_a) * power),
                        tss_dead = tss - chl_a_biomass)
             },
             packages = c("tidyverse", "ggpmisc")),
  
  tar_target(negative_tss_dead,
             nap_test %>%
               mutate(negative = ifelse(tss_dead < 0, 'negative', 'positive')) %>%
               group_by(negative,chl_ratio) %>%
               count() %>%
               pivot_wider(names_from = 'negative', values_from = 'n')  %>%
               mutate(percent_neg = negative / (positive + negative) * 100)),
  
  tar_target(chla_tss_dead_plot,
             
             #Subset for plotting purposes
             nap_test %>%
               # Remove negatives and very small numbers (ug/L of sediment is 
               # basically zero)
               filter(tss_dead > 0.001) %>%
               sample_frac(0.1) %>%
               ggplot(., aes(chl_a,tss_dead,color = type)) +
               facet_wrap(~chl_ratio) + 
               geom_point() + 
               scale_x_log10() + 
               scale_y_log10() + 
               stat_poly_eq() + 
               ggthemes::theme_few() + 
               scale_color_manual(values = c('seagreen3','skyblue3','saddlebrown'))),
  
  tar_target(simul_vis_methods,
             join_simul_data_w_methods(simul_methods = simul_methods,
                                       in_vis = in_vis),
             packages = c("tidyverse", "lubridate"))
  
  
)





















