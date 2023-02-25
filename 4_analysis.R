

p4_targets_list <- list(
  
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
  
  
  # # Unique site inventory 
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
  
  # tar_target(no_secchi,
  #            in_vis %>%
  #              select(-p_sand) %>%
  #              filter(across(c(chl_a,doc,tss), ~!is.na(.))) %>% 
  #              inner_join(site_vis) %>%
  #              filter(type != 'Facility') %>%
  #              #Set some reasonable thresholds, AquaSat is too generous
  #              filter(
  #                chl_a < 1000,## ug/L
  #                tss < 1000, ## mg/L
  #                doc < 50),
  #            format = "feather"),
  tar_target(unique_simul_mr,
             simul %>%
               distinct(SiteID, lat, long, type) %>%
               st_as_sf(.,coords = c('long','lat'), crs = 4326),
             packages = c("tidyverse", "sf")),
  
  tar_target(unique_simul,
             simultaneous_data %>%
               distinct(SiteID, lat, lon, type) %>%
               st_as_sf(.,coords = c('lon','lat'), crs = 4326),
             packages = c("tidyverse", "sf")),
  
  tar_target(unique_simul_strict,
             simultaneous_data_strict %>%
               distinct(SiteID, lat, lon, type) %>%
               st_as_sf(.,coords = c('lon','lat'), crs = 4326),
             packages = c("tidyverse", "sf")),
  
  # Where are sites with simultaneous observations of clarity constituents?
  # Issues with the mapview package here right now
  # tar_target(simul_vis,
  #            {
  #              # Remove flat geobuff which breaks display
  #              mapviewOptions(fgb = FALSE)
  #              mapview(unique_simul, zcol = 'type')
  #            },
  #            packages = c("tidyverse", "sf", "mapview")),
  
  # Make a temporary option for visualizing the simultaneous data because
  # simul_vis above is not working
  tar_target(simul_vis_gg_mr,
             {
               
               plot_data <- unique_simul_mr %>%
                 # Non-deprecated version of crs 2163
                 st_transform(crs = 9311)
               
               state_selection <- states() %>%
                 filter(!NAME %in% c("Alaska", "Hawaii", "American Samoa",
                                     "Guam", "Puerto Rico",
                                     "United States Virgin Islands",
                                     "Commonwealth of the Northern Mariana Islands")) %>%
                 st_transform(crs = 9311)
               
               plot_data %>%
                 ggplot() +
                 geom_sf(data = state_selection) +
                 geom_hex(data = bind_cols(as_tibble(plot_data),
                                           st_coordinates(plot_data)),
                          aes(x = X, y = Y),
                          alpha = 0.85) +
                 xlab(NULL) +
                 ylab(NULL) +
                 coord_sf(xlim = c(min(st_coordinates(state_selection)[,"X"]),
                                   max(st_coordinates(state_selection)[,"X"])),
                          ylim = c(min(st_coordinates(state_selection)[,"Y"]),
                                   max(st_coordinates(state_selection)[,"Y"]))) +
                 scale_fill_viridis_c("Record count") +
                 theme_bw()
             },
             packages = c("tidyverse", "sf", "tigris")), 
  
  tar_target(simul_vis_gg,
             {
               
               plot_data <- unique_simul %>%
                 # Non-deprecated version of crs 2163
                 st_transform(crs = 9311)
               
               state_selection <- states() %>%
                 filter(!NAME %in% c("Alaska", "Hawaii", "American Samoa",
                                     "Guam", "Puerto Rico",
                                     "United States Virgin Islands",
                                     "Commonwealth of the Northern Mariana Islands")) %>%
                 st_transform(crs = 9311)
               
               plot_data %>%
                 ggplot() +
                 geom_sf(data = state_selection) +
                 geom_hex(data = bind_cols(as_tibble(plot_data),
                                           st_coordinates(plot_data)),
                          aes(x = X, y = Y),
                          alpha = 0.85) +
                 xlab(NULL) +
                 ylab(NULL) +
                 coord_sf(xlim = c(min(st_coordinates(state_selection)[,"X"]),
                                   max(st_coordinates(state_selection)[,"X"])),
                          ylim = c(min(st_coordinates(state_selection)[,"Y"]),
                                   max(st_coordinates(state_selection)[,"Y"]))) +
                 scale_fill_viridis_c("Record count") +
                 theme_bw()
             },
             packages = c("tidyverse", "sf", "tigris")),
  
  tar_target(simul_vis_gg_strict,
             {
               
               plot_data <- unique_simul_strict %>%
                 # Non-deprecated version of crs 2163
                 st_transform(crs = 9311)
               
               state_selection <- states() %>%
                 filter(!NAME %in% c("Alaska", "Hawaii", "American Samoa",
                                     "Guam", "Puerto Rico",
                                     "United States Virgin Islands",
                                     "Commonwealth of the Northern Mariana Islands")) %>%
                 st_transform(crs = 9311)
               
               plot_data %>%
                 ggplot() +
                 geom_sf(data = state_selection) +
                 geom_hex(data = bind_cols(as_tibble(plot_data),
                                           st_coordinates(plot_data)),
                          aes(x = X, y = Y),
                          alpha = 0.85) +
                 xlab(NULL) +
                 ylab(NULL) +
                 coord_sf(xlim = c(min(st_coordinates(state_selection)[,"X"]),
                                   max(st_coordinates(state_selection)[,"X"])),
                          ylim = c(min(st_coordinates(state_selection)[,"Y"]),
                                   max(st_coordinates(state_selection)[,"Y"]))) +
                 scale_fill_viridis_c("Record count") +
                 theme_bw()
             },
             packages = c("tidyverse", "sf", "tigris")),
  
  # What is the general relationship between variables in log log space
  tar_target(log_simul_vis,
             {
               # log_simul <- simul %>%
               
               log_simul <- simultaneous_data %>%
                 mutate(across(c(secchi, chla, tss, doc),
                               log10)) %>%
                 filter(across(c(chla, doc, secchi, tss),
                               ~!is.na(.) & . < Inf & . > -Inf)) 
               
               log_simul %>%
                 # This used to use sample_frac but I'm not sure why so I removed it
                 ungroup() %>%
                 select(secchi,chla,tss,doc,type) %>%
                 ggpairs(lower = list(continuous = wrap('points',shape = 1)),
                         diag = list(continuous = wrap('densityDiag', alpha = 0.5)),
                         mapping = ggplot2::aes(color = type),
                         columns = c('secchi','chla','tss','doc')) +
                 ggthemes::theme_few() + 
                 scale_color_manual(values = c('seagreen3','skyblue3','saddlebrown'))            
             },
             packages = c("tidyverse", "GGally", "ggthemes")
  ),
  
  tar_render(model_report,
             "4_analysis/src/model_report.Rmd",
             packages = c("tidyverse", "lubridate", "feather", "ggpmisc", "Metrics",
                          "broom", "ggtern", "kableExtra"))
  
  
)