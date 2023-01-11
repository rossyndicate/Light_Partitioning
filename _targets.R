# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("tidyverse")
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

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
                packages = "sf")
)
