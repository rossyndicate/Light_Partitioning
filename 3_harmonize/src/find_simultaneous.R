
find_simultaneous <- function(site_info, chla_path, doc_path, sdd_path, tss_path){
  
  # Load data ---------------------------------------------------------------
  
  # Read in the exported harmonized datasets
  chla <- read_feather(chla_path)
  doc <- read_feather(doc_path)
  sdd <- read_feather(sdd_path)
  tss <- read_feather(tss_path)
  
  
  # Aggregate ---------------------------------------------------------------
  
  chla_agg <- chla %>%
    group_by(SiteID, date, lon, lat, datum) %>%
    summarize(mean_chla = mean(harmonized_value))
  
  doc_agg <- doc %>%
    group_by(SiteID, date, lon, lat, datum) %>%
    summarize(mean_doc = mean(harmonized_value))
  
  sdd_agg <- sdd %>%
    group_by(SiteID, date, lon, lat, datum) %>%
    summarize(mean_sdd = mean(harmonized_value))
  
  tss_agg <- tss %>%
    group_by(SiteID, date, lon, lat, datum) %>%
    summarize(mean_tss = mean(harmonized_value))
  
  
  # Determine simultaneous points -------------------------------------------
  
  simultaneous <- reduce(.x = list(chla_agg, doc_agg, sdd_agg, tss_agg),
                         .f = inner_join,
                         by = c('SiteID', 'date', 'lon', 'lat', 'datum'))
  
  return(simultaneous)
  
}