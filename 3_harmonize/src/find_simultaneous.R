
find_simultaneous <- function(chla_path, doc_path, sdd_path, tss_path, wqp_metadata){
  
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
  
  # Inform the user of the dataset size
  message(sprintf(paste0("The final dataset contains %s simultaneous records."), 
                  nrow(simultaneous)))
  
  
  # Generate export-ready dataset -------------------------------------------
  
  simul_clean <- simultaneous %>%
    left_join(x = .,
              y = wqp_metadata %>%
                select(MonitoringLocationIdentifier, lat, lon,
                       type = ResolvedMonitoringLocationTypeName),
              by = c("SiteID" = "MonitoringLocationIdentifier",
                     "lat", "lon")) %>%
    select(SiteID, type, date, lat, lon, chla = mean_chla, doc = mean_doc,
           secchi = mean_sdd, tss = mean_tss) %>%
    distinct()
  
  
  return(simul_clean)
  
}
















