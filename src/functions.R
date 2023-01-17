
join_simul_data_w_methods <- function(simul_methods, in_vis){
  
  simul_methods_filter <- simul_methods %>%
    filter(SiteID %in% in_vis$SiteID)
  
  # length(is.na(simul_methods$characteristicName))
  
  simul_methods_time_fix <- simul_methods_filter %>%
    mutate(time = as.character(format(date_time, '%H:%M:%S')), 
           date_only = ifelse(is.na(date_time) | time == '00:00:00',T,F), 
           date_unity = ymd_hms(ifelse(date_only == T,
                                       paste(date,'12:00:00'),
                                       as.character(date_time)),
                                tz = 'UTC')) %>%
    mutate(time = as.character(format(date_unity, '%H:%M:%S')),
           date_unity = ifelse(time == '00:00:00',
                               date_unity + hours(12),
                               date_unity) %>% #Convert back to poxict ()
             as.POSIXct(.,
                        origin = '1970-01-01 00:00:00',
                        tz = 'UTC')) %>%
    #remove any time stamps that are NA
    filter(!is.na(date_unity),
           year(date_unity) > 1900,
           year(date_unity) <= year(Sys.Date())) %>%
    #select(SiteID,date,characteristicName) %>%
    as_tibble() %>%
    select(-date_only) %>%
    mutate(harmonized_parameter = gsub('chl.a',
                                       'chl_a',
                                       harmonized_parameter))
  
  simul_vis_long <- in_vis %>%
    pivot_longer(cols = chl_a:tss, names_to = 'harmonized_parameter') 
  
  simul_vis_methods <- full_join(simul_methods_time_fix, simul_vis_long) 
  
  simul_vis_methods
}