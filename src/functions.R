
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


explore_atten_model <- function(nap_test){
  
  nap_est_no_secchi <- nap_test %>%
    ungroup() %>%
    filter(tss_dead > 0.01,
           secchi < 15 | is.na(secchi)) %>%
    mutate(secchi = ifelse(secchi < 0.1, 0.1, secchi),
           kd = (1 / (secchi)))
  
  no_secchi_clean <- nap_est_no_secchi %>%
    filter(chl_ratio == 234)
  
  nap_est <- nap_est_no_secchi %>%
    filter(!is.na(secchi))
  
  k_w <- 1 / 1.5 / max(nap_est$secchi)
  
  kd_mod <- function(df){
    mod <- lm((kd - k_w)  ~ 0 + tss_dead + doc + chl_a, data = df)
  }

  kd_resid_extract <- function(df, mod){
    df <- df %>%
      mutate(residuals = mod$residuals,
             pred = mod$fitted.values + k_w)
  }
  
  # nap_est <- nap_est %>%
  #   filter(SiteID %in% fine_sites$SiteID) 
  
  nap_mods <- group_by(nap_est, chl_ratio) %>%
    nest() %>%
    mutate(mods = map(data, kd_mod),
           data = map2(data, mods, kd_resid_extract))
  
  mod234 <- nap_mods %>%
    dplyr::filter(chl_ratio == 234)
  
  nap_resid <- nap_mods %>%
    dplyr::select(-mods) %>%
    unnest(data) %>%
    mutate(ratio_rmse = paste('ratio =', chl_ratio, ', R2 =',
                              round(cor(pred, kd)^2, 2)),
           year = year(date_unity))
  
  resid_plot <- nap_resid %>%
    sample_frac(0.2) %>%
    ggplot(., aes(kd, pred, color = year(date_unity))) + 
    geom_point(shape = 1) + 
    facet_wrap(~ratio_rmse) + 
    ggthemes::theme_few() + 
    xlab('kd (1/secchi)') +
    ylab('predicted kd') +
    scale_x_log10() + 
    scale_y_log10() +
    scale_color_viridis_c() + 
    stat_smooth(method = 'lm', se = F, color = 'black') +
    geom_abline(intercept = 0, slope = 1, col = 'red') 
  
  nap_slopes <- nap_mods %>%
    select(-data) %>%
    mutate(glance = map(mods, tidy)) %>%
    unnest(glance) %>%
    select(-mods,-std.error,-statistic) %>%
    pivot_wider(names_from = 'term', values_from = 'estimate')
  
  # Return items to pipeline
  list(nap_resid = nap_resid,
       resid_plot = resid_plot,
       no_secchi_clean = no_secchi_clean,
       nap_slopes = nap_slopes)
}


# Helper function used within characterize_error()
methods_summarizer <- function(param, nap_resid, simul_vis_methods){
  
  methods <- simul_vis_methods %>%
    filter(harmonized_parameter == param) %>%
    inner_join(nap_resid %>%
                 ungroup() %>%
                 filter(chl_ratio == 234) %>%
                 select(SiteID,date_unity,residuals,pred,kd)) 
  
  
  method_plot <- methods %>%
    group_by(analytical_method) %>%
    add_count() %>%
    mutate(method_count = paste('n =',n,'-',analytical_method)) %>%
    ggplot(., aes(kd,pred,color = method_count)) + 
    #geom_point(shape = 1) + 
    ggthemes::theme_few() + 
    xlab('kd (1.4/secchi)') +
    ylab('predicted kd') +
    scale_x_log10() + 
    scale_y_log10() + 
    stat_smooth(method = 'lm',se=F) +
    geom_abline(intercept = 0, slope = 1, col = 'black')
  
  
  method_error <- methods %>%
    group_by(analytical_method) %>%
    summarize(rmse = rmse(kd,pred),
              resid_cv = sd(residuals) / mean(residuals),
              n = n(),
              resid_se = sd(residuals) / (n^0.5),
              mdae = mdae(kd, pred),
              impact = (n * rmse) / nrow(.)) %>%
    arrange(-impact)  %>%
    mutate(across(where(is.numeric), ~(round(.x, 2))),
           param = param)
  
  name_error <- methods %>%
    group_by(characteristicName) %>%
    summarize(rmse = rmse(kd, pred),
              resid_cv = sd(residuals) / mean(residuals),
              n = n(),
              resid_se = sd(residuals) / (n^0.5),
              mdae = mdae(kd, pred),
              impact = (n * rmse) / nrow(.)) %>%
    arrange(-impact) %>%
    mutate(across(where(is.numeric), ~(round(.x, 2))),
           param = param)
  
  
  name_plot <- methods %>%
    group_by(characteristicName) %>%
    add_count() %>%
    mutate(name_count = paste('n =',n,'-',characteristicName)) %>%
    ggplot(., aes(kd,pred,color = name_count)) + 
    #geom_point(shape = 1) + 
    ggthemes::theme_few() + 
    xlab('kd (1.4/secchi)') +
    ylab('predicted kd') +
    scale_x_log10() + 
    scale_y_log10() + 
    stat_smooth(method = 'lm',se=F) +
    geom_abline(intercept = 0, slope = 1, col = 'black')
  
  return(list(method_error = method_error,
              name_error = name_error,
              method_plot = method_plot,
              name_plot = name_plot,
              methods = methods))
  
}


characterize_error <- function(nap_resid, simul_vis_methods){
  
  summarized_methods <- map(.x = c("chl_a", "doc", "tss", "secchi"),
                            .f = ~ methods_summarizer(.x, nap_resid, simul_vis_methods))
  
  method_errors <- summarized_methods %>%
    transpose() %>%
    pluck("method_error") %>%
    bind_rows()
  
  name_errors <- summarized_methods %>%
    transpose() %>%
    pluck("name_error") %>%
    bind_rows()
  
  methods <- summarized_methods %>%
    transpose() %>%
    pluck("methods")
  
  list(
    method_errors = method_errors,
    name_errors = name_errors,
    methods = methods
  )
}

fit_rpart_chl <- function(chl_methods, site){
  
  # Make sure working with chl_a data
  if(unique(chl_methods$harmonized_parameter) != "chl_a"){
    stop(paste0("This target expects chl_a data, but the harmonized_parameter is ",
                unique(chl_methods$harmonized_parameter)))
  }
  
  chl_vars <- chl_methods %>%
    # Looks like lat/long can be add from site object. This was missing from the
    # code, so hoping I didn't miss anything here
    left_join(x = ., y = site, by = c("SiteID", "source")) %>%
    select(residuals,lat, long,characteristicName, kd, analytical_method) %>%
    filter(!is.na(residuals), !is.na(lat), !is.na(long)) %>%
    mutate(across(characteristicName:analytical_method,
                  ~ifelse(is.na(.), 'unknown', .))) %>%
    mutate(residuals_bin = cut(abs(residuals / kd),
                               breaks = quantile(abs(residuals / kd),
                                                 c(0, 0.95, 0.99, 1)))) %>%
    na.omit(.)
  
  cart_mod <- rpart(residuals_bin ~ ., data = chl_vars %>% 
                      dplyr::select(-residuals,-kd),
                    # CP is a tuning knob for tree complexity
                    cp = 0.01)
  list(
    cart_mod = cart_mod,
    cart_plot = rpart.plot(cart_mod, type = 2, clip.right.labs = F,
                           branch = 0.3)
  )
  
}


fit_rpart_doc <- function(doc_methods, site){
  
  # Make sure working with doc data
  if(unique(doc_methods$harmonized_parameter) != "doc"){
    stop(paste0("This target expects doc data, but the harmonized_parameter is ",
                unique(doc_methods$harmonized_parameter)))
  }
  
  doc_vars <- doc_methods %>%
    # Looks like lat/long can be add from site object. This was missing from the
    # code, so hoping I didn't miss anything here
    left_join(x = ., y = site, by = c("SiteID", "source")) %>%
    select(residuals,lat, long,characteristicName, kd, analytical_method) %>%
    filter(!is.na(residuals), !is.na(lat), !is.na(long)) %>%
    mutate(across(characteristicName:analytical_method,
                  ~ifelse(is.na(.), 'unknown', .))) %>%
    mutate(residuals_bin = cut(abs(residuals / kd),
                               breaks = quantile(abs(residuals / kd),
                                                 c(0, 0.95, 0.99, 1)))) %>%
    na.omit(.)
  
  cart_mod <- rpart(residuals_bin ~ ., data = doc_vars %>% 
                      dplyr::select(-residuals,-kd),
                    # CP is a tuning knob for tree complexity
                    cp = 0.01)
  list(
    cart_mod = cart_mod,
    cart_plot = rpart.plot(cart_mod, type = 2, clip.right.labs = F,
                           branch = 0.3)
  )
  
}


fit_rpart_tss <- function(tss_methods, site){
  
  # Make sure working with tss data
  if(unique(tss_methods$harmonized_parameter) != "tss"){
    stop(paste0("This target expects tss data, but the harmonized_parameter is ",
                unique(tss_methods$harmonized_parameter)))
  }
  
  tss_vars <- tss_methods %>%
    # Looks like lat/long can be add from site object. This was missing from the
    # code, so hoping I didn't miss anything here
    left_join(x = ., y = site, by = c("SiteID", "source")) %>%
    select(residuals,lat, long,characteristicName, kd, analytical_method) %>%
    filter(!is.na(residuals), !is.na(lat), !is.na(long)) %>%
    mutate(across(characteristicName:analytical_method,
                  ~ifelse(is.na(.), 'unknown', .))) %>%
    mutate(residuals_bin = cut(abs(residuals / kd),
                               breaks = quantile(abs(residuals / kd),
                                                 c(0, 0.95, 0.99, 1)))) %>%
    na.omit(.)
  
  cart_mod <- rpart(residuals_bin ~ ., data = tss_vars %>% 
                      dplyr::select(-residuals,-kd),
                    # CP is a tuning knob for tree complexity
                    cp = 0.01)
  list(
    cart_mod = cart_mod,
    cart_plot = rpart.plot(cart_mod, type = 2, clip.right.labs = F,
                           branch = 0.3)
  )
  
}


fit_rpart_secchi <- function(secchi_methods, site){
  
  # Make sure working with secchi data
  if(unique(secchi_methods$harmonized_parameter) != "secchi"){
    stop(paste0("This target expects secchi data, but the harmonized_parameter is ",
                unique(secchi_methods$harmonized_parameter)))
  }
  
  secchi_vars <- secchi_methods %>%
    # Looks like lat/long can be add from site object. This was missing from the
    # code, so hoping I didn't miss anything here
    left_join(x = ., y = site, by = c("SiteID", "source")) %>%
    select(residuals,lat, long,characteristicName, kd, analytical_method) %>%
    filter(!is.na(residuals), !is.na(lat), !is.na(long)) %>%
    mutate(across(characteristicName:analytical_method,
                  ~ifelse(is.na(.), 'unknown', .))) %>%
    mutate(residuals_bin = cut(abs(residuals / kd),
                               breaks = quantile(abs(residuals / kd),
                                                 c(0, 0.95, 0.99, 1)))) %>%
    na.omit(.)
  
  cart_mod <- rpart(residuals_bin ~ ., data = secchi_vars %>% 
                      dplyr::select(-residuals,-kd),
                    # CP is a tuning knob for tree complexity
                    cp = 0.01)
  list(
    cart_mod = cart_mod,
    cart_plot = rpart.plot(cart_mod, type = 2, clip.right.labs = F,
                           branch = 0.3)
  )
  
}


# Compare strict and loose options on methods inclusions with modeling data
compare_model_variations <- function(nap_est, simul_vis_methods){
  
  nap_234 <- nap_est %>%
    filter(chl_ratio == 234) %>%
    mutate(methods = 'all')
  
  
  strict_methods <- simul_vis_methods %>%
    # Several options provided in the original script. All three of these together
    # result in a df with 0 records, but the bottom two provide a narrowed dataset
    # of ~600k before the join
    filter(
      # harmonized_parameter == 'tss' & is.na(analytical_method) & is.na(characteristicName),
      characteristicName %in% c('Chlorophyll a',
                                'Chlorophyll a, corrected for pheophytin',
                                'Organic carbon', 
                                'Depth, Secchi disk depth'),
      analytical_method %in% c('MONOCHROMATIC; SPECTROPHOTOMETRIC',
                               '10200 H ~ Chlorophyll a-b-c Determination',
                               'Total Organic Carbon by Combustion',
                               '5310 B ~ Total Organic Carbon by Combustion-Infrared Method',
                               'UV OR HEATED PERSULFATE OXIDATION',
                               'WET OXIDATION METHOD')
    ) %>%
    select(date_unity, SiteID) %>%
    distinct(.) %>%
    inner_join(nap_234) %>%
    mutate(methods = 'strict')
  
  k_w <- 1 / 1.5 / max(nap_est$secchi)
  
  kd_mod <- function(df){
    mod <- lm((kd - k_w)  ~ 0 + tss_dead + doc + chl_a, data = df)
  }
  
  kd_resid_extract <- function(df, mod){
    df <- df %>%
      mutate(residuals = mod$residuals,
             pred = mod$fitted.values + k_w)
  }
  
  strict_v_loose <- bind_rows(strict_methods, nap_234) %>%
    group_by(methods) %>%
    nest() %>%
    mutate(mods = map(data, kd_mod),
           data = map2(data, mods, kd_resid_extract))
  
  
  
  method_resid <- strict_v_loose %>%
    dplyr::select(-mods) %>%
    unnest(data) %>%
    mutate(ratio_rmse = paste('methods =',methods,', R2 =', round(cor(pred,kd)^2,2)),
           year = year(date_unity))
  
  # Returned to pipeline
  method_resid %>%
    sample_frac(0.2) %>%
    ggplot(., aes(kd,pred, color = year(date_unity))) + 
    geom_point(shape = 1) + 
    facet_wrap(~ratio_rmse) + 
    ggthemes::theme_few() + 
    xlab('kd (1/secchi)') +
    ylab('predicted kd') +
    scale_x_log10() + 
    scale_y_log10() +
    scale_color_viridis_c() + 
    stat_smooth(method = 'lm',se = F, color = 'black') +
    geom_abline(intercept = 0, slope = 1, col = 'red') 
  
}
