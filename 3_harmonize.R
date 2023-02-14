# Source the functions that will be used to build the targets in p3_targets_list
source("3_harmonize/src/clean_wqp_data.R")
source("3_harmonize/src/create_match_table.R")
source("3_harmonize/src/flag_duplicated_records.R")
source("3_harmonize/src/flag_missing_results.R")
source("3_harmonize/src/format_columns.R")
source("3_harmonize/src/get_p_codes.R")
source("3_harmonize/src/harmonization_report_helper_functions.R")
source("3_harmonize/src/harmonize_sdd.R")
source("3_harmonize/src/harmonize_tss.R")
source("3_harmonize/src/harmonize_chla.R")
source("3_harmonize/src/harmonize_doc.R")
source("3_harmonize/src/remove_duplicates.R")
source("3_harmonize/src/find_simultaneous.R")


p3_targets_list <- list(
  
  # Pre-harmonization data prep ---------------------------------------------
  
  # All columns in p2_wqp_data_aoi are of class character. Coerce select 
  # columns back to numeric, but first retain original entries in new columns
  # ending in "_original". The default option is to format "ResultMeasureValue"
  # and "DetectionQuantitationLimitMeasure.MeasureValue" to numeric, but 
  # additional variables can be added using the `vars_to_numeric` argument in 
  # format_columns(). By default, format_columns() will retain all columns, but
  # undesired variables can also be dropped from the WQP dataset using the 
  # optional `drop_vars` argument. 
  tar_target(
    p3_wqp_data_aoi_formatted,
    format_columns(p2_wqp_data_aoi),
    format = "feather"
  ),
  
  # Creates a match table with column names from WQP and shorter names to use
  # in renaming them
  tar_target(wqp_col_match,
             create_match_table()),
  
  # Get parameter codes for use in cleaning processes
  tar_target(
    name = p_codes,
    command = get_p_codes(),
    packages = c("tidyverse", "rvest", "janitor")
  ),
  
  # The input data
  tar_target(wqp_data_aoi_formatted_filtered,
             p3_wqp_data_aoi_formatted %>%
               left_join(x = .,
                         y = p1_char_names_crosswalk,
                         by = c("CharacteristicName" = "char_name")) %>%
               left_join(x = .,
                         y = p2_site_counts %>%
                           select(MonitoringLocationIdentifier, CharacteristicName,
                                  lon, lat, datum),
                         by = c("MonitoringLocationIdentifier", "CharacteristicName")),
             format = "feather"),
  
  # A quick separate step to export the dataset to a file for easier review
  # Not integrating it deeper into existing targets for now
  tar_file(wqp_data_aoi_formatted_filtered_out,
           {
             out_path <- "data/out/wqp_data_aoi_formatted_filtered.feather"
             
             write_feather(x = wqp_data_aoi_formatted_filtered,
                           path = out_path)
             
             out_path
           },
           packages = c("feather")),
  
  
  # Matchup tables ----------------------------------------------------------
  
  # Secchi depth method matchup table
  tar_file_read(name = sdd_analytical_method_matchup,
                command = "data/in/sdd_analytical_method_matchup.csv",
                read = read_csv(file = !!.x),
                cue = tar_cue("always")),
  
  # Secchi sample method matchup table
  tar_file_read(name = sdd_sample_method_matchup,
                command = "data/in/sdd_sample_method_matchup.csv",
                read = read_csv(file = !!.x),
                cue = tar_cue("always")),
  
  # Secchi equipment matchup table
  tar_file_read(name = sdd_equipment_matchup,
                command = "data/in/sdd_collection_equipment_matchup.csv",
                read = read_csv(file = !!.x),
                cue = tar_cue("always")),
  
  # Chla depth method matchup table
  tar_file_read(name = chla_analytical_method_matchup,
                command = "data/in/chla_analytical_method_matchup.csv",
                read = read_csv(file = !!.x),
                cue = tar_cue("always")),
  
  
  # Harmonization process ---------------------------------------------------
  
  tar_target(harmonized_tss,
             harmonize_tss(raw_tss = wqp_data_aoi_formatted_filtered %>%
                             filter(parameter == "tss"),
                           p_codes = p_codes,
                           match_table = wqp_col_match),
             packages = c("tidyverse", "lubridate", "pander", "feather")),
  
  tar_target(harmonized_chla,
             harmonize_chla(raw_chla = wqp_data_aoi_formatted_filtered %>%
                              filter(parameter == "chlorophyll"),
                            p_codes = p_codes,
                            match_table = wqp_col_match,
                            chla_analytical_method_matchup = chla_analytical_method_matchup),
             packages = c("tidyverse", "lubridate", "feather")),
  
  tar_target(harmonized_sdd,
             harmonize_sdd(raw_sdd = wqp_data_aoi_formatted_filtered %>%
                             filter(parameter == "secchi"),
                           p_codes = p_codes,
                           # Column renaming
                           match_table = wqp_col_match,
                           sdd_analytical_method_matchup = sdd_analytical_method_matchup,
                           sdd_sample_method_matchup = sdd_sample_method_matchup,
                           sdd_equipment_matchup = sdd_equipment_matchup),
             packages = c("tidyverse", "lubridate", "feather")),
  
  tar_target(harmonized_doc,
             harmonize_doc(raw_doc = wqp_data_aoi_formatted_filtered %>%
                             filter(parameter == "doc"),
                           p_codes = p_codes,
                           match_table = wqp_col_match),
             packages = c("tidyverse", "lubridate", "feather")),
  

# Find simultaneous records -----------------------------------------------

tar_target(simultaneous_data,
           find_simultaneous(site_info = p2_site_counts,
                             chla_path = harmonized_chla,
                             doc_path = harmonized_doc,
                             sdd_path = harmonized_sdd,
                             tss_path = harmonized_tss),
           packages = c("tidyverse", "lubridate", "feather"))  
  
)
