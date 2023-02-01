
harmonize_sdd <- function(raw_sdd, p_codes, match_table,
                          sdd_analytical_method_matchup,
                          sdd_sample_method_matchup,
                          sdd_equipment_matchup){
  
  # First step is to read in the data and do basic formatting and filtering
  raw_sdd <- raw_sdd %>%
    rename_with(~ match_table$short_name[which(match_table$wqp_name == .x)],
                .cols = match_table$wqp_name) %>%
    left_join(x = ., y = p_codes, by = "parm_cd") %>%
    # Remove trailing white space in labels (Is this still necessary?)
    mutate(year = year(date),
           units = trimws(units)) %>%
    filter(
      # year >= 1984,
      media %in% c("Water", "water"),
      type %in% c("Surface Water", "Water", "Estuary", "Ocean Water",
                  "Mixing Zone") | is.na(type)) %>%
    # Add an index to control for cases where there's not enough identifying info
    # to track a unique record
    rowid_to_column(., "index")
  
  # Identify samples that have no meaningful data
  sdd_no_data_samples <- raw_sdd %>%
    filter(is.na(value) & is.na(units) & is.na(lab_comments) & is.na(result_comments))
  
  # Remove fails and missing data
  sdd_fails_removed <- raw_sdd %>%
    filter(# Finalized data
      status %in% c('Accepted', 'Final', 'Historical', 'Validated'),
      # REMOVE failure-related field comments, slightly different list of words
      # than lab and result list (not including things that could be used
      # to describe field conditions like "warm", "ice", etc.)
      !grepl(
        pattern = paste0(
          c("fail", "suspect", "error", "beyond accept", "interference",
            "questionable", "outside of accept", "problem", "contaminat",
            "improper", "violation", "invalid", "unable", "no test", "cancelled",
            "instrument down", "no result", "time exceed", "not accept",
            "QC EXCEEDED"),
          collapse = "|"),
        x = field_comments,
        ignore.case = T
      ) |
        is.na(field_comments),
      # Remove failure-related lab (What about controls comments?):
      !grepl(
        pattern = paste0(
          c("fail", "suspect", "error", "beyond accept", "interference",
            "questionable", "outside of accept", "problem", "contaminat",
            "improper", "warm", "violation", "invalid", "unable", "no test",
            "cancelled", "instrument down", "no result", "time exceed",
            "not accept", "QC EXCEEDED", "not ice", "ice melt",
            "PAST HOLDING TIME", "beyond", "exceeded", "failed", "exceededs"),
          collapse = "|"),
        x = lab_comments,
        ignore.case = T
      ) |
        is.na(lab_comments),
      # Remove failure-related result comments
      !grepl(
        pattern = paste0(
          c("fail", "suspect", "error", "beyond accept", "interference",
            "questionable", "outside of accept", "problem", "contaminat",
            "improper", "warm", "violation", "invalid", "unable", "no test",
            "cancelled", "instrument down", "no result", "time exceed",
            "not accept", "QC EXCEEDED", "not ice", "ice melt",
            "PAST HOLDING TIME", "null", "unavailable", "exceeded", "rejected"),
          collapse = "|"),
        x = result_comments,
        ignore.case = T
      ) | is.na(result_comments),
      # No failure-related values
      !grepl(
        pattern = paste0(
          c("fail", "suspect", "error", "beyond accept", "interference",
            "questionable", "outside of accept", "problem", "contaminat",
            "improper", "warm", "violation", "invalid", "unable", "no test",
            "cancelled", "instrument down", "no result", "time exceed",
            "not accept", "QC EXCEEDED", "not ice", "ice melt",
            "PAST HOLDING TIME", "not done", "no reading", "no secchi",
            "not reported", "no data"),
          collapse = "|"),
        x = value,
        ignore.case = T
      ) | is.na(value),
      # Remove samples that have no values and no lab/result metadata
      !index %in% sdd_no_data_samples$index)
  
  # How many records removed due to fails, missing data, etc.?
  print(
    paste0(
      "Rows removed due to fails, missing data, etc.: ",
      nrow(raw_sdd) - nrow(sdd_fails_removed)
    )
  )
  
  # Now label rows that may have data in them still (i.e., some numeric and
  # some character data). For example, some have "_depth_ FT"
  sdd_values_flagged <- sdd_fails_removed %>%
    mutate(value_text_flag = if_else(
      condition = grepl(x = value,
                        # Adapted from https://stackoverflow.com/a/31761609
                        pattern = "^(?=.*?\\d)(?=.*?[a-zA-Z'\"<>])[a-zA-Z\\d \\.'\"<>]+$",
                        perl = TRUE),
      true = "May contain data",
      false = NA_character_
    ),
    # But we won't use these in the numeric value column
    value_numeric = as.numeric(value)) %>%
    filter(!is.na(value_numeric))
  
  # How many records removed due to converting to numeric values?
  print(
    paste0(
      "Rows removed while converting to numeric values: ",
      nrow(sdd_fails_removed) - nrow(sdd_values_flagged)
    )
  )
  
  
  # Now count the units column: 
  unit_counts <- raw_sdd %>%
    count(units) %>%
    arrange(desc(n))
  
  unit_conversion_table <- tibble(
    units = c("m", "ft", "cm", "in", "mm", "mi"),
    conversion = c(1, 0.3048, 0.01, 0.0254, 0.001, 1609.34)
  )
  
  converted_units_sdd <- sdd_values_flagged %>%
    inner_join(x = .,
               y = unit_conversion_table,
               by = "units") %>%
    mutate(harmonized_parameter = "chl.a",
           harmonized_value = value_numeric * conversion,
           harmonized_unit = "m") %>%
    # Limit based on theoretical maximum depth
    # https://www.nalms.org/secchidipin/monitoring-methods/the-secchi-disk/secchi-records/
    filter(abs(harmonized_value) <= 80)
  
  # How many records removed due to unit harmonization?
  print(
    paste0(
      "Rows removed while converting to numeric values: ",
      nrow(sdd_values_flagged) - nrow(converted_units_sdd)
    )
  )
  
  # Get an idea of how many analytical methods exist:
  print(
    paste0(
      "Number of secchi analytical methods present: ",
      length(unique(converted_units_sdd$analytical_method))
    )
  )
  
  analytical_counts <- converted_units_sdd %>%
    count(analytical_method) %>%
    arrange(desc(n))
  
  # Add a new column aggregating the analytical methods groups
  grouped_analytical_methods_sdd <- converted_units_sdd %>%
    left_join(x = .,
              y = sdd_analytical_method_matchup,
              by = c("analytical_method")) %>%
    filter(analytical_method_grouping != "unlikely")
  
  # How many records removed due to limits on analytical method?
  print(
    paste0(
      "Rows removed due to unlikely analytical methods: ",
      nrow(converted_units_sdd) - nrow(grouped_analytical_methods_sdd)
    )
  )
  
  # Now count the fraction column (the ungrouped version): 
  fraction_counts <- raw_sdd %>%
    count(fraction) %>%
    arrange(desc(n))
  
  # Create a column to lump things that do/don't make sense for the fraction column
  grouped_fractions_sdd <- grouped_analytical_methods_sdd %>%
    mutate(aquasat_fraction = if_else(
      condition = fraction %in% c(NA, "Total", " ", "None", "Unfiltered", "Field"),
      true = "Makes sense",
      false = "Nonsensical")) %>%
    filter(aquasat_fraction == "Makes sense")
  
  # How many records removed due to unlikely fraction types?
  print(
    paste0(
      "Rows removed due to unlikely fraction type: ",
      nrow(grouped_analytical_methods_sdd) - nrow(grouped_fractions_sdd)
    )
  )
  
  # Now count the sample_method column: 
  sample_counts <- raw_sdd %>%
    count(sample_method) %>%
    arrange(desc(n))
  
  # Add a new column describing the sample_method group:
  grouped_sample_methods_sdd <- grouped_fractions_sdd %>%
    left_join(x = .,
              y = sdd_sample_method_matchup,
              by = c("sample_method")) %>%
    filter(sample_method_grouping != "unlikely")
  
  # How many records removed due to unlikely sample methods?
  print(
    paste0(
      "Rows removed due to unlikely sample methods: ",
      nrow(grouped_fractions_sdd) - nrow(grouped_sample_methods_sdd)
    )
  )
  
  # Now count the collection_equipment column:
  equipment_counts <- raw_sdd %>%
    count(collection_equipment) %>%
    arrange(desc(n))
  
  group_equipment_sdd <- grouped_sample_methods_sdd %>%
    left_join(x = .,
              y = sdd_equipment_matchup,
              by = c("collection_equipment")) %>%
    filter(equipment_grouping != "unlikely")
  
  # How many records removed due to unlikely sample methods?
  print(
    paste0(
      "Rows removed due to unlikely sample methods: ",
      nrow(grouped_sample_methods_sdd) - nrow(group_equipment_sdd)
    )
  )
  
  
  # Export in memory-friendly way
  data_out_path <- "3_harmonize/out/harmonized_sdd.feather"
  
  write_feather(group_equipment_sdd,
                data_out_path)
  
  # Final dataset length:
  print(
    paste0(
      "Final number of records: ",
      nrow(group_equipment_sdd)
    )
  )
  
  return(data_out_path)
  
}