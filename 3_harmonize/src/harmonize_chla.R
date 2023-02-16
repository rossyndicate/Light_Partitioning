
harmonize_chla <- function(raw_chla, p_codes, chla_analytical_method_matchup){
  
  # Minor data prep ---------------------------------------------------------
  
  # First step is to read in the data and do basic formatting and filtering
  raw_chla <- raw_chla %>%
    # Link up USGS p-codes. and their common names can be useful for method lumping:
    left_join(x = ., y = p_codes, by = "parm_cd") %>%
    filter(
      media %in% c("Water", "water")) %>%
    # Add an index to control for cases where there's not enough identifying info
    # to track a unique record
    rowid_to_column(., "index")
  
  
  # Remove fails ------------------------------------------------------------
  
  chla_fails_removed <- raw_chla %>%
    filter(
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
            "PAST HOLDING TIME", "not done", "no reading", "not reported",
            "no data"),
          collapse = "|"),
        x = value,
        ignore.case = T
      ) | is.na(value))
  
  # How many records removed due to fails, missing data, etc.?
  print(
    paste0(
      "Rows removed due to fails, missing data, etc.: ",
      nrow(raw_chla) - nrow(chla_fails_removed)
    )
  )
  
  
  # Clean up MDLs -----------------------------------------------------------
  
  # Needs updating
  
  # Now label rows that may have data in them still (i.e., some numeric and
  # some character data)
  chla_values_flagged <- chla_fails_removed %>%
    mutate(value_text_flag = if_else(
      condition = grepl(x = value,
                        # Adapted from https://stackoverflow.com/a/31761609
                        pattern = "^(?=.*?\\d)(?=.*?[a-zA-Z'\"<>])[a-zA-Z\\d \\.'\"<>]+$",
                        perl = TRUE),
      true = "May contain data",
      false = NA_character_
    ),
    value_numeric = as.numeric(value),
    # Adapting from USGS pipeline:
    # Remove the symbol and leave the value when result starts with a < or >
    value_numeric = if_else(
      condition = grepl("^[<|>]", value),
      true = as.numeric(gsub("[[:space:]]", "", gsub("<|>", "", value))),
      false = value_numeric
    )
    )
  
  
  # Clean up approximated values --------------------------------------------
  
  # Needs updating
  
  
  # Clean up "greater than" values ------------------------------------------
  
  # Needs updating
  
  
  # Harmonize value units ---------------------------------------------------
  
  # Now count the units column: 
  unit_counts <- raw_chla %>%
    count(units) %>%
    arrange(desc(n))
  
  unit_conversion_table <- tibble(
    units = c("mg/l", "ppm", "ug/l", "mg/m3", "ppb", "mg/cm3", "ug/ml", "mg/ml", "ppt"),
    conversion = c(1000, 1000, 1, 1, 1, 1000000, 1000, 1000000, 1000000)
  )
  
  converted_units_chla <- chla_values_flagged %>%
    inner_join(x = .,
               y = unit_conversion_table,
               by = "units") %>%
    mutate(harmonized_parameter = "chl.a",
           harmonized_value = value_numeric * conversion,
           harmonized_unit = "ug/L") %>%
    # Temporary limits 
    filter(harmonized_value <= 1e6,
           harmonized_value >= 0)
  
  # How many records removed due to limits on values?
  print(
    paste0(
      "Rows removed due to unrealistic values: ",
      nrow(chla_values_flagged) - nrow(converted_units_chla)
    )
  )
  
  
  # Clean up depths ---------------------------------------------------------
  
  # As with value col, check for entries with potential salvageable data. But don't
  # create a flag column for this one
  salvage_depths <- raw_chla %>%
    filter(grepl(x = sample_depth, pattern = "-|>|<|=")) %>%
    count(sample_depth)
  
  salvage_depth_units <- raw_chla %>%
    count(sample_depth_unit)
  
  depth_unit_conversion_table <- tibble(
    depth_units = c("in", "ft", "feet", "cm", "m", "meters"),
    depth_conversion = c(0.0254, 0.3048, 0.3048, 0.01, 1, 1)
  )
  
  converted_depth_units_chla <- converted_units_chla %>%
    inner_join(x = .,
               y = depth_unit_conversion_table,
               by = c("sample_depth_unit" = "depth_units")) %>%
    mutate(harmonized_depth_value = as.numeric(sample_depth) * depth_conversion,
           harmonized_depth_unit = "m") %>%
    # Surface limits - for the time being using two columns for this. Make sure
    # numeric value is within +/-2m OR the raw character version indicates something
    # similar:
    filter(abs(harmonized_value) <= 2 |
             sample_depth %in% c("0-2", "0-0.5"))
  
  # How many records removed due to limits on depth?
  print(
    paste0(
      "Rows removed due to non-target depths: ",
      nrow(converted_units_chla) - nrow(converted_depth_units_chla)
    )
  )
  
  
  # Aggregate analytical methods --------------------------------------------
  
  # Get an idea of how many analytical methods exist:
  print(
    paste0(
      "Number of chla analytical methods present: ",
      length(unique(raw_chla$analytical_method))
    )
  )
  
  analytical_counts <- converted_depth_units_chla %>%
    count(analytical_method) %>%
    arrange(desc(n))
  
  # Add a new column aggregating the analytical methods groups, then filter out
  # unlikely ones
  grouped_analytical_methods_chla <- converted_depth_units_chla %>%
    left_join(x = .,
              y = chla_analytical_method_matchup,
              by = c("analytical_method")) %>%
    filter(!is.na(analytical_method_grouping),
           analytical_method_grouping != "unlikely")
  
  # How many records removed due to limits on analytical method?
  print(
    paste0(
      "Rows removed due to unlikely analytical methods: ",
      nrow(converted_depth_units_chla) - nrow(grouped_analytical_methods_chla)
    )
  )
  
  
  # Filter fractions --------------------------------------------------------
  
  # Now count the fraction column
  fraction_counts <- raw_chla %>%
    count(fraction) %>%
    arrange(desc(n))
  
  # Create a column to lump things that do/don't make sense for the fraction column
  grouped_fractions_chla <- grouped_analytical_methods_chla %>%
    mutate(aquasat_fraction = if_else(
      condition = fraction %in% c("Non-Filterable (Particle)", "Suspended",
                                  "Non-filterable", "<Blank>", "Acid Soluble"),
      true = "Nonsensical",
      false = "Makes sense")) %>%
    filter(aquasat_fraction == "Makes sense")
  
  # How many records removed due to unlikely fraction types?
  print(
    paste0(
      "Rows removed due to unlikely fraction type: ",
      nrow(grouped_analytical_methods_chla) - nrow(grouped_fractions_chla)
    )
  )
  
  
  # Aggregate sample methods ------------------------------------------------
  
  # Get an idea of how many sample methods exist:
  print(
    paste0(
      "Number of chla sample methods present in raw dataset: ",
      length(unique(raw_chla$sample_method)),
      ". Skipping due to length."
    )
  )
  
  
  # Aggregate collection equipment ------------------------------------------
  
  # Get an idea of how many equipment types exist:
  print(
    paste0(
      "Number of chla equipment types present in raw dataset: ",
      length(unique(raw_chla$collection_equipment)),
      ". Skipping due to length."
    )
  )
  
  
  # Export ------------------------------------------------------------------
  
  # Export in memory-friendly way
  data_out_path <- "3_harmonize/out/harmonized_chla.feather"
  
  write_feather(grouped_fractions_chla,
                data_out_path)
  
  # Final dataset length:
  print(
    paste0(
      "Final number of records: ",
      nrow(grouped_fractions_chla)
    )
  )
  
  return(data_out_path)
  
}
