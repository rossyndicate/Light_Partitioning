
harmonize_chla <- function(raw_chla, p_codes, match_table, chla_analytical_method_matchup){
  
  
  # First step is to read in the data and do basic formatting and filtering
  raw_chla <- raw_chla %>%
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
  chla_no_data_samples <- raw_chla %>%
    filter(is.na(value) & is.na(units) & is.na(lab_comments) & is.na(result_comments))
  
  # Remove fails and missing data
  chla_fails_removed <- raw_chla %>%
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
      !index %in% chla_no_data_samples$index)
  
  # How many records removed due to fails, missing data, etc.?
  print(
    paste0(
      "Rows removed due to fails, missing data, etc.: ",
      nrow(raw_chla) - nrow(chla_fails_removed)
    )
  )
  
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
  
  chla_fixed_unit_values_plot <- converted_units_chla %>%
    ggplot() + 
    geom_histogram(aes(value_numeric + 0.00001),
                   color = "black", fill = "white") +
    scale_x_log10() +
    ylab("Record count") +
    xlab("log10(chla value + 0.00001)") +
    ggtitle("Chlorophyll value spread after harmonizing units") +
    theme_bw()
  
  chla_fixed_unit_vals_out_path <- "figs/chla_fixed_unit_values_plot.png"
  
  # Export as file instead of RDS to save memory
  ggsave(filename = chla_fixed_unit_vals_out_path,
         plot = chla_fixed_unit_values_plot,
         device = "png", width = 7.5, height = 4.5, units = "in")
  
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
  
  # Plot methods grouping counts
  chla_analytical_method_groups_plot <- grouped_analytical_methods_chla %>%
    count(analytical_method_grouping) %>%
    ggplot() +
    geom_bar(aes(x = analytical_method_grouping, y = n),
             fill = "white",
             color = "black",
             stat = "identity") +
    ylab("Record count") +
    xlab("Analytical method group") +
    ggtitle("Chlorophyll counts aggregated by analytical method flag") +
    theme_bw()
  
  chla_analytical_methods_out_path <- "figs/chla_analytical_method_groups_plot.png"
  
  # Export as file instead of RDS to save memory
  ggsave(filename = chla_analytical_methods_out_path,
         plot = chla_analytical_method_groups_plot,
         device = "png", width = 7.5, height = 4.5, units = "in")
  
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
  
  # How many records removed due to limits on analytical method?
  print(
    paste0(
      "Rows removed due to unlikely fraction type: ",
      nrow(grouped_analytical_methods_chla) - nrow(grouped_fractions_chla)
    )
  )
  
  # Get an idea of how many analytical methods exist:
  print(
    paste0(
      "Number of chla sample methods present in raw dataset: ",
      length(unique(raw_chla$sample_method)),
      ". Skipping due to length."
    )
  )
  
  # Get an idea of how many analytical methods exist:
  print(
    paste0(
      "Number of chla equipment types present in raw dataset: ",
      length(unique(raw_chla$collection_equipment)),
      ". Skipping due to length."
    )
  )
  
  # Export in memory-friendly way
  data_out_path <- "3_harmonize/out/harmonized_chla.feather"
  
  write_feather(grouped_fractions_chla,
                data_out_path)
  
  
  return(
    list(
      harmonized_chla_feather = data_out_path,
      chla_fixed_unit_vals_plot = chla_fixed_unit_vals_out_path,
      chla_analytical_methods_plot = chla_analytical_methods_out_path,
      unit_counts = unit_counts,
      salvage_depths = salvage_depths,
      salvage_depth_units = salvage_depth_units,
      fraction_counts = fraction_counts
    )
  )
  
}
