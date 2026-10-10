library(readr)
library(dplyr)
library(tidyr)
library(cluster)
library(ggplot2)

source_file <- "D:/下载/business-establishments-and-jobs-data-by-business-size-and-anzsic.csv"
output_folder <- "D:/下载/final_analysis/clustering_comparison_20260930"

data_folder <- file.path(output_folder, "data")
figure_folder <- file.path(output_folder, "figures")
qa_folder <- file.path(output_folder, "qa")

dir.create(data_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(qa_folder, recursive = TRUE, showWarnings = FALSE)


cat("Step 1 of 5: prepare annual counts, cohorts and growth measures.\n")

source_hash_before <- unname(tools::md5sum(source_file))
stopifnot(source_hash_before == "092490f73fa22c83c82d95d1ccd7e010")
raw_data <- read_csv(source_file, show_col_types = FALSE)
expected_names <- c(
  "Census year", "CLUE small area", "ANZSIC indusrty",
  "Business size", "Total establishments", "Total jobs"
)
stopifnot(identical(names(raw_data), expected_names))
names(raw_data) <- c("year", "area", "industry", "business_size", "establishments", "jobs")
stopifnot(nrow(problems(raw_data)) == 0)
stopifnot(nrow(raw_data) == 13353)
stopifnot(!anyNA(raw_data[c("year", "area", "industry", "business_size", "establishments")]))
stopifnot(all(raw_data$establishments > 0))
stopifnot(all(raw_data$establishments == floor(raw_data$establishments)))
stopifnot(setequal(raw_data$year, 2002:2024))
source_keys <- c("year", "area", "industry", "business_size")
stopifnot(!any(duplicated(raw_data[source_keys])))

area_names <- sort(unique(raw_data$area))
industry_names <- sort(unique(raw_data$industry))
segments <- expand_grid(area = area_names, industry = industry_names)
segments <- arrange(segments, area, industry)
segment_numbers <- seq_len(nrow(segments))
segments$segment_id <- paste0("S", sprintf("%03d", segment_numbers))
segments <- select(segments, segment_id, area, industry)
stopifnot(nrow(segments) == 247)

grouped_records <- group_by(raw_data, area, industry, year)
observed_counts <- summarise(grouped_records,
  establishments = sum(establishments), .groups = "drop")
annual_grid <- expand_grid(segment_id = segments$segment_id, year = 2002:2024)
annual_counts <- left_join(annual_grid, segments, by = "segment_id")
annual_counts <- left_join(annual_counts, observed_counts,
                          by = c("area", "industry", "year"))
annual_counts <- select(annual_counts, segment_id, area, industry, year, establishments)
annual_counts <- arrange(annual_counts, segment_id, year)
write_csv(annual_counts, file.path(data_folder, "annual_counts.csv"))

records_by_year <- group_by(raw_data, year)
city_counts <- summarise(records_by_year,
  city_establishments = sum(establishments), .groups = "drop")
write_csv(city_counts, file.path(data_folder, "city_counts.csv"))

counts_to_reshape <- select(annual_counts, segment_id, year, establishments)
wide_counts <- pivot_wider(counts_to_reshape,
  names_from = year, values_from = establishments, names_prefix = "count_")
cohorts <- left_join(segments, wide_counts, by = "segment_id")
annual_columns <- paste0("count_", 2002:2024)
cohorts$observed_annual_points <- 0
cohorts$missing_annual_points <- 0
cohorts$complete_annual_history <- FALSE
cohorts$mean_annual_count <- NA_real_
cohorts$included_117 <- FALSE
cohorts$included_153 <- FALSE
cohorts$exclusion_reason_153 <- ""

biennial_years <- seq(2002, 2024, 2)
biennial_columns <- paste0("count_", biennial_years)
endpoint_columns <- c("count_2002", "count_2018", "count_2022", "count_2024")

for (row_number in seq_len(nrow(cohorts))) {
  yearly_counts <- as.numeric(cohorts[row_number, annual_columns])
  biennial_counts <- as.numeric(cohorts[row_number, biennial_columns])
  endpoint_counts <- as.numeric(cohorts[row_number, endpoint_columns])
  complete_biennial_history <- !anyNA(biennial_counts)
  complete_endpoints <- !anyNA(endpoint_counts)
  if (complete_biennial_history && complete_endpoints) {
    positive_endpoints <- all(endpoint_counts > 0)
    sufficient_2018_count <- cohorts$count_2018[row_number] >= 20
    cohorts$included_117[row_number] <- positive_endpoints && sufficient_2018_count
  }
  observed_points <- sum(!is.na(yearly_counts))
  missing_points <- sum(is.na(yearly_counts))
  cohorts$observed_annual_points[row_number] <- observed_points
  cohorts$missing_annual_points[row_number] <- missing_points
  complete_history <- missing_points == 0
  cohorts$complete_annual_history[row_number] <- complete_history

  if (complete_history) {
    mean_count <- mean(yearly_counts)
    cohorts$mean_annual_count[row_number] <- mean_count
    cohorts$included_153[row_number] <- mean_count >= 10
    if (mean_count < 10) {
      cohorts$exclusion_reason_153[row_number] <- "Mean annual count below 10"
    }
  } else if (observed_points == 0) {
    cohorts$exclusion_reason_153[row_number] <- "No records in source"
  } else {
    cohorts$exclusion_reason_153[row_number] <- "Incomplete annual history"
  }
}

cohorts$added_to_117 <- cohorts$included_153 & !cohorts$included_117
baseline_years <- c(2002, 2008, 2012, 2018, 2022, 2024)
for (baseline_year in baseline_years) {
  count_column <- paste0("count_", baseline_year)
  flag_column <- paste0("small_baseline_", baseline_year)
  cohorts[[flag_column]] <- cohorts[[count_column]] < 20
}

stopifnot(sum(cohorts$included_117) == 117)
stopifnot(sum(cohorts$included_153) == 153)
stopifnot(sum(cohorts$added_to_117) == 36)
stopifnot(all(cohorts$included_153[cohorts$included_117]))
stopifnot(sum(cohorts$complete_annual_history) == 209)
stopifnot(sum(cohorts$count_2024[cohorts$included_117]) == 13602)
stopifnot(sum(cohorts$count_2024[cohorts$included_153]) == 14038)
write_csv(cohorts, file.path(data_folder, "cohorts.csv"))
write_csv(filter(cohorts, added_to_117), file.path(data_folder, "added_36.csv"))

features_all <- filter(cohorts, included_153)
positive_columns <- paste0("count_", baseline_years)
stopifnot(all(as.matrix(features_all[positive_columns]) > 0))
features_all$log_2002_2018 <- 100 * log(features_all$count_2018 / features_all$count_2002) / 16
features_all$log_2002_2008 <- 100 * log(features_all$count_2008 / features_all$count_2002) / 6
features_all$log_2008_2012 <- 100 * log(features_all$count_2012 / features_all$count_2008) / 4
features_all$log_2012_2018 <- 100 * log(features_all$count_2018 / features_all$count_2012) / 6
features_all$log_2018_2022 <- 100 * log(features_all$count_2022 / features_all$count_2018) / 4
features_all$log_2022_2024 <- 100 * log(features_all$count_2024 / features_all$count_2022) / 2

write_csv(features_all, file.path(data_folder, "features_all.csv"))

cohort_coverage <- data.frame()
for (cohort_number in c(117, 153)) {
  included_column <- paste0("included_", cohort_number)
  selected_rows <- cohorts[cohorts[[included_column]], ]
  for (current_year in c(2002, 2008, 2012, 2018, 2022, 2024)) {
    count_column <- paste0("count_", current_year)
    included_establishments <- sum(selected_rows[[count_column]])
    city_establishments <- city_counts$city_establishments[city_counts$year == current_year]
    one_row <- data.frame(
      cohort = cohort_number,
      year = current_year,
      segments = nrow(selected_rows),
      included_establishments = included_establishments,
      city_establishments = city_establishments,
      coverage_pct = 100 * included_establishments / city_establishments
    )
    cohort_coverage <- bind_rows(cohort_coverage, one_row)
  }
}
write_csv(cohort_coverage, file.path(data_folder, "cohort_coverage.csv"))

peer_areas <- c(
  "Carlton", "Docklands", "East Melbourne", "Kensington", "Melbourne (CBD)",
  "Melbourne (Remainder)", "North Melbourne", "Parkville", "Port Melbourne",
  "South Yarra", "Southbank", "West Melbourne (Industrial)", "West Melbourne (Residential)"
)

peer_label_rows <- list(
  "Wholesale Trade" = c(1, 4, 0, 3, 3, 0, 2, 0, 2, 0, 3, 1, 2),
  "Transport, Postal and Warehousing" = c(0, 3, 0, 1, 1, 0, 2, 0, 2, 0, 3, 3, 1),
  "Retail Trade" = c(3, 4, 3, 4, 3, 3, 3, 2, 3, 0, 4, 0, 2),
  "Rental, Hiring and Real Estate Services" = c(2, 4, 0, 0, 3, 0, 3, 0, 4, 0, 2, 0, 0),
  "Public Administration and Safety" = c(2, 4, 3, 0, 3, 0, 3, 0, 3, 0, 0, 0, 0),
  "Professional, Scientific and Technical Services" = c(2, 4, 2, 4, 3, 2, 3, 4, 4, 0, 3, 0, 2),
  "Other Services" = c(3, 4, 3, 4, 3, 1, 3, 1, 4, 0, 3, 0, 2),
  "Mining" = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
  "Manufacturing" = c(3, 4, 0, 2, 3, 0, 3, 0, 3, 0, 3, 2, 3),
  "Information Media and Telecommunications" = c(2, 4, 0, 0, 2, 0, 1, 0, 3, 0, 3, 0, 1),
  "Health Care and Social Assistance" = c(2, 4, 3, 3, 3, 3, 4, 3, 0, 0, 3, 0, 3),
  "Financial and Insurance Services" = c(2, 4, 1, 0, 3, 1, 1, 0, 0, 0, 3, 0, 2),
  "Electricity, Gas, Water and Waste Services" = c(3, 3, 0, 0, 3, 0, 3, 0, 0, 0, 3, 3, 0),
  "Education and Training" = c(3, 4, 1, 0, 3, 0, 3, 4, 0, 3, 3, 0, 4),
  "Construction" = c(2, 4, 0, 4, 3, 0, 2, 0, 4, 0, 4, 0, 3),
  "Arts and Recreation Services" = c(3, 4, 3, 3, 3, 3, 3, 3, 3, 0, 3, 3, 3),
  "Agriculture, Forestry and Fishing" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  "Administrative and Support Services" = c(1, 4, 2, 0, 1, 1, 1, 0, 4, 0, 1, 0, 1),
  "Accommodation and Food Services" = c(3, 4, 3, 4, 3, 3, 3, 3, 4, 3, 3, 0, 3)
)

peer_number_rows <- list(
  "Wholesale Trade" = c(22, 14, NA, 27, 115, NA, 62, NA, 60, NA, 18, 146, 35),
  "Transport, Postal and Warehousing" = c(NA, 36, NA, 12, 101, NA, 32, NA, 36, NA, 13, 64, 18),
  "Retail Trade" = c(144, 177, 16, 29, 1439, 18, 97, 20, 22, NA, 178, NA, 44),
  "Rental, Hiring and Real Estate Services" = c(20, 43, NA, NA, 202, NA, 22, NA, 12, NA, 29, NA, NA),
  "Public Administration and Safety" = c(17, 15, 20, NA, 171, NA, 10, NA, 10, NA, NA, NA, NA),
  "Professional, Scientific and Technical Services" = c(139, 104, 72, 27, 1651, 63, 139, 16, 61, NA, 113, NA, 93),
  "Other Services" = c(140, 57, 49, 34, 790, 28, 135, 23, 12, NA, 47, NA, 57),
  "Mining" = c(NA, NA, NA, NA, 37, NA, NA, NA, NA, NA, NA, NA, NA),
  "Manufacturing" = c(27, 13, NA, 36, 251, NA, 57, NA, 72, NA, 15, 11, 21),
  "Information Media and Telecommunications" = c(20, 21, NA, NA, 160, NA, 26, NA, 17, NA, 26, NA, 13),
  "Health Care and Social Assistance" = c(101, 22, 207, 19, 375, 62, 83, 75, NA, NA, 23, NA, 19),
  "Financial and Insurance Services" = c(34, 31, 16, NA, 675, 33, 20, NA, NA, NA, 27, NA, 14),
  "Electricity, Gas, Water and Waste Services" = c(14, 10, NA, NA, 61, NA, 23, NA, NA, NA, 23, 10, NA),
  "Education and Training" = c(84, 13, 13, NA, 278, NA, 29, 35, NA, 13, 14, NA, 17),
  "Construction" = c(14, 17, NA, 17, 95, NA, 34, NA, 45, NA, 23, NA, 12),
  "Arts and Recreation Services" = c(41, 38, 41, 80, 145, 45, 47, 33, 10, NA, 46, 10, 20),
  "Agriculture, Forestry and Fishing" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  "Administrative and Support Services" = c(26, 25, 14, NA, 438, 18, 35, NA, 17, NA, 25, NA, 15),
  "Accommodation and Food Services" = c(261, 185, 65, 36, 1503, 41, 117, 67, 15, 14, 172, NA, 57)
)

peer_labels <- data.frame()
for (industry_name in names(peer_label_rows)) {
  one_industry <- data.frame(
    area = peer_areas,
    industry = industry_name,
    peer_cluster = peer_label_rows[[industry_name]],
    peer_displayed_mean = peer_number_rows[[industry_name]]
  )
  peer_labels <- bind_rows(peer_labels, one_industry)
}
peer_labels <- left_join(segments, peer_labels, by = c("area", "industry"))
peer_labels <- left_join(peer_labels,
                         select(cohorts, segment_id, mean_annual_count, included_153),
                         by = "segment_id")
stopifnot(nrow(peer_labels) == 247)
stopifnot(!anyNA(peer_labels$peer_cluster))
stopifnot(sum(peer_labels$peer_cluster > 0) == 153)
stopifnot(identical(peer_labels$peer_cluster > 0, peer_labels$included_153))
printed_rows <- peer_labels$peer_cluster > 0
stopifnot(all(peer_labels$peer_displayed_mean[printed_rows] ==
                round(peer_labels$mean_annual_count[printed_rows])))
peer_labels$source_document <- "D:/下载/discuss (5).docx"
peer_labels$source_figure <- "image8: Which cluster each industry-in-area belongs to"
peer_labels$transcription_method <- "Manual cell-colour transcription; numbers checked against rounded 23-year means"
write_csv(peer_labels, file.path(data_folder, "peer_labels_transcribed.csv"))

peer_annual <- left_join(annual_counts,
                         select(peer_labels, segment_id, peer_cluster), by = "segment_id")
peer_recorded_total <- sum(peer_annual$establishments[peer_annual$peer_cluster > 0], na.rm = TRUE)
city_recorded_total <- sum(raw_data$establishments)
peer_share_denominators <- data.frame()
for (group_number in 0:4) {
  group_rows <- filter(peer_labels, peer_cluster == group_number)
  group_history <- filter(peer_annual, peer_cluster == group_number)
  group_recorded_total <- sum(group_history$establishments, na.rm = TRUE)
  share_of_peer_segments <- NA_real_
  share_of_peer_establishments <- NA_real_
  if (group_number > 0) {
    share_of_peer_segments <- 100 * nrow(group_rows) / 153
    share_of_peer_establishments <- 100 * group_recorded_total / peer_recorded_total
  }
  one_row <- data.frame(
    peer_cluster = group_number,
    segments = nrow(group_rows),
    denominator_all_segments = 247,
    denominator_peer_segments = 153,
    share_of_all247_segments_pct = 100 * nrow(group_rows) / 247,
    share_of_peer153_segments_pct = share_of_peer_segments,
    recorded_establishments_sum_2002_2024 = group_recorded_total,
    city_recorded_establishments_sum_2002_2024 = city_recorded_total,
    peer153_recorded_establishments_sum_2002_2024 = peer_recorded_total,
    share_of_city_recorded_establishments_pct = 100 * group_recorded_total / city_recorded_total,
    share_of_peer153_recorded_establishments_pct = share_of_peer_establishments
  )
  peer_share_denominators <- bind_rows(peer_share_denominators, one_row)
}
write_csv(peer_share_denominators, file.path(data_folder, "peer_share_denominators.csv"))

source_hash_after <- unname(tools::md5sum(source_file))
stopifnot(source_hash_before == source_hash_after)
cat("Preparation complete: 247 combinations, old cohort 117, peer cohort 153, added 36.\n")
cat("All 153 coloured peer cells match the mean-count cohort and their printed numbers.\n")
print(count(peer_labels, peer_cluster))
print(cohort_coverage)


cat("Step 2 of 5: fit six models and check subsample stability.\n")

features <- read_csv(file.path(data_folder, "features_all.csv"), show_col_types = FALSE)
cohorts <- read_csv(file.path(data_folder, "cohorts.csv"), show_col_types = FALSE)

settings <- data.frame(
  experiment = paste0("E", 1:6),
  method = c("Ward", "Ward", "Ward", "Kmeans", "Kmeans", "Kmeans"),
  periods = c(5, 3, 5, 5, 3, 5),
  sample_n = c(117, 153, 153, 117, 153, 153),
  k = 4,
  model_seed = 30005,
  kmeans_nstart = 100,
  kmeans_iter_max = 100
)
write_csv(settings, file.path(data_folder, "experiment_settings.csv"))

three_features <- c("log_2002_2018", "log_2018_2022", "log_2022_2024")
five_features <- c("log_2002_2008", "log_2008_2012", "log_2012_2018", "log_2018_2022", "log_2022_2024")

sampling_records <- data.frame()

for (sample_size in c(117, 153)) {
  if (sample_size == 117) {
    sample_ids <- cohorts$segment_id[cohorts$included_117]
  } else {
    sample_ids <- cohorts$segment_id[cohorts$included_153]
  }

  sample_ids <- sort(sample_ids)
  retained_count <- floor(0.8 * sample_size)

  for (repetition in 1:200) {
    draw_seed <- 30005 + sample_size * 1000 + repetition
    set.seed(draw_seed)
    retained_ids <- sample(sample_ids, size = retained_count, replace = FALSE)
    retained_ids <- sort(retained_ids)

    new_sampling_rows <- data.frame(
      sample_n = sample_size,
      repetition = repetition,
      draw_seed = draw_seed,
      segment_id = retained_ids
    )
    sampling_records <- rbind(sampling_records, new_sampling_rows)
  }
}

write_csv(sampling_records, file.path(data_folder, "sampling_records.csv"))

fit_groups <- function(standardized_values, method, number_of_groups, random_seed) {
  warning_text <- ""
  error_text <- ""
  previous_warning_option <- getOption("warn")
  on.exit(options(warn = previous_warning_option))
  options(warn = 2)
  set.seed(random_seed)

  fitted <- try({
    if (method == "Ward") {
      distances <- dist(standardized_values, method = "euclidean")
      tree <- hclust(distances, method = "ward.D2")
      group_labels <- cutree(tree, k = number_of_groups)
      model_result <- list(cluster = group_labels, ifault = 0, iter = NA_integer_)
    } else {
      model_result <- kmeans(
        standardized_values,
        centers = number_of_groups,
        nstart = 100,
        iter.max = 100,
        algorithm = "Hartigan-Wong"
      )
    }
    model_result
  }, silent = TRUE)

  options(warn = previous_warning_option)
  fit_failed <- inherits(fitted, "try-error")
  valid <- FALSE
  ifault <- NA_integer_
  iterations <- NA_integer_
  labels <- rep(NA_integer_, nrow(standardized_values))

  if (fit_failed) {
    failure_text <- as.character(fitted)
    failure_was_warning <- grepl("(converted from warning)", failure_text, fixed = TRUE)
    if (failure_was_warning) {
      warning_text <- failure_text
    } else {
      error_text <- failure_text
    }
  } else {
    labels <- as.integer(fitted$cluster)
    ifault <- fitted$ifault
    iterations <- fitted$iter
    actual_group_count <- length(unique(labels))
    valid <- ifault == 0 && actual_group_count == number_of_groups
  }

  result <- list(
    labels = labels,
    valid = valid,
    ifault = ifault,
    iterations = iterations,
    warning = warning_text,
    error = error_text
  )
  return(result)
}

feature_values <- data.frame()
scaling_values <- data.frame()
candidate_memberships <- data.frame()
diagnostics <- data.frame()
fit_log <- data.frame()
stability_details <- data.frame()
subsample_memberships <- data.frame()
subsample_scaling <- data.frame()

for (experiment_number in 1:6) {
  current_setting <- settings[experiment_number, ]
  experiment_id <- current_setting$experiment

  if (current_setting$sample_n == 117) {
    selected_ids <- cohorts$segment_id[cohorts$included_117]
  } else {
    selected_ids <- cohorts$segment_id[cohorts$included_153]
  }

  selected_ids <- sort(selected_ids)
  selected_rows <- match(selected_ids, features$segment_id)
  selected_data <- features[selected_rows, ]
  selected_features <- five_features

  if (current_setting$periods == 3) {
    selected_features <- three_features
  }

  raw_matrix <- as.matrix(selected_data[selected_features])
  standardized_matrix <- raw_matrix

  for (feature_number in seq_along(selected_features)) {
    feature_name <- selected_features[feature_number]
    raw_values <- raw_matrix[, feature_number]
    feature_mean <- mean(raw_values)
    feature_sd <- sd(raw_values)
    stopifnot(is.finite(feature_sd), feature_sd > 0)
    standardized_values <- (raw_values - feature_mean) / feature_sd
    standardized_matrix[, feature_number] <- standardized_values

    new_scaling_row <- data.frame(
      experiment = experiment_id,
      feature = feature_name,
      mean = feature_mean,
      sd = feature_sd
    )
    scaling_values <- rbind(scaling_values, new_scaling_row)

    new_feature_rows <- data.frame(
      experiment = experiment_id,
      segment_id = selected_ids,
      feature = feature_name,
      raw_value = raw_values,
      standardized_value = standardized_values
    )
    feature_values <- rbind(feature_values, new_feature_rows)
  }

  distances <- dist(standardized_matrix)
  sample_count <- length(selected_ids)
  required_minimum_size <- max(5, ceiling(sample_count * 0.05))

  for (number_of_groups in 2:6) {
    fitted <- fit_groups(standardized_matrix, current_setting$method, number_of_groups, 30005)

    new_fit_row <- data.frame(
      experiment = experiment_id,
      stage = "full",
      k = number_of_groups,
      repetition = 0,
      seed = 30005,
      valid = fitted$valid,
      ifault = fitted$ifault,
      iterations = fitted$iterations,
      warning = fitted$warning,
      error = fitted$error
    )
    fit_log <- rbind(fit_log, new_fit_row)

    if (!fitted$valid) {
      write_csv(fit_log, file.path(data_folder, "fit_log.csv"))
      failure_message <- paste("Full-sample fit failed:", experiment_id, number_of_groups)
      stop(failure_message)
    }

    silhouette_result <- silhouette(fitted$labels, distances)
    silhouette_widths <- silhouette_result[, "sil_width"]
    mean_silhouette <- mean(silhouette_widths)
    group_sizes <- tabulate(fitted$labels, nbins = number_of_groups)
    minimum_group_size <- min(group_sizes)
    group_size_text <- paste(group_sizes, collapse = "/")

    new_diagnostic_row <- data.frame(
      experiment = experiment_id,
      k = number_of_groups,
      silhouette = mean_silhouette,
      min_group_size = minimum_group_size,
      required_min_size = required_minimum_size,
      group_sizes = group_size_text
    )
    diagnostics <- rbind(diagnostics, new_diagnostic_row)

    new_membership_rows <- data.frame(
      experiment = experiment_id,
      k = number_of_groups,
      segment_id = selected_ids,
      cluster = fitted$labels,
      silhouette_width = silhouette_widths
    )
    candidate_memberships <- rbind(candidate_memberships, new_membership_rows)

    if (number_of_groups == 4) {
      original_labels <- fitted$labels
    }
  }

  for (repetition in 1:200) {
    matches_sample_size <- sampling_records$sample_n == current_setting$sample_n
    matches_repetition <- sampling_records$repetition == repetition
    selected_draw <- matches_sample_size & matches_repetition
    retained_ids <- sampling_records$segment_id[selected_draw]
    retained_rows <- match(retained_ids, selected_ids)
    retained_raw <- raw_matrix[retained_rows, , drop = FALSE]
    retained_standardized <- retained_raw

    for (feature_number in seq_along(selected_features)) {
      retained_values <- retained_raw[, feature_number]
      retained_mean <- mean(retained_values)
      retained_sd <- sd(retained_values)
      retained_standardized[, feature_number] <- (retained_values - retained_mean) / retained_sd

      new_subsample_scaling_row <- data.frame(
        experiment = experiment_id,
        repetition = repetition,
        feature = selected_features[feature_number],
        mean = retained_mean,
        sd = retained_sd
      )
      subsample_scaling <- rbind(subsample_scaling, new_subsample_scaling_row)
    }

    fit_seed <- 30005 + repetition
    fitted <- fit_groups(retained_standardized, current_setting$method, 4, fit_seed)

    new_fit_row <- data.frame(
      experiment = experiment_id,
      stage = "subsample",
      k = 4,
      repetition = repetition,
      seed = fit_seed,
      valid = fitted$valid,
      ifault = fitted$ifault,
      iterations = fitted$iterations,
      warning = fitted$warning,
      error = fitted$error
    )
    fit_log <- rbind(fit_log, new_fit_row)

    new_subsample_memberships <- data.frame(
      experiment = experiment_id,
      repetition = repetition,
      segment_id = retained_ids,
      cluster = fitted$labels,
      valid_fit = fitted$valid
    )
    subsample_memberships <- rbind(subsample_memberships, new_subsample_memberships)

    for (original_group in 1:4) {
      original_members <- selected_ids[original_labels == original_group]
      common_members <- intersect(original_members, retained_ids)
      retained_group_count <- length(common_members)
      valid_comparison <- fitted$valid && retained_group_count > 0
      best_jaccard <- NA_real_
      best_group <- NA_integer_

      if (valid_comparison) {
        best_jaccard <- -1

        for (new_group in 1:4) {
          new_members <- retained_ids[fitted$labels == new_group]
          intersection_count <- length(intersect(common_members, new_members))
          union_count <- length(union(common_members, new_members))
          current_jaccard <- intersection_count / union_count

          if (current_jaccard > best_jaccard) {
            best_jaccard <- current_jaccard
            best_group <- new_group
          }
        }
      }

      new_stability_row <- data.frame(
        experiment = experiment_id,
        repetition = repetition,
        cluster = original_group,
        retained_group_n = retained_group_count,
        best_subsample_cluster = best_group,
        jaccard = best_jaccard,
        valid = valid_comparison
      )
      stability_details <- rbind(stability_details, new_stability_row)
    }
  }

  cat(experiment_id, "completed: 5 diagnostics and 200 subsamples.\n")
}

main_memberships <- candidate_memberships[candidate_memberships$k == 4, ]
stability_summary <- data.frame()

for (experiment_id in settings$experiment) {
  for (group_number in 1:4) {
    matches_experiment <- stability_details$experiment == experiment_id
    matches_group <- stability_details$cluster == group_number
    selected_rows <- matches_experiment & matches_group
    group_results <- stability_details[selected_rows, ]
    valid_values <- group_results$jaccard[group_results$valid]
    valid_repetitions <- length(valid_values)
    invalid_repetitions <- 200 - valid_repetitions
    mean_jaccard <- mean(valid_values)
    median_jaccard <- median(valid_values)
    minimum_jaccard <- min(valid_values)
    passes_stability <- valid_repetitions == 200 && mean_jaccard >= 0.75

    new_stability_summary_row <- data.frame(
      experiment = experiment_id,
      cluster = group_number,
      valid_repetitions = valid_repetitions,
      invalid_repetitions = invalid_repetitions,
      mean_jaccard = mean_jaccard,
      median_jaccard = median_jaccard,
      min_jaccard = minimum_jaccard,
      passes = passes_stability
    )
    stability_summary <- rbind(stability_summary, new_stability_summary_row)
  }
}

model_summary <- diagnostics[diagnostics$k == 4, ]
model_summary$min_mean_jaccard <- NA_real_
model_summary$all_200_valid <- FALSE

for (row_number in 1:6) {
  experiment_id <- model_summary$experiment[row_number]
  matches_experiment <- stability_summary$experiment == experiment_id
  group_results <- stability_summary[matches_experiment, ]
  model_summary$min_mean_jaccard[row_number] <- min(group_results$mean_jaccard)
  model_summary$all_200_valid[row_number] <- all(group_results$valid_repetitions == 200)
}

model_summary$passes_silhouette <- model_summary$silhouette >= 0.25
model_summary$passes_size <- model_summary$min_group_size >= model_summary$required_min_size
model_summary$passes_stability <- model_summary$min_mean_jaccard >= 0.75 & model_summary$all_200_valid
model_summary$passes_all <- model_summary$passes_silhouette & model_summary$passes_size & model_summary$passes_stability

write_csv(feature_values, file.path(data_folder, "feature_values.csv"))
write_csv(scaling_values, file.path(data_folder, "scaling_values.csv"))
write_csv(candidate_memberships, file.path(data_folder, "candidate_memberships.csv"))
write_csv(main_memberships, file.path(data_folder, "main_memberships.csv"))
write_csv(diagnostics, file.path(data_folder, "diagnostics.csv"))
write_csv(fit_log, file.path(data_folder, "fit_log.csv"))
write_csv(subsample_memberships, file.path(data_folder, "subsample_memberships.csv"))
write_csv(subsample_scaling, file.path(data_folder, "subsample_scaling.csv"))
write_csv(stability_details, file.path(data_folder, "stability_details.csv"))
write_csv(stability_summary, file.path(data_folder, "stability_summary.csv"))
write_csv(model_summary, file.path(data_folder, "model_summary.csv"))


cat("Step 3 of 5: compare memberships and summarise group paths.\n")

cohorts <- read_csv(file.path(data_folder, "cohorts.csv"), show_col_types = FALSE)
features <- read_csv(file.path(data_folder, "features_all.csv"), show_col_types = FALSE)
peer <- read_csv(file.path(data_folder, "peer_labels_transcribed.csv"), show_col_types = FALSE)
memberships <- read_csv(file.path(data_folder, "main_memberships.csv"), show_col_types = FALSE)
annual <- read_csv(file.path(data_folder, "annual_counts.csv"), show_col_types = FALSE)
city <- read_csv(file.path(data_folder, "city_counts.csv"), show_col_types = FALSE)

common_ids <- sort(cohorts$segment_id[cohorts$included_117])
added_ids <- sort(cohorts$segment_id[cohorts$added_to_117])
baseline_feature_names <- c("log_2002_2018", "log_2018_2022", "log_2022_2024")
baseline_feature_rows <- match(common_ids, features$segment_id)
baseline_features <- features[baseline_feature_rows, baseline_feature_names]
baseline_raw_values <- as.matrix(baseline_features)
baseline_standardized_values <- scale(baseline_raw_values)
baseline_distances <- dist(baseline_standardized_values)
baseline_tree <- hclust(baseline_distances, method = "ward.D2")
baseline_groups <- cutree(baseline_tree, k = 4)

baseline <- data.frame(
  segment_id = common_ids,
  cluster = baseline_groups
)
write_csv(baseline, file.path(data_folder, "baseline_ward_four_groups.csv"))

adjusted_rand <- function(first_labels, second_labels) {
  counts <- table(first_labels, second_labels)
  pairs_in_each_cell <- choose(counts, 2)
  paired_together <- sum(pairs_in_each_cell)
  first_group_sizes <- rowSums(counts)
  second_group_sizes <- colSums(counts)
  pairs_in_first_groups <- choose(first_group_sizes, 2)
  pairs_in_second_groups <- choose(second_group_sizes, 2)
  first_pairs <- sum(pairs_in_first_groups)
  second_pairs <- sum(pairs_in_second_groups)
  observation_count <- length(first_labels)
  all_pairs <- choose(observation_count, 2)
  expected_pairs <- first_pairs * second_pairs / all_pairs
  upper_bound <- (first_pairs + second_pairs) / 2
  denominator <- upper_bound - expected_pairs

  if (denominator == 0) {
    return(1)
  }

  result <- (paired_together - expected_pairs) / denominator
  return(result)
}

baseline_rows <- match(common_ids, baseline$segment_id)
baseline_labels <- baseline$cluster[baseline_rows]
peer_common_rows <- match(common_ids, peer$segment_id)
peer_common_labels <- peer$peer_cluster[peer_common_rows]
baseline_peer_ari <- adjusted_rand(baseline_labels, peer_common_labels)

baseline_peer <- data.frame(
  reference = "Peer",
  experiment = "Baseline",
  scope = "common117",
  n = 117,
  ari = baseline_peer_ari
)
write_csv(baseline_peer, file.path(data_folder, "baseline_peer.csv"))

permutations <- data.frame()

for (first in 1:4) {
  for (second in 1:4) {
    for (third in 1:4) {
      for (fourth in 1:4) {
        proposed_mapping <- c(first, second, third, fourth)
        number_of_distinct_labels <- length(unique(proposed_mapping))

        if (number_of_distinct_labels == 4) {
          permutations <- rbind(permutations, proposed_mapping)
        }
      }
    }
  }
}

label_map <- data.frame()
memberships_aligned <- data.frame()

for (experiment_id in paste0("E", 1:6)) {
  matches_experiment <- memberships$experiment == experiment_id
  selected_members <- memberships[matches_experiment, ]
  common_member_rows <- match(common_ids, selected_members$segment_id)
  original_labels <- selected_members$cluster[common_member_rows]
  peer_rows <- match(common_ids, peer$segment_id)
  peer_labels <- peer$peer_cluster[peer_rows]
  best_overlap <- -1
  best_mapping <- 1:4

  for (mapping_number in seq_len(nrow(permutations))) {
    proposed_mapping <- as.integer(permutations[mapping_number, ])
    relabelled <- proposed_mapping[original_labels]
    overlap <- sum(relabelled == peer_labels)

    if (overlap > best_overlap) {
      best_overlap <- overlap
      best_mapping <- proposed_mapping
    }
  }

  new_mapping_rows <- data.frame(
    experiment = experiment_id,
    cluster = 1:4,
    aligned_cluster = best_mapping,
    alignment_scope = "common117",
    total_matched = best_overlap
  )
  label_map <- rbind(label_map, new_mapping_rows)

  selected_members$aligned_cluster <- best_mapping[selected_members$cluster]
  matched_rows <- match(selected_members$segment_id, cohorts$segment_id)
  selected_members$area <- cohorts$area[matched_rows]
  selected_members$industry <- cohorts$industry[matched_rows]
  selected_members$added_to_117 <- cohorts$added_to_117[matched_rows]
  peer_rows <- match(selected_members$segment_id, peer$segment_id)
  selected_members$peer_cluster <- peer$peer_cluster[peer_rows]
  memberships_aligned <- rbind(memberships_aligned, selected_members)
}

comparison_metrics <- data.frame()
cross_tabs <- data.frame()
comparison_jaccard <- data.frame()
reference_names <- c("Peer", "Baseline", paste0("E", 1:6))

for (experiment_id in paste0("E", 1:6)) {
  matches_experiment <- memberships_aligned$experiment == experiment_id
  selected_members <- memberships_aligned[matches_experiment, ]
  scopes <- "common117"

  if (nrow(selected_members) == 153) {
    scopes <- c(scopes, "full153", "added36")
  }

  for (scope_name in scopes) {
    scope_ids <- common_ids

    if (scope_name == "full153") {
      scope_ids <- sort(selected_members$segment_id)
    }

    if (scope_name == "added36") {
      scope_ids <- added_ids
    }

    selected_rows <- match(scope_ids, selected_members$segment_id)
    labels <- selected_members$cluster[selected_rows]
    aligned_labels <- selected_members$aligned_cluster[selected_rows]

    for (reference_name in reference_names) {
      if (reference_name == experiment_id) {
        next
      }

      if (reference_name == "Peer") {
        reference_data <- data.frame(
          segment_id = peer$segment_id,
          cluster = peer$peer_cluster
        )
      } else if (reference_name == "Baseline") {
        reference_data <- baseline
      } else {
        matches_reference <- memberships$experiment == reference_name
        reference_data <- memberships[matches_reference, c("segment_id", "cluster")]
      }

      all_members_available <- all(scope_ids %in% reference_data$segment_id)

      if (!all_members_available) {
        next
      }

      reference_rows <- match(scope_ids, reference_data$segment_id)
      reference_labels <- reference_data$cluster[reference_rows]
      ari_value <- adjusted_rand(labels, reference_labels)
      aligned_ari <- adjusted_rand(aligned_labels, reference_labels)
      ari_difference <- abs(ari_value - aligned_ari)
      stopifnot(ari_difference < 1e-12)

      new_comparison_row <- data.frame(
        scope = scope_name,
        reference = reference_name,
        experiment = experiment_id,
        n = length(scope_ids),
        ari = ari_value
      )
      comparison_metrics <- rbind(comparison_metrics, new_comparison_row)

      for (reference_group in 1:4) {
        for (group_number in 1:4) {
          matches_reference_group <- reference_labels == reference_group
          matches_model_group <- labels == group_number
          matches_both_groups <- matches_reference_group & matches_model_group
          cell_count <- sum(matches_both_groups)
          matches_experiment <- label_map$experiment == experiment_id
          matches_group <- label_map$cluster == group_number
          mapping_rows <- matches_experiment & matches_group
          mapped_group <- label_map$aligned_cluster[mapping_rows]

          new_cross_table_row <- data.frame(
            scope = scope_name,
            reference = reference_name,
            experiment = experiment_id,
            reference_cluster = reference_group,
            cluster = group_number,
            aligned_cluster = mapped_group,
            n = cell_count
          )
          cross_tabs <- rbind(cross_tabs, new_cross_table_row)
        }
      }

      for (direction in c("experiment_to_reference", "reference_to_experiment")) {
        source_labels <- labels
        target_labels <- reference_labels

        if (direction == "reference_to_experiment") {
          source_labels <- reference_labels
          target_labels <- labels
        }

        for (source_group in 1:4) {
          source_members <- scope_ids[source_labels == source_group]
          source_member_count <- length(source_members)
          best_score <- NA_real_
          best_target <- NA_integer_

          if (source_member_count > 0) {
            best_score <- -1

            for (target_group in 1:4) {
              target_members <- scope_ids[target_labels == target_group]
              overlap_count <- length(intersect(source_members, target_members))
              union_count <- length(union(source_members, target_members))
              score <- overlap_count / union_count

              if (score > best_score) {
                best_score <- score
                best_target <- target_group
              }
            }
          }

          new_jaccard_row <- data.frame(
            scope = scope_name,
            reference = reference_name,
            experiment = experiment_id,
            direction = direction,
            source_cluster = source_group,
            source_n = source_member_count,
            best_target_cluster = best_target,
            jaccard = best_score
          )
          comparison_jaccard <- rbind(comparison_jaccard, new_jaccard_row)
        }
      }
    }
  }
}

profiles <- data.frame()
cluster_summary <- data.frame()
trajectories <- data.frame()
profile_features <- c("log_2002_2008", "log_2008_2012", "log_2012_2018", "log_2018_2022", "log_2022_2024")
city_2024 <- city$city_establishments[city$year == 2024]

for (experiment_id in paste0("E", 1:6)) {
  matches_experiment <- memberships_aligned$experiment == experiment_id
  selected_members <- memberships_aligned[matches_experiment, ]
  all_feature_rows <- match(selected_members$segment_id, features$segment_id)
  sample_2024 <- sum(features$count_2024[all_feature_rows])

  for (group_number in 1:4) {
    matches_group <- selected_members$cluster == group_number
    group_members <- selected_members[matches_group, ]
    feature_rows <- match(group_members$segment_id, features$segment_id)
    group_data <- features[feature_rows, ]
    aligned_group <- unique(group_members$aligned_cluster)
    group_member_count <- nrow(group_data)
    group_2018 <- sum(group_data$count_2018)
    group_2024 <- sum(group_data$count_2024)
    share_sample_2024 <- 100 * group_2024 / sample_2024
    share_city_2024 <- 100 * group_2024 / city_2024
    absolute_change <- group_2024 - group_2018
    percent_change <- 100 * (group_2024 / group_2018 - 1)
    added_member_count <- sum(group_data$added_to_117)

    new_cluster_summary_row <- data.frame(
      experiment = experiment_id,
      cluster = group_number,
      aligned_cluster = aligned_group,
      n = group_member_count,
      count_2018 = group_2018,
      count_2024 = group_2024,
      sample_2024 = sample_2024,
      city_2024 = city_2024,
      share_sample_2024_pct = share_sample_2024,
      share_city_2024_pct = share_city_2024,
      absolute_change = absolute_change,
      change_pct = percent_change,
      added_n = added_member_count
    )
    cluster_summary <- rbind(cluster_summary, new_cluster_summary_row)

    for (feature_name in profile_features) {
      log_values <- group_data[[feature_name]]
      cagr_values <- 100 * (exp(log_values / 100) - 1)
      mean_log_growth <- mean(log_values)
      median_log_growth <- median(log_values)
      mean_cagr <- mean(cagr_values)
      median_cagr <- median(cagr_values)

      new_profile_row <- data.frame(
        experiment = experiment_id,
        cluster = group_number,
        aligned_cluster = aligned_group,
        feature = feature_name,
        sample_n = group_member_count,
        mean_log = mean_log_growth,
        median_log = median_log_growth,
        mean_cagr = mean_cagr,
        median_cagr = median_cagr
      )
      profiles <- rbind(profiles, new_profile_row)
    }

    for (year_number in 2002:2024) {
      matches_year <- annual$year == year_number
      belongs_to_group <- annual$segment_id %in% group_members$segment_id
      annual_rows <- matches_year & belongs_to_group
      year_data <- annual[annual_rows, ]
      baseline_rows <- match(year_data$segment_id, group_data$segment_id)
      baseline_counts <- group_data$count_2018[baseline_rows]
      index_values <- 100 * year_data$establishments / baseline_counts
      median_index <- median(index_values)
      lower_quartile <- as.numeric(quantile(index_values, 0.25))
      upper_quartile <- as.numeric(quantile(index_values, 0.75))
      index_count <- length(index_values)

      new_trajectory_row <- data.frame(
        experiment = experiment_id,
        cluster = group_number,
        aligned_cluster = aligned_group,
        year = year_number,
        median_index = median_index,
        q25_index = lower_quartile,
        q75_index = upper_quartile,
        n = index_count
      )
      trajectories <- rbind(trajectories, new_trajectory_row)
    }
  }
}

peer_profiles <- data.frame()

for (group_number in 1:4) {
  peer_member_ids <- peer$segment_id[peer$peer_cluster == group_number]
  peer_feature_rows <- match(peer_member_ids, features$segment_id)
  group_data <- features[peer_feature_rows, ]
  group_member_count <- nrow(group_data)

  for (feature_name in profile_features) {
    peer_log_values <- group_data[[feature_name]]
    mean_peer_growth <- mean(peer_log_values)
    median_peer_growth <- median(peer_log_values)

    new_peer_profile_row <- data.frame(
      peer_cluster = group_number,
      n = group_member_count,
      feature = feature_name,
      mean_log = mean_peer_growth,
      median_log = median_peer_growth
    )
    peer_profiles <- rbind(peer_profiles, new_peer_profile_row)
  }
}

added_memberships <- memberships_aligned[memberships_aligned$added_to_117, ]

write_csv(label_map, file.path(data_folder, "label_map.csv"))
write_csv(memberships_aligned, file.path(data_folder, "memberships_aligned.csv"))
write_csv(comparison_metrics, file.path(data_folder, "comparison_metrics.csv"))
write_csv(cross_tabs, file.path(data_folder, "cross_tabs.csv"))
write_csv(comparison_jaccard, file.path(data_folder, "comparison_jaccard.csv"))
write_csv(profiles, file.path(data_folder, "profiles.csv"))
write_csv(cluster_summary, file.path(data_folder, "cluster_summary.csv"))
write_csv(trajectories, file.path(data_folder, "trajectories.csv"))
write_csv(peer_profiles, file.path(data_folder, "peer_profiles_recalculated.csv"))
write_csv(added_memberships, file.path(data_folder, "added36_memberships.csv"))
cat("Comparison scores, aligned labels, profiles and trajectories saved.\n")


cat("Step 4 of 5: save figures as PNG and PDF.\n")

coverage <- read_csv(file.path(data_folder, "cohort_coverage.csv"), show_col_types = FALSE)
comparison_metrics <- read_csv(
  file.path(data_folder, "comparison_metrics.csv"),
  show_col_types = FALSE
)
cross_tabs <- read_csv(file.path(data_folder, "cross_tabs.csv"), show_col_types = FALSE)
profiles <- read_csv(file.path(data_folder, "profiles.csv"), show_col_types = FALSE)
trajectories <- read_csv(file.path(data_folder, "trajectories.csv"), show_col_types = FALSE)
memberships <- read_csv(
  file.path(data_folder, "memberships_aligned.csv"),
  show_col_types = FALSE
)
diagnostics <- read_csv(file.path(data_folder, "diagnostics.csv"), show_col_types = FALSE)
stability <- read_csv(
  file.path(data_folder, "stability_summary.csv"),
  show_col_types = FALSE
)
label_map <- read_csv(file.path(data_folder, "label_map.csv"), show_col_types = FALSE)

experiment_ids <- c("E1", "E2", "E3", "E4", "E5", "E6")
experiment_labels <- c(
  "E1: Ward | 5 periods | 117 combinations",
  "E2: Ward | 3 periods | 153 combinations",
  "E3: Ward | 5 periods | 153 combinations",
  "E4: K-means | 5 periods | 117 combinations",
  "E5: K-means | 3 periods | 153 combinations",
  "E6: K-means | 5 periods | 153 combinations"
)
names(experiment_labels) <- experiment_ids
experiment_panel_labels <- c(
  "E1: Ward\n5 periods | 117 combinations",
  "E2: Ward\n3 periods | 153 combinations",
  "E3: Ward\n5 periods | 153 combinations",
  "E4: K-means\n5 periods | 117 combinations",
  "E5: K-means\n3 periods | 153 combinations",
  "E6: K-means\n5 periods | 153 combinations"
)

group_colours <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")
names(group_colours) <- c("1", "2", "3", "4")
group_shapes <- c(16, 17, 15, 18)
names(group_shapes) <- c("1", "2", "3", "4")

theme_set(theme_minimal(base_size = 12, base_family = "sans"))
theme_update(
  plot.title = element_text(face = "bold", size = 17),
  plot.subtitle = element_text(size = 11, margin = margin(b = 12)),
  plot.caption = element_text(size = 10, hjust = 0, margin = margin(t = 12)),
  strip.text = element_text(face = "bold", size = 11),
  panel.grid.minor = element_blank(),
  legend.position = "bottom",
  legend.title = element_text(face = "bold"),
  plot.margin = margin(15, 20, 15, 15)
)

save_plot <- function(plot_object, file_name, width, height) {
  png_name <- paste0(file_name, ".png")
  pdf_name <- paste0(file_name, ".pdf")
  png_path <- file.path(figure_folder, png_name)
  pdf_path <- file.path(figure_folder, pdf_name)
  ggsave(
    filename = png_path,
    plot = plot_object,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
  ggsave(
    filename = pdf_path,
    plot = plot_object,
    width = width,
    height = height,
    device = "pdf",
    bg = "white"
  )
}

figure_index <- data.frame(
  figure = character(),
  meaning = character(),
  data_source = character()
)

coverage_2024 <- coverage[coverage$year == 2024, ]
cohort_values <- as.character(coverage_2024$cohort)
requested_cohorts <- cohort_values %in% c("117", "153")
coverage_2024 <- coverage_2024[requested_cohorts, ]
coverage_order <- order(coverage_2024$segments)
coverage_2024 <- coverage_2024[coverage_order, ]
coverage_row_count <- nrow(coverage_2024)
if (coverage_row_count != 2) {
  stop("The coverage figure needs one row for each of the two cohorts.")
}

coverage_plot_data <- data.frame()
for (row_number in seq_len(coverage_row_count)) {
  current_row <- coverage_2024[row_number, ]
  cohort_name <- paste(current_row$segments, "combinations")
  combination_percent <- 100 * current_row$segments / 247
  rounded_combination_percent <- round(combination_percent, 1)
  combination_label <- paste0(
    current_row$segments,
    " / 247\n",
    rounded_combination_percent,
    "%"
  )
  included_establishments_label <- format(
    current_row$included_establishments,
    big.mark = ",",
    trim = TRUE
  )
  city_establishments_label <- format(
    current_row$city_establishments,
    big.mark = ",",
    trim = TRUE
  )
  rounded_coverage_percent <- round(current_row$coverage_pct, 1)
  establishment_label <- paste0(
    included_establishments_label,
    " / ",
    city_establishments_label,
    "\n",
    rounded_coverage_percent,
    "%"
  )
  new_rows <- data.frame(
    cohort = rep(cohort_name, 2),
    measure = c("Area-industry combinations", "Recorded establishments in 2024"),
    percent = c(combination_percent, current_row$coverage_pct),
    label = c(combination_label, establishment_label)
  )
  coverage_plot_data <- rbind(coverage_plot_data, new_rows)
}
write_csv(coverage_plot_data, file.path(data_folder, "plot_coverage.csv"))

coverage_plot <- ggplot(coverage_plot_data, aes(x = cohort, y = percent, fill = cohort)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = label), vjust = -0.25, size = 4) +
  facet_wrap(~measure, nrow = 1) +
  scale_fill_manual(values = c("117 combinations" = "#3F7186", "153 combinations" = "#D08D45")) +
  scale_y_continuous(limits = c(0, 112), breaks = seq(0, 100, 20)) +
  labs(
    title = "Sample coverage: 117 versus 153 combinations",
    subtitle = "The larger cohort retains the original 117 combinations and adds 36.",
    x = NULL,
    y = "Share of the stated denominator (%)",
    caption = "Coverage measures inclusion. It is not a measure of clustering accuracy."
  )
save_plot(coverage_plot, "01_cohort_coverage", 11, 5.5)
figure_record <- data.frame(
  figure = "01_cohort_coverage",
  meaning = "Compare the number of included combinations and coverage of recorded 2024 establishments.",
  data_source = "plot_coverage.csv; cohort_coverage.csv"
)
figure_index <- rbind(figure_index, figure_record)

agreement_rows <- comparison_metrics$scope == "common117"
requested_references <- comparison_metrics$reference %in% c("Peer", "Baseline")
agreement_rows <- agreement_rows & requested_references
agreement_data <- comparison_metrics[agreement_rows, ]
reversed_experiment_ids <- rev(experiment_ids)
reversed_experiment_labels <- rev(experiment_labels)
agreement_data$experiment_label <- factor(
  agreement_data$experiment,
  levels = reversed_experiment_ids,
  labels = reversed_experiment_labels
)
agreement_data$reference_label <- agreement_data$reference
peer_rows <- agreement_data$reference == "Peer"
baseline_rows <- agreement_data$reference == "Baseline"
agreement_data$reference_label[peer_rows] <- "Peer's figure: 4 groups"
agreement_data$reference_label[baseline_rows] <- "Our 3-period Ward baseline: 4 groups"
agreement_data$number_label <- sprintf("%.3f", agreement_data$ari)
write_csv(agreement_data, file.path(data_folder, "plot_agreement_common117.csv"))

agreement_plot <- ggplot(
  agreement_data,
  aes(x = ari, y = experiment_label, colour = reference_label, shape = reference_label)
) +
  geom_vline(xintercept = 0, colour = "#A0A0A0", linetype = "dashed") +
  geom_vline(xintercept = 1, colour = "#C8C8C8", linetype = "dotted") +
  geom_point(position = position_dodge(width = 0.45), size = 3.2) +
  geom_text(
    aes(label = number_label),
    position = position_dodge(width = 0.45),
    hjust = -0.25,
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = c("Peer's figure: 4 groups" = "#217C91", "Our 3-period Ward baseline: 4 groups" = "#B57332")) +
  scale_x_continuous(breaks = seq(-0.2, 1, 0.2), expand = expansion(mult = c(0.06, 0.16))) +
  labs(
    title = "How similar are the six solutions?",
    subtitle = "All comparisons use the same 117 combinations. Each solution has four groups.",
    x = "Adjusted Rand index (1 = identical grouping; 0 = chance-level agreement)",
    y = NULL,
    colour = "Reference",
    shape = "Reference",
    caption = "ARI compares pairs of combinations and ignores arbitrary group numbers. Agreement does not establish reliability."
  )
save_plot(agreement_plot, "02_agreement_common117", 12, 6.6)
figure_record <- data.frame(
  figure = "02_agreement_common117",
  meaning = "Compare similarity with the peer and the four-group baseline on the same 117 combinations.",
  data_source = "plot_agreement_common117.csv; comparison_metrics.csv"
)
figure_index <- rbind(figure_index, figure_record)

common_sample_rows <- cross_tabs$scope == "common117"
peer_comparison_rows <- cross_tabs$reference == "Peer"
cross_tab_rows <- common_sample_rows & peer_comparison_rows
cross_tab_data <- cross_tabs[cross_tab_rows, ]
complete_cells <- expand.grid(
  experiment = experiment_ids,
  aligned_cluster = 1:4,
  reference_cluster = 1:4
)
cross_tab_columns <- c("experiment", "aligned_cluster", "reference_cluster", "n")
cross_tab_values <- cross_tab_data[, cross_tab_columns]
cross_tab_data <- merge(complete_cells, cross_tab_values, all.x = TRUE)
empty_cross_tab_cells <- is.na(cross_tab_data$n)
cross_tab_data$n[empty_cross_tab_cells] <- 0
cross_tab_data$experiment_label <- factor(
  cross_tab_data$experiment,
  levels = experiment_ids,
  labels = experiment_panel_labels
)
cross_tab_data$our_group <- factor(cross_tab_data$aligned_cluster, levels = 4:1)
cross_tab_data$peer_group <- factor(cross_tab_data$reference_cluster, levels = 1:4)
write_csv(cross_tab_data, file.path(data_folder, "plot_cross_tabs_common117.csv"))

cross_tab_plot <- ggplot(cross_tab_data, aes(x = peer_group, y = our_group, fill = n)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = n), size = 4, colour = "#17212B") +
  facet_wrap(~experiment_label, ncol = 2) +
  scale_fill_gradient(low = "#F2F5F7", high = "#70B7C3") +
  coord_equal() +
  labs(
    title = "Where do memberships agree with the peer?",
    subtitle = "Each panel contains the same 117 combinations. Numbers are counts, not percentages.",
    x = "Peer's group number",
    y = "Our group number after matching",
    fill = "Combinations",
    caption = "Group names are matched one-to-one for readability. A perfect match puts every combination on the diagonal."
  ) +
  theme(panel.grid = element_blank())
save_plot(cross_tab_plot, "03_peer_cross_tabs_common117", 12, 11.5)
figure_record <- data.frame(
  figure = "03_peer_cross_tabs_common117",
  meaning = "Show the number of common combinations assigned to each pair of our groups and peer groups.",
  data_source = "plot_cross_tabs_common117.csv; cross_tabs.csv"
)
figure_index <- rbind(figure_index, figure_record)

period_names <- c(
  "log_2002_2008", "log_2008_2012", "log_2012_2018",
  "log_2018_2022", "log_2022_2024"
)
period_labels <- c("2002-08", "2008-12", "2012-18", "2018-22", "2022-24")
profile_data <- profiles
requested_period_rows <- profile_data$feature %in% period_names
profile_data <- profile_data[requested_period_rows, ]
if (nrow(profile_data) != 120) {
  stop("The profile figures need five periods for four groups in each experiment.")
}
profile_data$period_label <- factor(
  profile_data$feature,
  levels = period_names,
  labels = period_labels
)
profile_data$group_label <- factor(profile_data$aligned_cluster, levels = 1:4)
profile_data$experiment_label <- factor(
  profile_data$experiment,
  levels = experiment_ids,
  labels = experiment_labels
)
write_csv(profile_data, file.path(data_folder, "plot_profiles_five_periods.csv"))

median_profile_plot <- ggplot(
  profile_data,
  aes(x = period_label, y = median_log, group = group_label, colour = group_label, shape = group_label)
) +
  geom_hline(yintercept = 0, colour = "#B0B0B0", linetype = "dashed") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.8) +
  facet_wrap(~experiment_label, ncol = 2) +
  scale_colour_manual(values = group_colours) +
  scale_shape_manual(values = group_shapes) +
  labs(
    title = "Group profiles: the middle combination in each period",
    subtitle = "Each point is a group median. Positive values indicate growth; negative values indicate decline.",
    x = "Period",
    y = "Annualised log change (log points per year)",
    colour = "Matched group",
    shape = "Matched group",
    caption = paste(
      "All solutions are described using five periods; three-period models did not use all five separately for grouping.",
      "The plotted measure is 100 x log(end count / start count) / years, not ordinary percentage change.",
      sep = "\n"
    )
  )
save_plot(median_profile_plot, "04_group_profiles_median", 13, 10)
figure_record <- data.frame(
  figure = "04_group_profiles_median",
  meaning = "Describe each group's median annualised log change over the same five periods.",
  data_source = "plot_profiles_five_periods.csv; profiles.csv"
)
figure_index <- rbind(figure_index, figure_record)

mean_profile_plot <- ggplot(
  profile_data,
  aes(x = period_label, y = mean_log, group = group_label, colour = group_label, shape = group_label)
) +
  geom_hline(yintercept = 0, colour = "#B0B0B0", linetype = "dashed") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.8) +
  facet_wrap(~experiment_label, ncol = 2) +
  scale_colour_manual(values = group_colours) +
  scale_shape_manual(values = group_shapes) +
  labs(
    title = "Group profiles: the average combination in each period",
    subtitle = "Each combination receives equal weight. Large establishment counts do not receive extra weight.",
    x = "Period",
    y = "Annualised log change (log points per year)",
    colour = "Matched group",
    shape = "Matched group",
    caption = paste(
      "Means can differ from medians when a few combinations have unusually large changes.",
      "These are descriptions of our solutions, not a reconstruction of the peer's undocumented growth-rate formula.",
      sep = "\n"
    )
  )
save_plot(mean_profile_plot, "05_group_profiles_mean", 13, 10)
figure_record <- data.frame(
  figure = "05_group_profiles_mean",
  meaning = "Describe equally weighted group means and compare them with the median profiles.",
  data_source = "plot_profiles_five_periods.csv; profiles.csv"
)
figure_index <- rbind(figure_index, figure_record)

maximum_index <- max(trajectories$q75_index, na.rm = TRUE)
index_step <- 25
number_of_index_steps <- maximum_index / index_step
rounded_index_steps <- ceiling(number_of_index_steps)
upper_index_limit <- rounded_index_steps * index_step
for (experiment_name in experiment_ids) {
  path_data <- trajectories[trajectories$experiment == experiment_name, ]
  path_data$group_label <- factor(path_data$aligned_cluster, levels = 1:4)
  path_data$panel_label <- paste0(
    "Matched group ",
    path_data$aligned_cluster,
    " (n = ",
    path_data$n,
    ")"
  )
  group_order <- order(path_data$aligned_cluster)
  ordered_path_data <- path_data[group_order, ]
  panel_order <- unique(ordered_path_data$panel_label)
  path_data$panel_label <- factor(path_data$panel_label, levels = panel_order)
  if (length(panel_order) != 4) {
    stop("Each historical-path figure needs four group panels.")
  }
  missing_panel_labels <- is.na(path_data$panel_label)
  if (any(missing_panel_labels)) {
    stop("Historical-path panel labels must not be missing.")
  }
  path_plot <- ggplot(path_data, aes(x = year, y = median_index, colour = group_label, fill = group_label)) +
    geom_hline(yintercept = 100, colour = "#8F8F8F", linetype = "dashed") +
    geom_vline(xintercept = 2018, colour = "#CACACA", linetype = "dotted") +
    geom_ribbon(aes(ymin = q25_index, ymax = q75_index), alpha = 0.2, colour = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~panel_label, ncol = 2) +
    scale_colour_manual(values = group_colours) +
    scale_fill_manual(values = group_colours) +
    scale_x_continuous(breaks = c(2002, 2008, 2012, 2018, 2022, 2024)) +
    coord_cartesian(ylim = c(0, upper_index_limit)) +
    labs(
      title = paste0(experiment_name, ": historical paths within the four groups"),
      subtitle = experiment_labels[experiment_name],
      x = "Year",
      y = "Establishment index (each combination's 2018 = 100)",
      caption = paste(
        "Line: median. Shading: middle 50% of combinations, not a confidence interval or forecast.",
        "All six path figures use the same vertical scale. A value below 100 means below the combination's own 2018 count.",
        sep = "\n"
      )
    ) +
    theme(legend.position = "none")
  figure_name <- paste0("06_", experiment_name, "_historical_paths")
  save_plot(path_plot, figure_name, 12, 8)
  figure_meaning <- paste(
    experiment_name,
    "median and middle-50% historical paths after indexing each combination to its own 2018 count."
  )
  figure_record <- data.frame(
    figure = figure_name,
    meaning = figure_meaning,
    data_source = "trajectories.csv"
  )
  figure_index <- rbind(figure_index, figure_record)
}

requested_diagnostics <- diagnostics$experiment %in% experiment_ids
diagnostic_data <- diagnostics[requested_diagnostics, ]
diagnostic_data$experiment_label <- factor(
  diagnostic_data$experiment,
  levels = experiment_ids,
  labels = experiment_labels
)
silhouette_plot <- ggplot(diagnostic_data, aes(x = k, y = silhouette)) +
  geom_hline(yintercept = 0.25, colour = "#AE5C3B", linetype = "dashed") +
  geom_vline(xintercept = 4, colour = "#909090", linetype = "dotted") +
  geom_line(colour = "#356D82", linewidth = 0.9) +
  geom_point(colour = "#356D82", size = 2.8) +
  facet_wrap(~experiment_label, ncol = 2) +
  scale_x_continuous(breaks = 2:6) +
  labs(
    title = "Do the groups separate clearly?",
    subtitle = "Average silhouette for two to six groups. Higher values indicate better separation in the input features.",
    x = "Number of groups (k)",
    y = "Average silhouette",
    caption = "Dashed line: project check of 0.25. Dotted line: four groups used for the peer comparison. Neither establishes stability."
  )
save_plot(silhouette_plot, "07_silhouette_diagnostics", 12, 9)
figure_record <- data.frame(
  figure = "07_silhouette_diagnostics",
  meaning = "Compare separation for two to six groups; identify the fixed four-group comparison and the project threshold.",
  data_source = "diagnostics.csv"
)
figure_index <- rbind(figure_index, figure_record)

requested_stability_rows <- stability$experiment %in% experiment_ids
stability_plot_data <- stability[requested_stability_rows, ]
mapping_columns <- c("experiment", "cluster", "aligned_cluster")
stability_map <- label_map[, mapping_columns]
stability_plot_data <- merge(
  stability_plot_data,
  stability_map,
  by = c("experiment", "cluster")
)
stability_plot_data$group_label <- factor(stability_plot_data$aligned_cluster, levels = 1:4)
stability_plot_data$experiment_label <- factor(
  stability_plot_data$experiment,
  levels = reversed_experiment_ids,
  labels = reversed_experiment_labels
)
write_csv(stability_plot_data, file.path(data_folder, "plot_stability_k4.csv"))
stability_plot <- ggplot(
  stability_plot_data,
  aes(x = mean_jaccard, y = experiment_label, colour = group_label, shape = group_label)
) +
  geom_vline(xintercept = 0.75, colour = "#646464", linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.55), size = 3.2) +
  scale_colour_manual(values = group_colours) +
  scale_shape_manual(values = group_shapes) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(
    title = "Do the four-group solutions remain stable?",
    subtitle = "Each point is one group's average best-match Jaccard score across 200 subsamples.",
    x = "Mean Jaccard (1 = identical membership among the retained combinations)",
    y = NULL,
    colour = "Matched group",
    shape = "Matched group",
    caption = paste(
      "Dashed line: project requirement of at least 0.75 for every group. Each repeat retains 80% (rounded down).",
      "Standardisation and clustering are repeated. These scores measure algorithm sensitivity, not population confidence intervals.",
      sep = "\n"
    )
  )
save_plot(stability_plot, "08_stability_four_groups", 12, 6.8)
figure_record <- data.frame(
  figure = "08_stability_four_groups",
  meaning = "Check whether every group clears the mean Jaccard requirement under repeated subsampling.",
  data_source = "plot_stability_k4.csv; stability_summary.csv; label_map.csv"
)
figure_index <- rbind(figure_index, figure_record)

diagnostic_data$size_ratio <- diagnostic_data$min_group_size / diagnostic_data$required_min_size
diagnostic_data$size_label <- paste0(
  diagnostic_data$min_group_size,
  " / ",
  diagnostic_data$required_min_size
)
size_plot <- ggplot(diagnostic_data, aes(x = k, y = size_ratio)) +
  geom_hline(yintercept = 1, colour = "#AE5C3B", linetype = "dashed") +
  geom_vline(xintercept = 4, colour = "#909090", linetype = "dotted") +
  geom_line(colour = "#356D82", linewidth = 0.8) +
  geom_point(colour = "#356D82", size = 2.5) +
  geom_text(aes(label = size_label), vjust = -0.7, size = 3) +
  facet_wrap(~experiment_label, ncol = 2) +
  scale_x_continuous(breaks = 2:6) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.17))) +
  labs(
    title = "Are any groups too small for this comparison?",
    subtitle = "Point labels show smallest group size / required minimum. A ratio of 1 exactly meets the requirement.",
    x = "Number of groups (k)",
    y = "Smallest group / required minimum",
    caption = "The minimum is max(5, ceiling(5% of the cohort)): 6 for 117 combinations; 8 for 153 combinations."
  )
save_plot(size_plot, "09_group_size_diagnostics", 12, 9)
figure_record <- data.frame(
  figure = "09_group_size_diagnostics",
  meaning = "Compare the smallest group with the required minimum at each candidate number of groups.",
  data_source = "diagnostics.csv"
)
figure_index <- rbind(figure_index, figure_record)

area_names <- c(
  "Carlton", "Docklands", "East Melbourne", "Kensington", "Melbourne (CBD)",
  "Melbourne (Remainder)", "North Melbourne", "Parkville", "Port Melbourne",
  "South Yarra", "Southbank", "West Melbourne (Industrial)", "West Melbourne (Residential)"
)
industry_names <- c(
  "Accommodation and Food Services", "Administrative and Support Services",
  "Agriculture, Forestry and Fishing", "Arts and Recreation Services", "Construction",
  "Education and Training", "Electricity, Gas, Water and Waste Services",
  "Financial and Insurance Services", "Health Care and Social Assistance",
  "Information Media and Telecommunications", "Manufacturing", "Mining", "Other Services",
  "Professional, Scientific and Technical Services", "Public Administration and Safety",
  "Rental, Hiring and Real Estate Services", "Retail Trade",
  "Transport, Postal and Warehousing", "Wholesale Trade"
)
matrix_colours <- c(group_colours, "Outside cohort" = "#DDDDD9")
for (experiment_name in experiment_ids) {
  matrix_cells <- expand.grid(area = area_names, industry = industry_names)
  member_rows <- memberships$experiment == experiment_name
  member_rows <- member_rows & memberships$k == 4
  member_columns <- c("area", "industry", "aligned_cluster")
  current_members <- memberships[member_rows, member_columns]
  matrix_data <- merge(
    matrix_cells,
    current_members,
    by = c("area", "industry"),
    all.x = TRUE
  )
  matrix_data$cell_label <- as.character(matrix_data$aligned_cluster)
  matrix_data$colour_label <- as.character(matrix_data$aligned_cluster)
  excluded_cells <- is.na(matrix_data$aligned_cluster)
  matrix_data$cell_label[excluded_cells] <- ""
  matrix_data$colour_label[excluded_cells] <- "Outside cohort"
  matrix_data$area <- factor(matrix_data$area, levels = area_names)
  matrix_data$industry <- factor(matrix_data$industry, levels = industry_names)
  matrix_colour_labels <- c("1", "2", "3", "4", "Outside cohort")
  matrix_data$colour_label <- factor(
    matrix_data$colour_label,
    levels = matrix_colour_labels
  )
  plot_table_name <- paste0("plot_", experiment_name, "_membership_matrix.csv")
  write_csv(matrix_data, file.path(data_folder, plot_table_name))
  matrix_plot <- ggplot(matrix_data, aes(x = area, y = industry, fill = colour_label)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    geom_text(aes(label = cell_label), colour = "#17212B", size = 3.5) +
    scale_fill_manual(values = matrix_colours, drop = FALSE) +
    labs(
      title = paste0(experiment_name, ": which group contains each area-industry combination?"),
      subtitle = experiment_labels[experiment_name],
      x = NULL,
      y = NULL,
      fill = "Matched group",
      caption = paste(
        "Numbers and colours identify groups after one-to-one matching with the peer. They do not indicate risk or priority.",
        "Grey cells lie outside this model's cohort. Colours are aligned for comparison, not evidence that memberships are identical.",
        sep = "\n"
      )
    ) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      legend.position = "bottom"
    )
  figure_name <- paste0("10_", experiment_name, "_area_industry_matrix")
  save_plot(matrix_plot, figure_name, 16, 10)
  figure_meaning <- paste(
    experiment_name,
    "group membership across all 247 theoretical cells; grey cells are outside the cohort."
  )
  figure_data_source <- paste(plot_table_name, "memberships_aligned.csv", sep = "; ")
  figure_record <- data.frame(
    figure = figure_name,
    meaning = figure_meaning,
    data_source = figure_data_source
  )
  figure_index <- rbind(figure_index, figure_record)
}

write_csv(figure_index, file.path(data_folder, "figure_index.csv"))
message("Saved ", nrow(figure_index), " figures as PNG and PDF.")


cat("Step 5 of 5: independently verify data, models and results.\n")

checks <- data.frame(
  check = character(0),
  passed = logical(0),
  detail = character(0)
)

check_row <- function(check_name, passed, detail = "") {
  data.frame(check = check_name, passed = isTRUE(passed), detail = detail)
}

same_numbers <- function(first_values, second_values, tolerance = 1e-9) {
  if (length(first_values) != length(second_values)) {
    return(FALSE)
  }
  if (!all(is.na(first_values) == is.na(second_values))) {
    return(FALSE)
  }
  known_positions <- !is.na(first_values)
  differences <- abs(first_values[known_positions] - second_values[known_positions])
  return(all(differences < tolerance))
}

independent_ari <- function(first_labels, second_labels) {
  if (length(first_labels) != length(second_labels)) {
    stop("ARI needs two label lists for the same observations.")
  }
  if (anyNA(first_labels) || anyNA(second_labels)) {
    stop("ARI cannot use missing group labels.")
  }

  observation_count <- length(first_labels)
  if (observation_count < 2) {
    return(1)
  }
  overlap_table <- table(first_labels, second_labels)
  first_group_sizes <- rowSums(overlap_table)
  second_group_sizes <- colSums(overlap_table)

  pairs_together_in_both <- sum(overlap_table * (overlap_table - 1) / 2)
  pairs_together_in_first <- sum(first_group_sizes * (first_group_sizes - 1) / 2)
  pairs_together_in_second <- sum(second_group_sizes * (second_group_sizes - 1) / 2)
  all_pairs <- observation_count * (observation_count - 1) / 2

  expected_agreement <- pairs_together_in_first * pairs_together_in_second / all_pairs
  largest_agreement <- (pairs_together_in_first + pairs_together_in_second) / 2
  denominator <- largest_agreement - expected_agreement
  if (denominator == 0) {
    return(1)
  }
  adjusted_agreement <- (pairs_together_in_both - expected_agreement) / denominator
  return(adjusted_agreement)
}

example_first <- c(1, 1, 2, 2)
example_renamed <- c(8, 8, 3, 3)
example_different <- c(1, 2, 1, 2)
checks <- rbind(checks, check_row(
  "ARI is unchanged when group numbers are renamed",
  independent_ari(example_first, example_renamed) == 1
))
checks <- rbind(checks, check_row(
  "ARI detects a known different partition",
  abs(independent_ari(example_first, example_different) + 0.5) < 1e-12
))

raw_data <- read_csv(source_file, show_col_types = FALSE)
cohorts <- read_csv(file.path(data_folder, "cohorts.csv"), show_col_types = FALSE)
annual_counts <- read_csv(file.path(data_folder, "annual_counts.csv"), show_col_types = FALSE)
features_all <- read_csv(file.path(data_folder, "features_all.csv"), show_col_types = FALSE)

raw_keys <- raw_data[c("Census year", "CLUE small area", "ANZSIC indusrty", "Business size")]
checks <- rbind(checks, check_row("Source has 13353 rows", nrow(raw_data) == 13353))
checks <- rbind(checks, check_row("Source keys are complete and unique",
  !anyNA(raw_keys) && anyDuplicated(raw_keys) == 0))
checks <- rbind(checks, check_row("Source establishment counts are known nonnegative integers",
  !anyNA(raw_data$`Total establishments`) &&
    all(raw_data$`Total establishments` >= 0) &&
    all(raw_data$`Total establishments` == floor(raw_data$`Total establishments`))))

independent_annual <- aggregate(
  x = list(establishments = raw_data$`Total establishments`),
  by = list(
    area = raw_data$`CLUE small area`,
    industry = raw_data$`ANZSIC indusrty`,
    year = raw_data$`Census year`
  ),
  FUN = sum
)
independent_keys <- paste(independent_annual$area, independent_annual$industry,
                          independent_annual$year, sep = " | ")
saved_keys <- paste(annual_counts$area, annual_counts$industry,
                    annual_counts$year, sep = " | ")
source_positions <- match(saved_keys, independent_keys)
expected_annual_counts <- independent_annual$establishments[source_positions]
checks <- rbind(checks, check_row("Saved annual grid has 247 by 23 unique rows",
  nrow(annual_counts) == 247 * 23 && anyDuplicated(saved_keys) == 0))
checks <- rbind(checks, check_row("Every saved annual count agrees with source, including missing cells",
  same_numbers(annual_counts$establishments, expected_annual_counts)))

cohort_keys <- paste(cohorts$area, cohorts$industry, sep = " | ")
checks <- rbind(checks, check_row("Cohort table contains 247 distinct combinations",
  nrow(cohorts) == 247 && anyDuplicated(cohort_keys) == 0 && anyDuplicated(cohorts$segment_id) == 0))

years <- 2002:2024
independent_counts <- matrix(NA_real_, nrow = nrow(cohorts), ncol = length(years))
colnames(independent_counts) <- paste0("count_", years)
rownames(independent_counts) <- cohorts$segment_id
for (year_number in seq_along(years)) {
  requested_keys <- paste(cohorts$area, cohorts$industry, years[year_number], sep = " | ")
  requested_positions <- match(requested_keys, independent_keys)
  independent_counts[, year_number] <- independent_annual$establishments[requested_positions]
  saved_column <- paste0("count_", years[year_number])
  checks <- rbind(checks, check_row(
    paste("Cohort count column agrees with original data:", years[year_number]),
    same_numbers(cohorts[[saved_column]], independent_counts[, year_number])
  ))
}

expected_117 <- logical(nrow(cohorts))
expected_153 <- logical(nrow(cohorts))
independent_annual_mean <- rep(NA_real_, nrow(cohorts))
biennial_columns <- paste0("count_", seq(2002, 2024, 2))
endpoint_columns <- c("count_2002", "count_2018", "count_2022", "count_2024")
for (segment_number in seq_len(nrow(cohorts))) {
  history <- independent_counts[segment_number, ]
  biennial_history <- independent_counts[segment_number, biennial_columns]
  endpoints <- independent_counts[segment_number, endpoint_columns]

  if (!anyNA(biennial_history) && !anyNA(endpoints)) {
    expected_117[segment_number] <- all(endpoints > 0) && history["count_2018"] >= 20
  }
  if (!anyNA(history)) {
    independent_annual_mean[segment_number] <- mean(history)
    expected_153[segment_number] <- all(history > 0) && mean(history) >= 10
  }
}
checks <- rbind(checks, check_row("117 cohort exactly follows its original inclusion rules",
  identical(as.logical(cohorts$included_117), expected_117) && sum(expected_117) == 117))
checks <- rbind(checks, check_row("153 cohort exactly follows the complete 23-year mean rule",
  identical(as.logical(cohorts$included_153), expected_153) && sum(expected_153) == 153))
checks <- rbind(checks, check_row("The 153 cohort contains the 117 cohort plus 36 combinations",
  all(!expected_117 | expected_153) && sum(expected_153 & !expected_117) == 36 &&
    identical(as.logical(cohorts$added_to_117), expected_153 & !expected_117)))
checks <- rbind(checks, check_row("Complete-history annual means use all 23 years without rounding",
  same_numbers(cohorts$mean_annual_count[!is.na(independent_annual_mean)],
    independent_annual_mean[!is.na(independent_annual_mean)])))

feature_columns <- c("log_2002_2018", "log_2002_2008", "log_2008_2012",
                     "log_2012_2018", "log_2018_2022", "log_2022_2024")
feature_start_years <- c(2002, 2002, 2008, 2012, 2018, 2022)
feature_end_years <- c(2018, 2008, 2012, 2018, 2022, 2024)
independent_features <- matrix(NA_real_, nrow = nrow(cohorts), ncol = length(feature_columns))
rownames(independent_features) <- cohorts$segment_id
colnames(independent_features) <- feature_columns
feature_positions <- match(features_all$segment_id, cohorts$segment_id)
checks <- rbind(checks, check_row("Feature rows have distinct known combination identifiers",
  !anyNA(feature_positions) && !anyDuplicated(features_all$segment_id)))

for (feature_number in seq_along(feature_columns)) {
  start_column <- paste0("count_", feature_start_years[feature_number])
  end_column <- paste0("count_", feature_end_years[feature_number])
  start_counts <- independent_counts[, start_column]
  end_counts <- independent_counts[, end_column]
  valid_counts <- !is.na(start_counts) & !is.na(end_counts) & start_counts > 0 & end_counts > 0
  year_count <- feature_end_years[feature_number] - feature_start_years[feature_number]
  independent_features[valid_counts, feature_number] <-
    100 * log(end_counts[valid_counts] / start_counts[valid_counts]) / year_count

  saved_values <- features_all[[feature_columns[feature_number]]]
  expected_values <- independent_features[feature_positions, feature_number]
  checks <- rbind(checks, check_row(
    paste("Feature is independently recomputed:", feature_columns[feature_number]),
    same_numbers(saved_values, expected_values)
  ))
}

settings <- read_csv(file.path(data_folder, "experiment_settings.csv"), show_col_types = FALSE)
saved_features <- read_csv(file.path(data_folder, "feature_values.csv"), show_col_types = FALSE)
saved_scaling <- read_csv(file.path(data_folder, "scaling_values.csv"), show_col_types = FALSE)
candidates <- read_csv(file.path(data_folder, "candidate_memberships.csv"), show_col_types = FALSE)
main_memberships <- read_csv(file.path(data_folder, "main_memberships.csv"), show_col_types = FALSE)
diagnostics <- read_csv(file.path(data_folder, "diagnostics.csv"), show_col_types = FALSE)
model_summary <- read_csv(file.path(data_folder, "model_summary.csv"), show_col_types = FALSE)
sampling_records <- read_csv(file.path(data_folder, "sampling_records.csv"), show_col_types = FALSE)
subsample_memberships <- read_csv(file.path(data_folder, "subsample_memberships.csv"), show_col_types = FALSE)
subsample_scaling <- read_csv(file.path(data_folder, "subsample_scaling.csv"), show_col_types = FALSE)
stability_details <- read_csv(file.path(data_folder, "stability_details.csv"), show_col_types = FALSE)
stability_summary <- read_csv(file.path(data_folder, "stability_summary.csv"), show_col_types = FALSE)
fit_log <- read_csv(file.path(data_folder, "fit_log.csv"), show_col_types = FALSE)

expected_methods <- c("Ward", "Ward", "Ward", "Kmeans", "Kmeans", "Kmeans")
expected_periods <- c(5, 3, 5, 5, 3, 5)
expected_samples <- c(117, 153, 153, 117, 153, 153)
checks <- rbind(checks, check_row("Exactly the six requested experiments are present",
  identical(settings$experiment, paste0("E", 1:6)) &&
    identical(settings$method, expected_methods) &&
    all(settings$periods == expected_periods) &&
    all(settings$sample_n == expected_samples) && all(settings$k == 4)))
checks <- rbind(checks, check_row("Model settings retain the agreed seed and K-means starts",
  all(settings$model_seed == 30005) && all(settings$kmeans_nstart == 100) &&
    all(settings$kmeans_iter_max == 100)))
checks <- rbind(checks, check_row("Diagnostics contain six experiments and five candidate group counts",
  nrow(diagnostics) == 30 && !anyDuplicated(paste(diagnostics$experiment, diagnostics$k))))
checks <- rbind(checks, check_row("Stability details contain 4800 distinct group comparisons",
  nrow(stability_details) == 4800 &&
    !anyDuplicated(paste(stability_details$experiment, stability_details$repetition, stability_details$cluster))))
checks <- rbind(checks, check_row("Fit log contains all 30 full and 1200 subsample fits",
  sum(fit_log$stage == "full") == 30 && sum(fit_log$stage == "subsample") == 1200))

three_features <- c("log_2002_2018", "log_2018_2022", "log_2022_2024")
five_features <- c("log_2002_2008", "log_2008_2012", "log_2012_2018",
                   "log_2018_2022", "log_2022_2024")

for (sample_size in c(117, 153)) {
  if (sample_size == 117) {
    pool_ids <- sort(cohorts$segment_id[expected_117])
  } else {
    pool_ids <- sort(cohorts$segment_id[expected_153])
  }
  draw_checks <- logical(200)
  seed_checks <- logical(200)
  for (repetition in 1:200) {
    selected_records <- sampling_records$sample_n == sample_size & sampling_records$repetition == repetition
    saved_draw <- sampling_records[selected_records, ]
    expected_seed <- 30005 + sample_size * 1000 + repetition
    set.seed(expected_seed)
    expected_sample_size <- floor(sample_size * 0.8)
    expected_ids <- sample(pool_ids, size = expected_sample_size, replace = FALSE)
    expected_ids <- sort(expected_ids)
    draw_checks[repetition] <- identical(saved_draw$segment_id, expected_ids)
    seed_checks[repetition] <- nrow(saved_draw) == length(expected_ids) &&
      all(saved_draw$draw_seed == expected_seed) && !anyDuplicated(saved_draw$segment_id)
  }
  checks <- rbind(checks, check_row(paste("All 200 saved draws reproduce exactly for sample", sample_size),
    all(draw_checks) && all(seed_checks)))
}

for (experiment_number in 1:6) {
  experiment_id <- paste0("E", experiment_number)
  method <- expected_methods[experiment_number]
  sample_size <- expected_samples[experiment_number]
  if (sample_size == 117) {
    selected_ids <- sort(cohorts$segment_id[expected_117])
  } else {
    selected_ids <- sort(cohorts$segment_id[expected_153])
  }
  selected_features <- five_features
  if (expected_periods[experiment_number] == 3) {
    selected_features <- three_features
  }

  raw_matrix <- independent_features[selected_ids, selected_features, drop = FALSE]
  standardized_matrix <- scale(raw_matrix)
  distances <- dist(standardized_matrix, method = "euclidean")
  full_scaling_correct <- TRUE
  for (feature_number in seq_along(selected_features)) {
    feature_name <- selected_features[feature_number]
    saved_feature_rows <- saved_features[saved_features$experiment == experiment_id &
      saved_features$feature == feature_name, ]
    saved_positions <- match(selected_ids, saved_feature_rows$segment_id)
    saved_feature_rows <- saved_feature_rows[saved_positions, ]
    scaling_row <- saved_scaling[saved_scaling$experiment == experiment_id &
      saved_scaling$feature == feature_name, ]
    correct_values <- same_numbers(saved_feature_rows$raw_value, raw_matrix[, feature_number]) &&
      same_numbers(saved_feature_rows$standardized_value, standardized_matrix[, feature_number])
    correct_parameters <- nrow(scaling_row) == 1 &&
      same_numbers(scaling_row$mean, mean(raw_matrix[, feature_number])) &&
      same_numbers(scaling_row$sd, sd(raw_matrix[, feature_number]))
    full_scaling_correct <- full_scaling_correct && correct_values && correct_parameters
  }
  checks <- rbind(checks, check_row(paste(experiment_id, "uses independently recomputed features and sample-SD scaling"),
    full_scaling_correct))

  for (number_of_groups in 2:6) {
    if (method == "Ward") {
      independent_tree <- hclust(distances, method = "ward.D2")
      independent_labels <- cutree(independent_tree, k = number_of_groups)
    } else {
      set.seed(30005)
      independent_model <- kmeans(standardized_matrix, centers = number_of_groups,
        nstart = 100, iter.max = 100, algorithm = "Hartigan-Wong")
      independent_labels <- independent_model$cluster
    }
    candidate_rows <- candidates[candidates$experiment == experiment_id & candidates$k == number_of_groups, ]
    candidate_positions <- match(selected_ids, candidate_rows$segment_id)
    candidate_rows <- candidate_rows[candidate_positions, ]
    partition_matches <- nrow(candidate_rows) == sample_size &&
      independent_ari(independent_labels, candidate_rows$cluster) == 1
    checks <- rbind(checks, check_row(paste(experiment_id, "k", number_of_groups, "partition independently reproduces"),
      partition_matches))

    distance_matrix <- as.matrix(distances)
    independent_silhouettes <- numeric(sample_size)
    for (observation_number in seq_len(sample_size)) {
      same_group_positions <- which(independent_labels == independent_labels[observation_number])
      same_group_positions <- setdiff(same_group_positions, observation_number)
      if (length(same_group_positions) == 0) {
        independent_silhouettes[observation_number] <- 0
      } else {
        own_average <- mean(distance_matrix[observation_number, same_group_positions])
        other_averages <- numeric(0)
        for (other_group in sort(unique(independent_labels))) {
          if (other_group != independent_labels[observation_number]) {
            other_positions <- which(independent_labels == other_group)
            distances_to_other_group <- distance_matrix[observation_number, other_positions]
            other_group_average <- mean(distances_to_other_group)
            other_averages <- c(other_averages, other_group_average)
          }
        }
        closest_average <- min(other_averages)
        denominator <- max(own_average, closest_average)
        if (denominator == 0) {
          independent_silhouettes[observation_number] <- 0
        } else {
          independent_silhouettes[observation_number] <- (closest_average - own_average) / denominator
        }
      }
    }
    diagnostic_row <- diagnostics[diagnostics$experiment == experiment_id & diagnostics$k == number_of_groups, ]
    group_sizes <- table(independent_labels)
    correct_diagnostics <- nrow(diagnostic_row) == 1 &&
      same_numbers(candidate_rows$silhouette_width, independent_silhouettes) &&
      same_numbers(diagnostic_row$silhouette, mean(independent_silhouettes)) &&
      diagnostic_row$min_group_size == min(group_sizes) &&
      diagnostic_row$required_min_size == max(5, ceiling(sample_size * 0.05))
    checks <- rbind(checks, check_row(paste(experiment_id, "k", number_of_groups, "silhouettes and size diagnostics agree"),
      correct_diagnostics))

    if (number_of_groups == 4) {
      reference_labels <- candidate_rows$cluster
      main_rows <- main_memberships[main_memberships$experiment == experiment_id, ]
      main_positions <- match(selected_ids, main_rows$segment_id)
      main_rows <- main_rows[main_positions, ]
      checks <- rbind(checks, check_row(paste(experiment_id, "main result is the k=4 candidate"),
        all(main_rows$k == 4) && identical(main_rows$cluster, reference_labels) &&
          same_numbers(main_rows$silhouette_width, independent_silhouettes)))
    }
  }

  subsample_partition_checks <- logical(200)
  subsample_scaling_checks <- logical(200)
  jaccard_checks <- logical(200)
  independent_jaccards <- matrix(NA_real_, nrow = 200, ncol = 4)

  for (repetition in 1:200) {
    draw_rows <- sampling_records$sample_n == sample_size & sampling_records$repetition == repetition
    retained_ids <- sampling_records$segment_id[draw_rows]
    retained_raw <- independent_features[retained_ids, selected_features, drop = FALSE]
    retained_z <- scale(retained_raw)
    if (method == "Ward") {
      retained_tree <- hclust(dist(retained_z), method = "ward.D2")
      retained_labels <- cutree(retained_tree, k = 4)
    } else {
      set.seed(30005 + repetition)
      retained_model <- kmeans(retained_z, centers = 4, nstart = 100,
        iter.max = 100, algorithm = "Hartigan-Wong")
      retained_labels <- retained_model$cluster
    }

    saved_subsample <- subsample_memberships[subsample_memberships$experiment == experiment_id &
      subsample_memberships$repetition == repetition, ]
    membership_positions <- match(retained_ids, saved_subsample$segment_id)
    saved_subsample <- saved_subsample[membership_positions, ]
    subsample_partition_checks[repetition] <- nrow(saved_subsample) == floor(sample_size * 0.8) &&
      all(saved_subsample$valid_fit) && !anyNA(saved_subsample$cluster) &&
      independent_ari(retained_labels, saved_subsample$cluster) == 1

    saved_scale_rows <- subsample_scaling[subsample_scaling$experiment == experiment_id &
      subsample_scaling$repetition == repetition, ]
    scale_positions <- match(selected_features, saved_scale_rows$feature)
    saved_scale_rows <- saved_scale_rows[scale_positions, ]
    expected_means <- as.numeric(attr(retained_z, "scaled:center"))
    expected_deviations <- as.numeric(attr(retained_z, "scaled:scale"))
    subsample_scaling_checks[repetition] <-
      same_numbers(saved_scale_rows$mean, expected_means) &&
      same_numbers(saved_scale_rows$sd, expected_deviations)

    reference_positions <- match(retained_ids, selected_ids)
    retained_reference_labels <- reference_labels[reference_positions]
    repeat_correct <- TRUE
    for (reference_group in 1:4) {
      reference_members <- retained_ids[retained_reference_labels == reference_group]
      matches <- numeric(4)
      for (subsample_group in 1:4) {
        new_members <- retained_ids[retained_labels == subsample_group]
        common_members <- intersect(reference_members, new_members)
        combined_members <- union(reference_members, new_members)
        matches[subsample_group] <- length(common_members) / length(combined_members)
      }
      expected_jaccard <- max(matches)
      independent_jaccards[repetition, reference_group] <- expected_jaccard
      saved_detail <- stability_details[stability_details$experiment == experiment_id &
        stability_details$repetition == repetition & stability_details$cluster == reference_group, ]
      group_correct <- nrow(saved_detail) == 1 && saved_detail$valid &&
        saved_detail$retained_group_n == length(reference_members) &&
        same_numbers(saved_detail$jaccard, expected_jaccard)
      repeat_correct <- repeat_correct && group_correct
    }
    jaccard_checks[repetition] <- repeat_correct
  }

  checks <- rbind(checks, check_row(paste(experiment_id, "all 200 subsample partitions independently reproduce"),
    all(subsample_partition_checks)))
  checks <- rbind(checks, check_row(paste(experiment_id, "all 200 subsamples recalculate their own scaling"),
    all(subsample_scaling_checks)))
  checks <- rbind(checks, check_row(paste(experiment_id, "all 800 group Jaccards use the correct common members"),
    all(jaccard_checks)))

  for (group_number in 1:4) {
    group_values <- independent_jaccards[, group_number]
    summary_row <- stability_summary[stability_summary$experiment == experiment_id &
      stability_summary$cluster == group_number, ]
    summary_correct <- nrow(summary_row) == 1 && summary_row$valid_repetitions == 200 &&
      summary_row$invalid_repetitions == 0 && same_numbers(summary_row$mean_jaccard, mean(group_values)) &&
      same_numbers(summary_row$median_jaccard, median(group_values)) &&
      same_numbers(summary_row$min_jaccard, min(group_values)) &&
      summary_row$passes == (mean(group_values) >= 0.75)
    checks <- rbind(checks, check_row(paste(experiment_id, "group", group_number, "stability summary agrees"),
      summary_correct))
  }

  summary_row <- model_summary[model_summary$experiment == experiment_id, ]
  expected_minimum <- min(colMeans(independent_jaccards))
  expected_silhouette_pass <- summary_row$silhouette >= 0.25
  expected_size_pass <- summary_row$min_group_size >= max(5, ceiling(sample_size * 0.05))
  expected_stability_pass <- expected_minimum >= 0.75
  expected_all_pass <- expected_silhouette_pass && expected_size_pass && expected_stability_pass
  checks <- rbind(checks, check_row(paste(experiment_id, "overall status follows the unchanged thresholds"),
    nrow(summary_row) == 1 && same_numbers(summary_row$min_mean_jaccard, expected_minimum) &&
      summary_row$all_200_valid && summary_row$passes_silhouette == expected_silhouette_pass &&
      summary_row$passes_size == expected_size_pass &&
      summary_row$passes_stability == expected_stability_pass && summary_row$passes_all == expected_all_pass))
  cat("Independently checked", experiment_id, "including all 200 retained samples.\n")
}

peer <- read_csv(file.path(data_folder, "peer_labels_transcribed.csv"), show_col_types = FALSE)
baseline <- read_csv(file.path(data_folder, "baseline_ward_four_groups.csv"), show_col_types = FALSE)
label_map <- read_csv(file.path(data_folder, "label_map.csv"), show_col_types = FALSE)
aligned_memberships <- read_csv(file.path(data_folder, "memberships_aligned.csv"), show_col_types = FALSE)
comparison_metrics <- read_csv(file.path(data_folder, "comparison_metrics.csv"), show_col_types = FALSE)
cross_tabs <- read_csv(file.path(data_folder, "cross_tabs.csv"), show_col_types = FALSE)
comparison_jaccard <- read_csv(file.path(data_folder, "comparison_jaccard.csv"), show_col_types = FALSE)
profiles <- read_csv(file.path(data_folder, "profiles.csv"), show_col_types = FALSE)
group_summary <- read_csv(file.path(data_folder, "cluster_summary.csv"), show_col_types = FALSE)
trajectories <- read_csv(file.path(data_folder, "trajectories.csv"), show_col_types = FALSE)
peer_profiles <- read_csv(file.path(data_folder, "peer_profiles_recalculated.csv"), show_col_types = FALSE)
coverage <- read_csv(file.path(data_folder, "cohort_coverage.csv"), show_col_types = FALSE)

common_ids <- sort(cohorts$segment_id[expected_117])
added_ids <- sort(cohorts$segment_id[expected_153 & !expected_117])
peer_positions <- match(peer$segment_id, cohorts$segment_id)
peer_is_clustered <- peer$peer_cluster %in% 1:4
peer_clustered_ids <- peer$segment_id[peer_is_clustered]
checks <- rbind(checks, check_row("Transcribed peer membership covers exactly the independent 153 cohort",
  setequal(peer_clustered_ids, cohorts$segment_id[expected_153]) &&
    anyDuplicated(peer$segment_id) == 0 && all(tabulate(peer$peer_cluster, nbins = 4) == c(20, 25, 76, 32))))
checks <- rbind(checks, check_row("Transcription retains all 247 cells and marks only the 94 excluded cells as group zero",
  setequal(peer$segment_id, cohorts$segment_id) &&
    all(peer$peer_cluster %in% 0:4) &&
    all(peer_is_clustered == expected_153[peer_positions])))
peer_expected_means <- round(independent_annual_mean[peer_positions[peer_is_clustered]])
checks <- rbind(checks, check_row("The 153 transcribed displayed means match rounded source averages",
  same_numbers(peer$peer_displayed_mean[peer_is_clustered], peer_expected_means) &&
    all(is.na(peer$peer_displayed_mean[!peer_is_clustered]))))

baseline_matrix <- independent_features[common_ids, three_features, drop = FALSE]
standardised_baseline <- scale(baseline_matrix)
baseline_distances <- dist(standardised_baseline)
baseline_tree <- hclust(baseline_distances, method = "ward.D2")
independent_baseline <- cutree(baseline_tree, k = 4)
saved_baseline <- baseline$cluster[match(common_ids, baseline$segment_id)]
checks <- rbind(checks, check_row("Original three-period Ward k=4 baseline independently reproduces",
  independent_ari(independent_baseline, saved_baseline) == 1))

for (experiment_id in paste0("E", 1:6)) {
  experiment_rows <- main_memberships[main_memberships$experiment == experiment_id, ]
  original_labels <- experiment_rows$cluster[match(common_ids, experiment_rows$segment_id)]
  peer_labels <- peer$peer_cluster[match(common_ids, peer$segment_id)]
  best_overlap <- -1
  for (first in 1:4) {
    for (second in 1:4) {
      for (third in 1:4) {
        for (fourth in 1:4) {
          possible_mapping <- c(first, second, third, fourth)
          if (length(unique(possible_mapping)) == 4) {
            possible_overlap <- sum(possible_mapping[original_labels] == peer_labels)
            best_overlap <- max(best_overlap, possible_overlap)
          }
        }
      }
    }
  }
  mapping_rows <- label_map[label_map$experiment == experiment_id, ]
  mapping_positions <- match(1:4, mapping_rows$cluster)
  mapping_rows <- mapping_rows[mapping_positions, ]
  mapped_labels <- mapping_rows$aligned_cluster[original_labels]
  mapping_correct <- nrow(mapping_rows) == 4 &&
    setequal(mapping_rows$aligned_cluster, 1:4) &&
    all(mapping_rows$alignment_scope == "common117") &&
    all(mapping_rows$total_matched == best_overlap) &&
    sum(mapped_labels == peer_labels) == best_overlap &&
    independent_ari(mapped_labels, original_labels) == 1
  checks <- rbind(checks, check_row(paste(experiment_id, "display mapping maximises overlap without changing membership"),
    mapping_correct))

  aligned_rows <- aligned_memberships[aligned_memberships$experiment == experiment_id, ]
  expected_labels <- mapping_rows$aligned_cluster[aligned_rows$cluster]
  checks <- rbind(checks, check_row(paste(experiment_id, "aligned membership follows the saved mapping"),
    all(aligned_rows$aligned_cluster == expected_labels)))
}

ari_checks <- logical(nrow(comparison_metrics))
cross_table_checks <- logical(nrow(comparison_metrics))
comparison_jaccard_checks <- logical(nrow(comparison_metrics))
for (comparison_number in seq_len(nrow(comparison_metrics))) {
  comparison <- comparison_metrics[comparison_number, ]
  experiment_rows <- main_memberships[main_memberships$experiment == comparison$experiment, ]
  if (comparison$reference == "Peer") {
    reference_rows <- data.frame(segment_id = peer$segment_id, cluster = peer$peer_cluster)
  } else if (comparison$reference == "Baseline") {
    reference_rows <- baseline
  } else {
    reference_rows <- main_memberships[main_memberships$experiment == comparison$reference,
      c("segment_id", "cluster")]
  }

  comparison_ids <- common_ids
  if (comparison$scope == "full153") {
    comparison_ids <- sort(cohorts$segment_id[expected_153])
  }
  if (comparison$scope == "added36") {
    comparison_ids <- added_ids
  }
  experiment_labels <- experiment_rows$cluster[match(comparison_ids, experiment_rows$segment_id)]
  reference_labels <- reference_rows$cluster[match(comparison_ids, reference_rows$segment_id)]
  expected_ari <- independent_ari(experiment_labels, reference_labels)
  ari_checks[comparison_number] <- comparison$n == length(comparison_ids) &&
    same_numbers(comparison$ari, expected_ari)

  saved_cells <- cross_tabs[cross_tabs$experiment == comparison$experiment &
    cross_tabs$reference == comparison$reference & cross_tabs$scope == comparison$scope, ]
  reference_groups <- factor(reference_labels, levels = 1:4)
  experiment_groups <- factor(experiment_labels, levels = 1:4)
  expected_cells <- table(reference_groups, experiment_groups)
  cells_correct <- nrow(saved_cells) == 16 && sum(saved_cells$n) == length(comparison_ids)
  for (cell_number in seq_len(nrow(saved_cells))) {
    cell <- saved_cells[cell_number, ]
    expected_n <- expected_cells[cell$reference_cluster, cell$cluster]
    cells_correct <- cells_correct && cell$n == expected_n
  }
  cross_table_checks[comparison_number] <- cells_correct

  saved_matches <- comparison_jaccard[comparison_jaccard$experiment == comparison$experiment &
    comparison_jaccard$reference == comparison$reference & comparison_jaccard$scope == comparison$scope, ]
  matches_correct <- nrow(saved_matches) == 8
  for (match_number in seq_len(nrow(saved_matches))) {
    saved_match <- saved_matches[match_number, ]
    source_labels <- experiment_labels
    target_labels <- reference_labels
    if (saved_match$direction == "reference_to_experiment") {
      source_labels <- reference_labels
      target_labels <- experiment_labels
    }
    source_ids <- comparison_ids[source_labels == saved_match$source_cluster]
    expected_jaccard <- NA_real_
    expected_target <- NA_integer_
    if (length(source_ids) > 0) {
      all_match_scores <- numeric(4)
      for (target_group in 1:4) {
        target_ids <- comparison_ids[target_labels == target_group]
        shared_ids <- intersect(source_ids, target_ids)
        combined_ids <- union(source_ids, target_ids)
        all_match_scores[target_group] <- length(shared_ids) / length(combined_ids)
      }
      expected_jaccard <- max(all_match_scores)
      expected_target <- which.max(all_match_scores)
    }
    matches_correct <- matches_correct && saved_match$source_n == length(source_ids) &&
      same_numbers(saved_match$jaccard, expected_jaccard) &&
      same_numbers(saved_match$best_target_cluster, expected_target)
  }
  comparison_jaccard_checks[comparison_number] <- matches_correct
}
checks <- rbind(checks, check_row("All comparison ARIs use the correct shared observations",
  all(ari_checks)))
checks <- rbind(checks, check_row("All comparison cross-tables independently reproduce",
  all(cross_table_checks)))
checks <- rbind(checks, check_row("Both directions of every comparison Jaccard independently reproduce",
  all(comparison_jaccard_checks)))

baseline_peer <- read_csv(file.path(data_folder, "baseline_peer.csv"), show_col_types = FALSE)
peer_positions <- match(common_ids, peer$segment_id)
shared_peer_labels <- peer$peer_cluster[peer_positions]
expected_baseline_ari <- independent_ari(saved_baseline, shared_peer_labels)
checks <- rbind(checks, check_row("Baseline versus peer ARI independently reproduces on 117 shared combinations",
  nrow(baseline_peer) == 1 && baseline_peer$n == 117 &&
    same_numbers(baseline_peer$ari, expected_baseline_ari)))

profile_checks <- logical(nrow(profiles))
for (profile_number in seq_len(nrow(profiles))) {
  profile <- profiles[profile_number, ]
  selected_members <- main_memberships$experiment == profile$experiment & main_memberships$cluster == profile$cluster
  member_ids <- main_memberships$segment_id[selected_members]
  expected_log_values <- independent_features[member_ids, profile$feature]
  expected_cagr_values <- 100 * (exp(expected_log_values / 100) - 1)
  profile_checks[profile_number] <- profile$sample_n == length(member_ids) &&
    same_numbers(profile$mean_log, mean(expected_log_values)) &&
    same_numbers(profile$median_log, median(expected_log_values)) &&
    same_numbers(profile$mean_cagr, mean(expected_cagr_values)) &&
    same_numbers(profile$median_cagr, median(expected_cagr_values))
}
checks <- rbind(checks, check_row("Every group profile uses the independently recomputed member features",
  all(profile_checks)))

city_2024 <- sum(raw_data$`Total establishments`[raw_data$`Census year` == 2024])
group_summary_checks <- logical(nrow(group_summary))
for (group_number in seq_len(nrow(group_summary))) {
  summary <- group_summary[group_number, ]
  selected_members <- main_memberships$experiment == summary$experiment & main_memberships$cluster == summary$cluster
  member_ids <- main_memberships$segment_id[selected_members]
  whole_sample_ids <- main_memberships$segment_id[main_memberships$experiment == summary$experiment]
  expected_2018 <- sum(independent_counts[member_ids, "count_2018"])
  expected_2024 <- sum(independent_counts[member_ids, "count_2024"])
  expected_sample <- sum(independent_counts[whole_sample_ids, "count_2024"])
  expected_change <- expected_2024 - expected_2018
  group_summary_checks[group_number] <- summary$n == length(member_ids) &&
    summary$count_2018 == expected_2018 && summary$count_2024 == expected_2024 &&
    summary$city_2024 == city_2024 && summary$sample_2024 == expected_sample &&
    same_numbers(summary$share_sample_2024_pct, 100 * expected_2024 / expected_sample) &&
    same_numbers(summary$share_city_2024_pct, 100 * expected_2024 / city_2024) &&
    summary$absolute_change == expected_change &&
    same_numbers(summary$change_pct, 100 * expected_change / expected_2018) &&
    summary$added_n == sum(member_ids %in% added_ids)
}
checks <- rbind(checks, check_row("Group counts and shares use their stated source denominators",
  all(group_summary_checks)))

trajectory_checks <- logical(nrow(trajectories))
for (trajectory_number in seq_len(nrow(trajectories))) {
  trajectory <- trajectories[trajectory_number, ]
  selected_members <- main_memberships$experiment == trajectory$experiment &
    main_memberships$cluster == trajectory$cluster
  member_ids <- main_memberships$segment_id[selected_members]
  year_column <- paste0("count_", trajectory$year)
  expected_indices <- 100 * independent_counts[member_ids, year_column] /
    independent_counts[member_ids, "count_2018"]
  expected_lower_quartile <- quantile(expected_indices, 0.25)
  expected_upper_quartile <- quantile(expected_indices, 0.75)
  expected_lower_quartile <- as.numeric(expected_lower_quartile)
  expected_upper_quartile <- as.numeric(expected_upper_quartile)
  trajectory_checks[trajectory_number] <- trajectory$n == length(member_ids) &&
    same_numbers(trajectory$median_index, median(expected_indices)) &&
    same_numbers(trajectory$q25_index, expected_lower_quartile) &&
    same_numbers(trajectory$q75_index, expected_upper_quartile)
}
checks <- rbind(checks, check_row("Every historical path first indexes individual counts to 2018, then summarises",
  all(trajectory_checks)))
checks <- rbind(checks, check_row("All historical path medians and quartiles equal 100 at the baseline",
  all(trajectories$median_index[trajectories$year == 2018] == 100) &&
    all(trajectories$q25_index[trajectories$year == 2018] == 100) &&
    all(trajectories$q75_index[trajectories$year == 2018] == 100)))

peer_profile_checks <- logical(nrow(peer_profiles))
for (profile_number in seq_len(nrow(peer_profiles))) {
  profile <- peer_profiles[profile_number, ]
  member_ids <- peer$segment_id[peer$peer_cluster == profile$peer_cluster]
  values <- independent_features[member_ids, profile$feature]
  peer_profile_checks[profile_number] <- profile$n == length(member_ids) &&
    same_numbers(profile$mean_log, mean(values)) && same_numbers(profile$median_log, median(values))
}
checks <- rbind(checks, check_row("Transcribed peer group profiles recompute from the same raw input",
  all(peer_profile_checks)))

coverage_checks <- logical(nrow(coverage))
for (coverage_number in seq_len(nrow(coverage))) {
  coverage_row <- coverage[coverage_number, ]
  included <- expected_117
  if (coverage_row$cohort == 153) {
    included <- expected_153
  }
  year_column <- paste0("count_", coverage_row$year)
  included_total <- sum(independent_counts[included, year_column])
  city_total <- sum(raw_data$`Total establishments`[raw_data$`Census year` == coverage_row$year])
  coverage_checks[coverage_number] <- coverage_row$segments == sum(included) &&
    coverage_row$included_establishments == included_total && coverage_row$city_establishments == city_total &&
    same_numbers(coverage_row$coverage_pct, 100 * included_total / city_total)
}
checks <- rbind(checks, check_row("Cohort coverage uses original city totals in each stated year",
  all(coverage_checks)))

expected_md5 <- "092490f73fa22c83c82d95d1ccd7e010"
checks <- rbind(checks, check_row("Original input checksum remains unchanged",
  unname(tools::md5sum(source_file)) == expected_md5))
write_csv(checks, file.path(qa_folder, "check_results.csv"))
if (!all(checks$passed)) {
  print(checks[!checks$passed, ])
  stop("At least one independent check failed. See qa/check_results.csv.")
}
cat("All", nrow(checks), "independent R checks passed.\n")


session_information <- capture.output(sessionInfo())
writeLines(session_information, file.path(qa_folder, "session_info.txt"))
cat("All six clustering comparisons and independent checks completed.\n")
