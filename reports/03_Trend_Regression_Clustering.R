# Descriptive analysis for the City of Melbourne business research questions.
# Scope: establishments/jobs 2002-2024, area/industry/size mix, trend regression,
# and exploratory clustering. Results flag topics for assessment; they do not
# prescribe infrastructure/budgets or estimate policy effects.
# Run: Rscript 03_Trend_Regression_Clustering.R [folder-containing-CSV-files]
# Dependencies: base R, MASS, cluster

options(stringsAsFactors = FALSE, scipen = 999)
if (!requireNamespace("MASS", quietly = TRUE) || !requireNamespace("cluster", quietly = TRUE))
  stop("Please install MASS and cluster (install.packages(c('MASS','cluster'))).")

args <- commandArgs(trailingOnly = TRUE)
data_dir <- if (length(args)) args[1] else "/Users/yaoruicheng/CMCE30005-Group-5"
aggregate_path <- file.path(data_dir, "business-establishments-and-jobs-data-by-business-size-and-anzsic.csv")
address_path <- file.path(data_dir, "business-establishments-with-address-and-industry-classification.csv")
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- if (length(script_arg)) dirname(normalizePath(sub("^--file=", "", script_arg[1]))) else getwd()
out_dir <- file.path(script_dir, "analysis_outputs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(aggregate_path)) stop("Aggregate CSV not found: ", aggregate_path)
read_utf8 <- function(path) read.csv(path, check.names = FALSE, fileEncoding = "UTF-8-BOM")
save_csv <- function(x, filename) write.csv(x, file.path(out_dir, filename), row.names = FALSE, na = "")

# 1. Import, validate and establish the observation grain.
raw <- read_utf8(aggregate_path)
expected <- c("Census year", "CLUE small area", "ANZSIC indusrty", "Business size", "Total establishments", "Total jobs")
if (!all(expected %in% names(raw))) stop("Unexpected aggregate CSV headers: ", paste(names(raw), collapse = ", "))
names(raw)[match(expected, names(raw))] <- c("year", "area", "industry", "size", "establishments", "jobs")
raw$year <- as.integer(raw$year); raw$establishments <- as.numeric(raw$establishments); raw$jobs <- as.numeric(raw$jobs)
raw <- raw[complete.cases(raw[c("year", "area", "industry", "size", "establishments", "jobs")]), ]
key <- c("year", "area", "industry", "size")
duplicated_cells <- sum(duplicated(raw[key]))
dat <- aggregate(raw[c("establishments", "jobs")], raw[key], sum)
years <- sort(unique(dat$year)); areas <- sort(unique(dat$area))
if (!all(c(2002, 2024) %in% years)) warning("Observed years are ", min(years), "-", max(years), ".")

# 2. Overall annual change (Objective 1).
annual <- aggregate(dat[c("establishments", "jobs")], list(year = dat$year), sum)
annual <- annual[order(annual$year), ]
annual$establishment_yoy_pct <- c(NA, diff(annual$establishments) / head(annual$establishments, -1) * 100)
annual$jobs_yoy_pct <- c(NA, diff(annual$jobs) / head(annual$jobs, -1) * 100)
save_csv(annual, "01_annual_totals.csv")

# 3. Segment-specific log-linear regression. Slope is transformed to compound
# annual % change; CI, p-value and R2 are included. These are trend associations.
trend_model <- function(d, segment_col, count_col, metric) {
  lev <- unique(d[[segment_col]])
  ans <- lapply(lev, function(g) {
    z <- d[d[[segment_col]] == g & d[[count_col]] > 0, ]
    if (nrow(z) < 3 || length(unique(z$year)) < 3) return(NULL)
    fit <- lm(log(z[[count_col]]) ~ I(z$year - min(z$year)))
    # Newey-West HAC standard errors reduce overconfident inference when annual
    # residuals are heteroskedastic or serially correlated (lag selected by rule).
    X <- model.matrix(fit); e <- residuals(fit); n <- nrow(X)
    lag_n <- min(n - 1, floor(4 * (n / 100)^(2 / 9)))
    scores <- X * as.vector(e)
    meat <- crossprod(scores)
    if (lag_n > 0) for (ell in seq_len(lag_n)) {
      weight <- 1 - ell / (lag_n + 1)
      cross <- crossprod(scores[(ell + 1):n, , drop = FALSE], scores[1:(n - ell), , drop = FALSE])
      meat <- meat + weight * (cross + t(cross))
    }
    bread <- solve(crossprod(X)); vcov_hac <- bread %*% meat %*% bread
    b <- coef(fit)[2]; se <- sqrt(vcov_hac[2, 2]); critical <- qt(0.975, df = n - 2)
    ci <- b + c(-1, 1) * critical * se
    p <- 2 * pt(-abs(b / se), df = n - 2); sm <- summary(fit)
    data.frame(metric = metric, segment = as.character(g), n_years = nrow(z),
      first_value = z[[count_col]][which.min(z$year)], last_value = z[[count_col]][which.max(z$year)],
      endpoint_change_pct = (z[[count_col]][which.max(z$year)] / z[[count_col]][which.min(z$year)] - 1) * 100,
      annual_trend_pct = (exp(b) - 1) * 100, ci_low_pct = (exp(ci[1]) - 1) * 100,
      ci_high_pct = (exp(ci[2]) - 1) * 100, p_value = p, hac_lags = lag_n, r_squared = sm$r.squared)
  })
  do.call(rbind, ans)
}
sum_segment <- function(group, labels) aggregate(dat[c("establishments", "jobs")], c(list(year = dat$year), setNames(list(dat[[group]]), labels)), sum)
by_area <- sum_segment("area", "segment"); by_industry <- sum_segment("industry", "segment"); by_size <- sum_segment("size", "segment")
regression <- rbind(
  trend_model(by_area, "segment", "establishments", "establishments_by_area"),
  trend_model(by_area, "segment", "jobs", "jobs_by_area"),
  trend_model(by_industry, "segment", "establishments", "establishments_by_industry"),
  trend_model(by_industry, "segment", "jobs", "jobs_by_industry"),
  trend_model(by_size, "segment", "establishments", "establishments_by_size"),
  trend_model(by_size, "segment", "jobs", "jobs_by_size"))
regression$significant_at_05 <- regression$p_value < 0.05
save_csv(regression, "02_trend_regression.csv")

# 4. Compare latest-year area industry and size distribution (Objective 2).
latest_year <- max(years); latest <- dat[dat$year == latest_year, ]
area_total <- aggregate(latest[c("establishments", "jobs")], list(area = latest$area), sum)
mix <- function(column, file) {
  x <- aggregate(latest$establishments, list(area = latest$area, category = latest[[column]]), sum)
  names(x)[3] <- "establishments"
  x <- merge(x, area_total[c("area", "establishments")], by = "area", suffixes = c("", "_area"))
  x$share_pct <- 100 * x$establishments / x$establishments_area
  save_csv(x[order(x$area, -x$share_pct), ], file)
  x
}
area_industry_mix <- mix("industry", "03_latest_industry_mix_by_area.csv")
area_size_mix <- mix("size", "04_latest_business_size_mix_by_area.csv")
save_csv(area_total, "05_latest_area_totals.csv")

# Endpoint area performance and transparent assessment-screening indicators.
area_year <- aggregate(dat[c("establishments", "jobs")], list(year = dat$year, area = dat$area), sum)
first <- area_year[area_year$year == min(years), ]; last <- area_year[area_year$year == latest_year, ]
change <- merge(first, last, by = "area", suffixes = c("_first", "_latest"))
change$establishment_change_pct <- 100 * (change$establishments_latest / change$establishments_first - 1)
change$jobs_change_pct <- 100 * (change$jobs_latest / change$jobs_first - 1)
change$industry_hhi <- vapply(change$area, function(a) sum((area_industry_mix$share_pct[area_industry_mix$area == a] / 100)^2), numeric(1))
change$small_nonemploying_share_pct <- vapply(change$area, function(a) sum(area_size_mix$share_pct[area_size_mix$area == a & area_size_mix$category %in% c("Small business", "Non employing")]), numeric(1))
change$assessment_screen_rank <- rank(-change$establishment_change_pct) + rank(-change$jobs_change_pct)
save_csv(change[order(-change$assessment_screen_rank), ], "06_area_change_assessment_screen.csv")

# 5. Exploratory k-means on area-industry trajectories, summed over size groups.
# Features describe scale, log trend, volatility, 2020-21 deviation from 2019,
# and jobs intensity. Standardize first; choose k using mean silhouette (2-6).
area_ind_year <- aggregate(dat[c("establishments", "jobs")], list(year = dat$year, area = dat$area, industry = dat$industry), sum)
units <- unique(area_ind_year[c("area", "industry")])
feature_list <- lapply(seq_len(nrow(units)), function(i) {
  a <- units$area[i]; ind <- units$industry[i]
  z <- area_ind_year[area_ind_year$area == a & area_ind_year$industry == ind, ]
  z <- merge(data.frame(year = years), z, by = "year", all.x = TRUE)
  z$establishments[is.na(z$establishments)] <- 0; z$jobs[is.na(z$jobs)] <- 0
  pos <- z$establishments > 0
  slope <- if (sum(pos) >= 3) coef(lm(log(z$establishments[pos]) ~ z$year[pos]))[2] else NA_real_
  b19 <- z$establishments[z$year == 2019]; pandemic <- z$establishments[z$year %in% 2020:2021]
  data.frame(area = a, industry = ind, mean_establishments = mean(z$establishments),
    log_scale = log1p(mean(z$establishments)), annual_log_trend = slope,
    volatility_cv = if (mean(z$establishments) > 0) sd(z$establishments) / mean(z$establishments) else 0,
    pandemic_deviation_pct = if (length(b19) && b19 > 0 && length(pandemic)) 100 * (mean(pandemic) / b19 - 1) else NA_real_,
    jobs_per_establishment = sum(z$jobs) / max(1, sum(z$establishments)))
})
features <- do.call(rbind, feature_list); features <- features[complete.cases(features), ]
feature_cols <- c("log_scale", "annual_log_trend", "volatility_cv", "pandemic_deviation_pct", "jobs_per_establishment")
x <- scale(features[feature_cols]); if (any(!is.finite(x))) stop("Non-finite clustering feature; inspect the source data.")
k_values <- 2:min(6, nrow(x) - 1)
silhouette <- sapply(k_values, function(k) {
  set.seed(28); fit <- kmeans(x, centers = k, nstart = 50, iter.max = 100)
  mean(cluster::silhouette(fit$cluster, dist(x))[, 3])
})
best_k <- k_values[which.max(silhouette)]
set.seed(28); final <- kmeans(x, centers = best_k, nstart = 100, iter.max = 200)
features$cluster <- final$cluster
save_csv(features[order(features$cluster, features$area, features$industry), ], "07_area_industry_clusters.csv")
save_csv(data.frame(k = k_values, mean_silhouette = as.numeric(silhouette)), "08_cluster_selection.csv")
profile <- aggregate(features[feature_cols], list(cluster = features$cluster), mean)
profile$segment_count <- as.integer(table(factor(features$cluster, levels = profile$cluster)))
save_csv(profile, "09_cluster_profiles.csv")

# 6. Optional 2024 address-level context only. Exclude Vacant Space. Do not use
# its finer ANZSIC codes as if they were directly comparable with the 19-sector panel.
address_note <- "Address-level CSV absent; cross-check skipped."
if (file.exists(address_path)) {
  addr <- read_utf8(address_path)
  needed <- c("census_year", "clue_small_area", "industry_anzsic4_description")
  if (all(needed %in% names(addr))) {
    addr <- addr[addr$census_year == latest_year, ]
    addr$active <- !grepl("^Vacant Space$", trimws(addr$industry_anzsic4_description), ignore.case = TRUE)
    active_n <- aggregate(addr$active, list(area = addr$clue_small_area), sum); names(active_n)[2] <- "active_address_rows_ex_vacant"
    vacant_n <- aggregate(!addr$active, list(area = addr$clue_small_area), sum); names(vacant_n)[2] <- "vacant_space_rows"
    save_csv(merge(active_n, vacant_n, by = "area", all = TRUE), "10_address_2024_context.csv")
    address_note <- sprintf("Address CSV 2024 rows=%d; active excluding Vacant Space=%d; vacant-space=%d.", nrow(addr), sum(addr$active), sum(!addr$active))
  } else address_note <- "Address-level CSV headers differ from expected fields; cross-check skipped."
}

# 7. Plots.
png(file.path(out_dir, "annual_totals.png"), width = 1400, height = 650, res = 130)
par(mfrow = c(1, 2), mar = c(4.5, 4.8, 3, 1))
plot(annual$year, annual$establishments, type = "b", pch = 19, col = "#176B87", xlab = "Year", ylab = "Active establishments", main = "Establishments, 2002-2024")
plot(annual$year, annual$jobs, type = "b", pch = 19, col = "#B5542B", xlab = "Year", ylab = "Jobs", main = "Jobs, 2002-2024"); dev.off()
png(file.path(out_dir, "area_establishment_change.png"), width = 1200, height = 700, res = 130)
o <- order(change$establishment_change_pct); par(mar = c(5, 13, 3, 1))
barplot(change$establishment_change_pct[o], names.arg = change$area[o], horiz = TRUE, las = 1,
  col = ifelse(change$establishment_change_pct[o] < 0, "#B5542B", "#176B87"),
  xlab = "Change in establishments (%)", main = paste(min(years), "to", latest_year)); abline(v = 0, lty = 2); dev.off()

# 8. Generated text summary tied to the supplied research question.
overall_est <- 100 * (tail(annual$establishments, 1) / annual$establishments[1] - 1)
overall_jobs <- 100 * (tail(annual$jobs, 1) / annual$jobs[1] - 1)
ind_est <- regression[regression$metric == "establishments_by_industry", ]
ind_est_up <- ind_est[which.max(ind_est$annual_trend_pct), ]
ind_est_down <- ind_est[which.min(ind_est$annual_trend_pct), ]
small_trend <- regression[regression$metric == "establishments_by_size" & regression$segment == "Small business", ]
cat("Descriptive business trends, regression and clustering\n",
  "Period: ", min(years), "-", latest_year, " | Areas: ", length(areas),
  " | Industries: ", length(unique(dat$industry)), " | Size groups: ", length(unique(dat$size)), "\n\n",
  "Key question: active establishments changed ", round(overall_est, 1), "% and jobs changed ", round(overall_jobs, 1), "% overall.\n",
  "Worst endpoint establishment change: ", change$area[which.min(change$establishment_change_pct)],
  " (", round(min(change$establishment_change_pct), 1), "%); best: ", change$area[which.max(change$establishment_change_pct)],
  " (", round(max(change$establishment_change_pct), 1), "%).\n\n",
  "Subquestions: use 06 for area changes; filter 02 for industries and sizes; use 03-04 for each area's latest mix.\n",
  "Industry signals: fastest fitted establishment trend = ", ind_est_up$segment, " (", round(ind_est_up$annual_trend_pct, 1), "%/year); slowest = ", ind_est_down$segment, " (", round(ind_est_down$annual_trend_pct, 1), "%/year).\n",
  "Business-size signal: Small business establishments changed ", round(small_trend$endpoint_change_pct, 1), "% end-to-end; fitted trend ", round(small_trend$annual_trend_pct, 2), "%/year (p=", signif(small_trend$p_value, 3), ").\n",
  "Regression: log-linear segment-specific time trends; annual_trend_pct is compound annual change. CI/p-values use Newey-West HAC standard errors for serial correlation; they are not causal evidence.\n",
  "Clustering: standardized scale, trend, volatility, 2020-21 deviation and jobs intensity; k chosen by highest mean silhouette among 2-6 (k=", best_k,
  "). Treat clusters as exploratory profiles and inspect 07-09 before naming them.\n",
  "Assessment screen rank combines weak long-run establishment and job change; it prioritizes follow-up only, not projects, budgets, or policy outcomes.\n",
  address_note, "\nDuplicate source cells aggregated: ", duplicated_cells, "\n", sep = "",
  file = file.path(out_dir, "README_findings.txt"))
cat("Completed. Results: ", normalizePath(out_dir), "\n", sep = "")
