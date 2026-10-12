# =============================================================================
# Plots for final.R - plots for the final presentation deck
# =============================================================================

# ---- 0. Setup (run once at the top of the script) ---------------------------
library(tidyverse)
library(scales)
library(here)

# Colours matched to the deck (navy for growth, burnt orange for the COVID fall)
navy    <- "#14213D"
orange  <- "#B04516"
blue    <- "#2B5C9E"
muted   <- "#5F6673"
gridcol <- "#E2DDD0"

# Every final plot goes into plots/final/ with a white background
dir.create(here("plots", "final"), recursive = TRUE, showWarnings = FALSE)
save_final <- function(p, name, w = 10.4, h = 6) {
  ggsave(here("plots", "final", paste0(name, ".png")),
         plot = p, width = w, height = h, dpi = 300, bg = "white")
}

# Load the cleaned table saved at the end of workspace.Rmd
# (columns: census_year, clue_small_area, anzsic_indusrty, business_size,
#  total_active_establishments, total_jobs, jobs_suppressed)
rds_path <- here("data", "df_small_clean.rds")
if (file.exists(rds_path)) {
  df_small_clean <- readRDS(rds_path)
} else {
  # Fallback: rebuild what we need straight from the CSV
  df_small_clean <- read.csv(here("business-establishments-and-jobs-data-by-business-size-and-anzsic.csv")) |>
    janitor::clean_names() |>
    rename(total_active_establishments = total_establishments)
}

# census_year is a Date in workspace.Rmd; turn it into a plain number (2002, 2003, ...)
df <- df_small_clean |>
  mutate(year = if (inherits(census_year, "Date")) as.integer(format(census_year, "%Y"))
         else as.integer(census_year))


# ---- Plot 1: city-wide active establishments, 2002-2024 (slide 2) -----------
citywide <- df |>
  group_by(year) |>
  summarise(estabs = sum(total_active_establishments, na.rm = TRUE), .groups = "drop")

# Sanity check against the deck: 12,139 / 16,768 / 13,828 / 14,338
citywide |> filter(year %in% c(2002, 2018, 2022, 2024))

key <- citywide |> filter(year %in% c(2002, 2018, 2022, 2024))
v   <- setNames(key$estabs, key$year)

p_citywide <- ggplot(citywide, aes(year, estabs)) +
  # 2002–2018 in navy, 2018–2024 in orange (the two segments share the 2018 point)
  geom_line(data = filter(citywide, year <= 2018), colour = navy,   linewidth = 1.4, lineend = "round") +
  geom_line(data = filter(citywide, year >= 2018), colour = orange, linewidth = 1.4, lineend = "round") +
  geom_point(data = key, aes(colour = year >= 2018), size = 3.2, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = navy, `TRUE` = orange)) +
  # Labels on the key points
  annotate("text", x = 2018, y = v["2018"] + 380, label = paste0("2018 peak: ", comma(v["2018"])),
           colour = navy, fontface = "bold", size = 4.2, hjust = 1) +
  annotate("text", x = 2022, y = v["2022"] - 420, label = paste0("2022 low: ", comma(v["2022"])),
           colour = orange, fontface = "bold", size = 4.2) +
  annotate("text", x = 2024, y = v["2024"] + 420, label = paste0("2024: ", comma(v["2024"])),
           colour = muted, size = 4, hjust = 0.8) +
  # Change annotations
  annotate("text", x = 2011, y = 13300,
           label = paste0("+", comma(v["2018"] - v["2002"]), " establishments\n2002 to 2018"),
           colour = blue, fontface = "bold", size = 4.6, hjust = 0, lineheight = 0.95) +
  annotate("text", x = 2021.2, y = 16600,
           label = paste0("-", comma(v["2018"] - v["2022"]), "\nin 4 years"),
           colour = orange, fontface = "bold", size = 4.6, hjust = 0, lineheight = 0.95) +
  scale_x_continuous(breaks = c(2002, 2006, 2010, 2014, 2018, 2022, 2024),
                     expand = expansion(add = c(0.6, 0.9))) +
  scale_y_continuous(labels = comma, limits = c(11500, 18000),
                     breaks = seq(12000, 18000, 2000)) +
  labs(title = "Active (Non-vacant) Establishment Count from 2002-2024",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = gridcol, linewidth = 0.5),
    axis.text  = element_text(colour = muted),
    plot.title = element_text(face = "bold", colour = navy, size = 18, margin = margin(b = 12)),
    plot.caption = element_text(colour = muted, size = 9, hjust = 0),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(16, 20, 12, 12)
  )

p_citywide
save_final(p_citywide, "01_citywide_active_establishments")

# ---- Plot 2: all business addresses vs active establishments (slide 3) ------
# Needs the address-level file. Only census_year is read, so it loads quickly.
# (If you already have df_large_raw loaded from workspace.Rmd, you can skip the read_csv.)
df_large_years <- read_csv(
  here("business-establishments-with-address-and-industry-classification.csv"),
  col_select = census_year, show_col_types = FALSE
)

addr_vs_active <- df_large_years |>
  count(census_year, name = "all_addresses") |>                 # every address, incl. vacant
  rename(year = census_year) |>
  mutate(year = as.integer(year)) |>
  left_join(citywide |> rename(active = estabs), by = "year")   # citywide comes from Plot 1

# Sanity check: 2024 active should be 14,338
addr_vs_active |> filter(year %in% c(2002, 2024))

end_pts <- addr_vs_active |> filter(year == max(year))

p_addr_vs_active <- ggplot(addr_vs_active, aes(year)) +
  # Shaded gap = vacant premises
  geom_ribbon(aes(ymin = active, ymax = all_addresses), fill = gridcol, alpha = 0.9) +
  geom_line(aes(y = all_addresses), colour = muted, linewidth = 1.1) +
  geom_line(aes(y = active), colour = blue, linewidth = 1.3) +
  # Direct labels instead of a legend
  annotate("text", x = 2024.4, y = end_pts$all_addresses, label = "All\naddresses",
           colour = muted, fontface = "bold", size = 3.6, hjust = 0, lineheight = 0.9) +
  annotate("text", x = 2024.4, y = end_pts$active, label = "Active",
           colour = blue, fontface = "bold", size = 3.6, hjust = 0) +
  annotate("text", x = 2021, y = (end_pts$all_addresses + end_pts$active) / 2 + 400,
           label = "Vacant", colour = muted, fontface = "italic", size = 3.6) +
  scale_x_continuous(breaks = c(2002, 2008, 2013, 2018, 2024),
                     expand = expansion(add = c(0.5, 0.5))) +
  coord_cartesian(clip = "off") +                 # let end labels sit outside the panel
  scale_y_continuous(labels = comma, breaks = seq(12000, 20000, 4000)) +
  labs(title = "The aggregated file counts active businesses only",
       subtitle = "All business addresses vs active establishments, 2002-2024",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = gridcol, linewidth = 0.4),
    axis.text  = element_text(colour = muted),
    plot.title = element_text(face = "bold", colour = navy, size = 14),
    plot.subtitle = element_text(colour = muted, size = 10.5, margin = margin(b = 8)),
    plot.title.position = "plot",
    plot.margin = margin(10, 95, 8, 8)            # right margin holds the end labels
  )

p_addr_vs_active
save_final(p_addr_vs_active, "02_addresses_vs_active", w = 6.4, h = 3.3)


# ---- Plot 3: share of rows with jobs suppressed, stacked to 100% (slide 3) --
# Non-employing excluded: their missing jobs are genuine zeros, never suppressed
suppression <- df |>
  filter(business_size != "Non employing") |>
  group_by(business_size) |>
  summarise(Suppressed = mean(is.na(total_jobs)), .groups = "drop") |>
  mutate(Reported = 1 - Suppressed,
         business_size = str_remove(business_size, " business"),
         business_size = factor(business_size, levels = c("Small", "Medium", "Large"))) |>
  pivot_longer(c(Suppressed, Reported), names_to = "status", values_to = "share") |>
  mutate(status = factor(status, levels = c("Reported", "Suppressed")))   # Suppressed drawn first (left)

# Sanity check: Large 59%, Medium 31%, Small 14%
suppression |> filter(status == "Suppressed")

p_suppression <- ggplot(suppression, aes(share, business_size, fill = status)) +
  geom_col(width = 0.62) +
  geom_text(data = filter(suppression, status == "Suppressed"),
            aes(label = percent(share, accuracy = 1)),
            hjust = -0.25, colour = navy, fontface = "bold", size = 3.8,
            position = position_identity()) +
  scale_fill_manual(values = c(Suppressed = orange, Reported = gridcol),
                    breaks = c("Suppressed", "Reported"),
                    labels = c("Jobs suppressed", "Jobs reported")) +
  scale_x_continuous(labels = percent, expand = expansion(mult = c(0, 0.02))) +
  labs(title = "A significant amount of Jobs data is missing",
       subtitle = "Share of rows with total_jobs suppressed (cells of 2 or fewer establishments)",
       x = NULL, y = NULL, fill = NULL,
       caption = "Excludes non-employing businesses (zero jobs by definition)") +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.grid = element_blank(),
    axis.text.y = element_text(colour = navy, face = "bold", size = 12),
    axis.text.x = element_text(colour = muted),
    legend.position = "top", legend.justification = "left",
    legend.text = element_text(colour = muted, size = 10),
    legend.key.size = unit(10, "pt"),
    legend.margin = margin(0, 0, 0, 0),
    plot.title = element_text(face = "bold", colour = navy, size = 14),
    plot.subtitle = element_text(colour = muted, size = 10.5),
    plot.caption = element_text(colour = muted, size = 8.5, hjust = 0),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(10, 12, 8, 8)
  )

p_suppression
save_final(p_suppression, "03_jobs_suppression", w = 6.4, h = 3.0)

# ---- Shared helper: annualised log growth (same formula as Clustering.Rmd) --
ann_log_change <- function(start, end, years) (log(end + 1) - log(start + 1)) / years
to_pct_yr      <- function(g) (exp(g) - 1) * 100          # log rate -> % per year


# ---- Plot 4: city-wide growth in each of the 3 windows (slide 5) ------------
# One bar per window, on the same scale the classification rules use (+/-1% a year = flat)
v <- setNames(citywide$estabs, citywide$year)             # citywide comes from Plot 1

windows <- tibble(
  window = c("Baseline\n2002-18", "COVID shock\n2018-22", "Recovery\n2022-24"),
  start  = c(2002, 2018, 2022),
  end    = c(2018, 2022, 2024)
) |>
  mutate(
    n_start = v[as.character(start)],
    n_end   = v[as.character(end)],
    growth  = ann_log_change(n_start, n_end, end - start) * 100,   # log % a year, as in the rules
    change  = n_end - n_start,
    window  = factor(window, levels = window),
    label   = paste0(sprintf("%+.1f%%", growth), " a year\n",
                     comma(n_start), " to ", comma(n_end))
  )

# Sanity check: +2.0, -4.8, +1.8 (% a year)
windows |> select(window, n_start, n_end, growth)

p_windows <- ggplot(windows, aes(window, growth, fill = growth < 0)) +
  geom_blank() +                                   # sets the discrete x scale before the band
  # The +/-1% "flat" band from the classification rules
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -1, ymax = 1, fill = gridcol, alpha = 0.7) +
  annotate("text", x = 2.5, y = 0, label = "flat band\n(+/-1% a year)",          # sits between bars 2 and 3
           colour = muted, size = 3.4, lineheight = 0.9) +
  geom_hline(yintercept = 0, colour = muted, linewidth = 0.4) +
  geom_col(width = 0.55) +
  geom_text(aes(label = label,
                vjust = if_else(growth < 0, 1.25, -0.35)),
            colour = navy, size = 4, lineheight = 0.95, fontface = "bold") +
  scale_fill_manual(values = c(`FALSE` = blue, `TRUE` = orange), guide = "none") +
  scale_y_continuous(labels = \(x) paste0(x, "%"), limits = c(-6.8, 4.5),
                     breaks = seq(-6, 4, 2)) +
  labs(title = "Active Establishments across 3 Time Windows",
       subtitle = "City-wide annualised growth in active establishments, by window",
       x = NULL, y = NULL,
       caption = "Growth = annualised log change between window endpoints. Source: City of Melbourne CLUE, aggregated file") +
  theme_minimal(base_size = 14) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = gridcol, linewidth = 0.4),
    axis.text.x = element_text(colour = navy, face = "bold", size = 13, lineheight = 0.95),
    axis.text.y = element_text(colour = muted),
    plot.title = element_text(face = "bold", colour = navy, size = 18),
    plot.subtitle = element_text(colour = muted, size = 12, margin = margin(b = 10)),
    plot.caption = element_text(colour = muted, size = 9, hjust = 0),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(16, 20, 12, 12)
  )

p_windows
save_final(p_windows, "04_three_windows", w = 10.4, h = 6)


# ---- Shared helper: annualised log growth (same formula as Clustering.Rmd) --
ann_log_change <- function(start, end, years) (log(end + 1) - log(start + 1)) / years
to_pct_yr      <- function(g) (exp(g) - 1) * 100          # log rate -> % per year


# ---- Plot 4: line with 3 windows shaded, growth bars underneath (slide 5) ---
# Needs patchwork (already loaded in workspace.Rmd)
library(patchwork)
v <- setNames(citywide$estabs, citywide$year)             # citywide comes from Plot 1

windows <- tibble(
  window = c("Baseline\n2002-18", "COVID shock\n2018-22", "Recovery\n2022-24"),
  start  = c(2002, 2018, 2022),
  end    = c(2018, 2022, 2024)
) |>
  mutate(
    n_start = v[as.character(start)],
    n_end   = v[as.character(end)],
    growth  = ann_log_change(n_start, n_end, end - start) * 100,   # log % a year, as in the rules
    change  = n_end - n_start,
    window  = factor(window, levels = window),
    label   = paste0(sprintf("%+.1f%%", growth), " a year\n",
                     comma(n_start), " to ", comma(n_end))
  )

# Sanity check: +2.0, -4.8, +1.8 (% a year)
windows |> select(window, n_start, n_end, growth)

# Window colours: baseline/recovery blue tint, shock orange tint
win_fill <- c(blue, orange, blue)
win_alpha <- c(0.07, 0.13, 0.16)

# Shared x scale so the line and the bars line up year for year
x_scale <- scale_x_continuous(limits = c(2001.5, 2024.5), breaks = c(2002, 2010, 2018, 2022, 2024),
                              expand = expansion(0))

base_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = gridcol, linewidth = 0.4),
    axis.text = element_text(colour = muted)
  )

# -- Top panel: the line, with the three windows shaded --
p_line <- ggplot(citywide, aes(year, estabs)) +
  annotate("rect", xmin = windows$start, xmax = windows$end, ymin = -Inf, ymax = Inf,
           fill = win_fill, alpha = win_alpha) +
  geom_line(colour = navy, linewidth = 1.3, lineend = "round") +
  geom_point(data = filter(citywide, year %in% c(2018, 2022)),
             aes(colour = year == 2022), size = 3, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = navy, `TRUE` = orange)) +
  # Window names along the top
  annotate("text", x = windows$start + 0.25, y = 17900, hjust = 0, vjust = 1,
           label = c("Baseline 2002-18", "Shock\n2018-22", "Recovery\n2022-24"),
           colour = c(blue, orange, blue), fontface = "bold", size = 3.6, lineheight = 0.9) +
  annotate("text", x = 2017.6, y = v["2018"] + 120, label = paste0("Peak: ", comma(v["2018"])),
           colour = navy, size = 3.6, hjust = 1, vjust = 0) +
  annotate("text", x = 2022, y = v["2022"] - 300, label = paste0("Low since 2007: ", comma(v["2022"])),
           colour = orange, size = 3.6, hjust = 0.75, vjust = 1) +
  x_scale +
  scale_y_continuous(labels = comma, limits = c(11500, 18000), breaks = seq(12000, 18000, 2000)) +
  labs(title = "Active Establishments across 3 Time Windows",
       subtitle = "Active establishments (top) and annualised growth in each window (bottom)",
       x = NULL, y = NULL) +
  base_theme +
  theme(axis.text.x = element_blank())          # years shown once, under the bars

# -- Bottom panel: one bar per window, as wide as the window --
p_bars <- ggplot(windows) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -1, ymax = 1, fill = gridcol, alpha = 0.7) +
  geom_hline(yintercept = 0, colour = muted, linewidth = 0.4) +
  geom_rect(aes(xmin = start + 0.15, xmax = end - 0.15, ymin = 0, ymax = growth,
                fill = growth < 0)) +
  geom_text(aes(x = (start + end) / 2, y = growth, label = sprintf("%+.1f%%/yr", growth),
                vjust = if_else(growth < 0, 1.3, -0.4)),
            colour = navy, fontface = "bold", size = 3.8) +
  annotate("text", x = 2002.3, y = -1.3, label = "Grey band = flat (+/-1% a year)", colour = muted,
           size = 3.2, hjust = 0, vjust = 1) +               # sits just under the band
  scale_fill_manual(values = c(`FALSE` = blue, `TRUE` = orange), guide = "none") +
  x_scale +
  scale_y_continuous(labels = \(x) paste0(x, "%"), limits = c(-7, 4), breaks = c(-4, 0, 2)) +
  labs(x = NULL, y = NULL,
       caption = "Growth = annualised log change between window endpoints. Source: City of Melbourne CLUE, aggregated file") +
  base_theme +
  theme(plot.caption = element_text(colour = muted, size = 8.5, hjust = 0),
        plot.caption.position = "plot")

# -- Stack them (patchwork), line taller than bars --
p_windows <- (p_line / p_bars) +
  plot_layout(heights = c(2.2, 1)) &
  theme(plot.title = element_text(face = "bold", colour = navy, size = 17),
        plot.subtitle = element_text(colour = muted, size = 11, margin = margin(b = 8)),
        plot.title.position = "plot",
        plot.margin = margin(8, 16, 6, 10))

p_windows
save_final(p_windows, "04_three_windows", w = 10.4, h = 6.4)


# ---- Shared helper: annualised log growth (same formula as Clustering.Rmd) --
ann_log_change <- function(start, end, years) (log(end + 1) - log(start + 1)) / years
to_pct_yr      <- function(g) (exp(g) - 1) * 100          # log rate -> % per year


# ---- Plot 4: line with 3 windows shaded, growth bars underneath (slide 5) ---
# Needs patchwork (already loaded in workspace.Rmd)
library(patchwork)
v <- setNames(citywide$estabs, citywide$year)             # citywide comes from Plot 1

windows <- tibble(
  window = c("Baseline\n2002-18", "COVID shock\n2018-22", "Recovery\n2022-24"),
  start  = c(2002, 2018, 2022),
  end    = c(2018, 2022, 2024)
) |>
  mutate(
    n_start = v[as.character(start)],
    n_end   = v[as.character(end)],
    growth  = ann_log_change(n_start, n_end, end - start) * 100,   # log % a year, as in the rules
    change  = n_end - n_start,
    window  = factor(window, levels = window),
    label   = paste0(sprintf("%+.1f%%", growth), " a year\n",
                     comma(n_start), " to ", comma(n_end))
  )

# Sanity check: +2.0, -4.8, +1.8 (% a year)
windows |> select(window, n_start, n_end, growth)

# Window colours: baseline/recovery blue tint, shock orange tint
win_fill <- c(blue, orange, blue)
win_alpha <- c(0.07, 0.13, 0.16)

# Shared x scale so the line and the bars line up year for year
x_scale <- scale_x_continuous(limits = c(2001.5, 2024.5), breaks = c(2002, 2010, 2018, 2022, 2024),
                              expand = expansion(0))

base_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = gridcol, linewidth = 0.4),
    axis.text = element_text(colour = muted)
  )

# -- Top panel: the line, with the three windows shaded --
p_line <- ggplot(citywide, aes(year, estabs)) +
  annotate("rect", xmin = windows$start, xmax = windows$end, ymin = -Inf, ymax = Inf,
           fill = win_fill, alpha = win_alpha) +
  geom_line(colour = navy, linewidth = 1.3, lineend = "round") +
  geom_point(data = filter(citywide, year %in% c(2018, 2022)),
             aes(colour = year == 2022), size = 3, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = navy, `TRUE` = orange)) +
  # Window names along the top
  annotate("text", x = windows$start + 0.25, y = 17900, hjust = 0, vjust = 1,
           label = c("Baseline 2002-18", "Shock\n2018-22", "Recovery\n2022-24"),
           colour = c(blue, orange, blue), fontface = "bold", size = 3.6, lineheight = 0.9) +
  annotate("text", x = 2017.6, y = v["2018"] + 120, label = paste0("Peak: ", comma(v["2018"])),
           colour = navy, size = 3.6, hjust = 1, vjust = 0) +
  annotate("text", x = 2022, y = v["2022"] - 300, label = paste0("Low since 2007: ", comma(v["2022"])),
           colour = orange, size = 3.6, hjust = 0.75, vjust = 1) +
  x_scale +
  scale_y_continuous(labels = comma, limits = c(11500, 18000), breaks = seq(12000, 18000, 2000)) +
  labs(title = "Active Establishments across 3 Time Windows",
       subtitle = "Active establishments (top) and annualised growth in each window (bottom)",
       x = NULL, y = NULL) +
  base_theme +
  theme(axis.text.x = element_blank())          # years shown once, under the bars

# -- Bottom panel: one bar per window, as wide as the window --
p_bars <- ggplot(windows) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -1, ymax = 1, fill = gridcol, alpha = 0.7) +
  geom_hline(yintercept = 0, colour = muted, linewidth = 0.4) +
  geom_rect(aes(xmin = start + 0.15, xmax = end - 0.15, ymin = 0, ymax = growth,
                fill = growth < 0)) +
  geom_text(aes(x = (start + end) / 2, y = growth, label = sprintf("%+.1f%%/yr", growth),
                vjust = if_else(growth < 0, 1.3, -0.4)),
            colour = navy, fontface = "bold", size = 3.8) +
  annotate("text", x = 2002.3, y = -1.3, label = "Grey band = flat (+/-1% a year)", colour = muted,
           size = 3.2, hjust = 0, vjust = 1) +               # sits just under the band
  scale_fill_manual(values = c(`FALSE` = blue, `TRUE` = orange), guide = "none") +
  x_scale +
  scale_y_continuous(labels = \(x) paste0(x, "%"), limits = c(-7, 4), breaks = c(-4, 0, 2)) +
  labs(x = NULL, y = NULL,
       caption = "Growth = annualised log change between window endpoints. Source: City of Melbourne CLUE, aggregated file") +
  base_theme +
  theme(plot.caption = element_text(colour = muted, size = 8.5, hjust = 0),
        plot.caption.position = "plot")

# -- Stack them (patchwork), line taller than bars --
p_windows <- (p_line / p_bars) +
  plot_layout(heights = c(2.2, 1)) &
  theme(plot.title = element_text(face = "bold", colour = navy, size = 17),
        plot.subtitle = element_text(colour = muted, size = 11, margin = margin(b = 8)),
        plot.title.position = "plot",
        plot.margin = margin(8, 16, 6, 10))

p_windows
save_final(p_windows, "04_three_windows", w = 8.4, h = 6.4)


# ---- Plot 5: GFC vs COVID dip test, stacked to 19 industries (slide 5) ------
# Replicates section 2b of Clustering.Rmd (step1_gfc_window_test.Rmd):
# a window "dips" for an industry if its growth is lower than BOTH windows either side.
ind_year <- df |>
  group_by(anzsic_indusrty, year) |>
  summarise(est = sum(total_active_establishments, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = year, values_from = est, names_prefix = "y", values_fill = 0)

dip_test <- ind_year |>
  mutate(g_0208 = ann_log_change(y2002, y2008, 6),
         g_0812 = ann_log_change(y2008, y2012, 4),
         g_1218 = ann_log_change(y2012, y2018, 6),
         g_1822 = ann_log_change(y2018, y2022, 4),
         g_2224 = ann_log_change(y2022, y2024, 2),
         gfc_dip   = g_0812 < g_0208 & g_0812 < g_1218,
         covid_dip = g_1822 < g_1218 & g_1822 < g_2224)

dips <- tibble(
  shock = c("COVID\n2018-22", "GFC\n2008-12"),
  dipped = c(sum(dip_test$covid_dip), sum(dip_test$gfc_dip))
) |>
  mutate(did_not = nrow(dip_test) - dipped,
         shock = factor(shock, levels = rev(shock))) |>          # COVID on top
  pivot_longer(c(dipped, did_not), names_to = "status", values_to = "n") |>
  mutate(status = factor(status, levels = c("did_not", "dipped")))

# Sanity check: COVID 15, GFC 7 (of 19)
dips |> filter(status == "dipped")

p_dips <- ggplot(dips, aes(n, shock, fill = interaction(status, shock))) +
  geom_col(width = 0.6) +
  # Chance level: about 1 in 3 windows is lowest by chance (~6 of 19)
  geom_vline(xintercept = 19 / 3, colour = navy, linewidth = 0.7, linetype = "dashed") +
  annotate("text", x = 19 / 3 + 0.5, y = 2.48, label = "Chance Threshold (~6)",
           colour = navy, size = 3.6, hjust = 0, fontface = "bold") +
  geom_text(data = filter(dips, status == "dipped"),
            aes(x = n, label = paste0(n, "/19")),
            hjust = -0.2, colour = navy, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("dipped.COVID\n2018-22" = orange, "dipped.GFC\n2008-12" = "#9AA3AF",
                               "did_not.COVID\n2018-22" = gridcol, "did_not.GFC\n2008-12" = gridcol),
                    guide = "none") +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 19), limits = c(0, 19),
                     expand = expansion(mult = c(0, 0.01))) +
  scale_y_discrete(expand = expansion(add = c(0.5, 0.8))) +     # headroom for the line labels
  labs(title = "Only COVID's dip spread across most industries",
       subtitle = "Industries whose growth dipped below the windows either side (of 19)",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.grid = element_blank(),
    axis.text.y = element_text(colour = navy, face = "bold", size = 12, lineheight = 0.95),
    axis.text.x = element_text(colour = muted),
    plot.title = element_text(face = "bold", colour = navy, size = 14),
    plot.subtitle = element_text(colour = muted, size = 10.5),
    plot.title.position = "plot",
    plot.margin = margin(10, 14, 8, 8)
  )

p_dips
save_final(p_dips, "05_gfc_vs_covid_dips", w = 6.2, h = 3.0)

# ---- Shared theme for plots 6-8 (slides 11-13) -----------------------------
slide_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(colour = navy, face = "bold", size = 12, hjust = 0),
    plot.title = element_text(face = "bold", colour = navy, size = 14),
    plot.subtitle = element_text(colour = muted, size = 10.5, margin = margin(b = 8)),
    plot.caption = element_text(colour = muted, size = 8.5, hjust = 0),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(10, 16, 8, 8)
  )

# Totals for any grouping at 2018 / 2022 / 2024
totals_by <- function(group_col) {
  df |>
    filter(year %in% c(2018, 2022, 2024)) |>
    group_by({{ group_col }}, year) |>
    summarise(est = sum(total_active_establishments, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = year, values_from = est, names_prefix = "y")
}


# ---- Plot 6: change by business size, 2018 -> 2024 (slide 11) ---------------
by_size <- totals_by(business_size) |>
  mutate(change = y2024 - y2018,
         pct    = (y2024 / y2018 - 1) * 100,
         size   = str_remove(business_size, " business"),
         label_y = paste0(size, "\n", if_else(change > 0, "+", ""), comma(change), " businesses"),
         label_y = fct_reorder(label_y, pct, .desc = TRUE))   # Small at the top

# Sanity check: Small -20.4% (-2,616), Large +20.0% (+71)
by_size |> select(size, y2018, y2024, change, pct)

p_size <- ggplot(by_size, aes(pct, label_y, fill = pct < 0)) +
  geom_col(width = 0.6) +
  geom_vline(xintercept = 0, colour = navy, linewidth = 0.7) +
  geom_text(aes(label = sprintf("%+.1f%%", pct), hjust = if_else(pct < 0, 1.15, -0.15)),
            colour = if_else(by_size$pct < 0, orange, blue), fontface = "bold", size = 4.6) +
  scale_fill_manual(values = c(`FALSE` = blue, `TRUE` = orange), guide = "none") +
  scale_x_continuous(limits = c(-29, 29)) +
  labs(title = "Change in active establishments by business size",
       subtitle = "2018 to 2024, all CLUE areas",
       x = NULL, y = NULL) +
  slide_theme +
  theme(axis.text.y = element_text(colour = navy, size = 12, hjust = 0, lineheight = 1.05))

p_size
save_final(p_size, "06_change_by_size", w = 7, h = 4.4)


# ---- Plot 7: share of the 2018 level back, by industry (slide 12) -----------
# Grey bar = 2018 level (100%); fill = 2024 level; black tick = 2022 level
focus <- c("Accommodation and Food Services"                 = "Accommodation & Food\nRecovering",
           "Retail Trade"                                    = "Retail\n77% Recovering",
           "Professional, Scientific and Technical Services" = "Professional Services\n75% No recovery yet",
           "Financial and Insurance Services"                = "Finance & Insurance\n91% No recovery yet")

by_ind <- totals_by(anzsic_indusrty) |>
  filter(anzsic_indusrty %in% names(focus)) |>
  mutate(back_2022 = y2022 / y2018 * 100,
         back_2024 = y2024 / y2018 * 100,
         rising    = back_2024 > back_2022,
         label_y   = factor(focus[anzsic_indusrty], levels = rev(focus)),
         note      = paste0(sprintf("%.1f%%", back_2024),
                            if_else(rising, " (up", " (down"),
                            sprintf(" from %.1f%%)", back_2022)))

# Sanity check: A&F 98.2, Retail 81.9, Prof 75.0, Finance 74.7 (% of 2018 in 2024)
by_ind |> select(anzsic_indusrty, y2018, y2022, y2024, back_2022, back_2024)

p_industry <- ggplot(by_ind, aes(y = label_y)) +
  geom_col(aes(x = 100), fill = gridcol, width = 0.6) +                      # 2018 level
  geom_col(aes(x = back_2024, fill = interaction(rising, anzsic_indusrty == "Accommodation and Food Services")),
           width = 0.6) +
  geom_errorbar(aes(xmin = back_2022, xmax = back_2022), width = 0.8,      # 2022 tick
                colour = navy, linewidth = 1.1) +
  geom_text(aes(x = 101.5, label = note), hjust = 0, colour = navy, size = 3.6, fontface = "bold") +
  scale_fill_manual(values = c("TRUE.TRUE" = blue, "TRUE.FALSE" = "#F0A47A",
                               "FALSE.FALSE" = "#D9622B"), guide = "none") +
  scale_x_continuous(limits = c(0, 168), expand = expansion(0)) +
  labs(title = "Class shares from the City Planning track",
       subtitle =
         "Share of the 2018 level back by 2024 (grey = 2018 level, black tick = 2022)",
       x = NULL, y = NULL) +
  slide_theme +
  theme(axis.text.y = element_text(colour = navy, size = 11.5, hjust = 0, lineheight = 1.05))

p_industry
save_final(p_industry, "07_industry_back_to_2018", w = 7, h = 4.4)


# ---- Plot 8: change by area, 2018 -> 2024, with change since 2022 (slide 13)
by_area <- totals_by(clue_small_area) |>
  mutate(change = y2024 - y2018, since22 = y2024 - y2022)

top_areas <- by_area |> slice_min(change, n = 7) |> pull(clue_small_area)   # 7 biggest losses

area_plot <- by_area |>
  mutate(group = if_else(clue_small_area %in% top_areas, clue_small_area, "6 other areas")) |>
  group_by(group) |>
  summarise(change = sum(change), since22 = sum(since22), .groups = "drop") |>
  mutate(group = str_replace(group, "Melbourne \\(CBD\\)", "Melbourne CBD"),
         group = fct_reorder(group, change, .desc = TRUE),
         group = fct_relevel(group, "6 other areas"),                         # keep "other" at the bottom
         since_lab = case_when(since22 <= 0 ~ paste0(since22, ", falling"),
                               since22 <= 5 ~ paste0("+", since22, ", flat"),
                               TRUE         ~ paste0("+", since22)),
         stalled = since22 <= 5)

# Sanity check: CBD -1,568 (+240 since 2022); city total -2,430
area_plot
sum(by_area$change)

p_area <- ggplot(area_plot, aes(change, group)) +
  geom_col(aes(fill = group == "Melbourne CBD"), width = 0.65) +
  geom_text(aes(label = comma(change)), hjust = 1.1, colour = navy, fontface = "bold", size = 3.9) +
  geom_text(aes(x = 120, label = since_lab, colour = stalled), hjust = 0, fontface = "bold", size = 3.9) +
  annotate("text", x = 120, y = length(levels(area_plot$group)) + 0.75, label = "Since 2022",
           hjust = 0, colour = muted, size = 3.6) +
  scale_fill_manual(values = c(`TRUE` = orange, `FALSE` = "#D9622B"), guide = "none") +
  scale_colour_manual(values = c(`TRUE` = orange, `FALSE` = blue), guide = "none") +
  scale_x_continuous(limits = c(-1950, 650), expand = expansion(0)) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1.1))) +
  coord_cartesian(clip = "off") +
  labs(title = "Change in active establishments, 2018 to 2024 (city total: -2,430)",
       x = NULL, y = NULL) +
  slide_theme

p_area
save_final(p_area, "08_change_by_area", w = 7.4, h = 4.4)


# ---- Plot 9: the classification on both tracks (slide 10) -------------------
# Rebuilds the rule-based classes from df, same rules as Clustering.Rmd:
#   growth = annualised log change per window; +/- beyond 1% a year, else flat;
#   rules checked in order; MIN_EST = 10 (mean establishments a year, 2002-2024).
# (Needs ann_log_change() from plots_4_5.R; defined again here in case.)
ann_log_change <- function(start, end, years) (log(end + 1) - log(start + 1)) / years
MIN_EST <- 10
FLAT    <- 0.01

classify_units <- function(cols) {
  panel <- df |>
    group_by(across(all_of(c(cols, "year")))) |>
    summarise(est = sum(total_active_establishments, na.rm = TRUE), .groups = "drop") |>
    complete(nesting(!!!syms(cols)), year = 2002:2024, fill = list(est = 0))     # missing rows = 0
  wide <- panel |>
    group_by(across(all_of(cols))) |>
    summarise(mean_est = mean(est),
              y2002 = est[year == 2002], y2018 = est[year == 2018],
              y2022 = est[year == 2022], y2024 = est[year == 2024], .groups = "drop") |>
    filter(mean_est >= MIN_EST)
  code <- function(g) case_when(g > FLAT ~ "+", g < -FLAT ~ "-", TRUE ~ "flat")
  wide |>
    mutate(g_base = ann_log_change(y2002, y2018, 16),
           g_covid = ann_log_change(y2018, y2022, 4),
           g_rec = ann_log_change(y2022, y2024, 2),
           b = code(g_base), c = code(g_covid), r = code(g_rec),
           class = case_when(
             b == "-"                                              ~ "Long-run decline",
             c == "-" & r == "+" & 2 * g_rec >= -4 * g_covid       ~ "Recovered",
             c == "-" & r == "+"                                   ~ "Recovering",
             c == "-" | (b == "+" & c == "flat" & r == "-")        ~ "No recovery yet",
             TRUE                                                  ~ "Stable or growing"))
}

classes_cp  <- classify_units(c("clue_small_area", "anzsic_indusrty"))   # City Planning track
classes_fin <- classify_units(c("clue_small_area", "business_size"))     # Financial track

class_levels <- c("Stable or growing", "Recovered", "Recovering", "No recovery yet", "Long-run decline")
class_cols   <- c("Stable or growing" = blue, "Recovered" = "#8DB3DE", "Recovering" = "#F0A47A",
                  "No recovery yet" = "#D9622B", "Long-run decline" = "#7A2318")

class_table <- bind_rows(
  classes_cp  |> mutate(track = "City Planning track (industry x area)", unit = "combos"),
  classes_fin |> mutate(track = "Financial track (size x area)",        unit = "cells")
) |>
  group_by(track, unit, class) |>
  summarise(n = n(), est = sum(mean_est), .groups = "drop_last") |>
  mutate(share = est / sum(est) * 100) |>
  ungroup() |>
  mutate(class = factor(class, levels = rev(class_levels)),
         label = paste0(sprintf("%.1f%%", share), ", ", n, " ", unit))

# Sanity check: City Planning 28 / 13 / 34 / 44 / 34 combos (8.6 / 4.8 / 39.2 / 33.5 / 14.0%)
#               Financial     15 /  6 / 12 /  8 /  5 cells  (16.7 / 1.9 / 68.2 / 9.4 / 3.8%)
class_table |> arrange(track, desc(class)) |> select(track, class, n, share)
write_csv(class_table |> select(-label), here("plots", "final", "class_table.csv"))

# Still below 2018 (Recovering + No recovery yet), for the key-point card
class_table |> filter(class %in% c("Recovering", "No recovery yet")) |>
  group_by(track) |> summarise(still_below_2018 = sum(share))

p_classes <- ggplot(class_table, aes(share, class, fill = class)) +
  geom_col(aes(x = 100), fill = gridcol, width = 0.65) +             # grey track = 100% of establishments
  geom_col(width = 0.65) +
  geom_text(aes(label = label), hjust = -0.08, colour = navy, size = 3.8, fontface = "bold") +
  facet_wrap(~ track, nrow = 1) +
  scale_fill_manual(values = class_cols, guide = "none") +
  scale_x_continuous(limits = c(0, 100), expand = expansion(0)) +
  labs(
       x = NULL, y = NULL,
       caption = paste0("153 industry x area combos and 46 size x area cells averaging 10+ establishments a year. ")) +
  slide_theme +
  theme(strip.text = element_text(face = "bold", colour = navy, size = 12, hjust = 0, lineheight = 0.95),
        panel.spacing.x = unit(18, "pt"))

p_classes
save_final(p_classes, "09_classes_both_tracks", w = 18.4, h = 4.6)
