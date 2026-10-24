# CMCE30005 Business Analytics Challenge
## CMCE30005-Group-5 - Business establishments in Melbourne Project Brief

**Subject:** CMCE30005 Business Analytics Challenge, Semester 2 2026

**University:** University of Melbourne

**Team Members:** Yiming Pan, Ruicheng Yao, Caleb Lee

---   

## Business Problems

1. How have industries, areas and business sizes performed before, during and after COVID, by active establishments?
2. Which segments are of most concern to City of Melbourne policymakers now?

---

## Project Objectives & Scope

### Objectives

1. Explore the data for a reliable measure
2. Describe each segment's path with auditable rules
3. Forecast whether at-risk segments may return to 2018 levels
4. Recommend existing policy tools that fit each segment.


### Scope

- Identify who (which segment) is declining, recovering or stalled since COVID
- Identify where and how decision makers may act. 13 CLUE areas, 19 industries, 4 business sizes, with what kind of policy.
- Two-year establishment forecasts and their value for decline monitoring 


### Out of Scope

- Employment-related decisions (jobs data mostly suppressed)
- Why businesses open, close or move (no causal data)
- Past policy outcomes or budgets (no policy data)
- Seasonal forecasting and verification of post-2024 outcomes 



---

## Dataset

**Dataset name:** Business establishments in Melbourne Project

**Source:** City of Melbourne Open Data, https://data.melbourne.vic.gov.au/pages/home/

**Coverage:** Individual Business and Aggregated Counts of Establishments and Jobs

### Data Files

| File | Description | Size |
|------|-------------|------|
| `business-establishments-and-jobs-data-by-business-size-and-anzsic.csv` | Individual Business and industry classification| ~924 KB |
| `rbusiness-establishments-with-address-and-industry-classification.csv` | Aggregated counts of establishments and jobs | ~75 MB |

> **Note:** Data files are not committed to this repository due to size.
> Download from: [https://github.com/ruichengy-ops/CMCE30005-Group-5.git]

---

## Data Description & Quality

### Key Variables

- **Total Active Establishments**:
  - Count of operating businesses per year (excludes vacant premises)
- **Total Jobs**:
  - Employment by segment
    - 59% suppressed for Large;
    - 31% for Medium businesses;
    - 14% for Small businesses
  - Set aside
- **Vacancy Rates**:
  - Derived from address-level file to detect economic distress;
  - Measured by area
- **Growth Rates for Active Establishments/Jobs**:
  - Derived from time series of our first 2 Key Variables

### Data Quality Issues & Implications

- **Jobs suppression**
  - Large (59%)
  - Medium (31%)
  - Small (14%)
  — Limits jobs-based vulnerability analysis
- **Missing geolocations**
  - 4,785 records(0.04%, concentrated 2016/Docklands)
- **ANZSIC mapping**:
  - 945 truncated codes manually corrected
  - 433 classes mapped to 19 divisions
- **Annual aggregation**:
  - Census is an annual snapshot
  - No seasonal patterns detectable

**Implication**
1. Active establishments are the best measure of the City's business climate.
2. The comparison period changes the story. 

### Exploratory Data Analysis (EDA) Findings

- Citywide establishments: +18.1% (2002–2024) but –14.5% (2018–2024)
- Docklands: +620% (full period) but –12.9% (post-2018)
- Vacancy rates: Upward trend in 12 of 13 areas; concentrated post-2020, minimal recovery by 2024
- Industry range: +135% (Education & Training) to –47% (Wholesale Trade, Admin Support)
- Small businesses: +6.5% (full period) but –20.4% (post-2018)

---

## Methodology & Analytical Approach

### Methods

- **Temporal indexing**: Rebase each 2002 count to 100 for cross-comparable trend visualization
- **Growth metrics**: Year-on-year absolute, percentage, and compound annual growth rates
- **Disruption windows**: 2-year pre/post comparisons around GFC (2006-2012) and COVID-19 (2018-2024)
- **Predictive modelling**: Last-observation vs. linear-trend forecasts (evaluation: 2012–2022, test: 2024)
- **Clustering**: Area-industry combinations to identify additional vulnerability patterns

### Analytical Approach
- **Descriptive analysis**
  - Business size
    - The loss is a small-business loss
    - Small businesses are down everywhere, but stalled in some areas
  - Industry
    - Shops and hospitality are coming back
    - Office-based industries are not recovering
    - Some industries were shrinking before COVID
  - Area
    - The CBD carries most of the loss
    - Southbank and Melbourne (Remainder) are still falling
    - West Melbourne (Industrial) was already declining
    - Area totals can hide small-business stalls
- **Predictive Analysis**
  - Multiple regression
    - A clear linear model whose relationships can be explained
  - Random forests
    - Allows more complex relationships in the historical data
  - TabPFN
    - A pretrained approach designed for small tabular datasets
  - Naive benchmark
    - Predicts no change, acts as our control/benchmark



### Validation Approach

| Method | Assumption | How we validated/What validates it |
| ------- | ----------| ----------------------------------- |
| Rule-based classes | Our 5 Classes are credible | Robustness: re-ran the rules at ±0.5% and ±1.5%, with classes fragile if more than 20% of segments change class. Policy grounding: each class matches a situation the City's existing policies already respond to |
| Ignoring segments with <10 establishments | Segments under 10 establishments are too small to classify reliably |Dropping them still keeps 97.7% and 99.9% of establishments (Table 5) |
| Forecasting | Past data carry enough signal to forecast two years ahead | Explored further in Section 5 |



### Tools Used

R packages: readr, dplyr, tidyr, ggplot2; Version control: Git/GitHub

---

## Key Findings & Recommendations

- Small establishments and business support
  - Use Business Concierge as an entry point for diagnosing small operators’ needs and directing them to suitable advice, permit assistance or support
- Retail premises and shopfront support
  - Consider Shopfront Business Grants for eligible projects where fit-out costs impede occupation of genuinely vacant premises
- Office demand and building assessment
  - Investigate office demand where finance and professional-service establishments continue to decline
  - For persistently underused buildings, apply the Adaptive Reuse Guidelines to compare retaining and upgrading offices with feasible alternative uses (City of Melbourne, 2024)

---

## Limitations & Future Works

### 🔒 Limitations

- CLUE block boundaries were realigned in 2025
- Active establishment counts treat every operating location equally, which cannot show the full employment or economic impact of changes
- Few changed combinations do not guarantee stable weighted composition
- Self-selection, different underlying trends or spillovers could still bias estimates; existing CLUE data cannot estimate this effect

### ⏳ Future Works

1. Check boundary correspondence and harmonise geography before adding later releases
2. Policy effectiveness needs separate evaluation
3. Report uncertainty and delivery costs

**Timeline:** Weeks 9–11 modelling; Week 12 final report (23 October 2026)

---

*Last updated: [25/10/2026]*
