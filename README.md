# CMCE30005 Business Analytics Challenge
## CMCE30005-Group-5 - Business establishments in Melbourne Project Brief

**Subject:** CMCE30005 Business Analytics Challenge, Semester 2 2026

**University:** University of Melbourne

**Team Members:** Yiming Pan, Ruicheng Yao, Caleb Lee

---   

## Business Problem

How were different industries, areas, or business sizes performing from 2002-2024 primary by their number of active establishments and which of these segments may be of concern to City of Melbourne decision-makers moving forward?

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
- Whether at-risk segments may recover (forecast)


### Out of Scope

- Employment-related decisions (jobs data mostly suppressed)
- Why businesses open, close or move (no causal data)
- Past policy outcomes or budgets (no policy data)
- Seasonal swings or post-2024 conditions (data ends 2024)



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

### Data Quality Issues & Limitations

- **Jobs suppression**
  - Large (59%)
  - Medium (31%)
  - Small (14%)
  — limits jobs-based vulnerability analysis
- **Missing geolocations**
  - 4,785 records(0.04%, concentrated 2016/Docklands)
- **ANZSIC mapping**:
  - 945 truncated codes manually corrected
  - 433 classes mapped to 19 divisions
- **Annual aggregation**:
  - Census is an annual snapshot
  - no seasonal patterns detectable

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

### Validation Approach

- Visual trend assessment against known economic events (GFC, COVID peaks/troughs)
- Cross-validation of vacant vs. active establishment counts to detect data anomalies
- Sensitivity testing with alternative disruption windows (±1 year)
- MAE and RMSE metrics for forecast error measurement; residual plot inspection

### Tools Used

R packages: readr, dplyr, tidyr, ggplot2; Version control: Git/GitHub

---

## Analysis Progress & Next Steps

### ✅ Completed

- Data cleaning, validation, and exploratory data analysis
- Establishment trend visualisation by CLUE area, industry, business size
- Vacancy rate time-series and heatmap analysis
- Disruption comparison analysis (GFC vs. COVID recovery patterns)
- Identification of key at-risk segments (e.g., Professional Services –25% 2018–2024)

### ⏳ Remaining Work

1. Implement forecasting evaluation (last-observation vs. linear models; 2012–2022 holdout, 2024 test period)
2. Execute area-industry clustering to synthesise findings across three dimensions
3. Consolidate results into strategic vulnerability priorities for City of Melbourne
4. Generate final visualisations and policy-relevant summary tables

**Timeline:** Weeks 9–11 modelling; Week 12 final report (23 October 2026)

---

*Last updated: [25/10/2026]*
