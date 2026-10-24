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
- **Descriptive Analysis**
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
| Ignoring segments with <10 establishments | Segments under 10 establishments are too small to classify reliably |Dropping them still keeps 97.7% and 99.9% of establishments |
| Forecasting | Past data carry enough signal to forecast two years ahead | Explored further in analysis results |

---


## Analysis & Results
### Descriptive Analysis
  - Distribution of establishments across the 5 classes
    - About 75% sit in segments still below their 2018 level (Recovering and No recovery), and only 2-5% have fully recovered
    - A further 4-14% were already shrinking before COVID
    - The classes are robust: at most 10.9% of segments changed class under the alternative bands, well within our 20% limit
  - Tendency
    - The loss is concentrated in small businesses, office-based industries and the CBD
    - Customer-facing industries are recovering
    - some segments were declining before COVID
### Predictive Analysis
  - Predicting no change had the lowest average MAE across the six development years and in the separate 2024 assessment, in both tracks
  - Although these lists sometimes outperformed rankings based on recent changes or the expected hit rate from random selection, no model maintained a stable advantage
  - Some models indeed performed better in individual years, but this was insufficient to establish a reliable basis for policy targeting
  - The forecasts did not improve on the no-change benchmark
  - We’ve highlighted two possible reasons explained below
     
#### Data Preparation and Predictive Approach
  - Forecasting every eligible combination may have been too broad for a specific monitoring need
  - Different paths of decline or recovery may require different questions and information
  - Although area and industry or size were included, our preparation may not adequately represent these different business processes
  - This is a possible explanation, though not a demonstrated cause of the results

#### Data Limitations
  - The historical data from 2002 to 2024 provides only nine complete two-year periods when making forecasts for 2026
  - Repeated combinations cannot represent independent economic trajectorie
  - Those inputs mainly describe past counts without other factors such as local demand or operating pressures
  - More observations or different inputs might help, but this remains untested



---

## Key Findings & Recommendations


- Observed pattern 
  - Small establishments
    - 2,616 fewer than in 2018 (−20.4%); all 13 areas below 2018 in 2024
  - Retail establishments
    - 77 added after 2022, but still 18.1% below 2018 in 2024
  - Finance and professional services
    - Each about 25% below 2018 in 2024; both continued declining after 2022
- Policy Option
  - Business Concierge
    - Free guidance and referrals for starting or growing a business; not a cash grant
  - Shopfront Business Grants
    - Eligible fit-outs to help viable businesses activate vacant premises (City of Melbourne, 2026d)
  - Adaptive Reuse Guidelines
    - For persistently underused buildings, assess other uses alongside retaining and upgrading offices
- Evidence needed before action
  - Current cash-flow pressures, fees and permit problems; business identity and eligibility for any subsequent programme
  - Vacancy duration, lease conditions, local demand, financial viability and fit-out costs
  - Actual occupancy and office demand; building suitability, planning requirements and costs


---

## Limitations & Future Works

### 🔒 Limitations

- Since CLUE block boundaries changed in 2025, the later releases require geographical checks before comparison with our 13-area results
- Historical classifications are describing patterns rather than establishing their causes
- Fewer establishments do not directly demonstrate financial distress or vacant buildings
- Overall, the recommendations identify priorities for investigation, while current needs, programme eligibility and policy effectiveness still require separate evidence

### ⏳ Future Works

1. Future work should first reconsider the predictive task itself and build on earlier subgroup exploration
2. A more focused question could concern one historical class or a particular area–industry combination
3. Analysis afterwards should use comparable areas and periods and only information available at the forecast date
4. Policy effectiveness requires separate evaluation

---

*Last updated: [24/10/2026]*
