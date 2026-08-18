# Replication Package: "Decarbonizing Heat: The Impact of Heat Pumps and a Time-of-Use Heat Pump Tariff on Energy Demand"

Louise Bernard (Centre for Net Zero), Andy Hackett (Centre for Net Zero), Robert Metcalfe (Columbia University), Andrew R. Schein (Centre for Net Zero & NBER) — *American Economic Journal: Economic Policy*, AEJPol-2025-0575

This repository contains the code used to produce all tables and figures in the paper and its online appendices. It does **not** contain the underlying household-level data, which is confidential (see [Data Availability Statement](#data-availability-statement) below).

## Overview

The paper studies two related interventions delivered by a large UK energy retailer to residential customers: (1) the staggered installation of heat pumps, and (2) the introduction of "Cosy Octopus," a time-of-use electricity tariff designed for heat pump users. We use household half-hourly and monthly electricity and gas smart-meter data, combined with property and area-level public data, to estimate the effect of both interventions on energy demand, and to compute the marginal value of public funds of the UK's heat pump subsidy (the Boiler Upgrade Scheme).

The code is organized as a numbered pipeline of R scripts orchestrated by [`scripts/main.R`](scripts/main.R). See [Description of Programs](#description-of-programs) below for what each stage does and [List of Tables and Programs](#list-of-tables-and-programs) for how each exhibit in the paper maps to the script that produces it.

## Data Availability Statement

### Statement about rights

- The authors certify that they had legitimate access to all data used in this paper.
- The authors **do not have the right to redistribute** the confidential, household-level data described under "Restricted data" below. These data were made available to the authors through their affiliation with Centre for Net Zero (part of Octopus Energy Group Limited) under the retailer's own customer-data governance arrangements, and are subject to UK GDPR and contractual restrictions on redistribution to third parties.
- The authors have the right to redistribute, and do redistribute in this package, all code, and all publicly sourced reference data listed under "Public data" below (or, where a dataset's own license does not permit redistribution, a citation and access instructions in lieu of the file itself).

### Summary of availability

- [ ] Some data **cannot** be made publicly available (household-level smart-meter, tariff/agreement, and survey data)
- [x] Some data **are** publicly available (area- and policy-level reference data used for covariates, weighting, weather, carbon accounting, and welfare calculations)

### Restricted data

| Data category | Description | Source | Why it's restricted |
|---|---|---|---|
| Smart-meter consumption records | Half-hourly and monthly household electricity and gas consumption | Octopus Energy Group, via Centre for Net Zero | Personal data (UK GDPR); commercially confidential customer data; no right of redistribution |
| Account / tariff / agreement records | Contract start/end dates, tariff type, Cosy Octopus enrollment, heat pump installation/deal dates | Octopus Energy Group, via Centre for Net Zero | Same as above |
| Property attributes linked to accounts | EPC rating (linked via UPRN), floor area, predicted heat loss, property value, location, as merged to household accounts | Octopus Energy Group, via Centre for Net Zero | Personal data once linked to an identifiable household (UPRN-level linkage) |
| Customer survey responses | Heat pump usage survey (backup heating, comfort, NPS/feedback) fielded to Octopus Energy customers | Centre for Net Zero / Octopus Energy Group | Personal data; no independent ethics/consent framework governs redistribution (see [Ethics](#ethics-and-human-subjects) below) |
| EV detection flags | Household-level electric vehicle charging detection derived from consumption patterns | Centre for Net Zero, derived from restricted consumption data | Derived from restricted data above |

**Access for verification/replication purposes:** ⚠️ *TODO — confirm with Centre for Net Zero / Octopus Energy before submission.* As of this draft:
- There is **no established, author-independent process** by which an outside researcher can request access to these data (to be confirmed).
- Centre for Net Zero / Octopus Energy Group have indicated they are **unlikely** to be able to provide a private, unpublished copy of the data directly to the AEA Data Editor or a designated third-party replicator for verification purposes.
- Given the above, per the AEA Data and Code Availability Policy's provisions for non-public data, the authors commit to: (i) preserving the data and code for no less than five years following publication; (ii) providing reasonable assistance to requests for clarification and replication; (iii) making all code publicly available (done, this package); and (iv) publicly disclosing the source of the data with contact information (below).

**Contact for data provenance questions:** Louise Bernard, louise.bernard@centrefornetzero.org. <!-- TODO: confirm whether a dedicated CNZ/Octopus data-access or DPO contact should be listed instead of/in addition to a named author. -->

### Public data

All of the following are cited in the paper's bibliography (`overleaf-export/.../bibliography.bib`); citation keys are given for cross-reference. None of these files contain personal data.

| Dataset | Used for | Citation key | Redistributed in this package? |
|---|---|---|---|
| ONS mid-year income estimates (MSOA, FYE2023) | Local-area income covariate | `ons_income_fye2023` | To confirm — public ONS release, can likely be re-derived from source rather than shipped |
| ONS House Price Statistics for Small Areas (HPSSA), Dataset 3 | MSOA-level mean property prices | `ons_hpssa3` | Same as above |
| ONS postcode-to-geography lookups (2011/2021, 2021–2023 vintages) | Crosswalk from postcode to MSOA/LSOA/LAD | `ons_pcd_msoa2011_2021`, `ons_pcd_msoa2021_2023` | Same as above |
| UK ETS allowance prices | Carbon pricing scenarios (§ Operating Cost Scenarios) | `UKETS` | Yes/public |
| DESNZ Boiler Upgrade Scheme statistics | Subsidy program context | `DESNZ:BUS2025` | Yes/public |
| Greenhouse Gas Reporting: Conversion Factors (2023, 2025) | Carbon intensity of gas/electricity | `ukgov2023`, `ukgov2025` | Yes/public |
| Green Book & supplementary guidance (2020, 2023, 2024) | Discount rates, valuation of energy/GHG, appraisal data tables | `ukgov2020`, `ukgov2023valuation`, `ukgov2024` | Yes/public |
| MCS Data Dashboard | Average heat pump installation costs | `mcs_dashboard` | To confirm — dashboard export, redistribution terms not yet checked |
| Delta-EE / BEIS heating measures cost report | Counterfactual gas boiler cost | `delta2018` | Yes/public (BEIS Research Paper 2020/028) |
| Rennert et al. (2022), *Nature* | Social cost of carbon (IWG-lineage estimate) | `rennert2022` | Citation only (journal article, not a data extract) |
| BEIS Energy Follow-Up Survey | External benchmark for backup-heating prevalence | `beis2021efus` | Citation only |
| National Grid ESO / WattTime marginal carbon intensity | Marginal emissions factors | — | ⚠️ **WattTime data may be subject to commercial/API license terms** — to confirm before redistributing the raw extract; NGESO carbon intensity API data is public |
| UK & US social cost of carbon series (HMG, IWG) | Welfare/MVPF carbon valuation | — | To confirm exact public source per series |
| Air quality cost data | Air-quality externality in MVPF | `ukgov2024` (Table 15) | Yes/public, part of Green Book data tables |

Note: Energy Performance Certificate (EPC) ratings are **not** a public-data item here — they are supplied by Octopus Energy Group linked to household accounts via UPRN, and are therefore listed under "Restricted data" above, not shared.

**Open items flagged above (⚠️), to resolve before deposit:**
1. Confirm (with Octopus Energy Group / Centre for Net Zero legal or data protection contact) whether any formal, author-independent data access process exists or could be established, and get the exact wording/URL/contact to cite — AEA's policy is explicit that "upon request to the authors" alone does not satisfy the requirement.
2. Confirm redistribution terms for the WattTime extract and the MCS Data Dashboard export.
3. Decide whether the public reference datasets above should be **shipped as files** in the deposit (simplest, satisfies DCAS #2–3 for the portion of raw data that is public) or **only cited** with instructions to re-download (acceptable per DCAS #2 only if reproducible "within a reasonable time frame and with reasonable resources" — likely true here given these are small public tables).

### Ethics and human subjects

No separate IRB approval or formal Data Protection Impact Assessment was conducted for this research; the underlying customer survey and data use were treated as routine commercial customer analytics by Centre for Net Zero / Octopus Energy Group rather than as academic human-subjects research requiring independent ethics review.

⚠️ **TODO(Louise):** Per DCAS #10/#11, if the customer survey referenced in `scripts/01_11_cosy_survey_figures.R` (and cited in the paper, e.g. footnote on backup heating prevalence) is to be described as part of the replication package, please confirm: (a) whether the survey instrument/questionnaire can be included or described, and (b) whether any co-author's academic institution has its own IRB determination on file that should be cited instead. Given no formal review exists, you may want to loop in the AEA Data Editor early (per their guidance, "reach out... if you believe your particular situation is not covered by the examples and guidance") rather than have this surface for the first time during verification.

## Computational Requirements

⚠️ **TODO — to be filled in from the Vertex AI Workbench instance.** Run `scripts/utils_session_info.R` on the server (see instructions below) and paste the output in below. Structure follows the Social Science Data Editors template:

### Software
- R version: *TBD*
- Key R packages and versions used (from `scripts/main.R`): `knitr`, `kableExtra`, `did`, `fixest`, `data.table`, `lubridate`, `dplyr`, `ggplot2`, `RColorBrewer`, `tidyr`, `scales`, `readr`, `forcats`, `viridis`, `stringr`, `stargazer`, `panelView`, `readxl`, `purrr`, `progress`, `lfe`, `tibble`, `didimputation`, `ggtext`, `MatchIt`, `zoo`, `patchwork` — exact version numbers *TBD*
- Operating system: *TBD* (production environment is a GCP Vertex AI Workbench instance with a GCS bucket mounted via `gcsfuse`; see "Accessing the Development Environment" below)

### Hardware
- OS: *TBD*
- CPU: *TBD* (generation and core count)
- Memory: *TBD*
- Disk space required: *TBD* (raw data + scratch + output)

### Runtime
- Full pipeline (`source("scripts/main.R")`): *TBD wall-clock time*
- Per-stage breakdown: *TBD* (optional — only needed if runtime is heterogeneous across stages)

## Description of Programs

`scripts/main.R` is the master script: it installs/loads packages, sets global plot colors and the `fixest` estimation config, then sources six top-level orchestrators **in strict ascending numeric order** (`01_00` → `02_00` → `03_00` → `04_00` → `05_MVPF` → `06_...`). Each orchestrator in turn sources its own numbered sub-scripts (`0X_01`, `0X_02`, ... in order, verified directly against the `source()` calls in `01_00_cosy.R` and `02_00_heatpump.R`). Sub-scripts communicate through objects left in the global environment (not return values), and each orchestrator wipes its own environment down to a whitelist after each stage to prevent cross-contamination between stages.

| Script | Purpose |
|---|---|
| `01_00_cosy.R` | Cosy Octopus time-of-use tariff (electricity) analysis |
| `02_00_heatpump.R` | Heat pump (gas/electricity) analysis |
| `03_00_balance_tables_and_reweighting.R` | Balance tables and sample reweighting, shared across both analyses |
| `04_00_half_hourly_analysis.R` | Half-hourly consumption analysis |
| `05_MVPF.R` | Marginal value of public funds / welfare analysis |
| `06_sample_descriptive_statistics.R` | Sample descriptive statistics |

`scripts/archive/` holds superseded code kept for reference (an earlier version of `05_MVPF.R`, an earlier version of the loader now in `01_01_load_data.R`, and code split out of `04_00_half_hourly_analysis.R`) — not part of the replication path. `scripts/cosy---reproduction_files/`, `scripts/did-update_files/`, and `scripts/heatpump-installation_files/` are R Markdown knit artifacts, also not part of the replication path.

### Key variables

See [DATA_DICTIONARY.md](DATA_DICTIONARY.md) for the full variable-level metadata (DCAS #5): what each variable means, how it's constructed, and which script introduces it.

## Instructions for Data Preparation and Analysis

**Data preparation:** Each `0X_01_load_data.R` script guards its heavy merge with `if (!file.exists(...))`, building and caching to `data/scratch/*.RDS` on first run, and reading the cache thereafter. To force a rebuild, delete the relevant cached `.RDS` file (commented `file.remove(...)` lines at the top of each loader show which file to remove).

**Analysis:** From an R session with working directory set to the repository root:

```r
source("scripts/main.R")
```

This runs the full pipeline end-to-end with no manual intervention required, producing all figures (`graphs/`) and tables (`tables/`) referenced in the paper. To run a single stage instead, first run the package-loading and parameter block at the top of `main.R` (this defines `hp_color`, `cosy_color`, etc., and the `datapath` variable that later scripts depend on), then `source()` the relevant `0X_00_*.R` file directly.

**Environment:** `main.R` auto-detects the working directory to set `datapath`. On the production environment (GCP Vertex AI Workbench), the GCS bucket must be mounted before starting R:

```bash
gcsfuse --implicit-dirs --rename-dir-limit=100 --max-conns-per-host=100 cnz-oe-extract-57d7be9d0a /home/jupyter/gcs
```

`data/`, `graphs/`, and `tables/` are not version-controlled; they are populated by running the pipeline against the (restricted) input data.

## List of Tables and Programs

Every table/figure below is `\input{}`/`\includegraphics{}`'d directly into `overleaf-export/.../main.tex` from `tables/*.tex` / `graphs/*.png`. This is the complete set of active exhibits (24 tables, 48 figures) traced to the exact script and line that produces each one; figures with subgroup-parametrized filenames (e.g. per rate-period breakdowns) are generated in a loop from a single script call, noted below where relevant. Figures referenced only in commented-out `\includegraphics` lines in `main.tex` are excluded as not currently part of the published paper.

### Tables

| Exhibit | Producing script |
|---|---|
| `balance_table.tex` | `02_05_balance_table.R` |
| `balance_table_cosy.tex` | `01_05_balance_table.R` |
| `balance_table_cosy_survey.tex` | `03_00_balance_tables_and_reweighting.R` |
| `balance_table_cosy_adoption.tex` | `03_00_balance_tables_and_reweighting.R` |
| `balance_table_cosy_hp_random.tex` | `03_00_balance_tables_and_reweighting.R` |
| `balance_table_hp_adoption.tex` | `03_00_balance_tables_and_reweighting.R` |
| `balance_hp_matching.tex` | `03_00_balance_tables_and_reweighting.R` |
| `matching_hp.tex` | `03_00_balance_tables_and_reweighting.R` |
| `did_lcts.tex` | `01_07_lct_ownership_and_leavers.R` |
| `did_leavers.tex` | `01_07_lct_ownership_and_leavers.R` |
| `ev_charging.tex` | `01_07_lct_ownership_and_leavers.R` |
| `leavers_ev_numbers.tex` | `01_07_lct_ownership_and_leavers.R` |
| `survey_numbers.tex` | `01_11_cosy_survey_figures.R` |
| `survey_balance_numbers.tex` | `03_00_balance_tables_and_reweighting.R` |
| `calendar_last12m_summary.tex` | `01_06_DiD_analysis.R` |
| `hp_did_overall_detailed.tex` | `02_03_DiD_analysis_outputs.R` |
| `hp_did_never_treated_detailed.tex` | `02_03_DiD_analysis_outputs.R` |
| `hp_did_overall_cs_gas_only.tex` | `02_03_DiD_analysis_outputs.R` |
| `hp_did_ev.tex` | `02_06_ev_ownership.R` |
| `hp_ev_charging.tex` | `02_06_ev_ownership.R` |
| `hp_did_solar.tex` | `02_11_solar_PV_analysis.R` |
| `variance_decomp.tex` | `02_10_engineer_variance_analysis.R` |
| `MVPF.tex` | `05_MVPF.R` |
| `summary_prepost.tex` | `06_sample_descriptive_statistics.R` |

### Figures

| Exhibit | Producing script |
|---|---|
| `combined_weekly_installations_deals.png` | `02_04_summary_graphs.R` |
| `monthly_installation.png` | `02_04_summary_graphs.R` |
| `dynamic_hp_plot_combined.png` | `02_03_DiD_analysis_outputs.R` |
| `dynamic_hp_plot_combined_with_trends.png` | `02_03_DiD_analysis_outputs.R` |
| `hp_calendarplot_combined_with_annual_labels.png` | `02_03_DiD_analysis_outputs.R` (see note below) |
| `calendar_att_12m_with_quarter_points_and_cop.png` | `02_03_DiD_analysis_outputs.R` (see note below) |
| `HP_anticipation.png` | `02_03_DiD_analysis_outputs.R` |
| `hp_temperature_gas_elec.png` | `02_09_cop_analysis.R` |
| `quasi_cop.png` | `02_09_cop_analysis.R` (see note below) |
| `hp_event_study_overall.png` | `02_13_event_study.R` |
| `hp_data_availability.png` | `02_12_data_availability.R` |
| `hp_gas_data_availability.png` | `02_12_data_availability.R` |
| `engineer_vs_household_fe.png` | `02_10_engineer_variance_analysis.R` |
| `hp_region_combined.png` | `02_08_heterogeneity_analysis.R` |
| `hp_temperature_morning_off-peak.png`, `hp_temperature_afternoon_off-peak.png`, `hp_temperature_peak_rate.png`, `hp_temperature_other.png` | `02_08_heterogeneity_analysis.R` (looped over `rate_period`) |
| `hp_epc_overall.png` | `02_08_heterogeneity_analysis.R` (looped over `rate_period`) |
| `hp_floor_area_overall.png` | `02_08_heterogeneity_analysis.R` (looped over `rate_period`) |
| `hp_heatloss_overall.png` | `02_08_heterogeneity_analysis.R` (looped over `rate_period`) |
| `hp_hs_overall.png` | `02_08_heterogeneity_analysis.R` (looped over `rate_period`) |
| `hp_income_overall.png` | `02_08_heterogeneity_analysis.R` (looped over `rate_period`) |
| `hp_property_value_overall.png` | `02_08_heterogeneity_analysis.R` (looped over `rate_period`) |
| `Cosy Tariff.png`, `Rates_by_Rate_Period_and_GSP_Group.png`, `Rate_Changes_by_Period.png` | `01_02_rate_graphs.R` |
| `weekly_adoptions.png` | `01_03_summary_graphs.R` |
| `data_availability.png` | `01_04_data_availability.R` |
| `dynamic_att_combined.png`, `dynamic_att_combined_imputation.png` | `01_06_DiD_analysis.R` |
| `calendarplot_Morning Off-peak.png`, `calendarplot_Afternoon Off-peak.png`, `calendarplot_Peak Rate.png`, `calendarplot_Other.png` | `01_06_DiD_analysis.R` (looped over `rate_period`) |
| `did_controlling_hp_installation.png` | `01_09_cosy_and_hp_coadoption.R` |
| `leavers_ev.png` | `01_07_lct_ownership_and_leavers.R` |
| `eac_combined.png`, `eac_share_combined.png`, `cosy_epc_combined.png`, `floor_area_combined.png`, `floor_area_share_combined.png`, `heatloss_combined.png`, `heatloss_share_combined.png`, `income_category_combined.png`, `cosy_temperature_all.png`, `cosy_temperature_binned.png` | `01_08_heterogeneity_analysis.R` |
| `property_value_average_bill_saving.png` | `01_10_structural_winner.R` |
| `combined_impact_hourly_consumption.png` | `04_00_half_hourly_analysis.R` |
| `MVPF_sensitivity_heatmap.png`, `waterfall_hp_preferred.png` | `05_MVPF.R` |

**Notes:**
- `MVPF.tex` — an earlier version of `05_MVPF.R` (now in `scripts/archive/`) also wrote this filename; `05_MVPF.R` (as sourced by `main.R`) is the authoritative producer.
- `hp_calendarplot_combined_with_annual_labels.png`, `calendar_att_12m_with_quarter_points_and_cop.png`, and `quasi_cop.png` can also be conditionally overwritten by `02_14_gas_only_sample_robustness_check.R`, gated behind `Sys.getenv("WRITE_MAIN_FILENAMES")` (default off) — under a default run, the scripts named above are the effective producers.

## License

This package will be deposited in the AEA Data and Code Repository (openICPSR); license terms will be set at deposit time using openICPSR's default license (per DCAS #15). ⚠️ TODO — confirm with Octopus Energy Group/Centre for Net Zero whether their IP policy requires any deviation from the default before deposit.

## Accessing the Development Environment

1. Open the Vertex AI Workbench instance (trials instance 2): https://console.cloud.google.com/vertex-ai/workbench/instances?project=cnz-data-warehouse-d66eb552a5
2. Once the instance is active, click "Open JupyterLab".
3. Mount the GCS bucket (before starting R): see command above.
4. `cd cosy-analysis && R`, then `source("scripts/main.R")`.

---

*This README follows the [Social Science Data Editors' template README](https://social-science-data-editors.github.io/template_README/) and the [Data and Code Availability Standard (DCAS) v1.0](https://datacodestandard.org/). Items marked ⚠️ TODO must be resolved before this package is submitted to the AEA Data Editor.*
