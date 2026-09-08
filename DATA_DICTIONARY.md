# Data Dictionary

Variables that live in the panel/account-level/area-level data used throughout the pipeline — i.e., what each column means and where it comes from. This satisfies DCAS #5 (metadata).

## Section 1: Cosy tariff pipeline (`01_00_cosy.R` – `01_11_cosy_survey_figures.R`)

| Variable | Label | Introduced in | Construction notes |
|---|---|---|---|
| `total_consumption` | Consumption in kWh per period | `01_01_load_data.R:20` | Renamed from raw `total_read_value`; summed kWh over a rate period. Also re-derived for the synthetic `"Overall"` rate period as the daily sum across all periods (`01_01_load_data.R:107-108`). |
| `consumption_hh` | Half Hourly Consumption in kWh | `01_01_load_data.R:21` | Renamed from raw `mean_read_value`; mean kWh per half-hour settlement interval within the period. For `"Overall"`, computed as `total_consumption/48`. |
| `date` | Day | `01_01_load_data.R:22` | `as.Date(settlement_date)`. Note: `as.Date` is globally overridden in `01_00_cosy.R:157-165` so numeric inputs default to `origin="1970-01-01"`. |
| `cosy_contract_active` | Contract Active | `01_01_load_data.R:32-53` | Flag = 1 if `date` falls within a "Cosy Octopus" agreement's `[from, to]` window, else 0. Key Cosy treatment indicator. |
| `first_adoption` | — | `01_01_load_data.R:44` | `min(date)` among dates where `cosy_contract_active==1`, per `hashed_mpan`. Household's earliest Cosy start date. |
| `rate_period` (household panel) | Rate period | `01_01_load_data.R:112-124` | 5 levels: `"Morning Off-peak"`, `"Afternoon Off-peak"`, `"Peak Rate"`, `"Other"`, `"Overall"`. Independently re-derived with different category schemes in `01_02_rate_graphs.R:234-243` and `01_10_structural_winner.R:12-19`. |
| `gsp_group_id` | — | `01_01_load_data.R:172` | Raw field from `cosy_-_cosy_details_2024_06_25.csv`, used solely as the join key to merge in `weather` by GSP + date. Same underlying GSP grouping as `tariff_gsp_group_id`, referenced under a different name. |
| `tariff_gsp_group_id` | GSP | dict `01_00_cosy.R:28`; used `01_05_balance_table.R:24`, `01_02_rate_graphs.R:19,134` | Raw field identifying the tariff's Grid Supply Point group. |
| `energy_efficiency` | Energy Efficiency | raw covariate; used `01_01_load_data.R:173-181`, `01_05_balance_table.R:25,35` | EPC/SAP score (0-100+); feeds `epc_letter` and enters `m_adopters` as `log(energy_efficiency)`. |
| `epc_letter` | EPC | `01_01_load_data.R:173-181` | `case_when` bands on `energy_efficiency`: A ≥91, B 81-90, C 69-80, D 55-68, E 39-54, F 21-38, G ≤20. |
| `estimated_annual_consumption` | EAC | raw covariate; used `01_01_load_data.R:183`, `01_05_balance_table.R:25,35` | Enters `m_adopters` as `log(estimated_annual_consumption)`. |
| `eac_mwh` | EAC in MWh | `01_01_load_data.R:183` | `estimated_annual_consumption/1000`, matching the downstream bin labels. |
| `eac_mwh_category` | — | `01_08_heterogeneity_analysis.R:441-452` | Decile bins of `eac_mwh`. |
| `previous_contract` / `previous_is_variable` / `previous_is_charged_half_hourly` | Prev Is Variable | `01_01_load_data.R:156-158` | Attributes (`lag()`) of the contract immediately preceding the household's first Cosy Octopus contract. `previous_is_variable` itself isn't used in any regression or published table; `previous_is_charged_half_hourly` (used as "Prev is ToU" in `01_08`) is the one that feeds results. |
| `hdd` | Temp. Bin (°C) | `01_01_load_data.R:189-195` | Categorical: `round(daily_avg_air_temperature_celsius)` clipped to `[0,15]`. A capped ambient-temperature bin used as a fixed effect — not a heating-degree-days measure. The 15°C cap keeps the quasi-COP/empirical-efficiency ratio stable (see [MODEL_PARAMETERS_AND_ASSUMPTIONS.md](MODEL_PARAMETERS_AND_ASSUMPTIONS.md)). |
| `total_floor_area` | Floor Area (m sq) | raw covariate; used `01_08_heterogeneity_analysis.R:1005-1015` | Decile-binned into `total_floor_area_category`. Same underlying EPC field as `floor_area`, referenced under a different name in `01_05`. |
| `floor_area` | Floor area | raw covariate; used `01_05_balance_table.R:25,35` | `log(floor_area)` in `m_adopters`. |
| `property_value` (household-level) | Property value | raw covariate | `log(property_value)` in `m_adopters`; decile-binned into `property_value_category` for heterogeneity/savings figures. |
| `urbanity` / `urban` | Urban | `01_05_balance_table.R:28-32` | `urban=1` if `urbanity` in `{Large Urban Areas, Smaller Urban Areas}`; `=0` if rural/accessible; else `NA`. |
| `predicted_heatloss_watts` | Heatloss (W) | raw covariate | Decile-binned (in kW) for Figs A.33/A.34. |
| `region` | — | raw covariate | Interacted with `cosy_contract_active` for Fig A.36 / `tables/did_region.tex`. |
| `postcode` | — | raw covariate | Joined to ONS postcode→MSOA lookup for area-level income/deprivation/property-price covariates. |
| `daily_avg_air_temperature_celsius` | — | raw weather covariate | Source for `hdd`, `temp_degree`, and the temperature-bin filters. |
| `temp_degree` | — | `01_08_heterogeneity_analysis.R:126-136,229-236` | `round(daily_avg_air_temperature_celsius)` clipped to `[0,25]` for general temperature-heterogeneity figures (wider range than `hdd`, which is capped at 15°C specifically for the quasi-COP ratio). |
| `share_consumption` | Share of daily consumption | dict; constructed e.g. `01_08:461-478` | `total_consumption / sum(total_consumption)` grouped by `account_id`+`date` across the 4 non-"Overall" periods. |
| `share_daily` | — | `01_08_heterogeneity_analysis.R:225-241` | `consumption_hh / daily_total(consumption_hh)` — same measure as `share_consumption` (share of daily consumption in a given rate period), computed from a different part of the pipeline. |
| `ev_charging` | EV Charging | `01_07_lct_ownership_and_leavers.R:6-27,41-46` | Count of half-hour intervals with detected charging, expressed as a share of the period's slots. |
| `is_ev_detected` | — | `01_07:30-32` | `min(date)` of any detected EV-charging interval per `account_id`. |
| `has_ev` (smart-meter based) | EV User | `01_07:52-54` | Flag = 1 if `date >= is_ev_detected`. One of three EV-ownership measures in the codebase (smart-meter-detected and two self-reported survey measures); some disagreement between them is expected. |
| `Morning_Cosy`/`Afternoon_Cosy`/`Peak_Rate`/`Other` (Table 3 dummies) | — | `01_07:76-94` | Each =1 if that `rate_period` is (tied for) the day's *maximum* EV-charging-interval count; a household-day can flag more than one period. Variable names use "Cosy" terminology (`Morning_Cosy`/`Afternoon_Cosy`) while the `rate_period` values they test against use "Off-peak" terminology. |
| `contract_analysis`: `num_contracts`/`ongoing`/`ended`/`category` | — | `01_07:213-235` | Classifies each account as `"Stayed on Tariff (ongoing)"`, `"Tried then switched"`, `"Multiple contracts"`, or `"Other"`. |
| `leavers` | — | `01_07:251-252` | Flag = 1 if `category` ≠ `"Stayed on Tariff (ongoing)"` — includes households with multiple back-to-back contracts (e.g. auto-renewals), not just those with an actual coverage gap. |
| `leave_date` | — | `01_07:255-261` | Among `leavers==1`, first `date` at/after `first_adoption` where `cosy_contract_active==0`. |
| `week` / `firstweek` / `id` (DiD panel) | Week (dict: `settlement_week`) | `01_06_DiD_analysis.R:394-410`; re-derived `01_06:1006-1020`, `01_07:267-281` | `week`=weeks since `start_date`; `firstweek`=week of `first_adoption` (or `leave_date`); `id`=integer household id for `did::att_gt`, built via `cur_group_id()` on `hashed_mpan` in `01_06` and directly via `as.numeric(hashed_mpan)` in `01_07` — each script's `id` only needs to be unique within its own estimation call, so the two constructions don't need to agree with each other. |
| `Has EV` / `Has EV Charger` / `Home battery` / `Has Solar PV` | — | `01_07:328-350,364-365` | Parsed from the smart-tariff survey export. A second, self-reported "EV" concept alongside `has_ev`; some disagreement between the two is expected. |
| `installed_at` / `installed_at_2` / `is_hp_installed` (Cosy-pipeline version) | Is HP Installed | `01_09_cosy_and_hp_coadoption.R:70-90` | `installed_at`=OE-recorded HP install date; `installed_at_2`=self-reported (separate survey, `responses.csv`). |
| `Electric vehicle(s)` | — | `01_09:76-82` | Self-reported EV flag from the HP-installation survey — a third EV-ownership source; disagreement with the other two is expected. |
| `income_category` | — | `01_08:1544-1566` | Decile bins of MSOA `Total annual income (£)`, from `small_area_income_estimates_fye2023.xlsx` (natively 2021-MSOA-boundary, no crosswalk needed) — same vintage used in `01_05_balance_table.R`, `02_05_balance_table.R`, and `02_08_heterogeneity_analysis.R`. |
| `treated` (MSOA-level) | — | `01_05_balance_table.R:138-143,211` | Count, then `as.numeric(treated>0)` = 1 if the MSOA contains ≥1 Cosy adopter's postcode. |
| `adoption_week` | — | `01_05:27` | `round(difftime(floor_date(first_adoption,"week"), start_date, "weeks"))`; outcome variable in `m_adopters`. |
| `first_week` / `adoptions` | — | `01_03_summary_graphs.R:58-60` | `first_week`=`floor_date(first_adoption,"week")`; `adoptions`=count of households with that first-adoption week. Feeds Figure 3. |
| `has_hp`/`install_date`/`submit_date`/`responds_yes`/`knows_size`/`automation`/`lct_answered`/`kid` | — | `01_11_cosy_survey_figures.R:60-155` | Cleaned/derived fields from the raw `responses.csv` questionnaire export; dedup on `kid` (customer key), keeping latest `submit_ts`. Feed macros in `tables/survey_numbers.tex`. |

## Section 2: Heat pump pipeline (`02_00_heatpump.R` – `02_14_gas_only_sample_robustness_check.R`)

"[dict]" = label reused verbatim from `setFixest_dict()` in `02_00_heatpump.R:10-45`.

| Variable | Label | Introduced in | Construction notes |
|---|---|---|---|
| `account_id` | Household [dict] | `02_01_load_data.R:11` | Raw account identifier; panel unit throughout. |
| `hashed_mpan` | — | `02_01_load_data.R:132` | Raw anonymized meter-point identifier. |
| `date` | Day [dict] | `02_01_load_data.R:4,7` | Renamed from `settlement_date`. |
| `settlement_week` | Week [dict] | `02_01_load_data.R:95` | `floor_date(date, "week") + 1`. |
| `installed_at` | — | Raw covariate | Account-level heat-pump installation date. Basis for `is_hp_installed`, `firstweek`, `treated`. |
| `is_hp_installed` | Heat Pump Installed / Is HP Installed [dict] | `02_01_load_data.R:68` (daily: `installed_at <= date`) and `:176` (weekly: `installed_at < settlement_week`) | Treatment indicator. The weekly construction (`<`) governs the headline DiD results; the daily version (`<=`) is used only where a daily panel is genuinely needed. |
| `treated` | — | `02_01_load_data.R:86` | `max(is_hp_installed)` by `account_id` — ever-treated flag. |
| `consumption_hh` | Consumption in kWh per half hour [dict] | `02_01_load_data.R:5-6,20` | Divisors reflect period length: named periods span 6 half-hours (3h), "Other" spans 30 (15h), "Overall" spans 48 (24h). |
| `total_consumption` | Weekly Energy Consumption in kWh [dict] | `02_01_load_data.R:3,12` | |
| `elec_consumption` | Weekly Electricity Consumption (kWh) [dict] | `02_01_load_data.R:133` | Main electricity outcome for CS/TWFE models; weekly construction is the basis for the headline results. |
| `gas_consumption` | Weekly Gas Consumption (kWh) [dict] | `02_01_load_data.R:139-166` | Raw `weekly_consumption` renamed; weeks after a household's first gas reading with no match filled with 0 (not NA). |
| `weekly_consumption` | Weekly Gas Consumption in kWh [dict] | `02_01_load_data.R:139,151,160` | Raw column from `cosy_-_hp_users_gas` CSV. |
| `min_settlement_week` | — | `02_01_load_data.R:142,161` | Earliest gas-reading week per account; weeks before it are dropped, not zero-filled. |
| `share_consumption` | Share of daily consumption [dict] | `02_08_heterogeneity_analysis.R:758,767` | `total_consumption / sum(total_consumption)` within account-date, across non-"Overall" rate periods. |
| `hdd` | Temp. Bin (°C) | `02_01_load_data.R:52-58,189-194` | Rounded/capped ambient-temperature bin used as a fixed effect — not a heating-degree-day calculation. Same construction and 15°C cap rationale as Section 1's `hdd`. |
| `avg_heating_degree` / `daily_avg_heating_degree` | HDD [dict] | `02_01_load_data.R:183` | A genuine heating-degree-days figure — distinct from the `hdd` bin variable above. |
| `temp_degree` | — | `02_01_load_data.R:195-200`, `02_08:21-26` | Rounded temperature bucket 0–25°C. |
| `temp_rounded` | — | `02_09_cop_analysis.R:165` | Uncapped rounding, used only for the COP bootstrap. |
| `estimated_annual_consumption` | EAC [dict] | Raw | Used to pick the highest-EAC MPAN's covariates when an account has multiple meters. |
| `energy_efficiency` | Energy Efficiency [dict] | Raw covariate | SAP/EPC score (0–100); source for `epc_letter`. |
| `epc_letter` | EPC [dict] | `02_08:123-136` | Standard UK EPC-style bands. |
| `property_value` | Property value [dict] | Raw covariate | Deciled into `property_value_category`. |
| `total_floor_area` | Floor area (same field as `floor_area` in the Cosy pipeline; no separate `02_00` dict entry) | Raw covariate | Deciled into `total_floor_area_category`. |
| `latest_survey_heat_loss` | Heatloss (W) [dict] | Raw covariate | Deciled into `latest_survey_heat_loss_category`. |
| `hp_survey_is_solar_present` | Has Solar PV [dict] | Raw covariate | Interaction term in `02_11_solar_PV_analysis.R:89-93`. |
| `hp_survey_outcome_existing_heat_source` | — | Raw covariate | Heterogeneity by previous heat source, `02_08:219-230`. |
| `region` | — | Raw covariate | Heterogeneity by region, `02_08:624-668`. |
| `hp_engineer` | — | `02_10_engineer_variance_analysis.R:18-24` | Filtered to engineers with `n > 1` installations in-sample. Contains real installer names, not a hashed ID; this column stays within the restricted, non-shared data. Regression reference levels are set generically (first engineer in the relevant sample subset), not to a specific named individual. |
| `deal_created_at` | — | Raw covariate | `02_04_summary_graphs.R:26-31`; join key in `02_10:19`. |
| `income_category` | — | `02_08:346-349` | Decile bins of MSOA `Total annual income (£)`. |
| `ev_charging` (flag → count) | EV Charging [dict] | `02_06_ev_ownership.R:11,24-31` | Rate-period hours: morning 4–7, afternoon 13–16, peak 16–19, else "Other". |
| `is_ev_detected` | — | `02_06:34-36` | First date an EV-charging interval is observed. |
| `has_ev` | EV User [dict] | `02_06:45-46` | 1 if `is_ev_detected <= date`. |
| `Morning_Cosy`/`Afternoon_Cosy`/`Peak_Rate`/`Other` (dummies) | Charging EV [dict] | `02_06:92-98` | Outcomes of Table A.4. |
| `smart_tariff` | — | `02_07_switch_to_smart_tariff.R:7` | 1 if `product_display_name` ∈ a hardcoded list of 7 Octopus product names (see [MODEL_PARAMETERS_AND_ASSUMPTIONS.md](MODEL_PARAMETERS_AND_ASSUMPTIONS.md)). |
| `gas_accounts` / `has_gas` | — | `02_02_DiD_analysis.R:226-230` | Accounts with at least one non-missing `gas_consumption` observation — defines the gas-only sample. |
| `ids_cs_elec` / `ids_cs_gas` / `ids_cs_elec_gas_only` | — | `02_03:636-643,787-788`; `02_02:264-267` | Saved account-id vectors from each `att_gt()` estimation sample. |
| `weekly_installations` / `weekly_deals` | — | `02_04_summary_graphs.R:1-34` | Weekly counts of first HP installations and new Cosy-HP deals. |
| `treated` (MSOA-level) | — | `02_05_balance_table.R:70-83,158` | Count, then binarized (`>0`), of sample HP-installation accounts mapped to each 2021-boundary MSOA. |
| `Total annual income (£)` | — | `02_05:87-89` | ONS FYE2023 small-area income estimate, natively on 2021 MSOA boundaries. |
| `Property price (£)` | — | `02_05:99-126` | ONS HPSSA Dataset 3, 2011-vintage MSOA; converted to 2021 boundaries via a postcode-level crosswalk + averaging. |
| `Average HH Size`, `HH Not Deprived in Any Dim. (%)`, `Average Age`, `Share Level 4 Qualifications (%)` | — | `02_05:128-147` | Census-derived MSOA aggregates for the external-validity table (Table A.15). |

## Section 3: Balance tables, half-hourly, sample descriptives (`03_00`, `04_00`, `06_...`)

Excludes `05_MVPF.R` — its variables are almost entirely calibration constants and intermediate calculation objects rather than dataset columns; see [MODEL_PARAMETERS_AND_ASSUMPTIONS.md](MODEL_PARAMETERS_AND_ASSUMPTIONS.md).

| Variable | Label | Introduced in | Construction notes |
|---|---|---|---|
| `cosy_contract_active` | Contract Active | `03_00`:382 | Pre-existing column in `aggregated_data.RDS`. |
| `is_hp_installed` | Is HP Installed | `03_00`:572,876; `04_00`:110 | `as.numeric(installed_at <= date)`. Recreated independently in several places. |
| `treated` | HP/Cosy-treated-group flag (matching) or ever-installed flag (balance tables) | `03_00`:296,424 (matching), 575,879 (balance) | Matching blocks: `as.numeric(!sample == "Random Sample")`. Balance blocks: `max(is_hp_installed)` per `account_id`. Same name, two constructions serving different purposes. |
| `sample` | Group label ("Heat Pump Tariff" / "HP" / "Random Sample") | `03_00`:238,251,273 | Set on each of the three input CSVs before `rbind()` into `matching_data`. |
| `week` / `firstweek` / `id` | Week (dict) | `03_00`:258-262 | `week`/`firstweek` = weeks since `start_date` (panel start / HP installation). `id` = `cur_group_id()` after `group_by(account_id)`. |
| `energy_efficiency`, `property_value`, `floor_area`, `estimated_annual_consumption` | Various | `03_00`:236-299,559-583 | Raw covariates used as MatchIt covariates and balance-table variables. |
| `survey_selection`, `n_frame`, `respondent_account_numbers`, `responders`, etc. | Survey sampling frame + attrition counts | `03_00`:668-712 | Chain reconciling raw respondents → matched-to-frame → linked-to-property-data, feeding `tables/survey_balance_numbers.tex`. |
| `first_adoption` (04_00 construction) | Date of first Cosy Octopus agreement | `04_00`:201-208 | From `Cosy_-_agreement_data...csv`, filtered to `product_display_name == "Cosy Octopus"`; missing `agreement_valid_to` imputed as 2024-07-24. |
| `is_charged_half_hourly` | Half-hourly metering flag | `04_00`:51,54 | Sample filter explicitly selects non-HH accounts (`== FALSE`). |
| `settlement_date`, `settlement_time`, `settlement_period` | Calendar date; HH:MM; 1–48 half-hour index | `04_00`:106-115,220-228 | Period 1 = 00:00, +30 min per period. |
| `ev_charging` (half-hourly) | EV-charging-in-interval flag | `04_00`:27-28,120-125,232-234 | Left-join + `NA→0`; used as a `feols` control in all half-hourly models. |
| `read_value` | Cosy half-hourly electricity consumption (kWh) | `04_00`:183 | Renamed from parquet's `value` "to align with HP naming" — HP side uses raw `value` directly; same quantity, two column names across the two pipelines. |
| `daily_avg_heating_degree` | HDD | `04_00`:31-35,135,241 | From weather CSV. |
| `main_results`, `ELEC_KWH_CHANGE`, `GAS_KWH_CHANGE` | Estimated annual consumption change from HP installation | `05_MVPF`:30-33 (consumed by MVPF calc) | `fread("output/eff_df.csv") %>% filter(window == "Last 12 months")` — the entire empirical link between the DiD estimates (produced elsewhere) and the welfare calculation. Listed here because it's a genuine dataset extract, not a calibration constant. |
| `est_cs_elec_weekly`, `est_cs_gas_weekly`, `est_cs_overall`, `est_cs_morning`, `est_cs_afternoon`, `est_cs_peak`, `est_cs_other` | Callaway–Sant'Anna DiD estimation objects | `06_desc`:2-11 | Loaded from `scratch/*.RDS`, produced upstream. |
| `df_pre`, `df_post` | Pre-/post-adoption subsets used for summary stats | `06_desc`:42-43 | `df_pre`: `week + anticipation < firstweek`; `df_post`: `week >= firstweek`. |
| `elec_consumption`, `gas_consumption`, `consumption_hh` | Outcome columns summarized | `06_desc`:62-76 | Pulled from each `obj$DIDparams$data`; not created in this script. |
| `panel_A_samples`, `Panel` | Panel A/B grouping | `06_desc`:92,97-99 | Heat Pump Adoption Sample vs. Tariff Adoption Sample. |
