# Crossover Study Analysis Pipeline

![R](https://img.shields.io/badge/R-%3E%3D%204.0-276DC3?logo=r&logoColor=white)
![Platform](https://img.shields.io/badge/platform-Windows-0078D4?logo=windows&logoColor=white)
![License](https://img.shields.io/badge/license-Custom%20Attribution-green)

**Author:** Aidan Sauls &nbsp;|&nbsp; **Free to use** -- attribution required in published work &nbsp;|&nbsp; See [LICENSE](LICENSE)

A modular, settings-driven R pipeline for **2-period AB/BA crossover studies** with
binary-item post-tests. Drop in three CSVs and a config file and the pipeline produces
publication-ready figures, tables, and a plain-English audit log -- no R knowledge required
to run it.

### Features at a glance

| | |
|---|---|
| **One config file** | All study labels, item exclusions, colours, and thresholds in one YAML -- no editing R code |
| **Full + restricted scoring** | Automatically runs both complete and item-excluded variants side by side |
| **Multi-variant support** | One BAT run can launch several exclusion variants and generate a comparison config for you |
| **Complete audit trail** | Every run writes a SUMMARY.txt with every intermediate value needed to verify any result by hand |
| **Psychometric flags** | Five criteria automatically flag suspicious items with a threshold-documented notes block |
| **Interactive launcher** | `RunPipeline.bat` guides you through all options -- no command line needed |
| **~30 figures, ~24 tables** | Named and numbered outputs across primary, period effects, psychometrics, and supplementary sections |

---

## Data privacy and repository safety

**This repository contains only safe example data.**
Real participant data is kept entirely outside this repository and must never be committed.

### What this repository contains (public / safe to share)

| | Location |
|---|---|
| Pipeline code | `R/` |
| Example dataset (100 synthetic participants) | `study_data/example_data/` |
| Example dataset config | `config/example_data.yml` |
| Comparison configs | `config/` (see [Canonical configs](#canonical-configs) below) |
| Documentation | `README.md`, `config/README.md`, `study_data/README.md` |
| Launcher scripts | `RunPipeline.bat`, `Run-Pipeline.ps1` |

### What must remain local (never commit)

| | Why |
|---|---|
| Real study CSV files | Contain participant-level data |
| Any file with participant emails or identifiers | Privacy |
| `study_data/<real study folder>/` | Real participant data lives here locally |
| `.rds` files produced by real runs | Derived participant-level data |
| `outputs/<real run>/` | All derived outputs from real data |
| `outputs/<real run>/InternalUse/` | Participant email map is written here |
| Pipeline logs from real runs | May contain participant identifiers |

All of the above are blocked by `.gitignore`. Do not override or bypass it.
See [`study_data/README.md`](study_data/README.md) for instructions on placing and running local data.

### Safe-to-push rule of thumb

**Safe:** `R/` code · `config/` files · `study_data/example_data/` · documentation · `.gitignore` · launcher scripts

**Not safe:** real study CSVs · participant email files · `.rds` files from real runs · `outputs/` from real runs · any `InternalUse/` directory · pipeline logs containing participant identifiers

When in doubt: run `git status` and inspect `git diff --cached` before every push.

### Canonical configs

| Purpose | File |
|---|---|
| Run the example dataset (public, safe) | [`config/example_data.yml`](config/example_data.yml) |
| Canonical research comparison (local only — references local run outputs) | [`config/Comparison_Y1_Y1&Y6_Excluded.yml`](config/Comparison_Y1_Y1&Y6_Excluded.yml) |
| Archived / superseded configs | `config/archive/` |

The canonical comparison config references two runs (`PilotData_N15_excl_y1` and
`PilotData_N15_excl_y1_y6`) whose source data and outputs exist only on the
researcher's local machine. A fresh clone can verify the pipeline using
`config/example_data.yml` and the example dataset; see [Quick start](#1-quick-start).

---

## Contents

- [Data privacy and repository safety](#data-privacy-and-repository-safety)

1. [Quick start](#1-quick-start)
2. [Study design](#2-study-design)
3. [Data format](#3-data-format)
4. [Running the pipeline](#4-running-the-pipeline)
5. [Configuration](#5-configuration)
6. [Multiple analysis variants](#6-multiple-analysis-variants)
7. [Outputs](#7-outputs)
8. [Logs and audit trail](#8-logs-and-audit-trail)
9. [Statistical tests reference](#9-statistical-tests-reference)
10. [Psychometric flags](#10-psychometric-flags)
11. [Requirements](#11-requirements)
12. [Attribution](#12-attribution)

---

## 1. Quick start

1. **Install R >= 4.0** from <https://cran.r-project.org/>
   (required packages install automatically on the first run)
2. **Put your three CSV files** in a folder -- e.g. `study_data/my_study/`
3. **Open `config/study_config.yml`** and set `study.name` to match your folder name
4. **Double-click `RunPipeline.bat`** and follow the prompts

Outputs land in `outputs/<study_name>/`.
To verify everything works first, run with the bundled 100-participant example
dataset in `study_data/example_data/`. Either double-click `RunPipeline.bat`,
choose option [1], and select `example_data` from the list -- or from a terminal:

```powershell
$env:PIPELINE_CONFIG = "config\example_data.yml"
$env:STUDY_DATA_PATH = "study_data\example_data"
Rscript R\run_all.R
```

Expect: `Completed: 7 OK, 0 errors` in about 60 seconds.

---

## 2. Study design

### What the pipeline expects

A **2-period AB/BA crossover** design where:
- Every participant completes **Period 1**, then **Period 2**
- One period uses the **Intervention** condition; the other uses the **Control** condition
- Each condition is assessed with a **binary-item post-test** (one test form per condition)
- Half the participants receive the intervention in Period 1 (sequence AB); the other half receive it in Period 2 (sequence BA)
- Two test forms exist (**Form X** and **Form Y**); which form carries the intervention is also counterbalanced

### Timeline

```
Sequence AB (Intervention first):
  Period 1: [Intervention condition] --> Post-test Form A
  Period 2: [Control condition]      --> Post-test Form B

Sequence BA (Control first):
  Period 1: [Control condition]      --> Post-test Form B
  Period 2: [Intervention condition] --> Post-test Form A
```

### 4-cell structure

Combining sequence (AB/BA) with which form carries the intervention gives four subgroups:

| Subgroup | Period 1 | Period 2 |
|---|---|---|
| AB, X = Intervention | Form X (Intervention) | Form Y (Control) |
| AB, Y = Intervention | Form Y (Intervention) | Form X (Control) |
| BA, X = Intervention | Form Y (Control) | Form X (Intervention) |
| BA, Y = Intervention | Form X (Control) | Form Y (Intervention) |

All primary analyses use the within-person Intervention-minus-Control contrast. The
4-cell breakdown is an internal consistency check: the effect should replicate across
all four cells regardless of which form or period carried the intervention.

### Full vs restricted scoring

The pipeline always computes **two** scores for every participant:

- **Full scoring** -- all items on the form are included. This is the primary analysis.
- **Restricted scoring** -- items listed in `item_exclusions` in the config are dropped
  before scoring. Useful as a sensitivity check (e.g., "Does the result hold if we
  exclude the item with questionable content validity?").

When no items are excluded, full and restricted scores are identical and restricted
results are not separately reported.

---

## 3. Data format

Three CSV files are required in your data folder.

### `assignment.csv` -- one row per participant

| Column | Required | Notes |
|---|---|---|
| `Participant_ID` | Yes | Any unique identifier (text or number) |
| `Intervention_Order` | Yes | `"1st"` or `"2nd"` -- when they received the intervention |
| `Control_Order` | No | `"1st"` or `"2nd"` -- automatically derived as the inverse of `Intervention_Order` if omitted; validated against it when present |
| `X_Order` | Yes | `"1st"` or `"2nd"` -- when they took Form X |
| `Y_Order` | Yes | `"1st"` or `"2nd"` -- when they took Form Y |

> The legacy column name `AI_Order` is automatically recognised as `Intervention_Order`.
> Column names are configurable in `config/study_config.yml` under `columns:`.

### `posttest_x_items.csv` and `posttest_y_items.csv` -- one row per participant

| Column | Required | Notes |
|---|---|---|
| `Participant_ID` | Yes | Must match values in `assignment.csv` |
| `Time_taken` | No | Completion time -- accepts `"X min Y sec"`, `"MM:SS"`, or bare seconds |
| `X1`, `X2`, ... | Yes | One column per item; values must be `0` (incorrect) or `1` (correct) |

Item columns are detected automatically from the header -- any column that is not
`Participant_ID` or `Time_taken` is treated as an item column.

See [`study_data/example_data/`](study_data/example_data/) for ready-to-run sample files (100 participants, 15 items per form).

---

## 4. Running the pipeline

### Option A -- Double-click RunPipeline.bat (Windows, recommended)

Guides you through selecting a config file, data folder, and run options. Menus:

| Mode | Description |
|---|---|
| 1 | Use default `study_data\` folder with `config\study_config.yml` |
| 2 | Specify a folder containing your three CSV files |
| 3 | Specify each CSV file path individually |
| 4 | Manual data-entry wizard (type responses; no CSV files needed) |
| 5 | Choose which analysis modules to run individually |

### Option B -- PowerShell

```powershell
# Set the config and data path, then run
$env:PIPELINE_CONFIG  = "config\study_config.yml"
$env:STUDY_DATA_PATH  = "study_data\my_study"
Rscript R\run_all.R

# Reuse previously loaded data (skip import + scoring):
$env:REUSE_DATA = "1"
Rscript R\run_all.R
```

### Option C -- Command prompt (Windows)

```bat
set PIPELINE_CONFIG=config\study_config.yml
set STUDY_DATA_PATH=study_data\my_study
Rscript R\run_all.R
```

### Environment variables

| Variable | Default | Description |
|---|---|---|
| `PIPELINE_CONFIG` | `config\study_config.yml` | Path to the YAML config file |
| `STUDY_DATA_PATH` | `study_data\` | Folder containing the three input CSVs |
| `REUSE_DATA` | `0` | Set to `1` to skip import + scoring when `.rds` files already exist in `outputs/<study>/rds/` |
| `ANALYSIS_MODULES` | (all) | Comma-separated list of modules to run (see below) |

### Modules

Each module is an R script that can be run independently or as part of the full
pipeline. They run in order; later modules depend on outputs from earlier ones.

| Module | Script | What it does |
|---|---|---|
| `import` | `01_data_import.R` | Loads and validates the three CSV files; checks participant IDs match across files; checks for missing data |
| `scores` | `02_score_calculation.R` | Computes full and restricted scores for each participant, form, period, and condition; creates the `analysis_data.rds` used by all later modules |
| `psychometrics` | `03_psychometrics.R` | Item difficulty (p-correct), point-biserial discrimination, KR-20, Cronbach's alpha, omega, split-half, DIF by sequence, ability-stratified item analysis, suspicious item detection |
| `analyses` | `04_analyses.R` | Intervention effect (paired t), period effect, carryover test (Grizzle), sequence x period interaction, period-specific intervention effect, 4-subgroup contrasts, linear mixed-effects models |
| `figures` | `05_figures.R` | All publication PNG figures |
| `tables` | `06_tables.R` | All publication tables as CSV and PNG |
| `demographics` | `07_demographics.R` | Optional age/gender/training summary tables and SF-36 domain scores (only when `demographics.generate: true` in config) |

To run only specific modules (must have previously run `import` and `scores`):

```powershell
$env:ANALYSIS_MODULES = "psychometrics,analyses,figures,tables"
$env:REUSE_DATA       = "1"
Rscript R\run_all.R
```

---

## 5. Configuration

All settings live in a single YAML file (default: `config/study_config.yml`).
You never need to edit R code. Changes to the config take effect on the next run.

### Full annotated config

```yaml
# ============================================================
# study:  Identity and condition labels
# ============================================================
study:
  # Determines the output folder: outputs/<name>/
  name: "my_study"

  # These labels appear on figure axes, table row headers, and
  # the SUMMARY.txt audit log. Change to match your study.
  intervention_label: "Intervention"
  control_label:      "Control"
  form_x_label:       "Form X"
  form_y_label:       "Form Y"

# ============================================================
# columns:  CSV column name mapping
# Override here only if your CSV headers differ from the defaults.
# ============================================================
columns:
  participant_id:       "Participant_ID"    # also: Participant, participant, ID
  intervention_order:   "Intervention_Order"  # also: AI_Order (legacy)
  form_x_order:         "X_Order"
  form_y_order:         "Y_Order"
  time_taken:           "Time_taken"        # set to null to ignore time columns

# ============================================================
# item_exclusions:  Which items to drop for restricted scoring
# List item column names in lowercase (e.g. x1, y3, y6).
# Leave empty [] for no exclusions.
# The full score (all items) is always computed alongside the
# restricted score. When exclusions = [], full = restricted.
# ============================================================
item_exclusions:
  x: []           # items to exclude from Form X restricted scoring
  y: [y3, y6]     # items to exclude from Form Y restricted scoring

# ============================================================
# scores:  Scoring parameters
# ============================================================
scores:
  # All scores are scaled to this maximum for comparability.
  # E.g. scale_to: 10 makes a 15-item form score range 0-10.
  scale_to: 10

# ============================================================
# analysis:  Statistical settings
# ============================================================
analysis:
  alpha:          0.05   # significance threshold for all tests
  ci_level:       0.95   # confidence interval coverage (95%)
  min_n_carryover: 4     # minimum n per group to run the carryover test

  # Ceiling/floor reference lines on score plots.
  # null = auto-compute as 95%/5% of scores.scale_to.
  ceiling_threshold: null
  floor_threshold:   null

# ============================================================
# figures:  Visual settings
# ============================================================
figures:
  dpi:             300
  width_in:        7.5    # default figure width (inches)
  height_in:       5.0    # default figure height (inches)
  include_titles:  false  # false = no title on figure (add in manuscript)
  base_font_size:  12
  font_family:    "sans"  # "sans", "serif", or a named font

  # Condition colors (hex codes)
  color_intervention: "#2E8B57"  # sea green
  color_control:      "#CD853F"  # warm tan

  # Period colors
  color_period1:  "#4682B4"  # steel blue
  color_period2:  "#B22222"  # firebrick

  # Sequence group colors (for spaghetti plots)
  color_seq_ab:   "#2E8B57"  # intervention-first sequence
  color_seq_ba:   "#8B4513"  # control-first sequence

  # Form colors (for form-comparison plots)
  color_form_x:   "#4682B4"
  color_form_y:   "#B22222"

  # Axis labels -- change to match your outcome measure
  y_axis_score_label: "Score (0-10)"
  x_axis_form_label:  "Test Form"

  # Appearance
  point_alpha:       0.75   # transparency for individual points
  point_size:        2.2
  line_alpha:        0.55   # transparency for spaghetti lines
  errorbar_width:    0.18
  show_ceiling_floor_lines: true

  # "none" = no annotations; "stars" = asterisk for p < .05
  significance_style: "none"

# ============================================================
# tables:  Table export settings
# ============================================================
tables:
  export_csv:      true
  export_png:      true    # requires the gt package
  include_titles:  false
  digits_default:  3
  digits_percent:  1

# ============================================================
# psychometrics:  Item analysis thresholds
# ============================================================
psychometrics:
  ability_strata:          4     # number of ability groups (4 = quartiles)
  top_miss_threshold:      0.25  # flag if >=25% of top group misses the item
  bottom_hit_threshold:    0.60  # flag if >=60% of bottom group gets it right
  min_item_rest_r:         0.10  # flag if item-rest r < 0.10
  max_item_missing_rate:   0.05  # warn if >5% of responses for an item are NA
  max_participant_missing_rate: 0.20

# ============================================================
# optional_analyses:  Toggle individual analyses on/off
# ============================================================
optional_analyses:
  run_time_analysis:             true
  run_normality_tests:           true
  run_restricted_comparison:     true
  run_model_comparison:          true
  run_ceiling_effects_figure:    true
  run_period_specific_intervention: true
  run_subgroup4_contrasts:       true

# ============================================================
# demographics:  Optional participant demographics module
# Set generate: true and add demographics.csv to your data folder.
# ============================================================
demographics:
  generate: false
  file: "demographics.csv"
  columns:
    participant:    "participant"
    age:            "age"
    gender:         "gender"
    training_level: "training_level"

# ============================================================
# sf36:  Optional SF-36 health survey sub-module
# Only active when demographics.generate: true
# ============================================================
sf36:
  generate: false
  file: "sf36_scores.csv"
```

---

## 6. Multiple analysis variants

Real studies often require several parallel analyses -- for example, one with all items
included and one with a problematic item excluded. Rather than editing the same config
file repeatedly, create one config file per variant. The `study.name` value determines
the output subfolder, so each variant produces isolated outputs.

**Example:**

```
config/
  my_study_all_items.yml       # study.name: "my_study_all"
  my_study_excl_y3.yml         # study.name: "my_study_excl_y3"  item_exclusions.y: [y3]

outputs/
  my_study_all/                # results from my_study_all_items.yml
  my_study_excl_y3/            # results from my_study_excl_y3.yml
```

Run each variant:

```powershell
$env:PIPELINE_CONFIG = "config\my_study_all_items.yml"
$env:STUDY_DATA_PATH = "study_data\my_study"
Rscript R\run_all.R

$env:PIPELINE_CONFIG = "config\my_study_excl_y3.yml"
$env:STUDY_DATA_PATH = "study_data\my_study"
Rscript R\run_all.R
```

### Comparison figures (side-by-side panels)

To generate composite figures comparing multiple completed runs side by side, create
a comparison config and run the comparison script:

```yaml
# config/comparison_my_study.yml
comparison:
  runs:
    - name:  "my_study_all"
      label: "All Items"
    - name:  "my_study_excl_y3"
      label: "Y3 Excluded"
  layout: "side_by_side"   # or "top_bottom"
  output_name: "comparison_my_study"
  figure_groups: "all"     # or e.g. ["primary", "period_effects"]
  label_font_size: 28
  dpi: 300
```

```powershell
$env:COMPARISON_CONFIG = "config\comparison_my_study.yml"
Rscript R\run_comparison.R
```

Composite panels are saved to `outputs/comparison_my_study/`.

---

## 7. Outputs

All outputs go to `outputs/<study_name>/`. Figures are PNG; tables are CSV and PNG.

### Figures (`figures/`)

#### `primary/`
| File | Description |
|---|---|
| `01_paired_scores_intervention_vs_control` | Within-person line plot: each participant's intervention and control scores connected; group means with 95% CI shown |
| `02_paired_scores_period1_vs_period2` | Same layout, Period 1 vs Period 2 |
| `03_intervention_violin` | Violin + box: score distributions for intervention and control |
| `04_subgroup_spaghetti` | Four-panel spaghetti plot -- one panel per subgroup cell |
| `05_paired_scores_x_vs_y_form` | Form X vs Form Y within-person scores |
| `06_intervention_effect_by_subgroup` | Mean effect size (Cohen's dz) with CI for each of the four subgroups |
| `07_overall_effect_summary` | Single-panel summary: mean difference + CI for full and restricted scoring |
| `08_subgroup_mean_bars` | Bar chart of condition means by subgroup |
| `09_score_distributions_all_conditions` | Overlaid histograms for all four score columns |
| `10_score_delta_dotplot` | Dot plot of per-participant intervention-minus-control differences, sorted by magnitude |

#### `period_effects/`
| File | Description |
|---|---|
| `11_period1_vs_period2_overall` | Period 1 vs Period 2 scores (all participants) |
| `12_period_effect_by_sequence` | Period effect separately for each sequence group |
| `13_period_by_condition` | Period scores broken down by condition |
| `14_ceiling_floor_by_condition` | Score distribution near ceiling and floor, by condition |
| `15_score_difference_histogram` | Histogram of within-person score differences |
| `16_period_specific_intervention_effect` | Int-minus-Ctl difference for participants where intervention was in P1 vs P2 |
| `17_crossover_means_2x2` | 2x2 mean lines: Period (x) x Sequence (colour) |
| `18_crossover_2x2_with_condition_labels` | Same with condition labels on each data point |

#### `item_analysis/`
| File | Description |
|---|---|
| `19_alpha_if_deleted_form_x`, `..._form_y` | Bar chart: KR-20 / alpha if each item is removed |
| `20_inter_item_correlation_form_x`, `..._form_y` | Inter-item correlation heatmap (colour = Pearson r) |

#### `psychometrics/`
| File | Description |
|---|---|
| `21_item_difficulty_form_x`, `..._form_y` | P(correct) per item with error bars |
| `22_item_discrimination_form_x`, `..._form_y` | Point-biserial r per item |
| `23_score_distributions_by_form` | Per-form score histogram with mean line |
| `24_ability_stratified_heatmap` | Item x ability stratum, coloured by p(correct) |
| `25_suspicious_items_scatter` | Top-quartile miss rate vs bottom-quartile hit rate; threshold lines; flagged items labelled |
| `26_item_discrimination_boxplot` | Boxplot of item-rest r distribution per form |
| `27_item_response_matrix` | Tile heatmap: participant x item, coloured by response (0/1/NA) |
| `28_endorsement_by_sequence` | P(correct) per item split by sequence group |

#### `supplementary/`
| File | Description |
|---|---|
| `29_full_vs_restricted_scores` | Scatter: full score vs restricted score (only when items are excluded) |
| `30_per_participant_profiles` | Spaghetti across both forms for every participant |
| `31_time_taken_by_condition` | Completion time distribution by condition |

#### `descriptive/`
| File | Description |
|---|---|
| `32_score_violin_by_sequence` | Score distributions by sequence group |
| `33_correlation_matrix` | Correlation matrix heatmap of all score columns |

---

### Tables (`tables/` and `tables_png/`)

Tables are saved as CSV in `tables/` and as formatted PNG in `tables_png/`.

#### `descriptive/`
| File | Description |
|---|---|
| `01_participant_flow` | Total enrolled, complete, excluded, per sequence group |
| `02_descriptive_statistics` | Mean, SD, median, IQR, min, max, ceiling%, floor% for all score columns |
| `03_subgroup4_descriptives` | Score descriptives broken down by the four subgroup cells |

#### `primary/`
| File | Description |
|---|---|
| `04_intervention_effect_contrasts` | N, means, difference, SD of differences, 95% CI, Cohen's dz, t, df, p |
| `08_period_contrasts` | Period 2 vs Period 1: same statistics |
| `09_subgroup4_contrasts` | Intervention effect within each of the four subgroup cells |
| `10_full_vs_restricted_comparison` | Effect size / p-value for full vs restricted scoring |

#### `period_effects/`
| File | Description |
|---|---|
| `11_carryover_grizzle` | Carryover test: Period-1 means by sequence group, t, p |
| `12_sequence_period_interaction` | Welch t on the difference in P2-minus-P1 between AB and BA sequences |
| `13_period_specific_intervention` | Int-minus-Ctl by period administration group, t, p |

#### `psychometrics/`
| File | Description |
|---|---|
| `05_item_analysis_summary` | Per-item: p(correct), item-rest r, alpha-if-deleted, Excluded? flag |
| `06_reliability_summary` | KR-20, alpha, omega_t, split-half (SB-corrected), mean inter-item r -- full and restricted per form |
| `07_dif_by_sequence` | DIF analysis: chi-squared or Fisher test per item comparing endorsement between sequence groups |
| `15_normality_tests` | Shapiro-Wilk p-values for each score column |
| `16_time_analysis` | Completion time per condition (mean, SD, min, max) if Time_taken present |

#### `item_analysis/`
| File | Description |
|---|---|
| `17_suspicious_items` | Items that triggered one or more psychometric flags; includes a `# Notes` block at the end of the CSV explaining every flag criterion and the threshold used |
| `18_ability_stratified_item_difficulty` | P(correct) per item per ability stratum; same `# Notes` block |
| `19_item_endorsement_rates` | Raw endorsement rate per item per sequence group |

#### `mixed_models/`
| File | Description |
|---|---|
| `20_lme_fixed_effects` | LME model fixed-effect coefficients, SE, t, p |
| `21_lme_model_comparison` | Likelihood ratio test: Model 1 (no condition) vs Model 2 (with condition) |

#### `exploratory/`
| File | Description |
|---|---|
| `22_dif_by_period` | DIF analysis by period (P1 vs P2) for each item |

#### `supplementary/`
| File | Description |
|---|---|
| `23_full_vs_restricted_effect_comparison` | Effect sizes and CIs for full and restricted scoring |
| `24_ceiling_floor_by_subgroup` | Proportion at ceiling and floor by subgroup cell |

---

## 8. Logs and audit trail

Every run writes three files to `outputs/<study_name>/logs/`.

### `*_run.log` -- detailed plain-text log

A timestamped record of the entire run. Lines are prefixed by type:

| Prefix | Meaning |
|---|---|
| `[INFO]` | General progress messages |
| `[WARN]` | Non-fatal warnings (e.g. DIF flag, missing data above threshold) |
| `[CHECK]` | Data validation results |
| `[CALC]` | Score formulas and derived variable computations |
| `[STAT]` | Full statistical test results (all parameters logged for each test) |
| `[FLAG]` | Psychometric flags raised (item, form, reason, detail) |
| `[DF]` | Compact data frame summaries (dimensions, column names and types) |

Includes: R version and session info, package versions, all file paths, score
computation formulas, complete test outputs (t, df, CI, p for every contrast),
DIF results, LME coefficients, and every figure/table filename produced.

### `*_session.json` -- machine-readable session record

Structured JSON containing module timing, status codes, warning and error lists,
psychometric flag inventory, and output file counts. Suitable for automated checks
or reading programmatically across multiple runs.

### `*_SUMMARY.txt` -- human-readable audit summary

A self-contained plain-text report that lets you verify every number without opening R.

**Header block:** study name, the exact config file path used, data directory,
start/end timestamps, total elapsed time, module status (OK / ERROR), cumulative
warnings, and the full psychometric flags list.

**Analysis Settings section:** alpha, CI level, and which items were excluded --
taken directly from the config at run time, so you can always confirm what settings
produced a given result.

**Results sections:** one section per analysis type; each section header names the
statistical method used. Every entry shows the raw ingredients needed to verify the
calculation by hand:

```
  [ Intervention Effect  (within-person paired t-test, two-tailed) ]
    Full scoring -- group means & SDs:
        N=100  Intervention M=8.2 SD=2.1  Control M=7.9 SD=2.0
    Full scoring -- t-test result:
        diff=0.30  SD(diff)=1.85  SE=0.185  95% CI [-0.07, 0.67]
        dz=0.162  t(99)=1.621  p = 0.108

  [ Carryover Test  (Grizzle 1965 -- Welch t on Period-1 scores) ]
    Full scoring:
        Int-first n=50 M=7.9 SD=2.1 | Ctl-first n=50 M=7.7 SD=1.9
        t=0.52  p = 0.607  -> No evidence of carryover

  [ Sequence x Period Interaction  (Welch t on per-person P2-minus-P1 differences) ]
    Full scoring:
        AB n=50 mean-delta=0.61 SD=3.1 | BA n=50 mean-delta=0.39 SD=2.8
        t=0.35  p = 0.726
```

**Verifying numbers manually:**
- `SE = SD(diff) / sqrt(N)`
- `t = diff / SE` with `df = N - 1`
- `dz = diff / SD(diff)` (Cohen's within-subjects effect size)
- For between-group tests (carryover, seq x period): both group Ns, means, and SDs
  are shown so you can compute the Welch t by hand or in a calculator

---

## 9. Statistical tests reference

| Analysis | Method | What is being tested |
|---|---|---|
| Intervention effect | Within-person paired t-test (two-tailed) | Is the mean Intervention score different from the mean Control score across all participants? |
| Period effect | Within-person paired t-test (two-tailed) | Is Period 2 systematically higher than Period 1 (practice/learning effect)? |
| Carryover test | Grizzle (1965) Welch t-test | Do the two sequence groups have different Period-1 scores? A significant difference suggests the first period's condition "carried over" and contaminated the second period. Compares Period-1 means between AB and BA groups using an independent-samples Welch t-test. |
| Sequence x Period interaction | Independent-samples Welch t-test | Does the within-person P2-minus-P1 score change differ between AB and BA sequences? A significant interaction indicates the period effect is not the same for both sequences, which can indicate a carryover of learning. |
| Period-specific intervention effect | Independent-samples Welch t-test | Is the within-person Intervention-minus-Control difference different for participants who received the intervention in Period 1 vs Period 2? Tests whether the period of administration moderates the effect. |
| 4-subgroup contrasts | Within-person paired t-test per cell | Replicates the intervention contrast within each of the four sequence-by-form cells as an internal consistency check. |
| Linear mixed-effects models | `lmer()` (lme4) with random intercept per participant | Estimates the intervention effect while accounting for repeated measures. Model 1: outcome ~ period + (1 | participant). Model 2: outcome ~ condition + period + (1 | participant). A likelihood ratio test compares the two. |
| DIF by sequence | Chi-squared (or Fisher's exact when expected counts < 5) | Does item endorsement rate differ between sequence groups? Significant result suggests differential item functioning. |

### Effect size

**Cohen's dz** is used throughout for within-person comparisons:

```
dz = mean(Intervention - Control differences) / SD(Intervention - Control differences)
```

Conventional benchmarks: small = 0.20, medium = 0.50, large = 0.80.

---

## 10. Psychometric flags

Items meeting one or more of the following criteria are listed in
`tables/item_analysis/17_suspicious_items.csv` with a `# Flags` count.
Thresholds are set in `config` under `psychometrics:` and are reprinted in a
`# Notes` block at the end of each item-analysis CSV so the file is self-documenting.

| Flag | Criterion | What it means | Default threshold |
|---|---|---|---|
| Top-Q miss | Proportion incorrect for the highest-ability quartile exceeds threshold | High-ability participants are missing this item unexpectedly often -- possible key error, ambiguity, or content unfamiliarity | > 25% |
| Bot-Q hit | Proportion correct for the lowest-ability quartile exceeds threshold | Low-ability participants get this item right at an unusually high rate -- item may be too easy or guessing is inflating scores | > 60% |
| Reversed discrimination | P(correct) is lower in the top quartile than the bottom | Item is harder for high-ability participants than low-ability ones -- strong signal of a problem | any reversal |
| Non-monotone across Q | P(correct) drops more than 10 percentage points between any two adjacent ability quartiles | Performance does not consistently increase with ability | > 10 pp drop |
| Low item-rest r | Item-rest correlation below threshold | Item does not co-vary with the rest of the test -- may be measuring something different | < 0.10 |

Flags are informational -- they do not automatically exclude items. Use the flags
alongside the full item-analysis table and your subject-matter knowledge to decide
whether exclusion is warranted. If items are excluded, re-run the pipeline with the
updated `item_exclusions` config and compare full-vs-restricted outputs.

---

## 11. Requirements

- **R >= 4.0** from <https://cran.r-project.org/>
- **Windows** for `RunPipeline.bat` (the R scripts themselves are cross-platform)

### Required packages (auto-installed on first run)
`dplyr`, `tidyr`, `readr`, `ggplot2`, `scales`, `broom`, `yaml`, `janitor`,
`tibble`, `stringr`, `purrr`, `digest`

### Optional packages (enhanced output)
| Package | What it enables |
|---|---|
| `lme4`, `lmerTest` | Linear mixed-effects models |
| `psych` | Omega reliability coefficient |
| `gt` | Formatted PNG tables |
| `webshot2` | Rendering gt tables to PNG |
| `patchwork` | Multi-panel figure composition |
| `ggbeeswarm` | Beeswarm jitter in scatter plots |
| `ragg` | High-quality PNG rendering |
| `ggrepel` | Non-overlapping figure labels |

If optional packages are missing, the corresponding outputs are skipped with a warning
rather than halting the run.

---

## 12. Attribution

If this pipeline or any of its outputs appear in published or publicly shared work:

**In text:**
> Statistical analyses were conducted using the Crossover Study Analysis Pipeline
> (Sauls, 2026; https://github.com/AidanSauls/crossover-study-pipeline).

**Acknowledgements:**
> The authors thank Aidan Sauls for the Crossover Study Analysis Pipeline
> (https://github.com/AidanSauls/crossover-study-pipeline).

**Software citation:**
> Sauls, A. (2026). *Crossover Study Analysis Pipeline* [Computer software].
> https://github.com/AidanSauls/crossover-study-pipeline

---

Copyright (c) 2026 Aidan Sauls -- see [LICENSE](LICENSE)
