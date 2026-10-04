# Crossover Study Analysis Pipeline

Reproducible R pipeline for two-period within-subject crossover studies with two
conditions and two counterbalanced test forms.

The repository contains the full analysis code and synthetic example data. Real
study data, real-study outputs, and reviewer reproduction bundles are local-only
and are excluded from Git.

## Quick start

On Windows, double-click:

```text
RunPipeline.bat
```

For a normal study run, point the launcher to a folder containing:

```text
assignment.csv
posttest_x_items.csv
posttest_y_items.csv
```

The synthetic example under `study_data/example_data/` can be used to confirm
that R and required packages are working.

## Configuration

Configuration is intentionally split into two layers.

### `config/study_config.yml`

Controls study meaning and analysis behavior:

- study/condition/form labels
- input column mapping
- direct-identifier handling
- item exclusions
- score scaling
- inferential settings
- post-hoc power targets
- optional analyses
- manuscript wording
- Figure S1 wording and whether it is generated

### `config/visual_config.yml`

Controls presentation only:

- figure dimensions and DPI
- fonts and colors
- point/line geometry
- legend layout
- Figure S1 styling
- manuscript table widths and font sizes
- figure-specific visual adjustments

The example study uses `config/example_data.yml` and inherits visual defaults
from `config/visual_config.yml`.

## Input data

The pipeline expects three CSV files.

### `assignment.csv`

At minimum:

```text
Participant_ID
Intervention_Order
Control_Order
X_Order
Y_Order
```

Order values may be written as `1st` / `2nd`, `first` / `second`, `1` / `2`, or
`period1` / `period2`.

### `posttest_x_items.csv`

One row per participant with the participant key and item columns such as:

```text
Participant_ID,X1,X2,X3,...
```

### `posttest_y_items.csv`

One row per participant with the participant key and item columns such as:

```text
Participant_ID,Y1,Y2,Y3,...
```

An optional configured timing column may also be present.

See `study_data/README.md` for the detailed input contract.

## Direct identifiers and deidentification

Anonymous study participant codes such as `P001` are treated as analysis IDs.
Obvious direct identifiers are detected separately, including common names for:

- email addresses
- first/last/full names
- student or university IDs
- employee IDs
- medical-record numbers
- dates of birth
- phone numbers
- addresses

Study-specific identifier column names can be added under:

```yaml
privacy:
  additional_direct_identifier_columns: []
```

If no direct identifiers are detected, the pipeline does **not** create a data
copy under outputs.

If direct identifiers are detected, the run creates:

```text
outputs/<study>/extra/data/
  identified/
  deidentified/
  deidentification/
```

The downstream analysis uses the deidentified frames. The identified source copy
is retained locally only so the authorized research team does not lose linkage.
It is never included in a reviewer bundle.

## Output organization

A normal study run has only two human-facing top-level routes:

```text
outputs/<study>/
  manuscript_selected/
  extra/
```

### `manuscript_selected/`

This is the authoritative publication-facing set:

```text
manuscript_selected/
  main_body/
    figures/
    tables/
  supplement/
    figures/
    tables/
  README.md
  manifest_validation.csv
```

This is the only place where outputs are assigned publication roles such as
Figure 1, Figure S1, Table 2, or Table S3.

### `extra/`

This contains everything useful that was not selected for the manuscript:

```text
extra/
  figures/
  tables/
  analysis_objects/
  logs/
```

Figures and tables are organized by what they contain rather than by labels such
as `primary`, `supplementary`, or `exploratory`. Typical figure categories are:

```text
study_design/
condition_effects/
score_distributions/
item_level/
psychometrics/
sequence_and_period/
model_diagnostics/
power_and_precision/
timing/
ceiling_and_floor/
sample_characteristics/
other/
```

Conditional routes may also appear under `extra/`, such as `data/`, comparison
outputs, or a reviewer bundle.

## Logging

The standalone audit-tree system is retired. A full run now writes one obvious
terminal-style transcript:

```text
outputs/<study>/extra/logs/<timestamp>_run.log
outputs/<study>/extra/logs/latest_run.log
```

The same folder also contains:

```text
effective_config.yml
session_info.txt
```

`latest_run.log` is the first file to inspect after either a successful or failed
run. The full pipeline is fail-fast: if a module errors, later modules and
manuscript assembly do not continue using incomplete or stale outputs.

## Run modes

`RunPipeline.bat` provides the normal interactive interface for:

- standard full runs
- custom data locations
- individual CSV selection
- manual data entry
- selected analysis modules
- opening outputs
- viewing the latest run log
- rerunning a study from existing analysis objects
- comparison figures

After a run finishes or fails, the launcher remains open so errors can be read.

From a terminal, the main entry point is:

```text
Rscript R/run_all.R
```

Useful environment variables include:

```text
PIPELINE_CONFIG
PIPELINE_VISUAL_CONFIG
STUDY_DATA_PATH
STUDY_NAME
ANALYSIS_MODULES
REUSE_DATA
ITEM_EXCLUSIONS
CSV_ASSIGNMENT
CSV_POSTTEST_X
CSV_POSTTEST_Y
```

## Reviewer reproduction bundle

The reviewer bundle is deliberately separate from the public Git repository.
The repository contains only synthetic example data/configuration; the reviewer
bundle contains the authorized study's deidentified data and exact effective
configuration needed to reproduce the reported outputs.

The reviewer-bundle builder is intentionally **local-only** and ignored by Git.
For an authorized study workspace, keep these two local utility files at the
project root / `R/` respectively:

```text
MakeReviewerBundle.bat
R/12_build_reviewer_package.R
```

After a successful full study run, double-click `MakeReviewerBundle.bat`.

The generated bundle contains:

- only the R scripts needed for manuscript reproduction
- the exact effective study configuration
- the visual configuration
- reconstructed deidentified study CSVs
- reference `manuscript_selected/` outputs
- a sanitized reference run log/session record
- `RunReviewer.bat`
- a validator comparing regenerated manuscript tables and checking required
  manuscript figures

It does **not** contain:

- Git metadata
- identified data
- the non-manuscript extra figure/table library
- synthetic example data/configs
- development comparison tools

A reviewer run outputs only the numbered `manuscript_selected/` set plus compact
run logs needed to inspect reproduction.

## Script map

```text
R/00_setup.R                    global setup, config, output routing, helpers
R/01_data_import.R              import, identifier detection, deidentification
R/02_score_calculation.R        scoring and analysis-ready data
R/03_psychometrics.R            item/reliability analyses
R/04_analyses.R                 primary/supporting statistical analyses
R/05_figures.R                  canonical figures + Figure S1
R/06_tables.R                   canonical extra tables
R/07_demographics.R             optional demographics/SF-36 outputs
R/08_comparison_figures.R       optional cross-run comparison output
R/09_audit.R                    retired compatibility shim
R/10_manuscript_selected.R      final numbered manuscript/supplement assembly
R/11_publication_outputs.R      retired compatibility shim
R/12_build_reviewer_package.R   local-only reviewer-bundle builder
R/run_all.R                     fail-fast normal/reviewer pipeline orchestrator
R/run_comparison.R              comparison entry point
```

## Reproducibility behavior

A successful run records the resolved configuration after runtime overrides in:

```text
outputs/<study>/extra/logs/effective_config.yml
```

This prevents an exclusion or runtime choice from silently disappearing when a
reviewer package is created later.

Binary R analysis objects are retained under:

```text
outputs/<study>/extra/analysis_objects/
```

They are local derived artifacts and are not intended for Git.

## Repository safety

The Git ignore rules are designed so real study-data subfolders and generated
outputs do not get committed. The tracked repository should contain code,
documentation, configuration templates, and synthetic example data only.

Before pushing changes, inspect `git status` and run the repository safety check
if needed:

```powershell
powershell -ExecutionPolicy Bypass -File tools\Check-RepoSafety.ps1
```

## License and citation

See `LICENSE`, `COPYRIGHT.txt`, and `CITATION.cff` for reuse and citation terms.
