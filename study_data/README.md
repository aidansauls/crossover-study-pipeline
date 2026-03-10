# study_data/

Place your study data here. This folder is not committed to the repository --
participant data should never be version-controlled.

## Folder structure

Create one subfolder per study, with the subfolder name matching the `study.name`
value in your config file:

```
study_data/
  my_study/
    assignment.csv
    posttest_x_items.csv
    posttest_y_items.csv
```

---

## File specifications

### assignment.csv

One row per participant. Describes the crossover order for each person.

| Column | Required | Values | Notes |
|---|---|---|---|
| `Participant_ID` | Yes | Any unique identifier | Text or number |
| `Intervention_Order` | Yes | `1st` or `2nd` | Which period the participant received the intervention |
| `Control_Order` | Yes | `1st` or `2nd` | Which period the participant received the control condition |
| `X_Order` | Yes | `1st` or `2nd` | Which period the participant took Form X |
| `Y_Order` | Yes | `1st` or `2nd` | Which period the participant took Form Y |

**Example:**

```csv
Participant_ID,Intervention_Order,Control_Order,X_Order,Y_Order
P001,1st,2nd,1st,2nd
P002,2nd,1st,2nd,1st
P003,1st,2nd,2nd,1st
P004,2nd,1st,1st,2nd
```

Notes:
- The pipeline accepts `1st`/`2nd` (case-insensitive). Do not use `1`/`2` or `First`/`Second`.
- `Intervention_Order` and `Control_Order` must both be present and must be inverses
  of each other for every participant. The pipeline will error if either is missing
  or contains an unrecognized value.
- An optional `Email` column is silently removed before analysis and saved to
  `outputs/<study>/InternalUse/participant_email_map.csv`.

---

### posttest_x_items.csv

One row per participant, one column per item. Contains raw item-level responses
for Form X (the first posttest form).

| Column | Required | Values | Notes |
|---|---|---|---|
| `Participant_ID` | Yes | Matches assignment.csv | Used to join with assignment and Form Y data |
| `X1`, `X2`, ... `Xn` | Yes | `0` (incorrect) or `1` (correct) | One column per scored item |

**Example:**

```csv
Participant_ID,X1,X2,X3,X4,X5
P001,1,0,1,1,0
P002,0,1,1,0,1
P003,1,1,0,1,1
P004,0,0,1,0,1
```

Notes:
- Column names must start with the prefix configured in `study_config.yml`
  (`items.x_prefix`, default `X`).
- Missing (`NA`) values are supported; participants with excessive missing data
  can be flagged via `analysis.min_complete_items` in the config.
- Item order within the file does not matter.

---

### posttest_y_items.csv

One row per participant, one column per item. Contains raw item-level responses
for Form Y (the second posttest form). Identical structure to `posttest_x_items.csv`.

| Column | Required | Values | Notes |
|---|---|---|---|
| `Participant_ID` | Yes | Matches assignment.csv | Used to join with assignment and Form X data |
| `Y1`, `Y2`, ... `Yn` | Yes | `0` (incorrect) or `1` (correct) | One column per scored item |

**Example:**

```csv
Participant_ID,Y1,Y2,Y3,Y4,Y5
P001,0,1,1,0,1
P002,1,0,1,1,0
P003,1,1,1,0,0
P004,0,1,0,1,1
```

Notes:
- Column names must start with the prefix configured in `study_config.yml`
  (`items.y_prefix`, default `Y`).
- The number of items in Form X and Form Y do not need to match; scores are
  scaled to a common metric defined by `scores.scale_to` in the config (default: 10).

---

## Pointing the pipeline at your data

Use `RunPipeline.bat` and choose option [1] (automatic scan) or [2] (manual path).
The scanner looks for subfolders of `study_data/` that contain all three required files.

---

## Example dataset

A fully-synthetic example dataset is included in this folder at `study_data/example_data/`.
It is the fastest way to verify your R environment is set up correctly before
working with your own data.

**What it contains:**

| File | Description |
|---|---|
| `assignment.csv` | 100 synthetic participants with randomised condition and form order |
| `posttest_x_items.csv` | Responses to 15 binary items on Form X |
| `posttest_y_items.csv` | Responses to 15 binary items on Form Y |

**How to run it:**

The easiest way is `RunPipeline.bat` option [1] — the automatic scanner will find
`study_data/example_data/` and list it alongside any real studies you have. Select
it and choose `config/example_data.yml` when prompted.

Alternatively, from a terminal:

```powershell
$env:PIPELINE_CONFIG = "config\example_data.yml"
$env:STUDY_DATA_PATH = "study_data\example_data"
Rscript R\run_all.R
```

Outputs will appear in `outputs/example_data/`.
