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

## Example data

The `example_data/` folder (in the repo root) contains a ready-to-run 100-participant
dataset. Run it with `example_data.yml` to verify your R environment is set up
correctly before using your own data.
