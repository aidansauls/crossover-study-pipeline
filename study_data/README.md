# study_data/

Place local study inputs here. Real participant data must not be committed to the
repository. Only the synthetic `example_data/` directory is intended for Git.

## Folder structure

Use one folder per study:

```text
study_data/
  my_study/
    assignment.csv
    posttest_x_items.csv
    posttest_y_items.csv
```

The folder name does not have to equal the final output name when `STUDY_NAME` is
overridden, but keeping them aligned is usually less confusing.

## `assignment.csv`

Required logical fields are configured in `config/study_config.yml`:

```text
participant_id
intervention_order
control_order
form_x_order
form_y_order
```

With the default column mapping, the CSV headers are:

```text
Participant_ID
Intervention_Order
Control_Order
X_Order
Y_Order
```

Example:

```csv
Participant_ID,Intervention_Order,Control_Order,X_Order,Y_Order
P001,1st,2nd,1st,2nd
P002,2nd,1st,2nd,1st
P003,1st,2nd,2nd,1st
P004,2nd,1st,1st,2nd
```

The two condition-order columns must be complementary for every participant, as
must the two form-order columns.

The order parser accepts common first/second encodings including:

```text
1st / 2nd
first / second
1 / 2
period1 / period2
```

## `posttest_x_items.csv`

One row per participant with the participant key and binary scored items:

```csv
Participant_ID,X1,X2,X3,X4,X5
P001,1,0,1,1,0
P002,0,1,1,0,1
P003,1,1,0,1,1
P004,0,0,1,0,1
```

## `posttest_y_items.csv`

Same structure for Form Y:

```csv
Participant_ID,Y1,Y2,Y3,Y4,Y5
P001,0,1,1,0,1
P002,1,0,1,1,0
P003,1,1,1,0,0
P004,0,1,0,1,1
```

The forms do not have to contain the same number of included items. Participant
scores are rescaled to the common maximum configured under `scores.scale_to`.

## Optional timing column

If the configured timing column is present in the posttest files, the pipeline
parses common formats such as numeric seconds, `MM:SS`, `HH:MM:SS`, or strings
such as `9 min 46 sec`.

## Direct identifiers are allowed locally, but handled explicitly

For a real local analysis you may receive source files containing direct
identifiers such as names, emails, student IDs, medical-record numbers, dates of
birth, phone numbers, or addresses.

The import stage detects common direct-identifier column names. Additional
study-specific identifier columns can be declared in `study_config.yml`:

```yaml
privacy:
  additional_direct_identifier_columns:
    - local_subject_number
```

If direct identifiers are detected, the pipeline creates:

```text
outputs/<study>/extra/data/
  identified/
    assignment.csv
    posttest_x_items.csv
    posttest_y_items.csv
    participant_id_mapping.csv   # only when the participant key itself is direct

  deidentified/
    assignment.csv
    posttest_x_items.csv
    posttest_y_items.csv

  deidentification/
    manifest.csv
```

Downstream analysis uses the deidentified copies. Identified copies remain local
and are never included in a reviewer bundle.

If no direct identifiers are detected, `extra/data/` is not created at all.

A generic anonymous participant code such as `P001` is treated as a study ID.
A configured participant key explicitly named like `student_id` is treated as a
direct identifier and is pseudonymized before downstream analysis.

## Example data

`study_data/example_data/` is fully synthetic. Run it with
`config/example_data.yml` to verify the environment before using real data.

The Windows launcher will discover it automatically.

From PowerShell, an equivalent direct run is:

```powershell
$env:PIPELINE_CONFIG = "config\example_data.yml"
$env:STUDY_DATA_PATH = "study_data\example_data"
Rscript R\run_all.R
```

Generated output appears under:

```text
outputs/example_data/
  manuscript_selected/
  extra/
```

## Public-repository boundary

The tracked repository should contain only:

- code
- documentation
- config templates/example config
- synthetic example data

Real study inputs, all generated outputs, identified/deidentified local copies,
and reviewer reproduction bundles remain local and are ignored by Git.
