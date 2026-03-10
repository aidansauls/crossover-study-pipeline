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

## Required files

| File | Description |
|---|---|
| `assignment.csv` | Participant group assignments (one row per participant) |
| `posttest_x_items.csv` | Item-level responses for Posttest X |
| `posttest_y_items.csv` | Item-level responses for Posttest Y |

See [README section 3](../README.md#3-preparing-your-data) for the required column
format for each file.

## Pointing the pipeline at your data

Use `RunPipeline.bat` and choose option [1] (automatic scan) or [2] (manual path).
Alternatively, set the environment variable before running:

```powershell
$env:STUDY_DATA_PATH = "study_data\my_study"
```

## Example data

The `example_data/` folder (in the repo root) contains a ready-to-run 100-participant
dataset. Run it with `example_data.yml` to verify your R environment is set up
correctly before using your own data.
