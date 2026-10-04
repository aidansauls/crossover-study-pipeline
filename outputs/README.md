# outputs/

Generated study outputs land here. `outputs/` is derived material and should not be committed for real participant data.

## Standard study-run structure

Each study run has only two human-facing top-level routes:

```text
outputs/
  my_study/
    manuscript_selected/
      main_body/
        figures/
        tables/
      supplement/
        figures/
        tables/
      README.md
      manifest_validation.csv

    extra/
      figures/
        ceiling_and_floor/
        condition_effects/
        item_level/
        model_diagnostics/
        power_and_precision/
        psychometrics/
        sample_characteristics/
        score_distributions/
        sequence_and_period/
        study_design/
        timing/
        other/
      tables/
        ...semantic categories...
      analysis_objects/
      logs/
      data/                 # created only when direct identifiers are detected
        identified/
        deidentified/
        deidentification/
      reviewer_bundle/      # created only when explicitly requested
```

`manuscript_selected/` is the authoritative numbered manuscript/supplement set. A figure or table promoted into this directory is not kept as a duplicate manuscript-role file under `extra/`.

`extra/` contains everything that is useful for analysis, checking, diagnostics, exploration, or reproducibility but is not part of the selected manuscript set. Figure and table folders are named for what the output contains, not for whether it was historically called primary, supplementary, or exploratory.

## Identified and deidentified data

`extra/data/` is created only when the import step detects obvious direct identifiers such as names, email addresses, student IDs, MRNs, dates of birth, phone numbers, or addresses, including any study-specific identifier columns listed in the study config.

When direct identifiers are detected:

- `identified/` preserves the supplied source files and any ID mapping needed internally.
- `deidentified/` contains the analysis-safe CSV copies used downstream.
- `deidentification/manifest.csv` records which direct-identifier columns were detected.

When no direct identifiers are detected, no `extra/data/` folder is created.

## Logs

A full run writes a terminal-style transcript under:

```text
extra/logs/<timestamp>_run.log
extra/logs/latest_run.log
```

`latest_run.log` is the obvious first place to look after either a successful or failed run. The effective configuration and session information are also stored in `extra/logs/` for reproducibility.

## Reviewer bundle

The reviewer bundle is not generated on every run. Run `MakeReviewerBundle.bat` after a successful full study run.

The builder writes under:

```text
extra/reviewer_bundle/
  reviewer_reproducibility_bundle/
  reviewer_reproducibility_bundle.zip
```

The reviewer package contains only the scripts needed to reproduce `manuscript_selected/`, the exact effective configs, deidentified study inputs reconstructed from the analysis object, reference manuscript-selected outputs, and reviewer-facing logs/validation. It does not contain Git metadata, example data/configs, identified data, or the non-manuscript `extra/` figure/table library.

## REUSE_DATA

Set `REUSE_DATA=1` to reuse `extra/analysis_objects/*.rds` and skip import/scoring on a rerun. The Windows launcher exposes this as the rerun option after a completed run.
