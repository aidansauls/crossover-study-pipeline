# outputs/

Generated outputs land here. This folder is not committed to the repository --
all figures and tables are fully reproducible from your data and config file.

## Structure

One subfolder is created per study run, named after the `study.name` in the config:

```
outputs/
  my_study/
    figures/          PNG figures (primary, secondary, descriptive, etc.)
    tables/           CSV tables
    tables_png/       Formatted PNG versions of tables
    logs/             run.log, session JSON, and SUMMARY.txt
    rds/              Intermediate R objects (reuse with REUSE_DATA=1)
    InternalUse/      Participant-identifiable files (never share)
```

## REUSE_DATA

If the pipeline has already run once for a study, you can skip the data import and
scoring steps on subsequent runs by setting `REUSE_DATA=1`. The BAT launcher's
option [8] (Rerun last study) does this automatically.

## Comparison runs

When running multiple exclusion variants (multi-run mode or separate configs),
each variant gets its own subfolder. The comparison figures workflow reads from
multiple subfolders to build side-by-side panel figures.
