# config/

The pipeline deliberately separates study meaning from presentation settings.

## `study_config.yml`

This is the study/analysis configuration. It controls things that change what the data mean or how the analysis is defined, including:

- study and condition labels
- input column mapping
- direct-identifier handling
- item exclusions
- score scaling
- analysis settings
- post-hoc power targets
- manuscript wording
- whether optional analyses and Figure S1 are generated

Do not put font sizes, colors, table widths, or figure dimensions here.

## `visual_config.yml`

This is the presentation configuration. It controls things that change how outputs look without changing the analysis, including:

- figure dimensions and DPI
- font sizes and families
- colors
- point/line geometry
- legend layout
- Figure S1 styling
- manuscript-selected figure sizing
- ordinary and manuscript table sizing/column widths

`PIPELINE_VISUAL_CONFIG` can point to a different visual config when needed.

## `example_data.yml`

This is the example study configuration used with the repository's example data. It demonstrates the study/analysis schema without duplicating visual settings; visual defaults continue to come from `visual_config.yml`.

## Runtime overrides

The Windows launcher and comparison workflow may temporarily override selected settings through environment variables such as:

- `STUDY_NAME`
- `STUDY_DATA_PATH`
- `PIPELINE_CONFIG`
- `PIPELINE_VISUAL_CONFIG`
- `ITEM_EXCLUSIONS`
- `REUSE_DATA`
- `ANALYSIS_MODULES`

A successful full run writes the resolved effective configuration to:

```text
outputs/<study>/extra/logs/effective_config.yml
```

That effective configuration is what the reviewer-bundle builder uses, so item-exclusion or other runtime choices are not silently lost.

## Comparison configs

Comparison runs use their own `comparison_*.yml` files. The normal study-run output library is organized semantically under `extra/`; historical group names in older comparison configs are translated where possible for backward compatibility.
