# config/

This folder holds YAML configuration files. Each file controls one analysis run.
The `study.name` value in each config determines the output subfolder under `outputs/`.

## Files

| File | Purpose |
|---|---|
| `study_config.yml` | Master template -- copy and rename this for your own study |
| `example_data.yml` | Config used by the bundled 100-participant example dataset |

## Creating your own config

1. Copy `study_config.yml` and rename it -- e.g. `my_study.yml`
2. Set `study.name` to match your data folder name under `study_data/`
3. Update labels, item exclusions, colors, and thresholds as needed
4. Run with `RunPipeline.bat` and select your config when prompted

## Multiple analysis variants

To run several exclusion variants of the same dataset, create one config file
per variant with different `study.name` values and different `item_exclusions`.
The pipeline's multi-run mode (option [2] at the exclusions prompt) automates this.

See the main [README](../README.md#6-multiple-analysis-variants) for the full
explanation and the comparison figures workflow.

## Comparison configs

Comparison configs (for generating side-by-side panel figures from multiple runs)
live here too. Use the naming convention `comparison_<study>.yml`.
The BAT's option [9] lists all `comparison_*.yml` files automatically.
See [README section 6](../README.md#6-multiple-analysis-variants) for the YAML format.
