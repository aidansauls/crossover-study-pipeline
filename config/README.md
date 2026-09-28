# config/

This folder holds YAML configuration files. Each file controls one analysis run.
The `study.name` value in each config determines the output subfolder under `outputs/`.

## Files

| File | Purpose |
|---|---|
| `study_config.yml` | Master template -- copy and rename this for your own study |
| `example_data.yml` | Config used by the bundled 100-participant example dataset |
| `comparison_example.yml` | Generic comparison-workflow example; edit run names locally |

## Creating your own config

1. Copy `study_config.yml` and rename it -- e.g. `my_study.yml`
2. Set `study.name` to match your data folder name under `study_data/`
3. Update labels, item exclusions, colors, and thresholds as needed
4. Review `publication_outputs` to choose the main figures/tables and whether
   the complete supplementary figure/table trees should be assembled
5. Run with `RunPipeline.bat` and select your config when prompted

The default main-output IDs are:

- Figures: `paired_score_plot`, `power_curve`, `item_endorsement_by_sequence`
- Tables: `score_descriptive_summary`, `primary_paired_contrast`,
  `supporting_analysis_summary`

The assembled files are written to
`outputs/<study>/publication_outputs/main_figures/` and `main_tables/`. Set
`include_supplementary_figures` or `include_supplementary_tables` to `false`
when the full supplementary export is not wanted. Advanced users may put an
output-relative PNG/table path in a main list to select a custom output.
The power figure is additionally exported as a vector PDF when
`figures.power_curve.export_pdf` is `true` (the default).

The tracked template remains generic. After each successful run, the pipeline
writes the exact resolved settings (including runtime overrides) to
`outputs/<study>/run_provenance/effective_config.yml`. Keep real-study configs
local; do not add them to the public repository.

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
The tracked `comparison_example.yml` uses only generic names; copy it locally for
study-specific comparisons.
