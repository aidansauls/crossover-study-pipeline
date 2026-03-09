# Study Data Directory

This directory contains CSV data files for different studies.

## Structure

Each study has its own subdirectory:
- `pilot/` - Pilot study data (n=11, 20 questions, no demographics/SF-36)
- `current/` - Ongoing data collection (will be created when you archive CSVs)
- `study1/`, `study2/`, etc. - Future studies

## Data Export Workflow

1. **Export from Python app:**
   ```powershell
   cd C:\Users\fertn\OneDrive\Desktop\Research\study_app
   .\.venv\Scripts\Activate.ps1
   python export_to_csv.py
   ```
   This creates CSVs in the pipeline folder (parent directory)

2. **Run R analysis:**
   ```powershell
   cd ..\crossover-study-pipeline
   $env:STUDY_NAME = "current"  # or "pilot", etc.
   Rscript run_all.R
   ```

3. **Archive CSVs (optional):**
   ```powershell
   # After analysis completes
   Move-Item ..\crossover-study-pipeline\*.csv current\
   ```

## Pilot Study

Location: `study_data/pilot/`

Files:
- assignment.csv (11 participants)
- posttest_x_items.csv (20 questions)
- posttest_y_items.csv (20 questions)
- README.txt (documentation)

**Note:** Pilot has 20 questions and no demographics/SF-36 data.

## Current/Future Studies

When you export data and run analysis:
- CSVs will have 25 questions
- Demographics and SF-36 data included (if collected)
- Archive to `study_data/current/` or appropriate folder

See the root `README.md` for complete workflow details.
