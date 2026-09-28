# =============================================================================
# Run-Comparison.ps1  (legacy helper -- kept in tools\ for scripting use)
#
# The interactive way to run comparison figures is now RunPipeline.bat option [9].
# Use this script if you prefer PowerShell or need to automate from a pipeline.
#
# Requires:
#   - The runs named in config\comparison_example.yml have completed.
#   - magick R package (installed automatically on first run via 00_setup.R)
#
# Output:
#   outputs\comparison_example\figures\<subfolder>\<figure>.png
#
# To compare different runs, duplicate this script and point
# COMPARISON_CONFIG at a different YAML file.
# =============================================================================

$ErrorActionPreference = "Stop"

# Change to project root (wherever this script lives)
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Definition
Set-Location (Split-Path -Parent $scriptDir)

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
$env:COMPARISON_CONFIG = "config\comparison_example.yml"

$Rscript = (Get-Command Rscript.exe -ErrorAction SilentlyContinue).Source

# Auto-detect an R installation if Rscript is not on PATH.
if (-not $Rscript -or -not (Test-Path $Rscript)) {
    $candidates = Get-ChildItem "C:\Program Files\R" -Filter "Rscript.exe" -Recurse -ErrorAction SilentlyContinue |
                  Sort-Object LastWriteTime -Descending
    if ($candidates) { $Rscript = $candidates[0].FullName }
    else { Write-Error "Rscript.exe not found. Please install R."; exit 1 }
}

Write-Host ""
Write-Host "========================================================================"
Write-Host "  COMPARISON PANEL FIGURES"
Write-Host "  Config : $($env:COMPARISON_CONFIG)"
Write-Host "  Rscript: $Rscript"
Write-Host "========================================================================"
Write-Host ""

# ---------------------------------------------------------------------------
# Run
# ---------------------------------------------------------------------------
& $Rscript R\run_comparison.R 2>&1

$exitCode = $LASTEXITCODE
Write-Host ""
if ($exitCode -eq 0) {
    Write-Host "Comparison figures complete."
    Write-Host "Output: outputs\comparison_example\figures\"
} else {
    Write-Host "Comparison figures FAILED (exit code $exitCode)." -ForegroundColor Red
}
