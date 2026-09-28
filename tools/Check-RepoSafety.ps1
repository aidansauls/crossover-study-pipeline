[CmdletBinding()]
param()

$ErrorActionPreference = "Stop"

$repoRoot = (& git rev-parse --show-toplevel 2>$null).Trim()
if (-not $repoRoot) {
    throw "Run this script from inside the Git repository."
}

$tracked = @(& git -C $repoRoot ls-files)
$failures = [System.Collections.Generic.List[string]]::new()

foreach ($path in $tracked) {
    $slashPath = $path -replace '\\', '/'

    if ($slashPath -like 'study_data/*' -and
        $slashPath -ne 'study_data/README.md' -and
        $slashPath -notlike 'study_data/example_data/*') {
        $failures.Add("Unexpected tracked study data: $slashPath")
    }

    if ($slashPath -like 'outputs/*' -and $slashPath -ne 'outputs/README.md') {
        $failures.Add("Unexpected tracked generated output: $slashPath")
    }

    $fullPath = Join-Path $repoRoot $path
    if (-not (Test-Path -LiteralPath $fullPath -PathType Leaf)) { continue }

    if ([IO.Path]::GetExtension($path) -ieq '.csv') {
        $header = [IO.File]::ReadLines($fullPath) | Select-Object -First 1
        if ($header -match '(?i)(^|[,;`t])\s*["'']?[^,;`t]*email[^,;`t]*["'']?\s*($|[,;`t])') {
            $failures.Add("Tracked CSV has an email-named column: $slashPath")
        }
    }

    $extension = [IO.Path]::GetExtension($path).ToLowerInvariant()
    $textExtensions = @(
        '.r', '.rmd', '.md', '.txt', '.csv', '.tsv', '.yml', '.yaml',
        '.json', '.ps1', '.bat', '.sh', '.cff', '.toml', '.ini'
    )
    if ($extension -notin $textExtensions -and
        [IO.Path]::GetFileName($path) -notin @('.gitignore', '.gitattributes')) {
        continue
    }

    try {
        $text = [IO.File]::ReadAllText($fullPath)
    } catch {
        $failures.Add("Could not inspect tracked text file: $slashPath")
        continue
    }

    if ($text -match '(?i)\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}\b') {
        $failures.Add("Tracked text contains an email address: $slashPath")
    }
    if ($text -match '(?i)([A-Z]:\\(?:Users|GitHub)\\|/Users/)') {
        $failures.Add("Tracked text contains an absolute local-machine path: $slashPath")
    }
}

if ($failures.Count -gt 0) {
    Write-Host "REPOSITORY SAFETY CHECK: FAIL" -ForegroundColor Red
    $failures | Sort-Object -Unique | ForEach-Object { Write-Host "- $_" }
    exit 1
}

Write-Host "REPOSITORY SAFETY CHECK: PASS" -ForegroundColor Green
Write-Host "Checked $($tracked.Count) tracked files; no prohibited study data, generated outputs, email addresses, email columns, or absolute local paths were found."
exit 0
