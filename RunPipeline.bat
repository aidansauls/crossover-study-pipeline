@echo off
setlocal enabledelayedexpansion
chcp 65001 >nul 2>&1

:: =============================================================================
:: RunPipeline.bat
:: Crossover Study Analysis Pipeline
::
:: Usage:
::   RunPipeline.bat                      (fully interactive)
::   RunPipeline.bat --mode=1             (standard, prompts for folder)
::   RunPipeline.bat --mode=2 --data="path\to\folder"
::   RunPipeline.bat --study=my_study --data="path\to\folder" --modules=all
:: =============================================================================

title Crossover Study Analysis Pipeline

:: ---- Parse command-line arguments ----
set "ARG_MODE="
set "ARG_DATA="
set "ARG_STUDY="
set "ARG_MODULES=all"
set "ARG_SILENT=0"
set "ARG_REUSE=0"
set "ARG_CSV_X="
set "ARG_CSV_Y="
set "ARG_CSV_A="
set "ARG_EXCL="

for %%A in (%*) do (
    set "arg=%%~A"
    if /i "!arg:~0,7!"=="--mode=" set "ARG_MODE=!arg:~7!"
    if /i "!arg:~0,7!"=="--data=" set "ARG_DATA=!arg:~7!"
    if /i "!arg:~0,8!"=="--study=" set "ARG_STUDY=!arg:~8!"
    if /i "!arg:~0,10!"=="--modules=" set "ARG_MODULES=!arg:~10!"
    if /i "!arg:~0,8!"=="--silent" set "ARG_SILENT=1"
    if /i "!arg:~0,7!"=="--reuse" set "ARG_REUSE=1"
    if /i "!arg:~0,9!"=="--csv-x=" set "ARG_CSV_X=!arg:~9!"
    if /i "!arg:~0,9!"=="--csv-y=" set "ARG_CSV_Y=!arg:~9!"
    if /i "!arg:~0,9!"=="--csv-a=" set "ARG_CSV_A=!arg:~9!"
    if /i "!arg:~0,7!"=="--excl=" set "ARG_EXCL=!arg:~7!"
)

:: ---- Get pipeline root ----
set "PIPELINE_ROOT=%~dp0"
:: Remove trailing backslash
if "!PIPELINE_ROOT:~-1!"=="\" set "PIPELINE_ROOT=!PIPELINE_ROOT:~0,-1!"

:: =============================================================================
:: BANNER
:: =============================================================================
cls
echo.
echo  ================================================================
echo   CROSSOVER STUDY ANALYSIS PIPELINE
echo   Copyright (c) 2026 Aidan Sauls
echo   Free to use -- attribution required in published work
echo  ================================================================
echo.

:: =============================================================================
:: FIND RSCRIPT
:: =============================================================================
call :FIND_RSCRIPT
if "!RSCRIPT!"=="" (
    echo  [ERROR] R not found. Please install R from https://cran.r-project.org/
    echo          and ensure Rscript.exe is on your PATH or in C:\Program Files\R\
    pause
    exit /b 1
)
echo  R found: !RSCRIPT!
echo.

:: =============================================================================
:: MAIN MENU  (label so runs can loop back here)
:: =============================================================================
:: Direct-launch shortcuts (command-line flags — first entry only)
if not "!ARG_MODE!"=="" goto :MODE_!ARG_MODE! 2>nul
if "!ARG_SILENT!"=="1" goto :MODE_1

:MAIN_MENU
:: Reset per-run state so each visit starts clean
set "DATA_FOLDER="
set "STUDY_NAME="
set "ITEM_EXCL="
set "ITEM_EXCLUSIONS="
set "CSV_ASSIGNMENT="
set "CSV_POSTTEST_X="
set "CSV_POSTTEST_Y="
set "FILES_OK=1"
set "RUN_TYPE=1"
set "ARG_REUSE=0"
cls
echo.
echo  ================================================================
echo   CROSSOVER STUDY ANALYSIS PIPELINE
echo   R: !RSCRIPT!
echo  ================================================================
echo.
echo  Select a run mode:
echo.
echo    [1] Standard run       - Use default study_data\ folder
echo    [2] Specify folder     - Point to a folder with your CSV files
echo    [3] Specify CSV files  - Provide paths to each CSV individually
echo    [4] Manual data entry  - Enter participant data interactively
echo    [5] Custom modules     - Choose which analyses to run
echo    ----
echo    [6] Open outputs folder in Explorer
echo    [7] View last run SUMMARY.txt
if "!LAST_STUDY_NAME!"=="" (
echo    [8] Rerun last study    (no prior run this session)
) else (
echo    [8] Rerun last study    [!LAST_STUDY_NAME!]  skip import+scores
)
echo    [9] Run comparison figures
echo    [0] Exit
echo.
set /p "MENU_CHOICE=  Choice [1]: "
if "!MENU_CHOICE!"=="" set "MENU_CHOICE=1"
if "!MENU_CHOICE!"=="0" exit /b 0
if "!MENU_CHOICE!"=="1" goto :MODE_1
if "!MENU_CHOICE!"=="2" goto :MODE_2
if "!MENU_CHOICE!"=="3" goto :MODE_3
if "!MENU_CHOICE!"=="4" goto :MODE_4
if "!MENU_CHOICE!"=="5" goto :MODE_5
if "!MENU_CHOICE!"=="6" goto :MODE_6
if "!MENU_CHOICE!"=="7" goto :MODE_7
if "!MENU_CHOICE!"=="8" goto :MODE_8
if "!MENU_CHOICE!"=="9" goto :MODE_9
echo  Invalid choice -- please try again.
goto :MAIN_MENU

:: =============================================================================
:: MODE 1 — Standard run (study_data\ folder)
:: =============================================================================
:MODE_1
echo.
echo  ---- MODE 1: Standard Run ----
echo.

if not "!ARG_DATA!"=="" (
    set "DATA_FOLDER=!ARG_DATA!"
    goto :MODE_1_FOLDER_FOUND
)

set "DEFAULT_DATA=%PIPELINE_ROOT%\study_data"
if exist "!DEFAULT_DATA!" (
    echo  Available data folders in study_data\:
    set "FOLDER_COUNT=0"
    for /d %%D in ("!DEFAULT_DATA!\*") do (
        set /a FOLDER_COUNT+=1
        set "FOLDER_!FOLDER_COUNT!=%%D"
        echo    [!FOLDER_COUNT!] %%~nxD
    )
    echo    [M] Enter a custom path
    echo.
    if !FOLDER_COUNT! == 0 (
        echo  No subfolders found in study_data\
        goto :PROMPT_CUSTOM_PATH
    )
    set /p "FC=  Select folder number [1]: "
    if "!FC!"=="" set "FC=1"
    if /i "!FC!"=="M" goto :PROMPT_CUSTOM_PATH
    set "DATA_FOLDER="
    for /l %%I in (1,1,!FOLDER_COUNT!) do (
        if "%%I"=="!FC!" set "DATA_FOLDER=!FOLDER_%%I!"
    )
    if not "!DATA_FOLDER!"=="" goto :MODE_1_FOLDER_FOUND
    echo  Invalid selection.
    goto :PROMPT_CUSTOM_PATH
) else (
    goto :PROMPT_CUSTOM_PATH
)

:PROMPT_CUSTOM_PATH
set /p "DATA_FOLDER=  Enter path to data folder: "
if "!DATA_FOLDER!"=="" (
    echo  [ERROR] No path provided.
    pause
    goto :MAIN_MENU
)

:MODE_1_FOLDER_FOUND
:: Remove surrounding quotes if present
set "DATA_FOLDER=!DATA_FOLDER:"=!"
if not exist "!DATA_FOLDER!\" (
    echo  [ERROR] Folder not found: !DATA_FOLDER!
    pause
    goto :MAIN_MENU
)
goto :PROMPT_STUDY_NAME

:: =============================================================================
:: MODE 2 — Specify folder
:: =============================================================================
:MODE_2
echo.
echo  ---- MODE 2: Specify Folder ----
echo.
echo  Required files in the folder:
echo    assignment.csv  posttest_x_items.csv  posttest_y_items.csv
echo.
if not "!ARG_DATA!"=="" (
    set "DATA_FOLDER=!ARG_DATA:"=!"
) else (
    set /p "DATA_FOLDER=  Folder path: "
)
set "DATA_FOLDER=!DATA_FOLDER:"=!"
if not exist "!DATA_FOLDER!\" (
    echo  [ERROR] Folder not found: !DATA_FOLDER!
    pause
    goto :MAIN_MENU
)
goto :PROMPT_STUDY_NAME

:: =============================================================================
:: MODE 3 — Specify individual CSV paths
:: =============================================================================
:MODE_3
echo.
echo  ---- MODE 3: Specify Individual CSV Files ----
echo.
echo  Note: Item exclusions only work when CSVs contain item columns (X1, X2, Y1, Y2...).
echo.

if not "!ARG_CSV_A!"=="" (
    set "CSV_ASSIGNMENT=!ARG_CSV_A!"
) else (
    set /p "CSV_ASSIGNMENT=  Path to assignment.csv: "
)
set "CSV_ASSIGNMENT=!CSV_ASSIGNMENT:"=!"
if not exist "!CSV_ASSIGNMENT!" (
    echo  [ERROR] File not found: !CSV_ASSIGNMENT!
    pause
    goto :MAIN_MENU
)

if not "!ARG_CSV_X!"=="" (
    set "CSV_POSTTEST_X=!ARG_CSV_X!"
) else (
    set /p "CSV_POSTTEST_X=  Path to posttest_x_items.csv: "
)
set "CSV_POSTTEST_X=!CSV_POSTTEST_X:"=!"
if not exist "!CSV_POSTTEST_X!" (
    echo  [ERROR] File not found: !CSV_POSTTEST_X!
    pause
    goto :MAIN_MENU
)

if not "!ARG_CSV_Y!"=="" (
    set "CSV_POSTTEST_Y=!ARG_CSV_Y!"
) else (
    set /p "CSV_POSTTEST_Y=  Path to posttest_y_items.csv: "
)
set "CSV_POSTTEST_Y=!CSV_POSTTEST_Y:"=!"
if not exist "!CSV_POSTTEST_Y!" (
    echo  [ERROR] File not found: !CSV_POSTTEST_Y!
    pause
    goto :MAIN_MENU
)

:: Use the directory of the assignment CSV as a temporary DATA_FOLDER
for %%F in ("!CSV_ASSIGNMENT!") do set "DATA_FOLDER=%%~dpF"
if "!DATA_FOLDER:~-1!"=="\" set "DATA_FOLDER=!DATA_FOLDER:~0,-1!"

goto :PROMPT_ITEM_EXCLUSIONS

:: =============================================================================
:: MODE 4 — Manual data entry
:: =============================================================================
:MODE_4
echo.
echo  ---- MODE 4: Manual Data Entry ----
echo.
echo  Launching the data entry wizard...
echo  (Requires R — this may take a moment to start.)
echo.
"!RSCRIPT!" "%PIPELINE_ROOT%\tools\data_entry.R"
if errorlevel 1 (
    echo.
    echo  [ERROR] Data entry wizard failed.
    pause
    exit /b 1
)
echo.
echo  Data entry complete. Return to the menu and use Mode 2 to analyse your data.
echo.
pause
goto :MAIN_MENU

:: =============================================================================
:: MODE 5 — Custom module selection
:: =============================================================================
:MODE_5
echo.
echo  ---- MODE 5: Custom Module Selection ----
echo.
echo  Available analysis modules:
echo    import        - Load and validate CSV files           (always run)
echo    scores        - Calculate scores                      (always run)
echo    psychometrics - Item analysis, reliability, DIF
echo    analyses      - Contrasts, period effects, mixed models
echo    figures       - All PNG figures
echo    tables        - All CSV + PNG tables
echo.
echo  Enter comma-separated list, or 'all' for everything.
echo.
set /p "CUSTOM_MODULES=  Modules to run [all]: "
if "!CUSTOM_MODULES!"=="" set "CUSTOM_MODULES=all"
set "ARG_MODULES=!CUSTOM_MODULES!"
echo.
echo  Now select data source:
echo    [1] Standard (study_data\ folder)
echo    [2] Specify folder
echo    [3] Specify individual CSV files
echo.
set /p "DS_CHOICE=  Data source [1]: "
if "!DS_CHOICE!"=="" set "DS_CHOICE=1"
if "!DS_CHOICE!"=="1" goto :MODE_1
if "!DS_CHOICE!"=="2" goto :MODE_2
if "!DS_CHOICE!"=="3" goto :MODE_3
echo  Invalid choice -- returning to menu.
pause
goto :MAIN_MENU

:: =============================================================================
:: MODE 6 — Open outputs folder in Explorer
:: =============================================================================
:MODE_6
set "_OUTROOT=%PIPELINE_ROOT%\outputs"
if not exist "!_OUTROOT!" mkdir "!_OUTROOT!" 2>nul
explorer "!_OUTROOT!"
goto :MAIN_MENU

:: =============================================================================
:: MODE 7 — View last run SUMMARY.txt
:: =============================================================================
:MODE_7
echo.
set "LATEST_SUMMARY="
for /f "delims=" %%F in ('dir /b /s /o-d "%PIPELINE_ROOT%\outputs\*_SUMMARY.txt" 2^>nul') do (
    if "!LATEST_SUMMARY!"=="" set "LATEST_SUMMARY=%%F"
)
if "!LATEST_SUMMARY!"=="" (
    echo  No SUMMARY.txt found in outputs\ yet. Run an analysis first.
    echo.
    pause
    goto :MAIN_MENU
)
echo  Showing: !LATEST_SUMMARY!
echo.
type "!LATEST_SUMMARY!"
echo.
pause
goto :MAIN_MENU

:: =============================================================================
:: MODE 8 — Rerun last study (REUSE_DATA=1, skips import + scores)
:: =============================================================================
:MODE_8
if "!LAST_STUDY_NAME!"=="" (
    echo.
    echo  No previous run found in this session. Run a study first.
    echo.
    pause
    goto :MAIN_MENU
)
echo.
echo  ---- Rerun: !LAST_STUDY_NAME! (skipping import + scores) ----
echo.
set "STUDY_NAME=!LAST_STUDY_NAME!"
set "STUDY_DATA_PATH=!LAST_DATA_FOLDER!"
set "ANALYSIS_MODULES=all"
set "REUSE_DATA=1"
set "ITEM_EXCLUSIONS=!LAST_ITEM_EXCL!"
if defined LAST_CONFIG set "PIPELINE_CONFIG=!LAST_CONFIG!"
echo  Running...
echo.
pushd "!PIPELINE_ROOT!"
"!RSCRIPT!" --vanilla "%PIPELINE_ROOT%\R\run_all.R"
set "EXIT_CODE=!ERRORLEVEL!"
popd
echo.
echo  ================================================================
if "!EXIT_CODE!"=="0" (
    echo   RUN COMPLETE: !STUDY_NAME!
    echo  ================================================================
    echo.
    echo   Output location: %PIPELINE_ROOT%\outputs\!STUDY_NAME!\
    echo.
) else (
    echo   RUN FAILED  ^(exit code: !EXIT_CODE!^)
    echo  ================================================================
    echo.
    echo   Check outputs\!STUDY_NAME!\logs\ for error details.
    echo.
)
goto :POST_RUN_MENU

:: =============================================================================
:: MODE 9 — Run comparison figures
:: =============================================================================
:MODE_9
set "_DELETE_YAML=0"
set "COMP_CONFIG="
echo.
echo  ---- Run Comparison ----
echo.
echo  Setup options:
echo    [1] Build comparison interactively  (pick runs, assign labels)
echo    [2] Use a saved comparison config
echo    [M] Enter a custom config path
echo.
set /p "_M9C=  Choice [1]: "
if "!_M9C!"=="" set "_M9C=1"
if "!_M9C!"=="1" goto :M9_INTERACTIVE
if "!_M9C!"=="2" goto :M9_YAML
if /i "!_M9C!"=="M" goto :M9_CUSTOM
echo  Invalid choice.
goto :MODE_9

:: ---------------------------------------------------------------------------
:: M9_INTERACTIVE — guided setup: pick runs, assign labels, choose groups
:: ---------------------------------------------------------------------------
:M9_INTERACTIVE
echo.
echo  ---- Interactive Comparison Setup ----
echo.
echo  Available run output folders:
echo.
set "_OUT_COUNT=0"
for /d %%D in ("%PIPELINE_ROOT%\outputs\*") do (
    set /a _OUT_COUNT+=1
    set "_OUT_!_OUT_COUNT!=%%~nxD"
    echo    [!_OUT_COUNT!] %%~nxD
)
if !_OUT_COUNT! == 0 (
    echo  No output folders found. Run a standard analysis first.
    echo.
    pause
    goto :MAIN_MENU
)
echo.
echo  Select runs to compare (minimum 2). Enter a run number, then Enter.
echo  Press Enter on a blank line when done.
echo.
set "_SEL_COUNT=0"

:_M9I_PICK
if !_SEL_COUNT! EQU 0 goto :_M9I_NO_SHOW
echo  Runs selected so far:
set /a "_M9I_SI=1"
:_M9I_SH
if !_M9I_SI! GTR !_SEL_COUNT! goto :_M9I_SH_END
call set "_M9I_SN=%%_SEL_!_M9I_SI!_NAME%%"
call set "_M9I_SL=%%_SEL_!_M9I_SI!_LABEL%%"
echo    !_M9I_SI!. !_M9I_SN!  [!_M9I_SL!]
set /a _M9I_SI+=1
goto :_M9I_SH
:_M9I_SH_END
echo.

:_M9I_NO_SHOW
if !_SEL_COUNT! GEQ 2 goto :_M9I_OPTIONAL
set /a "_M9I_NEXT=_SEL_COUNT+1"
set "_PKS="
set /p "_PKS=  Run !_M9I_NEXT! [number]: "
if "!_PKS!"=="" echo  At least 2 runs required.
if "!_PKS!"=="" goto :_M9I_PICK
goto :_M9I_VALIDATE

:_M9I_OPTIONAL
set /a "_M9I_NEXT=_SEL_COUNT+1"
set "_PKS="
set /p "_PKS=  Add run !_M9I_NEXT! [number, or Enter to finish]: "
if "!_PKS!"=="" goto :_M9I_PICK_DONE

:_M9I_VALIDATE
set "_VALID_PKS=0"
for /l %%I in (1,1,!_OUT_COUNT!) do if "%%I"=="!_PKS!" set "_VALID_PKS=1"
if "!_VALID_PKS!"=="0" echo  Not a valid number (1-!_OUT_COUNT!).
if "!_VALID_PKS!"=="0" goto :_M9I_PICK
:: Duplicate check
set "_DUPE=0"
set /a "_M9I_DI=1"
:_M9I_DC
if !_M9I_DI! GTR !_SEL_COUNT! goto :_M9I_DC_END
call set "_M9I_EN=%%_SEL_!_M9I_DI!_NAME%%"
call set "_M9I_CN=%%_OUT_!_PKS!%%"
if "!_M9I_EN!"=="!_M9I_CN!" set "_DUPE=1"
set /a _M9I_DI+=1
goto :_M9I_DC
:_M9I_DC_END
if "!_DUPE!"=="1" echo  Already in list.
if "!_DUPE!"=="1" goto :_M9I_PICK
:: Store selection, prompt for label
set /a _SEL_COUNT+=1
call set "_SEL_!_SEL_COUNT!_NAME=%%_OUT_!_PKS!%%"
call set "_M9I_DEF=%%_OUT_!_PKS!%%"
set /p "_LBL=  Label [!_M9I_DEF!]: "
if "!_LBL!"=="" set "_LBL=!_M9I_DEF!"
:: Sanitize & in labels (& is a CMD separator and causes parse errors when expanded)
set "_LBL=!_LBL: & = and !"
set "_LBL=!_LBL:&= and !"
set "_SEL_!_SEL_COUNT!_LABEL=!_LBL!"
goto :_M9I_PICK

:_M9I_PICK_DONE
echo  DEBUG: _M9I_PICK_DONE reached, _SEL_COUNT=!_SEL_COUNT!
:: Output folder name
echo.
set "_CMP_DEFAULT_OUT=comparison_custom"
set /p "_CMP_OUTNAME=  Output folder name [!_CMP_DEFAULT_OUT!]: "
if "!_CMP_OUTNAME!"=="" set "_CMP_OUTNAME=!_CMP_DEFAULT_OUT!"
:: Sanitize shell-unsafe characters from folder name
set "_CMP_OUTNAME=!_CMP_OUTNAME:&=-and-!"
set "_CMP_OUTNAME=!_CMP_OUTNAME:(=!"
set "_CMP_OUTNAME=!_CMP_OUTNAME:)=!"
set "_CMP_OUTNAME=!_CMP_OUTNAME:<=!"
set "_CMP_OUTNAME=!_CMP_OUTNAME:>=!"
set "_CMP_OUTNAME=!_CMP_OUTNAME:|=!"
echo  DEBUG: _CMP_OUTNAME after sanitize = [!_CMP_OUTNAME!]

:: Figure groups
echo.
echo  Figure groups to stitch:
echo    [A] All subfolders
echo    [P] Primary + Mixed Models + Psychometrics  (recommended)
echo    [C] Custom  (enter comma-separated names, e.g. primary,mixed_models)
echo.
set "_FG="
set /p "_FG=  Choice [P]: "
if "!_FG!"=="" set "_FG=P"

:: Save as reusable YAML?
echo.
set "_SAVE_YML="
set /p "_SAVE_YML=  Save as a reusable comparison config? [Y/n]: "
if "!_SAVE_YML!"=="" set "_SAVE_YML=Y"
if /i "!_SAVE_YML!"=="n" goto :_M9I_TEMP_YAML
:: Y path — prompt for filename
set "_YAML_DEFAULT=!_CMP_OUTNAME!"
set /p "_YAML_FNAME=  Filename without .yml [!_YAML_DEFAULT!]: "
if "!_YAML_FNAME!"=="" set "_YAML_FNAME=!_YAML_DEFAULT!"
:: Sanitize shell-unsafe characters from config filename
set "_YAML_FNAME=!_YAML_FNAME:&=-and-!"
set "_YAML_FNAME=!_YAML_FNAME:(=!"
set "_YAML_FNAME=!_YAML_FNAME:)=!"
set "_YAML_FNAME=!_YAML_FNAME:<=!"
set "_YAML_FNAME=!_YAML_FNAME:>=!"
set "_YAML_FNAME=!_YAML_FNAME:|=!"
set "_YAML_PATH=%PIPELINE_ROOT%\config\!_YAML_FNAME!.yml"
set "_DELETE_YAML=0"
echo  DEBUG: _YAML_FNAME=[!_YAML_FNAME!] _YAML_PATH=[!_YAML_PATH!]
goto :_M9I_WRITE_YAML
:_M9I_TEMP_YAML
set "_YAML_PATH=%PIPELINE_ROOT%\config\_comparison_temp.yml"
set "_DELETE_YAML=1"

:_M9I_WRITE_YAML
echo  DEBUG: _M9I_WRITE_YAML starting, writing to [!_YAML_PATH!]
(
echo comparison:
echo   runs:
) > "!_YAML_PATH!"
set /a "_M9I_YI=1"
:_M9I_YL
if !_M9I_YI! GTR !_SEL_COUNT! goto :_M9I_YL_END
call set "_M9I_YN=%%_SEL_!_M9I_YI!_NAME%%"
call set "_M9I_YL=%%_SEL_!_M9I_YI!_LABEL%%"
(
echo     - name:  "!_M9I_YN!"
echo       label: "!_M9I_YL!"
) >> "!_YAML_PATH!"
set /a _M9I_YI+=1
goto :_M9I_YL
:_M9I_YL_END
(
echo   layout:       "side_by_side"
echo   output_name:  "!_CMP_OUTNAME!"
echo   common_scale: false
echo   dpi:          300
echo   label_font_size: 36
) >> "!_YAML_PATH!"

if /i "!_FG!"=="A" goto :_M9I_FG_ALL
if /i "!_FG!"=="C" goto :_M9I_FG_CUSTOM
:: default = P
(
echo   figure_groups:
echo     - "primary"
echo     - "mixed_models"
echo     - "psychometrics"
) >> "!_YAML_PATH!"
goto :_M9I_FG_DONE

:_M9I_FG_ALL
echo   figure_groups: "all"  >> "!_YAML_PATH!"
goto :_M9I_FG_DONE

:_M9I_FG_CUSTOM
set "_CGRPS="
set /p "_CGRPS=  Group names (comma-separated, e.g. primary,mixed_models): "
echo   figure_groups:  >> "!_YAML_PATH!"
set "_CGRPS_SPC=!_CGRPS:,= !"
for %%G in (!_CGRPS_SPC!) do echo     - "%%G"  >> "!_YAML_PATH!"

:_M9I_FG_DONE
echo  DEBUG: _M9I_FG_DONE reached
set "COMP_CONFIG=!_YAML_PATH!"
echo  DEBUG: before config-saved echo, _DELETE_YAML=[!_DELETE_YAML!]
echo.
if "!_DELETE_YAML!"=="1" echo  [Config written (temporary): !_YAML_PATH!]
if "!_DELETE_YAML!"=="0" echo  [Config saved: !_YAML_PATH!]
echo  DEBUG: after config-saved echo
echo  DEBUG: about to goto _COMP_RUN_M9
goto :_COMP_RUN_M9

:: ---------------------------------------------------------------------------
:: M9_YAML — pick from existing comparison_*.yml files
:: ---------------------------------------------------------------------------
:M9_YAML
echo.
echo  Saved comparison configs in config\:
echo.
set "COMP_COUNT=0"
for %%F in ("%PIPELINE_ROOT%\config\comparison_*.yml") do (
    set /a COMP_COUNT+=1
    set "COMP_!COMP_COUNT!=%%F"
    echo    [!COMP_COUNT!] %%~nxF
)
if !COMP_COUNT! == 0 (
    echo  No saved configs found. Use option [1] to build one interactively.
    echo.
    pause
    goto :MODE_9
)
echo.
set /p "CC=  Select [1]: "
if "!CC!"=="" set "CC=1"
set "COMP_CONFIG="
for /l %%I in (1,1,!COMP_COUNT!) do if "%%I"=="!CC!" set "COMP_CONFIG=!COMP_%%I!"
if "!COMP_CONFIG!"=="" (
    echo  Invalid selection.
    pause
    goto :MODE_9
)
goto :_COMP_RUN_M9

:: ---------------------------------------------------------------------------
:: M9_CUSTOM — type a path
:: ---------------------------------------------------------------------------
:M9_CUSTOM
set /p "COMP_CONFIG=  Path to comparison config: "
set "COMP_CONFIG=!COMP_CONFIG:"=!"

:: ---------------------------------------------------------------------------
:: _COMP_RUN_M9 — validate and execute
:: ---------------------------------------------------------------------------
:_COMP_RUN_M9
echo  DEBUG: entering _COMP_RUN_M9
if "!COMP_CONFIG!"=="" (
    echo  No config specified.
    pause
    goto :MAIN_MENU
)
if not exist "!COMP_CONFIG!" (
    echo  [ERROR] File not found: !COMP_CONFIG!
    pause
    goto :MAIN_MENU
)
echo.
echo  Running comparison...
echo.
set "COMPARISON_CONFIG=!COMP_CONFIG!"
pushd "!PIPELINE_ROOT!"
"!RSCRIPT!" --vanilla "%PIPELINE_ROOT%\R\run_comparison.R"
set "_CE9=!ERRORLEVEL!"
popd
echo  DEBUG: Rscript exit code = !_CE9!
if "!_DELETE_YAML!"=="1" del "!COMP_CONFIG!" 2>nul
if "!_CE9!"=="0" (
    echo.
    echo  Comparison complete.
    if not "!_CMP_OUTNAME!"=="" (
        echo  Outputs written to: %PIPELINE_ROOT%\outputs\!_CMP_OUTNAME!\
    ) else (
        echo  Outputs written to: %PIPELINE_ROOT%\outputs\
    )
    echo.
    explorer "%PIPELINE_ROOT%\outputs"
) else (
    echo.
    echo  [ERROR] Comparison pipeline failed ^(exit code !_CE9!^).
    echo  Check outputs for details.
    echo.
)
echo.
:_M9_POSTRUN_WAIT
set "_M9CHOICE="
set /p "_M9CHOICE=  Press Enter to return to the main menu. "
if not "!_M9CHOICE!"=="" goto :_M9_POSTRUN_WAIT
echo  DEBUG: returning to MAIN_MENU from interactive comparison
goto :MAIN_MENU

:: =============================================================================
:: PROMPT STUDY NAME
:: =============================================================================
:PROMPT_STUDY_NAME

:: Item exclusions prompt (for modes 1 & 2)
:PROMPT_ITEM_EXCLUSIONS
if not "!ARG_EXCL!"=="" set "ITEM_EXCL=!ARG_EXCL!"
if not "!ARG_EXCL!"=="" goto :PROMPT_STUDY_NAME_2
echo.
echo  Run type:
echo    [1] Single analysis
echo    [2] Multiple variants  (runs pipeline once per exclusion set, for comparison figures)
echo.
set /p "RUN_TYPE=  [1]: "
if "!RUN_TYPE!"=="" set "RUN_TYPE=1"
if "!RUN_TYPE!"=="2" goto :MULTI_RUN_SETUP
echo.
echo  Item exclusions (optional):
echo  Items to exclude from restricted scoring -- e.g. y1   or   y1, y6
echo  Enter X-form items as x1, x2 ...  Y-form items as y1, y2 ...
echo  Press Enter to skip.
echo.
set /p "ITEM_EXCL=  Exclusions: "

:PROMPT_STUDY_NAME_2
if not "!ARG_STUDY!"=="" set "STUDY_NAME=!ARG_STUDY!"
if not "!ARG_STUDY!"=="" goto :SHOW_CONFIG

:: Derive default study name from folder
for %%D in ("!DATA_FOLDER!") do set "DEFAULT_STUDY=%%~nxD"
echo.
set /p "STUDY_NAME=  Study name for output folder [!DEFAULT_STUDY!]: "
if "!STUDY_NAME!"=="" set "STUDY_NAME=!DEFAULT_STUDY!"
goto :SHOW_CONFIG

:: =============================================================================
:: MULTI-RUN SETUP  (multiple exclusion variants, one pipeline run per set)
:: =============================================================================
:MULTI_RUN_SETUP
echo.
echo  ---- Multi-Run Variant Setup ----
echo.
echo  Enter one exclusion set per variant. The pipeline runs once per set,
echo  each saving to its own output folder under outputs\.
echo.
echo  Format: y1     or   y1,y6     or   x3,y1,y6
echo  Type "none" for a full-scoring (no exclusions) baseline variant.
echo  Press Enter on a blank line when done entering sets.
echo.

:: Base study name
for %%D in ("!DATA_FOLDER!") do set "DEFAULT_STUDY=%%~nxD"
set /p "BASE_STUDY=  Base study name [!DEFAULT_STUDY!]: "
if "!BASE_STUDY!"=="" set "BASE_STUDY=!DEFAULT_STUDY!"
echo.

:: Collect exclusion sets interactively
set "EXCL_COUNT=0"

:_MR_COLLECT
set /a EXCL_COUNT+=1
set "ES="
set /p "ES=  Variant !EXCL_COUNT! (blank to finish): "
if "!ES!"=="" (
    set /a EXCL_COUNT-=1
    goto :_MR_COLLECT_DONE
)
set "EXCL_SET_!EXCL_COUNT!=!ES!"
:: Derive output folder name suffix
if /i "!ES!"=="none" (
    set "VARIANT_NAME_!EXCL_COUNT!=!BASE_STUDY!_full"
) else (
    set "_VFX=!ES: =!"
    set "_VFX=!_VFX:,=_!"
    set "VARIANT_NAME_!EXCL_COUNT!=!BASE_STUDY!_excl_!_VFX!"
)
goto :_MR_COLLECT

:_MR_COLLECT_DONE
if !EXCL_COUNT! == 0 echo.
if !EXCL_COUNT! == 0 echo  No variants entered. Switching to single-run mode.
if !EXCL_COUNT! == 0 goto :PROMPT_STUDY_NAME_2

:: Show planned runs
echo.
echo  Planned runs:
echo.
set "_SHI=1"
:_MR_SHOWLOOP
if !_SHI! GTR !EXCL_COUNT! goto :_MR_SHOWLOOP_DONE
call set "_SHE=%%EXCL_SET_!_SHI!%%"
call set "_SHN=%%VARIANT_NAME_!_SHI!%%"
echo    Run !_SHI!: !_SHN!   (exclude: !_SHE!)
set /a _SHI+=1
goto :_MR_SHOWLOOP
:_MR_SHOWLOOP_DONE

:: Check required data files once before the loop
echo.
echo  Checking data files...
set "FILES_OK=1"
if defined CSV_ASSIGNMENT (
    call :CHECK_FILE "!CSV_ASSIGNMENT!"                    "assignment.csv"
    call :CHECK_FILE "!CSV_POSTTEST_X!"                   "posttest_x_items.csv"
    call :CHECK_FILE "!CSV_POSTTEST_Y!"                   "posttest_y_items.csv"
) else (
    call :CHECK_FILE "!DATA_FOLDER!\assignment.csv"        "assignment.csv"
    call :CHECK_FILE "!DATA_FOLDER!\posttest_x_items.csv" "posttest_x_items.csv"
    call :CHECK_FILE "!DATA_FOLDER!\posttest_y_items.csv" "posttest_y_items.csv"
)
if "!FILES_OK!"=="0" (
    echo.
    echo  [ERROR] Required files missing. Exiting.
    pause
    exit /b 1
)

:: Resolve config
set "CONFIG_FILE=%PIPELINE_ROOT%\config\study_config.yml"
if not exist "!CONFIG_FILE!" set "CONFIG_FILE="

echo.
set /p "_MRC=  Proceed with !EXCL_COUNT! runs? [Y/n]: "
if /i "!_MRC!"=="n" goto :MULTI_RUN_SETUP

:: ---- Run loop ----
set "_MRI=1"
set "_MR_OK=0"
set "_MR_FAIL=0"

:_MR_RUNLOOP
if !_MRI! GTR !EXCL_COUNT! goto :_MR_RUNDONE
call set "_CE=%%EXCL_SET_!_MRI!%%"
call set "_CN=%%VARIANT_NAME_!_MRI!%%"
if /i "!_CE!"=="none" set "_CE=NONE"

echo.
echo  ================================================================
echo   RUN !_MRI! / !EXCL_COUNT!  --  !_CN!
if /i "!_CE!"=="NONE" (
    echo   Exclusions : none  ^(full scoring^)
) else (
    echo   Exclusions : !_CE!
)
echo  ================================================================
echo.

set "STUDY_NAME=!_CN!"
set "STUDY_DATA_PATH=!DATA_FOLDER!"
set "ANALYSIS_MODULES=!ARG_MODULES!"
set "REUSE_DATA=!ARG_REUSE!"
set "ITEM_EXCLUSIONS=!_CE!"
if defined CONFIG_FILE set "PIPELINE_CONFIG=!CONFIG_FILE!"
if defined CSV_ASSIGNMENT set "CSV_ASSIGNMENT=!CSV_ASSIGNMENT!"
if defined CSV_POSTTEST_X set "CSV_POSTTEST_X=!CSV_POSTTEST_X!"
if defined CSV_POSTTEST_Y set "CSV_POSTTEST_Y=!CSV_POSTTEST_Y!"

pushd "!PIPELINE_ROOT!"
"!RSCRIPT!" --vanilla "%PIPELINE_ROOT%\R\run_all.R"
set "_VE=!ERRORLEVEL!"
popd

if "!_VE!"=="0" (
    echo.
    echo  [DONE] !_CN!
    set /a _MR_OK+=1
) else (
    echo.
    echo  [FAIL] !_CN!  ^(exit code !_VE!^)
    set /a _MR_FAIL+=1
)
set /a _MRI+=1
goto :_MR_RUNLOOP

:_MR_RUNDONE
echo.
echo  ================================================================
echo   MULTI-RUN COMPLETE
echo  ================================================================
echo   Runs completed : !_MR_OK! / !EXCL_COUNT!
if !_MR_FAIL! GTR 0 echo   Runs failed    : !_MR_FAIL!
echo.
echo   Output folders:
set "_SLI=1"
:_MR_SUMLOOP
if !_SLI! GTR !EXCL_COUNT! goto :_MR_SUMLOOP_DONE
call set "_SLN=%%VARIANT_NAME_!_SLI!%%"
echo     outputs\!_SLN!\
set /a _SLI+=1
goto :_MR_SUMLOOP
:_MR_SUMLOOP_DONE

if !_MR_OK! GTR 1 (
    echo.
    set /p "_MRQ=  Generate cross-run comparison now? [Y/n]: "
    if /i "!_MRQ!" neq "n" call :_AUTO_COMPARE
)
:: Save last-run variant for Mode 8 rerun
set "LAST_STUDY_NAME=!_CN!"
set "LAST_DATA_FOLDER=!DATA_FOLDER!"
set "LAST_ITEM_EXCL="
set "LAST_CONFIG=!CONFIG_FILE!"
echo.
set /p "_MRN=  Press Enter for main menu, or Q to quit: "
if /i "!_MRN!"=="Q" exit /b !_MR_FAIL!
goto :MAIN_MENU

:: =============================================================================
:: SHOW CONFIG SUMMARY
:: =============================================================================
:SHOW_CONFIG
echo.
echo  ================================================================
echo   CONFIGURATION SUMMARY
echo  ================================================================
echo.
echo   Data folder   : !DATA_FOLDER!
if defined CSV_ASSIGNMENT (
    echo   Assignment CSV: !CSV_ASSIGNMENT!
    echo   Form X CSV    : !CSV_POSTTEST_X!
    echo   Form Y CSV    : !CSV_POSTTEST_Y!
)
echo   Study name    : !STUDY_NAME!
echo   Modules       : !ARG_MODULES!
if not "!ITEM_EXCL!"=="" echo   Item exclusions: !ITEM_EXCL!
echo.
echo   Output path   : %PIPELINE_ROOT%\outputs\!STUDY_NAME!\
echo.

:: Check required files
echo  Checking files...
set "FILES_OK=1"

if defined CSV_ASSIGNMENT (
    call :CHECK_FILE "!CSV_ASSIGNMENT!" "assignment.csv"
    call :CHECK_FILE "!CSV_POSTTEST_X!" "posttest_x_items.csv"
    call :CHECK_FILE "!CSV_POSTTEST_Y!" "posttest_y_items.csv"
) else (
    call :CHECK_FILE "!DATA_FOLDER!\assignment.csv" "assignment.csv"
    call :CHECK_FILE "!DATA_FOLDER!\posttest_x_items.csv" "posttest_x_items.csv"
    call :CHECK_FILE "!DATA_FOLDER!\posttest_y_items.csv" "posttest_y_items.csv"
)

if "!FILES_OK!"=="0" (
    echo.
    echo  [ERROR] Required files missing -- returning to menu.
    pause
    goto :MAIN_MENU
)
echo  All required files found.
echo.

:: Check config file
set "CONFIG_FILE=%PIPELINE_ROOT%\config\study_config.yml"
if exist "!CONFIG_FILE!" (
    echo  Config  : study_config.yml [OK]
) else (
    echo  Config  : config\study_config.yml [NOT FOUND — using defaults]
    set "CONFIG_FILE="
)
echo.

:: =============================================================================
:: RUN PIPELINE
:: =============================================================================
echo  ================================================================
echo   RUNNING PIPELINE
echo  ================================================================
echo.
echo  Study: !STUDY_NAME!
echo  This may take a few minutes...
echo.

:: Set environment variables
set "STUDY_NAME=!STUDY_NAME!"
set "STUDY_DATA_PATH=!DATA_FOLDER!"
set "ANALYSIS_MODULES=!ARG_MODULES!"
set "REUSE_DATA=!ARG_REUSE!"

if defined CONFIG_FILE (
    set "PIPELINE_CONFIG=!CONFIG_FILE!"
)

:: Individual CSV overrides (mode 3)
if defined CSV_ASSIGNMENT   set "CSV_ASSIGNMENT=!CSV_ASSIGNMENT!"
if defined CSV_POSTTEST_X   set "CSV_POSTTEST_X=!CSV_POSTTEST_X!"
if defined CSV_POSTTEST_Y   set "CSV_POSTTEST_Y=!CSV_POSTTEST_Y!"

:: Item exclusions → parse into config env vars
if not "!ITEM_EXCL!"=="" (
    set "ITEM_EXCLUSIONS=!ITEM_EXCL!"
)

:: Change to pipeline root so relative paths work in R
pushd "!PIPELINE_ROOT!"

set "START_TIME=%TIME%"
"!RSCRIPT!" --vanilla "%PIPELINE_ROOT%\R\run_all.R"
set "EXIT_CODE=!ERRORLEVEL!"
popd

echo.
echo  ================================================================

if "!EXIT_CODE!"=="0" (
    echo   PIPELINE COMPLETED SUCCESSFULLY
    echo  ================================================================
    echo.
    set "OUT_DIR=%PIPELINE_ROOT%\outputs\!STUDY_NAME!"
    echo   Output location: !OUT_DIR!
    echo.
    :: Count outputs
    set "FIG_COUNT=0"
    set "CSV_COUNT=0"
    for /r "!OUT_DIR!\figures" %%F in (*.png) do set /a FIG_COUNT+=1
    for /r "!OUT_DIR!\tables"  %%F in (*.csv) do set /a CSV_COUNT+=1
    echo   Figures (PNG) : !FIG_COUNT!
    echo   Tables  (CSV) : !CSV_COUNT!
    echo.
    echo   Next steps:
    echo     1. Open outputs\!STUDY_NAME!\figures\  to review figures
    echo     2. Open outputs\!STUDY_NAME!\tables\   to review tables
    echo     3. Check outputs\!STUDY_NAME!\logs\    for the run log
    echo.
) else (
    echo   PIPELINE FAILED  (exit code: !EXIT_CODE!)
    echo  ================================================================
    echo.
    echo   Check outputs\!STUDY_NAME!\logs\ for error details.
    echo.
)

:: Save context for rerun / post-run menu
set "LAST_STUDY_NAME=!STUDY_NAME!"
set "LAST_DATA_FOLDER=!STUDY_DATA_PATH!"
set "LAST_ITEM_EXCL=!ITEM_EXCLUSIONS!"
set "LAST_CONFIG=!PIPELINE_CONFIG!"

:POST_RUN_MENU
echo    [M] Main menu
echo    [O] Open output folder in Explorer
echo    [V] View SUMMARY.txt
echo    [R] Rerun  (skip import + scores)
echo    [Q] Quit
echo.
set /p "PR=  [M]: "
if "!PR!"=="" set "PR=M"
if /i "!PR!"=="M" goto :MAIN_MENU
if /i "!PR!"=="O" (
    explorer "%PIPELINE_ROOT%\outputs\!LAST_STUDY_NAME!"
    goto :POST_RUN_MENU
)
if /i "!PR!"=="V" (
    set "_SF="
    for /f "delims=" %%F in ('dir /b /s /o-d "%PIPELINE_ROOT%\outputs\!LAST_STUDY_NAME!\logs\*_SUMMARY.txt" 2^>nul') do (
        if "!_SF!"=="" set "_SF=%%F"
    )
    if "!_SF!"=="" (
        echo  No SUMMARY.txt found yet.
    ) else (
        echo.
        type "!_SF!"
    )
    echo.
    pause
    goto :POST_RUN_MENU
)
if /i "!PR!"=="R" (
    echo.
    echo  Rerunning !LAST_STUDY_NAME! ^(REUSE_DATA=1^)...
    echo.
    set "STUDY_NAME=!LAST_STUDY_NAME!"
    set "STUDY_DATA_PATH=!LAST_DATA_FOLDER!"
    set "ANALYSIS_MODULES=all"
    set "REUSE_DATA=1"
    set "ITEM_EXCLUSIONS=!LAST_ITEM_EXCL!"
    if defined LAST_CONFIG set "PIPELINE_CONFIG=!LAST_CONFIG!"
    pushd "!PIPELINE_ROOT!"
    "!RSCRIPT!" --vanilla "%PIPELINE_ROOT%\R\run_all.R"
    set "EXIT_CODE=!ERRORLEVEL!"
    popd
    goto :POST_RUN_MENU
)
if /i "!PR!"=="Q" exit /b !EXIT_CODE!
goto :POST_RUN_MENU

:: =============================================================================
:: SUBROUTINES
:: =============================================================================

:: =============================================================================
:: _AUTO_COMPARE — auto-generate comparison from just-finished multi-run variants
:: Called via CALL from _MR_RUNDONE; returns when done (goto :EOF)
:: =============================================================================
:_AUTO_COMPARE
set "_AC_YAML=%PIPELINE_ROOT%\config\_comparison_temp.yml"
set "_AC_OUT=comparison_!BASE_STUDY!"
(
echo comparison:
echo   runs:
) > "!_AC_YAML!"
set /a "_AC_I=1"
:_AC_LOOP
if !_AC_I! GTR !EXCL_COUNT! goto :_AC_LOOP_END
call set "_AC_N=%%VARIANT_NAME_!_AC_I!%%"
call set "_AC_E=%%EXCL_SET_!_AC_I!%%"
set "_AC_L=Restricted scoring: !_AC_E!"
if /i "!_AC_E!"=="NONE"   set "_AC_L=Full scoring"
if /i "!_AC_E!"=="y1"     set "_AC_L=Restricted scoring: Y1 excluded"
if /i "!_AC_E!"=="y1,y6"  set "_AC_L=Restricted scoring: Y1 and Y6 excluded"
(
echo     - name:  "!_AC_N!"
echo       label: "!_AC_L!"
) >> "!_AC_YAML!"
set /a _AC_I+=1
goto :_AC_LOOP
:_AC_LOOP_END
(
echo   layout:       "side_by_side"
echo   output_name:  "!_AC_OUT!"
echo   common_scale: false
echo   dpi:          300
echo   label_font_size: 36
echo   figure_groups:
echo     - "primary"
echo     - "mixed_models"
echo     - "psychometrics"
) >> "!_AC_YAML!"
echo.
echo  Running comparison: outputs\!_AC_OUT!\
echo.
set "COMPARISON_CONFIG=!_AC_YAML!"
pushd "!PIPELINE_ROOT!"
"!RSCRIPT!" --vanilla "%PIPELINE_ROOT%\R\run_comparison.R"
set "_AC_EC=!ERRORLEVEL!"
popd
del "!_AC_YAML!" 2>nul
if "!_AC_EC!"=="0" (
    echo.
    echo  [OK] Comparison complete: outputs\!_AC_OUT!\
) else (
    echo.
    echo  [ERROR] Comparison failed ^(exit code !_AC_EC!^). Check outputs for details.
)
echo.
goto :EOF

:FIND_RSCRIPT
set "RSCRIPT="
:: Try PATH first
where Rscript >nul 2>&1
if not errorlevel 1 (
    for /f "delims=" %%R in ('where Rscript 2^>nul') do (
        set "RSCRIPT=%%R"
        goto :RSCRIPT_FOUND
    )
)
:: Search common install locations
for /d %%V in ("C:\Program Files\R\R-4*" "C:\Program Files\R\R-3*") do (
    if exist "%%V\bin\Rscript.exe" (
        set "RSCRIPT=%%V\bin\Rscript.exe"
        goto :RSCRIPT_FOUND
    )
)
goto :RSCRIPT_DONE
:RSCRIPT_FOUND
:RSCRIPT_DONE
exit /b 0

:CHECK_FILE
:: %1 = path, %2 = friendly name
if exist %1 (
    echo   [OK]      %~2
) else (
    echo   [MISSING] %~2  --> %~1
    set "FILES_OK=0"
)
exit /b 0
