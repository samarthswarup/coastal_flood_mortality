##########################################
# 1 - Python code to extract data from   #
# SLIIDERS                               #
#                                        #
# 2026-06-24                             #
# Ryan Calder			         #
# Notes added by Claude                  #
##########################################

# • This script is designed to be run via RStudio using the `reticulate` package.
#   Running line-by-line with Cmd/Ctrl+Enter may cause issues/errors,
#   particularly at the beginning before entering Python mode. Copying/pasting
#   blocks into the console may work better.
#
# • Python version: tested with Python 3.11.13.
#   It should work with other versions >= 3.8, but minor changes may be needed.
#   To check your version:
#     - macOS/Linux: open Terminal and run `python3 --version`
#     - Windows: open Command Prompt or PowerShell and run `python --version`
#
# • Required Python packages (install before running for the first time):
#     pip install zarr numpy xarray dask pandas netCDF4
#   Or with conda:
#     conda install -c conda-forge zarr numpy xarray dask pandas netCDF4
#
# • Project layout expected by this script (and every other script in the
#   pipeline): a single project root folder containing this script alongside
#   three data folders:
#     <project root>/Inputs/                    <- raw/external source data
#     <project root>/Intermediate outputs/      <- files written by each step
#     <project root>/Output/                    <- final tables/figures
#
# • Required input files for THIS step (place under Inputs/ before running):
#     Inputs/SLIIDERS data/sliiders-v1.2.zarr.zip
#     Inputs/SLIIDERS data/slr_allscens_byseg.nc.zip
#
# • Output:
#     - CSV files saved to: Intermediate outputs/Output from Step 1/
#     - SLR files saved to: Intermediate outputs/Output from Step 1/SLR_<date>/
#
# • Windows users: use raw strings (r"path\\to\\file") or os.path.join() for paths.

# ---------- R setup (copy-paste into console, do not run line-by-line) ----------
# library(reticulate)
# use_condaenv("r-reticulate311", conda = "~/miniforge3/bin/conda", required = TRUE)
# py_config()
# ---------- end R setup ---------------------------------------------------------

#!/opt/homebrew/bin/python3

import os
import zipfile
import xarray as xr
import pandas as pd

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

DATE        = "2026-06-24"

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This is the ONLY machine-specific line in this script.
WD          = os.path.expanduser("~/coastal_flood_mortality")

INPUTS_DIR  = "Inputs/SLIIDERS data"     # relative to WD
OUT_DIR     = "Intermediate outputs/Output from Step 1"   # relative to WD
SLR_YEARS   = [2050, 2100]

os.chdir(WD)
os.makedirs(OUT_DIR, exist_ok=True)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def unzip(zip_path, extract_to):
    """Extract a zip archive and print confirmation."""
    with zipfile.ZipFile(zip_path, "r") as zf:
        zf.extractall(extract_to)
    print(f"Unzipped: {zip_path} → {extract_to}")


def extract_variable(dataset, var_name, out_path):
    """Convert an xarray variable to a CSV and print a preview."""
    df = dataset[var_name].to_dataframe(name=var_name).reset_index()
    print(f"\n{var_name} preview:")
    print(df.head())
    df.to_csv(out_path, index=False)
    print(f"Saved: {out_path}")


# ---------------------------------------------------------------------------
# I. Extract SLIIDERS zarr archive and load dataset
# ---------------------------------------------------------------------------

unzip(
    zip_path   = os.path.join(WD, INPUTS_DIR, "sliiders-v1.2.zarr.zip"),
    extract_to = INPUTS_DIR,
)

sliiders_raw = xr.open_dataset(
    os.path.join(INPUTS_DIR, "sliiders-v1.2.zarr"),
    engine="zarr",
    chunks={},
)

# ---------------------------------------------------------------------------
# II. Export SLIIDERS variables to CSV
# ---------------------------------------------------------------------------

for var in ["pop_scale", "pop_2019", "surge_height"]:
    extract_variable(
        dataset  = sliiders_raw,
        var_name = var,
        out_path = os.path.join(OUT_DIR, f"{var}_data {DATE}.csv"),
    )

# ---------------------------------------------------------------------------
# III. Extract SLR NetCDF archive and export per-scenario CSVs
# ---------------------------------------------------------------------------

unzip(
    zip_path   = os.path.join(WD, INPUTS_DIR, "slr_allscens_byseg.nc.zip"),
    extract_to = INPUTS_DIR,
)

ds = xr.open_dataset(
    os.path.join(INPUTS_DIR, "slr_allscens_byseg.nc"),
    engine="netcdf4",
)

slr_out_dir = os.path.join(OUT_DIR, f"SLR_{DATE}")
os.makedirs(slr_out_dir, exist_ok=True)

for scen in ds["scenario"].values:
    for year in SLR_YEARS:
        for quant in ds["quantile"].values:
            subset   = ds.sel(scenario=scen, year=year, quantile=quant)
            df       = subset.to_dataframe().reset_index()
            filename = f"{scen}_{year}_{int(quant * 100):02d}.csv"
            filepath = os.path.join(slr_out_dir, filename)
            df.to_csv(filepath, index=False)
            print(f"Saved: {filepath}")

print("\nStep 1 complete.")
exit()