#!/usr/bin/env python
# coding: utf-8

import zarr
import numpy as np
import csv
import xarray as xr
import dask.array as da
import pandas as pd
import dask.dataframe as dd
import wget
import zipfile

###Download the SLIIDERS dataset from Zenodo
sliiders_url = 'https://zenodo.org/records/10779331/files/sliiders-v1.2.zarr.zip?download=1'
local_path = './'
print('Downloading https://zenodo.org/records/10779331/files/sliiders-v1.2.zarr.zip')
wget.download(sliiders_url, out = local_path)
print('Done.')

###Unzip
print('Unzipping...')
with zipfile.ZipFile('./sliiders-v1.2.zarr.zip', 'r') as zip_ref:
    zip_ref.extractall(local_path+'sliiders-v1.2.zarr/')
print('Done.')

###OPENS the SLIIDERS_ECON Folder containing Zarr Files
inp = xr.open_dataset(local_path + "sliiders-v1.2.zarr", engine='zarr', chunks={})

###Helps to visualoze pop_scale dask array
popscale = inp.pop_scale

####Extract pop_scale dataframe from the dask array ###
year_country_data = popscale.sel(year=popscale['year'].values, country=popscale['country'].values)
year_country_data = year_country_data.compute()
reset_coords_data = year_country_data.reset_coords()

# Converts the reset coordinates Dask array to a Pandas DataFrame
df = reset_coords_data.to_dataframe()

csv_file = 'updated_pop.csv'
# Saves the pivoted DataFrame as a CSV file
df.to_csv(csv_file)
print('Saved file ./updated_pop.csv')

#### Extract 2019 population data of each segment ####
pop2019 = inp.pop_2019
pop2019 = pop2019.compute()
reset_coords_data = pop2019.reset_coords()

# Convert the reset coordinates Dask array to a Pandas DataFrame
df = reset_coords_data.to_dataframe()

csv_file = 'pop2019_new.csv'

# Saves the pivoted DataFrame as a CSV file
df.to_csv(csv_file)
print('Saved file ./pop2019_new.csv')

#### Extract surge height data of each segment ####
surge_height = inp.surge_height
surge_height = surge_height.compute()
reset_coords_data = surge_height.reset_coords()

# Convert the reset coordinates Dask array to a Pandas DataFrame
df = reset_coords_data.to_dataframe()

csv_file = 'surge_height.csv'

# Save the pivoted DataFrame as a CSV file
df.to_csv(csv_file)
print('Saved file ./surge_height.csv')
print('All done.')
