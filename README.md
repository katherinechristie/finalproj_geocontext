# Geographic context

## Data/directory structure

### Raw input data

The input data, if not read from a link or API, are stored in the following location: `Dropbox/qss20_finalproj_rawdata/geocontext/raw/`

We use the following input files:

- `tl_2016_[state2digcode]_tract`: zipped files containing tract-level shapefiles for 6 focal states. Downloaded manually from: GROUP ADD LINK

### Cleaned data

We store cleaned data at the following location: `Dropbox/qss20_finalproj_rawdata/geocontext/intermediate/`

The script-specific notes below outline cleaned data products

## Code and order to run

- [00_geocode_mergewithACS_plot.ipynb](https://github.com/katherinechristie/finalproj_geocontext/blob/main/00_geocode_mergewithACS_plot.ipynb)

  - Takes in:
    - API key for GeoCodio geocoding API (GROUP MOVE TO CREDS FILE)
    - Census API key
    - Tract shapefiles stored in `raw`
    - Link to WHD investigations data
  - What it does:
    - If `RUN_GEOCODE` is True, geocodes the address of the worksite and writes state-specific pkl files of geocode results; if `RUN_GEOCODE` is false, loads those pkl files
    - Loads tract shapefiles and uses `sjoin` to find the tract location of each geocoded worksite
    - Uses Census API to pull tract-level demographics (ACS 5-year estimates; year is a parameter)
    - Merges demographics to tracts
    - Visualizations (GROUP PROBABLY MOVE TO SEP NOTEBOOK)
  - Outputs:
    - pickle files with each state's geocoded worksites
    - `count_viols_bytract_wpoly.p`: pickle of a geopandas df with all tracts for each of the 6 TRLA states and their total count of H2A violations/investigations
    - `exact_viol_location_wtract.p`: pickle of a geopandas df with all worksite locations/tract demographics merged on
