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

- [00_h2a_summary.ipynb](https://github.com/katherinechristie/finalproj_geocontext/blob/main/code/00_h2a_summary.ipynb)

  - Takes in:
    - Link to WHD investigations data
  - What it does:
    - Cleans and subsets WHD data for H-2A cases in TRLA states after 2016
    - Creates summary tables of cases by state and by year
    - Shows that repeat addresses are caused by 1) employers being investigated multiple times at the same address and 2) different employers investigated seperately that share an address

- [01_geocode_mergewithACS_plot_studentversion.ipynb](https://github.com/katherinechristie/finalproj_geocontext/blob/main/code/01_geocode_mergewithACS_plot_studentversion.ipynb)

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
    - Function that takes state plot, violation data, demographic data and creates a figure 
    - Creates visualizations for all 6 states on each of the three demograhic variables 
    - By-state linear regression for all 6 states on all 3 demographic variables
  - Outputs:
    - pickle files with each state's geocoded worksites
    - `count_viols_bytract_wpoly.p`: pickle of a geopandas df with all tracts for each of the 6 TRLA states and their total count of H2A violations/investigations
    - `exact_viol_location_wtract.p`: pickle of a geopandas df with all worksite locations/tract demographics merged on
    - 18 visualizations (3 for each of the 6 states)  
    - R squared regression values 
