# MpiIsoApp development version
## Version 23.07.1
## New Features
- _Missing Values Imputation AssignR_: Multiple imputation for AssignR using the "mice" package

## Version 23.06.4

## New Features
- _Modeling tabs_: The text for center point estimates 
  - now contains the underlying grid length (#169)
  - can be hidden again by removing inputs for latitude and longitude

## Updates
- removed recalculation of predictions when changing the latitude or longitude for center estimates

## Version 23.06.4

### New Features
- _Export of Saved maps_ is now included within the feature to down- and upload models (#2)
  - additionally, the list of saved models is downloaded
  - upload checks for a list of saved maps and loads it if it is available

## Version 23.06.3

### Bug Fixes
- _Export of time series_: 
  - fixes export of pdf files together with a .gif (#107)
  - now .gif files are always based on jpeg files, for .gif no conversion from pdf is required anymore

## Version 23.06.2

## New Features
- _Tab Data_: new input to select an ontological schema, e.g. the "IsoMemo" schema (#102)

## Version 23.06.1

### Updates
- add spinner to indicate that app is initializing
- optimize loading of tabs when initializing
- use DataTools 23.06.1 to fix issues with importing data without internet connection (#157)

## Version 23.06.0

### Bug Fixes
- _Interactive Map_:
  - fix option to use a fixed point colour (#161)

## Version 23.05.4

### Updates
- option in KernelTimeR to choose between temporal group or spatial cluster

## Version 23.05.3

### New Features
- _Export of time series_: optionally add a gif file or only export a gif (#107)

## Version 23.05.2

### New Features
- _Interactive Map_: 
  - map types are grouped by having borders or names (#133)
  - option to add more maps from different providers: https://leaflet-extras.github.io/leaflet-providers/preview/

## Version 23.05.1

### Updates
- _Interactive Map_: add option "[Fixed]" for point size, colour and symbol (#13)

## Version 23.04.3.3

### Bug Fixes
- apply bug fix from main on beta (#142)

## Version 23.04.3.2

### Bug Fixes
- apply bug fix from main on beta (#122)

## Version 23.04.3

### Updates
  - removing the calculation of clusters for unfiltered data
  - renaming columns in the excel export
  - renaming "Number of clusters (optional)" to "Possible range for clusters"
  - increasing the maximum number of clusters from 20 to 50 for mclust
  - add option to show spatial centroids

## Version 23.04.2

### New Features
- _Interactive map_: 
  - option to set the symbol dependent on values of a character column (#13)
  - show/hide legends for size (if a variable is used) and symbols 

### Updates
- optimized behavior of updating data and show/hide legends on the interactive map

## Version 23.04.1.10

## Bug fixes
- fixes an error when trying to reach the API without internet connetcion (#157)

## Version 23.04.1.7

### Bug Fixes
- fix case if date columns are not numeric (#110)

## Version 23.04.1.6

### Bug Fixes
- add default API_BASE_URL (#142)

## Version 23.04.1.5

### Bug Fixes
- fix bug in conversion of lat/long when using import in modeling tabs (#122)

## Version 23.04.1

### New Features

- _Model tabs_: Option to load a model
  - download a model (all user inputs, data and/or the model output) as a .zip file
  - upload online models that are stored in github
  - upload local models that were downloaded in a past session
  - check if the model to be uploaded comes from the active tab

## Version 23.03.6

### New Features

- Adds mclust as new clustering option for KernelR and KernelTimeR
- Adds columns to excel for KernelTimeR: centroid full data, centroid sliced data, centroid temporal data

### Bug Fixes

- Unifies colors for clustering map and clustering plot within KernelTimeR
- Fixing crash in mapSim, mapDiff and kernel plotting function after updates regarding categorical
target/dependent variable

## Version 23.03.5

### Updates

- Show type of user created maps (#19)

## Version 23.03.4

### Updates

- Categorical target/dependent variable option for AverageR/TimeR models. This follows a 1vsall approach using logistic regression, which in the Bayesian case is performed using a Polya-Gamma latent variable during Gibbs-sampling (https://arxiv.org/abs/1205.0310)

## Version 23.03.3

### Updates

- Option to decide whether to keep the first or last duplicate row.

## Version 23.03.2

### Updates

- Option to set max number of characters to be displayed in the table.

## Version 23.03.1

### Updates

- _Import Data_ module in the tab _Query with SQL_: confirm using GPT3 before option to upload a key

## Version 23.02.4

### Updates

- _Import Data_ module in the tab _Query with SQL_: option to use GPT3 for creation of SQL queries

## Version 23.02.3

### Updates

- Option to decide if column with duplicate rows is added
- Option to ignore spaces during duplicate detection
- Option to specify a string that is used for duplicate detection

## Version 23.02.2

### Updates

- the _Import Data_ module was now integrated into the modeling tabs _AverageR_, _TimeR_,
  _SpreadR_, _KernelR_, _KernelTimeR_, and _AssignR_ (#91, PR #98)
  - applicable when "Upload file" is selected under "Data source" in the left sidebar

## Version 23.02.1

### New features

- A new button "detect duplicates" has been added that opens a pop-up where duplicates can be detected and removed from the dataset.

## Version 23.01.4.1

### Bug Fixes

- inputs for latitude and longitude were falsely hidden for time plots in TimeR and KernelTimeR (#94)

## Version 23.01.4

### Updates

- when using _Pandora_ skin:
  - the _Import Data_ module is now imported from the new package IsoAppTools (#91, PR 92)
  - now changes of functionality
  - all redundant code was removed

## Version 23.01.3

### New features

- when using _Pandora_ skin, in _Import Data_:
  - option to use SQL queries to prepare or combine data before import (#37, PR #89)
    - use in-memory tables and columns
    - provide and apply a query
    - preview and accept the result
    
### Bug Fixes

- fixed bug when importing data with no numeric columns:
  - data was not displayed because no lat/long columns were found
    
## Version 23.01.2

### New features

- under Saved/Create Maps tab (#19)
  - option to add a square
  - update of the UI

- fixed sidebars with auto scroll in all tabs (#4)

## Version 23.01.1

### New features

- interactive map: 
  - option to assign colour of symbol by selecting a certain data column (#13)
    - choose a colour palette
    - reverse colours
  - option to assign size of symbol by selecting a certain data column (#13)
    - set a factor to adjust the size
    - if no column is selected, the factor sets the general point size
  - option to adjust opacity

## Version 22.12.4

### New features

- when using _Pandora_ skin, in _Import Data_:
  - option to prepare data before import or merge
    - rename columns
    - join columns
    - split columns
    - delete columns
  - option to merge two data tables before import (#37)
    - optionally select all common columns
    - specify the merge operation
    - apply the merge for reviewing and error checking
    - accept the merged file to finally import it into the app
  - checks for correct column names in file imports
  - update column names of files if naming conflicts
  - new tests

### Updates

- disable accept button(s) by default in _Import Data_

## Version 22.12.3.1

### Bug Fixes

- inputs for latitude and longitude were falsely hidden for time plots in TimeR and KernelTimeR (#94)

## Version 22.12.3

### Updates

- _export of plots_ after modelling: hide option to create a time series for
_Time Course_ plot type (#8)

### Bug Fixes

- fix naming issue when data contains columns exactly named "latitude" or "longitude"
  - columns will be renamed if coordinate conversion fails

## Version 22.12.2

### Updates

- add notification if conversion of lng/lat was successful (#67)

### Bug Fixes

- fix reset of lng/lat columns when switching coord formats

## Version 22.12.1

### Bug Fixes

- fix export of time plot as time series in TimeR and KernelTimeR (#8)

## Version 22.11.1

### Updates

- extract z scale settings ("Estimation type", "Show estimates", Min/Max range, "Restrict range")
into their own module
- new module zScale applied in the modelling tabs: _AverageR_, _KernelR_, _TimeR_, _KernelTimeR_,
_SpreadR_, _OperatoR_, _LocateR_
- switch the title for min/max range dependent on the "Plot type"

### Bug fixes

- fix update of min/max values after removing a restriction (min/max values had been overwritten
before, that is, removing a restriction did not reset min/max values) (#27)
- remove duplicated re-rendering of the map when changing "Estimation type" or restricting the range
- update choices of "Estimation type" when switching the "Plot type" in _TimeR_ since some types
are not implemented for "Time course"
- debounce inputs for min/max range to prevent re-rendering of the map when typing min/max values 

## Version 22.10.3

### Updates

- new UI to specify the section of maps of modelling tabs (_AverageR, TimeR, SpreadR, KernelR,
KernelTimeR, OperatorR, LocateR_)
  - dynamically use the buttons up, down, center, ...
  - apply a button "Set (Time and) Map Section" to set
    - time (only for _TimeR_ and _KernelTimeR_),
    - zoom in degrees of longitude, or
    - lat/long of upper left corner
- new modules that encapsulate the functionality behind the new UI
- applying the new modules in all modelling tabs and removing copy-paste code

### Bug fixes

- removes the slow **automatic** updates for inputs of time and zoom (#8)

## Version 22.10.2

### Bug fixes

- fixing the point radius changing with latitude, now set radius in pixel (#44)

## Version 22.10.1

### New features

- specify the sheet of an xlsx/xls file in the import dialog
- new button "Center Map" in the _Interactive map_ section that centers the map at the mean 
longitude and latitude of the displayed data

### Enhancements

- for Pandora skin: set default Latitude and Longitude columns after data load if match was 
found (#67)
  - note: the default format is "decimal degrees", here we still apply the check for numeric columns

### Bug fixes

- fix failing of import of xlsx files with sheets (-> option to select the sheet)
- fix missing columns in the selection of `Longitude` and `Latitude` if `Coordinate format` was not
equal "decimal degrees" (#62)
- fix in the removal of points on the interactive map after using filters (#63)

## Version 22.09.3

### New features

- export a time series of _spatio-temporal-average_ plots in the selected file format condensed
into a zip file

## Version 22.09.2

### New features

- option to adjust the format of the axes in time course plots (decimal places and number of labels)
- option to adjust the decimal places of the center estimates for mapType == "Map" or "Spread"

## Version 22.09.1

### Enhancements

- In the _Modelling_ tab under _TimeR_ and _KernelTimeR_: new (numeric) input field for the
`Time selection` in addition to the slider input `Time selection`

## Version 22.08.2.1

### Updates

- text update of the modeling tab for the _pandora / isomemo search app_ (#9)

## Version 22.08.2

### Enhancements

- when using _Pandora_ skin: updates of the UI in the _Import Data_ pop-up menu (#39)
- update label names in the _maps_ tab (#48)

### Bug fixes
- fixes in the modeling tab _AssignR_: (#46)
