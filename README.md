# swampBioaccumulationAnalysis

R package to analyze SWAMP Bioaccumulation Monitoring Data

## Installation

You can install the **swampBioaccumulationAnalysis** package from
GitHub.

``` r
install.packages("remotes") # if you have not installed "remotes" package
remotes::install_github("CAWaterBoardDataCenter/swamp-bioaccumulation-analysis")
```

The source code for **swampBioaccumulationAnalysis** package is
available on GitHub at

- <https://github.com/CAWaterBoardDataCenter/swamp-bioaccumulation-analysis>.

## Usage

The **swampBioaccumulationAnalysis** package provides several functions
that can be used to analyze your dataset.

- `summary_tables()`
- `length_adjusted_averages()`
- `mercury_by_species_barplot()`
- `mercury_vs_length_scatterplot()`
- `all_analyses()`

To use these functions, first, you should load the package as follows:

``` r
library(swampBioaccumulationAnalysis)
```

### Data Source Requirements

**swampBioaccumulationAnalysis** is designed to analyze tissue datasets
downloaded from the [CEDEN Advanced Query
Tool](https://ceden.waterboards.ca.gov/Home/virtAqtTool), in .tsv file
format, and any source files evaluated by package functions will need to
be in that file format and data stucture.

It’s recommended that users place the .tsv file downloaded from CEDEN
AQT download in the folder that they have set as their R working
directory, so they can run the analysis functions simply using the
filename for the functions’ filename argument. (Otherwise they will need
to pass the dataset file’s filepath relative to their R working
directory, or its absolute filepath on their machine, as the filename
argument.)

For example, if your dataset was in a file named “CEDENDownload.tsv” in
the folder `C:\Users\yourname\Documents\` on your local machine, you
could set your R working directory by running the following in your R
console:

``` r
setwd("C:\Users\yourname\Documents")
```

and then you could run a function from the package on your dataset as
follows:

``` r
summary_tables("CEDENDownlaod.tsv")
```

### Function Details

- `summary_tables(`*`filename`*`)` Outputs a six tab (.xlsx) spreadsheet
  with summary tables for your dataset:
  - Analytes included in the dataset, detection limits, number of
    observations, and frequencies of detection and reporting (in tab
    *AnalytesObservsDetects*)
  - Mercury in individual sport fish (in tab *SportfishMercuryIndiv*)
  - Summary of sport fish results, with composites or means at each
    location (in tab *SportfishCompsAndMeans*)
  - Summary of prey fish results (in tab *PreyfishComposites*)
  - Scientific and common names of prey fish species in the dataset, the
    number of locations in which they were sampled, and their minimum,
    median, and maximum total lengths (mm) (in tab *PreyfishCounts*)
  - Scientific and common names of sport fish species in the dataset,
    the number of locations in which they were sampled, numbers of
    individual or composite samples, their minimum, median, and maximum
    total lengths (mm), and whether they were analyzed as composites or
    individuals (in tab *SportfishCounts*)
- `length_adjusted_averages(`*`filename`*`)` Outputs a three tab (.xlsx)
  spreadsheet with length-adjusted averages (in tab
  *LengthAdjustedAverages*), length-adjusted averages with additional
  summary calculations (in tab *LengthAdjustedAveragesDetail*), and
  intermediate length-adjusted result calculations (in tab
  *SupportingCalculations*)
- `mercury_by_species_barplot(`*`filename`*`,`*`fish_class`*`)` Outputs
  a (.jpeg file) bar plot for mercury concentration by species, for a
  given source file and type of fish (sport or prey)
- `mercury_vs_length_scatterplot(`*`filename`*`,`*`species_common_name`*`)`
  Outputs a (.jpeg file) scatterplot of fish mercury concentration vs
  fish length in (mm) for a given source file and species
- `all_analyses(`*`filename`*`)` Runs all of the other functions in the
  package on your source file. (The *mercury_by_species_barplot()*
  function is run for both sport and prey fish. The
  *mercury_vs_length_scatterplot()* function is run for every species
  present in the dataset with mercury results.) **If you would like to
  generate the full set of tables and figures that the package can
  create for your dataset, you can do so solely by running this one
  function.**

Once the package is installed and loaded, you can run `?functionname` or
`help("functionname")` in the R Console to view full function-specific
documentation

## Package Data

The package contains external data (.csv) files in the installation
directory’s extdata/ subdirectory that contain reference and lookup list
information supporting the analysis functions. There are three data
files:

- `mercury-units.csv` Contains unit conversions to ppm for reported
  units for concentration of mercury in fish tissue, in order for for
  the dataset’s mercury in fish results to be compared with statewide
  water quality objectives.
- `species-type-lookup-table.csv` Contains classifications of fish
  species (as prey, sport or both). Fish species classified as both
  include threshold values (for total fish length in mm) to determine
  whether individual fish in the dataset are treated as prey or sport
  fish.
- `summing-lookup.csv` Contains a crosswalk of individual analytes to
  summing groups, in order for the sum of results for an analyte group
  to be reported in place of the individual analyte results in some
  summary tables (e.g. “Sum of PCBs” in place of individual PCB
  congeners).
