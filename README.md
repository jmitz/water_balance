# Water Balance Projections

This project is meant to download information from the [Yellowstone Solutions thredds server](http://www.yellowstone.solutions/thredds/catalog.html) for water balance projections using a few specified values.
The values that need to be defined are at the top of each rmd file. They are:

| Parameter    | Description                                                                             |
|--------------|-----------------------------------------------------------------------------------------|
| site         | a common name for the site you are looking at                                           |
| lat          | latitude of the site                                                                    |
| lon          | longitude of the site                                                                   |
| model_bc     | which model have you selected to represent the "best case" scenario? Options are given in the code.|
| model_bc_rcp | what is the RCP of the model you have selected to represent the "best case" scenario? Options are given in the code |
| model_bc_rcp_name | a more "readable" version of the name for the RCP                                  |
| model_wc | which model have you selected to represent the "worst case" scenario? Options are given in the code.|
| model_wc_rcp | what is the RCP of the model you have selected to represent the "worst case" scenario? Options are given in the code |
| model_wc_rcp_name | a more "readable" version of the name for the RCP                                  |
| past_data | which data set have you selected to represent historical data? Options are given in the code. |
| dry_year | What is a historically dry year in the latitude and longitude you are looking at? Possibly a year with a lot of fire? |
| wet_year | what is a historically wet year in the latitude and logitude you are looking at? Possibly a year with flooding? |

This code runs in 4.1, but works with earlier versions (not tested to see how far back it will work). Additionally, this code requires use of project directories and a basic understanding of the `here` package. File organization is key here, there are many articles on how to best organize your files, why projects (rather than using setwd()) are crucial to reproducible data, and why we should all be using them. I won't go into that here, but [this blog](https://martinctc.github.io/blog/rstudio-projects-and-working-directories-a-beginner's-guide/) has a good summary. If you need help understanding how to set up a directory in R, [this website](https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects) explains it pretty well.

When opening and running the project for always make sure to open the .Rproj file, otherwise your code may tell you that it cannot find the file, causing the code to abort. 


## To update R to latest version:

### Enter the following code for Windows:

1. install.packages("installr")
2. library(installr)
3. updateR()

- [More information here](https://www.r-statistics.com/2015/06/a-step-by-step-screenshots-tutorial-for-upgrading-r-on-windows/#:~:text=If%20you%20are%20running%20R,installr%20updateR()%20%23%20updating%20R.)


### Enter the following code for Mac/Linux

1. install.packages('devtools') assuming it is not already installed
2. library(devtools)
3. install_github('andreacirilloac/updateR')
4. library(updateR)
5. updateR(admin_password = 'Admin user password')

- [More information here](http://www.andreacirillo.com/2018/03/10/updater-package-update-r-version-with-a-function-on-mac-osx/)


- R may tell you to open RGui
  - [More information about RGui here](https://www.dummies.com/programming/r/how-to-navigate-rgui/)

### How to have (and switch between) multiple versions of R on RStudio

You can run this code on a newer version of R but still have access to your older version if needed. This can be done by going to tools -> global options and changing the version of R to run this code. [This website](https://support.rstudio.com/hc/en-us/articles/212364537-Multiple-Versions-of-R-in-RStudio-Server-Pro) gives a bit more information about doing this, and [this website](https://cran.r-project.org/bin/windows/base/old/) gives access to all the previous and most recent version of R.

Oftentimes, people need to work in older versions of R for certain packages. Fortunately, RStudio can support more than one version, allowing you to switch between them easily. [This website](http://derekogle.com/IFAR/supplements/installations/InstallRStudioWin.html) gives detail instructions on how to switch between Rstudo versions.


## Using this project.
### Required libraries:
- `here`
  1. install.packages('here')
  2. library(here)
  - This was required to run model_selection_graph.R and not in the default installation.

### Run the scripts in the following order:

1. model_selection_graph.R OR select your models using the scatterplot on the [MACA website](https://climate.northwestknowledge.net/MACA/vis_scatterplot.php).
   + Keep in mind that model selection isn't always intuitive, but using the scatterplot, you can select for what type of future you would like to see. There are some models that do not represent certain parts of the country well, so be aware of that as well.
   + This allows you to select which models you believe will best bracket your climate futures. Once you have run this code, you must select two models from the graph along with their RCPs (either 4.5 or 8.5) which will be input into the following two steps
2. water_balance_data.Rmd
    + this downloads all the data required to run the graphs script. I recommend running it over night, as the data can take quite some time to download.
3. water_balance_graphs.Rmd
    + this is the final report, which displays the data in graphical form
    
4. run_rmd.R
   + Allows multiple sites to be run at once using a csv that has column headers as displayed in this table.

| Park |Lat | Long | model_bc | model_bc_rcp |model_wc | model_wc_rcp | wet_year | dry_year |
|------|-----|------|---------|--------------|---------|--------------|----------|----------|
|Bandelier National Monument|35.75758546|-106.3054344|inmcm4|rcp45|NorESM1-M|rcp85|1997|2011|
|Yellowstone National Park|44.59644457|-110.5471962|MRI-CGCM3|rcp85|IPSL-CM5A-LR|rcp85|1997|1988|

   + You should be able to use the run_rmd.R to run the report using the rum_multiple_sites.csv file. This is especially useful if you have a number of latitudes and longitudes you would like to look at, it will essentially loop through the data download and the report generation for each latitude and longitude.
   + The data download section will need to be unremarked to execute the download code.

### Model and RCP settings
[General Circulation Models or Global Climate Model(GCMs)](https://en.wikipedia.org/wiki/General_circulation_model)
| Abbrieviation   | On THREDDS | Description                                                   |
|-----------------|------------|---------------------------------------------------------------|
| bcc-csm1-1      | No         |  |
| bcc-csm1-1-m    | No         |  |
| BNU-ESM         | On THREDDS |  |
| CanESM2         | On THREDDS |  |
| CCSM4           | On THREDDS |  |
| CNRM-CM5        | On THREDDS |  |
| CSIRO-Mk3-6-0   | On THREDDS |  |
| GFDL-ESM2G      | On THREDDS |  |
| GFDL-ESM2M      | No         |  |
| HadGEM2-CC365   | On THREDDS |  |
| HadGEM2-ES365   | No         |  |
| inmcm4          | On THREDDS |  |
| IPSL-CM5A-MR    | No         |  |
| IPSL-CM5A-LR    | On THREDDS |  |
| IPSL-CM5B-LR    | No         |  |
| MIROC5          | On THREDDS |  |
| MIROC-ESM       | No         |  |
| MIROC-ESM-CHEM  | No         |  |
| MRI-CGCM3       | On THREDDS |  |
| NorESM1-M       | On THREDDS |  |