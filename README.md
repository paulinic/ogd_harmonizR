# Open Government Data inspection, harmonization and linking
Capstone project in the data mining with R course at University of Lucerne, spring 2025

## Idea
In Switzerland we have an extensive pool of Open Government datasets which is even more growing since the "open by default" clause has been wirtten into law for any software that is being programmed for the Swiss parliment. 
The challenge still is, that there are different data types, three national langguages and different data formats being provided from the federal institutions, 26 cantons, lots many communes. The aim of the project therefore is to create 
an R package that will support Data Scientists in working with Open Government Data. The packages allows the user to inspect, harmonize and link data sets in R. 

## Data
The data will be collected from the Swiss Open Government Data portal opendata.swiss. 

## Methods
With the help of the LLM Claude by Anthropic AI, function scripts were created to inspect, harmonize and link the different data sets found on opendata.swiss. The LLM also supported in building the package, us9ng the devtools package.The package itself 
is written in R and being built with the tool R Studio. 

## Testing 

### Unit Tests with testthat package
Link to the testthat package: https://testthat.r-lib.org/ For the testing there were different test scripts wirtten. For testing the utility functions, the inspecting functions and the harmonization functions, the testthat package was used. 
The testthat package is a powerful and flexible testing framework for R. It provides a set of functions to write unit tests, integration tests, and other types of tests for R code. The testthat package is widely used in the R community and is a standard tool for testing R packages.
The tests seem to be successful. 

### Integration Tests with testthat package 
to be done 

## Packaging the R package 
The package is structured in a way that allows for easy installation and use. The package includes a DESCRIPTION file, which contains metadata about the package, such as its name, version, author, and dependencies. The package also includes a NAMESPACE file, which specifies the functions and objects that are exported from the package.
The package is organized into subdirectories, each containing R scripts that define the functions and objects in the package. The package also includes a README file, which provides an overview of the package and its functionality.

## Installation
The package is designed to be easy to install and use. The package can be installed from GitHub using the devtools package. The package can not yet be installed from CRAN using the install.packages function.


### Comining Git Repositories
For the first part of the project, the author worked in a different Github repository. The author then created a new repository for the second part of the projec and to get more structure as well as more order in the repo itself. 
The author then combined the two repositories into one as not to loose all the commits done in the first repository. The author used the following instructions to combine the two repositories: https://gist.github.com/msrose/2feacb303035d11d2d05
