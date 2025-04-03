# Open Government Data inspection, harmonization and linking
Capstone project in the data mining with R course at University of Lucerne, spring 2025

## Idea
In Switzerland we have an extensive pool of Open Government datasets which is even more growing since the "open by default" clause has been wirtten into law for any software that is being programmed for the Swiss parliment. 
The challenge still is, that there are different data types, three national langguages and different data formats being provided from the federal institutions, 26 cantons, lots many communes. The aim of the project therefore is to create 
an R package that will support Data Scientists in working with Open Government Data. The packages allows the user to inspect, harmonize and link data sets in R. 

## Data
The data will be collected from the Swiss Open Government Data portal opendata.swiss. 

## Methods
With the help of the LLM Claude by Anthropic AI, function scripts were created to inspect, harmonize and link the different data sets found on opendata.swiss. 

## Testing 

### Unit Tests with testthat package
Link to the testthat package: https://testthat.r-lib.org/ For the testing there were different test scripts wirtten. For testing the utility functions, the inspecting functions and the harmonization functions, the testthat package was used. 
The testthat package is a powerful and flexible testing framework for R. It provides a set of functions to write unit tests, integration tests, and other types of tests for R code. The testthat package is widely used in the R community and is a standard tool for testing R packages.
The tests seem to be successfull. 

### Integration Tests with testthat package 

## Packaging the R package 
