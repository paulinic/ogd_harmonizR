# Install required packages if needed
install.packages(c("devtools", "roxygen2"))

# Create package structure
devtools::create("harmonizeR")

devtools::install("harmonizeR")
