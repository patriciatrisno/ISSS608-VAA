# Install the jsonlite package if not already installed
if (!require(jsonlite)) {
  install.packages("jsonlite")
}

# Load the jsonlite package
library(jsonlite)

# Specify the path to your JSON file
json_file_path <- "/Users/patriciatrisno/Downloads/MC2_release/FILAH.json"

# Read the JSON file
json_data <- fromJSON(json_file_path)

# Print the JSON data
print(json_data)