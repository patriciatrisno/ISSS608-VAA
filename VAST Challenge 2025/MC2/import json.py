import json
import pandas as pd  # Import pandas for tabular display

# Open and load a JSON file
file_path = '/Users/patriciatrisno/Downloads/MC2_release/FILAH.json'  # Replace with your JSON file path
with open(file_path, 'r') as file:
    data = json.load(file)

# Convert JSON data to a pandas DataFrame
df = pd.DataFrame(data)

# Print the data as a table
print(df)
