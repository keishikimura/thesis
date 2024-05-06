#!/bin/bash

# Define the directories you need for your project
directories=(
    "AI/figures"
    "AI/data/raw"
    "AI/data/generated"
    "AI/figures/graphical"
    "AI/figures/citations"
    "AI/tables"
)

# Function to create directories if they do not exist
create_directory() {
    dir=$1
    if [ ! -d "$dir" ]; then
        echo "Creating directory: $dir"
        mkdir -p "$dir"
    else
        echo "Directory already exists: $dir"
    fi
}

# Loop through the array and create directories as needed
for dir in "${directories[@]}"; do
    create_directory "$dir"
done

echo "Setup complete!"