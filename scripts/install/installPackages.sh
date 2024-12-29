#!/bin/zsh

# Check if the user provided the file name as an argument
if [[ -z "$1" ]]; then
  echo "Usage: $0 <package_file>"
  exit 1
fi

# Get the file name from the first argument
PACKAGE_FILE="$1"

# Check if the file exists and is readable
if [[ ! -f "$PACKAGE_FILE" || ! -r "$PACKAGE_FILE" ]]; then
  echo "Error: File '$PACKAGE_FILE' does not exist or is not readable."
  exit 1
fi

# Initialize a log file for failed packages
FAILED_LOG="failed_packages.log"
: >"$FAILED_LOG" # Clear the log file if it already exists

# Read the file line by line
while IFS= read -r package; do
  # Skip empty lines or lines starting with '#'
  [[ -z "$package" || "$package" =~ ^# ]] && continue

  # Attempt to install the package
  echo "Installing: $package"
  if yay -S --needed --noconfirm $package; then
    echo "Successfully installed: $package"
  else
    echo "Failed to install: $package" >>"$FAILED_LOG"
  fi
done <"$PACKAGE_FILE"

echo "Installation process completed."
if [[ -s "$FAILED_LOG" ]]; then
  echo "Some packages failed to install. Check '$FAILED_LOG' for details."
else
  echo "All packages installed successfully."
fi
