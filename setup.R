# Installation and Setup Script for FTOLP Data Pipeline
# Run this script once to set up your environment

# Check R version
cat("Checking R version...\n")
r_version <- getRversion()
if (r_version < "4.0.0") {
  warning("R version 4.0.0 or higher is recommended. You have: ", r_version)
} else {
  cat("✓ R version:", as.character(r_version), "\n")
}

# Install required packages
cat("\nInstalling required packages...\n")

required_packages <- c(
  "tidyverse",    # Data manipulation
  "haven",        # SPSS file I/O
  "labelled",     # Variable labels
  "readxl",       # Excel files
  "lubridate",    # Date handling
  "writexl",      # Excel output
  "ggplot2",      # Visualization
  "here",         # Path management
  "rstatix",      # Statistical tests
  "PerFit"        # Person-fit analysis
)

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(new_packages) > 0) {
  cat("Installing:", paste(new_packages, collapse = ", "), "\n")
  install.packages(new_packages, dependencies = TRUE)
} else {
  cat("✓ All required packages are already installed\n")
}

# Verify installation
cat("\nVerifying package installation...\n")
all_installed <- TRUE
for (pkg in required_packages) {
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("✓", pkg, "\n")
  } else {
    cat("✗", pkg, "- FAILED\n")
    all_installed <- FALSE
  }
}

if (all_installed) {
  cat("\n✓ All packages successfully installed and loaded!\n")
} else {
  cat("\n✗ Some packages failed to install. Please check the error messages above.\n")
}

# Check if config file exists
cat("\nChecking configuration...\n")
config_path <- "config/paths.R"
if (file.exists(config_path)) {
  cat("✓ Configuration file found:", config_path, "\n")
  cat("\nIMPORTANT: Please review and update the paths in", config_path, "\n")
  cat("to match your local directory structure.\n")
} else {
  cat("✗ Configuration file not found:", config_path, "\n")
  cat("Please ensure you're running this script from the project root directory.\n")
}

# Display next steps
cat("\n" , rep("=", 60), "\n", sep = "")
cat("Setup complete! Next steps:\n")
cat(rep("=", 60), "\n", sep = "")
cat("\n1. Edit config/paths.R to set your data directory paths\n")
cat("2. Run the pipeline scripts in order:\n")
cat("   - source('src/pipeline/01_split.R')\n")
cat("   - source('src/pipeline/02_clean.R')\n")
cat("   - source('src/pipeline/03_merge.R')\n")
cat("\nSee docs/QUICK_START.md for detailed instructions.\n\n")
