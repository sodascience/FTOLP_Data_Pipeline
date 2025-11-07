# .Rprofile for FTOLP Data Pipeline
# This file is automatically loaded when you open this R project

# Print welcome message
.First <- function() {
  cat("\n")
  cat("╔═══════════════════════════════════════════════════════════════════╗\n")
  cat("║                  FTOLP Data Pipeline Project                     ║\n")
  cat("╚═══════════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  cat("📋 Quick Start:\n")
  cat("  1. Configure paths: edit config/paths.R\n")
  cat("  2. Install packages: source('setup.R')\n")
  cat("  3. Run pipeline:\n")
  cat("     - source('src/pipeline/01_split_raw.R')\n")
  cat("     - source('src/pipeline/02_clean.R')\n")
  cat("     - source('src/pipeline/03_merge_general.R')\n")
  cat("\n")
  cat("📚 Documentation:\n")
  cat("  - README.md - Full documentation\n")
  cat("  - docs/QUICK_START.md - Quick reference\n")
  cat("  - IMPROVEMENTS_SUMMARY.md - See what changed\n")
  cat("\n")
  
  # Check if config has been set up
  if (file.exists("config/paths.R")) {
    tryCatch({
      source("config/paths.R", local = TRUE)
      cat("✓ Configuration loaded from config/paths.R\n")
    }, error = function(e) {
      cat("⚠ Warning: Could not load config/paths.R\n")
      cat("  Please review and fix any errors in the configuration.\n")
    })
  } else {
    cat("⚠ Configuration file not found!\n")
    cat("  Please ensure config/paths.R exists.\n")
  }
  cat("\n")
}

# Cleanup message on exit (optional)
.Last <- function() {
  cat("\nGoodbye from FTOLP Data Pipeline!\n\n")
}

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Increase output width for better readability
options(width = 100)

# Make sure here package is available for path management
if (!requireNamespace("here", quietly = TRUE)) {
  cat("ℹ Installing 'here' package for path management...\n")
  install.packages("here")
}
