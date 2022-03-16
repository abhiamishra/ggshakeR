# Create configuration file for lintr
# Source this file in package root directory

# List here files to exclude from lint checking, as a character vector
excluded_files <- c(
  list.files("data",      recursive = TRUE, full.names = TRUE),
  list.files("docs",      recursive = TRUE, full.names = TRUE),
  list.files("inst/doc",  recursive = TRUE, full.names = TRUE),
  list.files("man",       recursive = TRUE, full.names = TRUE),
  list.files("vignettes", recursive = TRUE, full.names = TRUE)
)

### Do not edit after this line ###

library(magrittr)
library(dplyr)

# Make sure we start fresh
if (file.exists(".lintr")) { file.remove(".lintr") }

fileName <- ".lintr"
r <- file(fileName, open = "w")

# List current lints
lintstuff <- lintr::lint_package() %>%
  as.data.frame() %>%
  group_by(linter) %>%
  tally(sort = TRUE) 

cat(file = r, 
    sprintf("linters: with_defaults(\n"))


for (i in 1:nrow(lintstuff)) {
  cat(file = r, sprintf("%s\n",
                        paste0(lintstuff$linter[[i]], " = NULL, # ", collapse = "\n    ")))  
}

cat(file = r,
    sprintf(")"))

cat(file = r, sprintf("\nexclusions: list(\n    %s\n  )",
        paste0('"', excluded_files, '"', collapse = ",\n    ")) )

close(r)

# Clean up workspace
remove(excluded_files)