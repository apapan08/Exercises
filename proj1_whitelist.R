allowed_files = c("proj1.md",
                  "proj1.Rmd",
                  "README.md",
                  "proj1.Rproj",
                  "proj1_whitelist.R",
                  "proj1_files",
                  ".gitignore",
                  "data")

files = dir()
disallowed_files = files[!(files %in% allowed_files)]

if (length(disallowed_files != 0)) {
  cat("Disallowed files found:\n")
  cat("  (remove the following files from your repo)\n\n")

  for (file in disallowed_files)
    cat("*", file, "\n")

  quit("no", 1, FALSE)
}
