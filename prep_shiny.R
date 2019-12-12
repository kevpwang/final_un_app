library(fs)

# specify overwrite = TRUE

file.copy("clean-data/majs_gdps.rds", "un_app", overwrite = TRUE)
file.copy("clean-data/issues_majs.rds", "un_app", overwrite = TRUE)
