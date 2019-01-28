x <- try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))
if (x != "try-error") {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  print("no errors")
} else {
  filename <- "server.R"
  filepath <- file.choose() # browse and select your_file.R in the window
  dir <- substr(filepath, 1, nchar(filepath) - nchar(filename))
  setwd(dir)
}
rm(list = setdiff(ls(), ""))