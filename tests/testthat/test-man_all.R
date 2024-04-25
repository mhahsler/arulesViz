skip_on_cran()

# test that code does not error

path <-getwd()
setwd(tempdir())

test_examples(path = paste0(path, "/../.."))

setwd(path)