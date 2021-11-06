test_that("data preparation for simulated HMM works", {
  controls = set_controls()
  out = prepare_data(controls, seed = 1)
  expect_snapshot(out)
  expect_snapshot(unlist(out))
})

test_that("data preparation for empirical HMM works", {
  skip_if_offline()
  file = paste0(tempdir(),"/dax.csv")
  download_data(symbol = "^GDAXI", file = file, verbose = FALSE)
  ### with dates
  controls = list(
    states  = 2,
    sdds    = "t",
    horizon = 400,
    data    = list(file        = file,
                   data_column = "Close",
                   to          = "2021-10-01")
  )
  controls = set_controls(controls)
  out = prepare_data(controls)
  expect_snapshot(out)
  expect_snapshot(unlist(out))
  ### without dates
  controls = list(
    states  = 2,
    sdds    = "t",
    horizon = 400,
    data    = list(file        = file,
                   date_column = NA,
                   data_column = "Close",
                   ### 'from' should be ignored in this case
                   from        = "2020-01-01")
  )
  controls = set_controls(controls)
  out = prepare_data(controls)
  expect_snapshot(out)
  expect_snapshot(unlist(out))
  ### with logreturns
  controls = list(
    states  = 2,
    sdds    = "t",
    horizon = 400,
    data    = list(file        = file,
                   data_column = "Close",
                   logreturns  = TRUE)
  )
  controls = set_controls(controls)
  out = prepare_data(controls)
  expect_snapshot(out)
  expect_snapshot(unlist(out))
})

# test_that("data preparation for simulated HHMM works", {
#   ### without 'period'
#   controls = set_controls(controls = list(hierarchy = TRUE))
#   out = prepare_data(controls, seed = 1)
#   expect_snapshot(out)
#   expect_snapshot(unlist(out))
#   ### with 'period'
#   controls = set_controls(controls = list(hierarchy = TRUE,
#                                           horizon   = c(100,NA),
#                                           period    = "y"))
#   out = prepare_data(controls, seed = 1)
#   expect_snapshot(out)
#   expect_snapshot(unlist(out))
# })
# 
# test_that("data preparation for empirical HHMM works", {
#   skip_if_offline()
#   file = paste0(tempdir(),"/dax.csv")
#   download_data(symbol = "^GDAXI", file = file, verbose = FALSE)
#   ### with dates
#   controls = list(
#     hierarchy = TRUE,
#     data    = list(file        = c(file,file),
#                    data_column = c("Close","Close"),
#                    to          = "2021-10-01")
#   )
#   controls = set_controls(controls)
#   out = prepare_data(controls)
#   expect_snapshot(out)
#   expect_snapshot(unlist(out))
#   ### with logreturns
#   controls = list(
#     states  = 2,
#     sdds    = "t",
#     horizon = 400,
#     data    = list(file        = file,
#                    data_column = "Close",
#                    logreturns  = TRUE,
#                    to          = "2021-10-01")
#   )
#   controls = set_controls(controls)
#   out = prepare_data(controls)
#   expect_snapshot(out)
#   expect_snapshot(unlist(out))
# })
