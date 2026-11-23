#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("resolve_export_folder() validates its inputs", {
  testthat::expect_error(
    resolve_export_folder(
      dir_contract = 1,
      dir_data_contract = "Outputs/Data",
      dir_data = "Outputs/Data_temp"
    ),
    regexp = "dir_contract"
  )
  testthat::expect_error(
    resolve_export_folder(
      dir_contract = "Outputs/Data/Neoclimate",
      dir_data_contract = NA_character_,
      dir_data = "Outputs/Data_temp"
    ),
    regexp = "dir_data_contract"
  )
  testthat::expect_error(
    resolve_export_folder(
      dir_contract = "Outputs/Data/Neoclimate",
      dir_data_contract = "Outputs/Data",
      dir_data = c("A", "B")
    ),
    regexp = "dir_data"
  )
  testthat::expect_error(
    resolve_export_folder(
      dir_contract = "Outputs/Data/Neoclimate",
      dir_data_contract = "Outputs/Data",
      dir_data = ""
    ),
    regexp = "dir_data"
  )
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("resolve_export_folder() replaces the leading folder", {
  testthat::expect_equal(
    resolve_export_folder(
      dir_contract = "Outputs/Data/Neoclimate",
      dir_data_contract = "Outputs/Data",
      dir_data = "Outputs/Data_temp"
    ),
    "Outputs/Data_temp/Neoclimate"
  )
  testthat::expect_equal(
    resolve_export_folder(
      dir_contract = "Outputs/Data/Neoclimate",
      dir_data_contract = "Outputs/Data",
      dir_data = "Outputs/Data"
    ),
    "Outputs/Data/Neoclimate"
  )
  testthat::expect_equal(
    resolve_export_folder(
      dir_contract = "Outputs/Data/A/B",
      dir_data_contract = "Outputs/Data",
      dir_data = "Test"
    ),
    "Test/A/B"
  )
})

testthat::test_that("resolve_export_folder() maps the folder itself", {
  testthat::expect_equal(
    resolve_export_folder(
      dir_contract = "Outputs/Data",
      dir_data_contract = "Outputs/Data",
      dir_data = "Outputs/Data_temp"
    ),
    "Outputs/Data_temp"
  )
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("resolve_export_folder() stops on another folder", {
  testthat::expect_error(
    resolve_export_folder(
      dir_contract = "Outputs/Other/Neoclimate",
      dir_data_contract = "Outputs/Data",
      dir_data = "Outputs/Data_temp"
    ),
    regexp = "Outputs/Other/Neoclimate"
  )
  # A name that only starts with the folder name is not inside it
  testthat::expect_error(
    resolve_export_folder(
      dir_contract = "Outputs/DataX/Neoclimate",
      dir_data_contract = "Outputs/Data",
      dir_data = "Outputs/Data_temp"
    ),
    regexp = "Outputs/DataX/Neoclimate"
  )
})
