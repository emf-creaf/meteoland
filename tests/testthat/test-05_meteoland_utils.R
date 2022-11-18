temperature <- seq(5, 30, 5)
relative_humidity <- seq(50, 100, 10)
dew_temperature <- seq(0, 5, 1)

test_that("humidity_relative2dewtemperature works as expected", {

  # vector
  expect_type(
    (test_res <- humidity_relative2dewtemperature(temperature, relative_humidity)),
    "double"
  )
  expect_length(test_res, length(temperature))

  # matrix
  temperature_matrix <- matrix(temperature, nrow = 2, ncol = 3)
  relative_humidity_matrix <- matrix(relative_humidity, nrow = 2, ncol = 3)
  expect_no_error(
    (test_res_matrix <- humidity_relative2dewtemperature(temperature_matrix, relative_humidity_matrix))
  )
  expect_true(inherits(test_res_matrix, "matrix"))
  expect_equal(dim(test_res_matrix), c(2,3))

  expect_error(
    humidity_relative2dewtemperature(letters, LETTERS)
  )
})

test_that("humidity_dewtemperature2relative works as expected", {

})
