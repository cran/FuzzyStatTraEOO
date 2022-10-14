
test_that("FuzzyNumber instance works (initialize)", {
  ## CONSTRUCTOR ERRORS
  ## invalid parameter: not an array
  expect_error(FuzzyNumber$new())
  expect_error(FuzzyNumber$new(3))
  expect_error(FuzzyNumber$new("a"))
  expect_error(FuzzyNumber$new(matrix()))
  ## more parameters than needed
  expect_error(FuzzyNumber$new(c(), 1))
  ## invalid parameter: array of number that are not numeric
  array <- array(c(0, 5, 1,-1,-1,-1, 2, 1, "a"), dim = c(3, 3))
  expect_error(FuzzyNumber$new(array))
  array <- array(c(NULL, Inf, -Inf, NA, NaN, NULL), dim = c(2, 3))
  expect_error(FuzzyNumber$new(array))

  ## valid parameter
  ## array saved correctly in the attribute fnAlphaLevels
  ## correct called to checkValidity
  array <-
    array(c(0.0, 0.5, 1.0,-1.5,-1.0,-1.0, 2.0, 1.5, 1.0), dim = c(3, 3))
  fuzzy <- FuzzyNumber$new(array)

  expect_equal(class(fuzzy$getFNAlphaLevels())[2], "array")
  expect_equal(class(fuzzy$is_valid()), "logical")

})

test_that("FuzzyNumber checkValidity and is_valid methods", {
  ## valid array
  ## valid FuzzyNumber
  array <-
    array(c(0.0, 0.5, 1.0,-1.5,-1.0,-1.0, 2.0, 1.5, 1.0), dim = c(3, 3))
  fuzzy <- FuzzyNumber$new(array)

  ## more parameters than needed
  expect_error(fuzzy$is_valid(1))

  expect_equal(fuzzy$is_valid(), TRUE)

  ## invalid array
  ## invalid FuzzyNumber
  array <- array(c(1, 2, 3, 4), dim = c(2, 1, 2))
  fuzzy <- FuzzyNumber$new(array)

  expect_output(
    FuzzyNumber$new(array),
    "the array passed as argument in the constructor has more than 1 dimension; more than 1 dimension means more than 1 fuzzy number"
  )
  expect_equal(fuzzy$is_valid(), FALSE)

  ## invalid array
  ## invalid FuzzyNumber
  array <- array(c(0, 0.1, 0.5, 1, 2, 3), dim = c(3, 2))
  fuzzy <- FuzzyNumber$new(array)

  expect_output(
    FuzzyNumber$new(array),
    "each fuzzy number should be characterized by means of a matrix with 3 columns: the first column will be the alpha-levels, the second one their infimum values and the third one their supremum values"
  )
  expect_equal(fuzzy$is_valid(), FALSE)

  ## invalid array
  ## invalid FuzzyNumber
  array <- array(c(1, 2, 3, 4, 5, 6), dim = c(2, 3))
  fuzzy <- FuzzyNumber$new(array)

  expect_output(FuzzyNumber$new(array),
                "the minimum alpha-level should be 0 and the maximum 1")
  expect_equal(fuzzy$is_valid(), FALSE)

  ## invalid array
  ## invalid FuzzyNumber
  array <- array(c(0, 0, 1, 2, 3, 4, 5, 0, 1), dim = c(3, 3))
  fuzzy <- FuzzyNumber$new(array)

  expect_output(FuzzyNumber$new(array),
                "the alpha-levels have to increase from 0 to 1")
  expect_equal(fuzzy$is_valid(), FALSE)

  ## invalid array
  ## invalid FuzzyNumber
  array <- array(c(0, 0.5, 1, 2, 3, 2, 5, 6, 7), dim = c(3, 3))
  fuzzy <- FuzzyNumber$new(array)

  expect_output(FuzzyNumber$new(array),
                "the infimum values have to be non-decreasing")
  expect_equal(fuzzy$is_valid(), FALSE)

  ## invalid array
  ## invalid FuzzyNumber
  array <- array(c(0, 0.5, 1, 2, 3, 4, 5, 0, 1), dim = c(3, 3))
  fuzzy <- FuzzyNumber$new(array)

  expect_output(FuzzyNumber$new(array),
                "the supremum values have to be non-creasing")
  expect_equal(fuzzy$is_valid(), FALSE)

  ## invalid array
  ## invalid FuzzyNumber
  array <- array(c(0, 0.5, 1, 2, 3, 4, 6, 5, 3), dim = c(3, 3))
  fuzzy <- FuzzyNumber$new(array)

  expect_output(
    FuzzyNumber$new(array),
    "the infimum value has to be smaller or equal than the supremum value for each alpha-level"
  )
  expect_equal(fuzzy$is_valid(), FALSE)

})

test_that("FuzzyNumber getFNAlphalevels method", {
  ## checks the dimension
  ## checks random values
  fuzzy <-
    FuzzyNumber$new(array(c(
      0.0, 0.5, 1.0,-1.5,-1.0,-1.0, 2.0, 1.5, 1.0
    ), dim = c(3, 3)))

  ## more parameters than needed
  expect_error(fuzzy$getFNAlphaLevels(1))

  expect_equal(dim(fuzzy$getFNAlphaLevels())[1], 3)
  expect_equal(dim(fuzzy$getFNAlphaLevels())[2], 3)
  expect_equal(fuzzy$getFNAlphaLevels()[2], 0.5)
  expect_equal(fuzzy$getFNAlphaLevels()[4], -1.5)

  ## checks the dimension
  ## checks random values
  fuzzy <- FuzzyNumber$new(array(c(1, 2), dim = c(2, 1)))
  expect_equal(dim(fuzzy$getFNAlphaLevels())[1], 2)
  expect_equal(dim(fuzzy$getFNAlphaLevels())[2], 1)
  expect_equal(fuzzy$getFNAlphaLevels()[1], 1)
  expect_equal(fuzzy$getFNAlphaLevels()[2], 2)

})
