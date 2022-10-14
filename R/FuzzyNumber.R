#' @title R6 Class representing a 'FuzzyNumber'.
#'
#' @description
#' A 'FuzzyNumber' is an array of dimension nl x 3 x 1. It can be valid or not.
#'
#' @note In case you find (almost surely existing) bugs or have recommendations
#' for improving the method, comments are welcome to the above mentioned mail addresses.
#'
#' @author Andrea Garcia Cernuda <uo270115@uniovi.es>
#'
#' @import R6
#'
#' @export FuzzyNumber
FuzzyNumber <- R6::R6Class(
  classname = "FuzzyNumber",
  private = list(
    fnAlphaLevels = NULL,

    # TRUE when the fuzzy number fulfills the checkValidity method conditions.
    isValid = NULL,

    # Checks whether the inner conditions are met.
    # fnAlphaLevels array of dimension nl x 3 x 1.
    # R must fulfill the next conditions:
    # 1) The number of columns should be 3.
    # 2) All the fuzzy numbers have to have the same column of \eqn{\alpha}-levels.
    # 3) The minimum \eqn{\alpha}-level should be 0 y the maximum 1.
    # 4) The \eqn{\alpha}-levels have to increase from 0 to 1.
    # 5) The infimum values have to be non-decreasing.
    # 6) The supremum values have to be non-creasing.
    # 7) The infimum value has to be smaller or equal than the supremum value for each \eqn{\alpha}-level.
    #
    # verbose specifies if the user wants to see in the console the messages that could be written
    #
    # return TRUE whether the inner conditions are met, otherwise FALSE.
    checkValidity = function(verbose = TRUE)
    {
      valid <- TRUE

      nl <- dim(private$fnAlphaLevels)[1]
      ncol <- dim(private$fnAlphaLevels)[2]
      r   <-   dim(private$fnAlphaLevels)[3]

      if (!is.na(r) && r != 1)
      {
        if (verbose) {
          print(
            "the array passed as argument in the constructor has more than 1 dimension; more than 1 dimension means more than 1 fuzzy number"
          )
        }
        valid <- FALSE
      }

      else
        if (ncol != 3)
        {
          if (verbose) {
            print(
              "each fuzzy number should be characterized by means of a matrix with 3 columns: the first column will be the alpha-levels, the second one their infimum values and the third one their supremum values"
            )
          }
          valid <- FALSE
        }

      else
        if (private$fnAlphaLevels[1, 1] != 0 |
            private$fnAlphaLevels[nl, 1] != 1)
        {
          if (verbose) {
            print("the minimum alpha-level should be 0 and the maximum 1")
          }
          valid <- FALSE
        }

      else
        if (length(unique(private$fnAlphaLevels[, 1])) != nl |
            all(private$fnAlphaLevels[, 1] == sort(private$fnAlphaLevels[, 1])) == FALSE)
        {
          if (verbose) {
            print("the alpha-levels have to increase from 0 to 1")
          }
          valid <- FALSE
        }

      else
        if (all(abs(private$fnAlphaLevels[, 2] - apply(
          as.matrix(private$fnAlphaLevels[, 2]),
          2,
          sort
        )) <=
        10 ^ (-10))
        == FALSE)
        {
          if (verbose) {
            print("the infimum values have to be non-decreasing")
          }
          valid <- FALSE
        }

      else
        if (all(abs(
          private$fnAlphaLevels[, 3] - apply(as.matrix(private$fnAlphaLevels[, 3]), 2, sort, decreasing =
                                             TRUE)
        ) <= 10 ^ (-10))
        == FALSE)
        {
          if (verbose) {
            print("the supremum values have to be non-creasing")
          }
          valid <- FALSE
        }

      else
        if (all(private$fnAlphaLevels[nl, 3] - private$fnAlphaLevels[nl, 2] >= 0) == FALSE)
        {
          if (verbose) {
            print(
              "the infimum value has to be smaller or equal than the supremum value for each alpha-level"
            )
          }
          valid <- FALSE
        }

      private$isValid <- valid

      return (valid)
    }
  ),
  public = list(
    #' @description
    #' This method creates a 'FuzzyNumber' object with all its attributes set.
    #'
    #' @param fnAlphaLevels is an array of dimension nl x 3 x 1 (general fuzzy number).
    #' nl is the number of considered \eqn{\alpha}-levels and 3 is the number of
    #' columns of the array. The first column represents the number of considered
    #' \eqn{\alpha}-levels, the second one represents their infimum values and the
    #' third and last column represents their supremum values.
    #'
    #' @details See examples.
    #'
    #' @return The FuzzyNumber object created with all its attributes set.
    #'
    #' @examples
    #' # Example 1:
    #' FuzzyNumber$new(array(c(0.0,0.5,1.0,-1.5,-1.0,-1.0,2.0,1.5,1.0),dim=c(3,3)))
    #'
    #' # Example 2:
    #' FuzzyNumber$new(array(c(1,2,3,4),dim=c(2,1,2)))
    #'
    #' # Example 3:
    #' FuzzyNumber$new(array(c(0,0.1,0.5,1,2,3),dim=c(3,2)))
    #'
    #' # Example 4:
    #' FuzzyNumber$new(array(c(1,2,3,4,5,6),dim=c(2,3)))
    #'
    #' # Example 5:
    #' FuzzyNumber$new(array(c(0,0,1,2,3,4,5,0,1),dim=c(3,3)))
    #'
    #' # Example 6:
    #' FuzzyNumber$new(array(c(0,0.5,1,2,3,2,5,6,7),dim=c(3,3)))
    #'
    #' # Example 7:
    #' FuzzyNumber$new(array(c(0,0.5,1,2,3,4,5,0,1),dim=c(3,3)))
    #'
    #' # Example 8:
    #' FuzzyNumber$new(array(c(0,0.5,1,2,3,4,6,5,3),dim=c(3,3)))
    initialize = function(fnAlphaLevels = NA) {
      stopifnot(is.array(fnAlphaLevels))
      stopifnot(is.double(fnAlphaLevels))
      if (anyNA(fnAlphaLevels) ||
          any(is.infinite(fnAlphaLevels)) ||
          any(is.nan(fnAlphaLevels))) {
        stop("The FuzzyNumber cannot contain Inf, -Inf, Na, NaN.")
      }

      private$fnAlphaLevels <- fnAlphaLevels

      private$checkValidity()
    },

    #' @description
    #' This method gives the 'fnAlphaLevels' array of the 'FuzzyNumber'.
    #'
    #' @details See examples.
    #'
    #' @return The array fnAlphaLevels of the FuzzyNumber object.
    #'
    #' @examples
    #' FuzzyNumber$new(array(c(0.0,0.5,1.0,-1.5,-1.0,-1.0,2.0,1.5,1.0),dim=c(3,3))
    #' )$getFNAlphaLevels()
    getFNAlphaLevels = function() {
      return(private$fnAlphaLevels)
    },

    #' @description
    #' This method gives information whether the 'FuzzyNumber' is valid
    #' regarding its \eqn{\alpha}-levels, its infimum and its supremum values.
    #'
    #' @details See examples.
    #'
    #' @return TRUE whether the FuzzyNumber object has a valid array,
    #' otherwise FALSE.
    #'
    #' @examples
    #' FuzzyNumber$new(array(c(0.0,0.5,1.0,-1.5,-1.0,-1.0,2.0,1.5,1.0),dim=c(3,3))
    #' )$is_valid()
    is_valid = function() {
      return(private$isValid)
    }
  )
)
