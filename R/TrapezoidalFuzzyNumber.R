#' @title R6 Class representing a 'TrapezoidalFuzzyNumber'.
#'
#' @description
#' A 'TrapezoidalFuzzyNumber' is characterized by their four values inf0, inf1,
#' sup1 and sup0. Depending on its' values the 'TrapezoidalFuzzyNumber' can be valid
#' or not.
#'
#' @note In case you find (almost surely existing) bugs or have recommendations
#' for improving the method, comments are welcome to the above mentioned mail addresses.
#'
#' @author Andrea Garcia Cernuda <uo270115@uniovi.es>
#'
#' @import R6
#'
#' @export TrapezoidalFuzzyNumber
TrapezoidalFuzzyNumber <-
  R6::R6Class(
    classname = "TrapezoidalFuzzyNumber",
    # inherit = FuzzyNumber,
    private = list(
      inf0 = NULL,

      inf1 = NULL,

      sup1 = NULL,

      sup0 = NULL,

      # TRUE when the fuzzy number fulfills the checkValidity method conditions.
      isValid = NULL,

      # TRUE if inf0, inf1, sup1 ans sup0 are positive real numbers.
      isPositive = NULL,

      # verbose specifies if the user wants to see in the console the messages that
      # could be written
      # return TRUE whether the inner conditions are met, otherwise FALSE.
      checkValidity = function(verbose = TRUE)
      {
        private$isValid <-
          (
            private$inf0 <= private$inf1 &&
              private$inf1 <= private$sup1 &&
              private$sup1 <= private$sup0
          )

        if (!private$isValid && verbose) {
          print(
            "The TrapezoidalFuzzyNumber is not valid as inf0, inf1, sup1 and sup0 are not non-decreasing."
          )
        }

        return (private$isValid)
      }
    ),
    public = list(
      #' @description
      #' This method creates a 'TrapezoidalFuzzyNumber' object with all its attributes
      #' set.
      #'
      #' @param inf0 is a real number that corresponds to the infimum of support
      #' of the trapezoidal fuzzy number.
      #' @param inf1 is a real number that corresponds to the infimum of core of
      #' the trapezoidal fuzzy number
      #' @param sup1 is a real number that corresponds to the supremum of core
      #' of the trapezoidal fuzzy numbers
      #' @param sup0 is a real number that corresponds to the supremum of support
      #' of the trapezoidal fuzzy numbers
      #'
      #' @details See examples.
      #'
      #' @return The TrapezoidalFuzzyNumber object created with all its attributes set.
      #'
      #' @examples
      #' # Example 1:
      #' TrapezoidalFuzzyNumber$new(1,2,3,4)
      #'
      #' # Example 2:
      #' TrapezoidalFuzzyNumber$new(-8,-6,-4,-2)
      #'
      #' # Example 3:
      #' TrapezoidalFuzzyNumber$new(-1,-1,2,3)
      #'
      #' # Example 4:
      #' TrapezoidalFuzzyNumber$new(1,2,3,3)
      #'
      #' # Example 5:
      #' TrapezoidalFuzzyNumber$new(1,0,2,3)
      #'
      #' # Example 6:
      #' TrapezoidalFuzzyNumber$new(-1,0,-0.5,0)
      #'
      #' # Example 7:
      #' TrapezoidalFuzzyNumber$new(-2,-4,-6,-8)
      initialize = function(inf0 = NA,
                            inf1 = NA,
                            sup1 = NA,
                            sup0 = NA)
      {
        stopifnot(
          is.double(inf0) &&
            !is.infinite(inf0) &&
            !is.na(inf0) && !is.nan(inf0) && !is.null(inf0)
        )
        stopifnot(
          is.double(inf1) &&
            !is.infinite(inf1) &&
            !is.na(inf1) && !is.nan(inf1) && !is.null(inf1)
        )
        stopifnot(
          is.double(sup1) &&
            !is.infinite(sup1) &&
            !is.na(sup1) && !is.nan(sup1) && !is.null(sup1)
        )
        stopifnot(
          is.double(sup0) &&
            !is.infinite(sup0) &&
            !is.na(sup0) && !is.nan(sup0) && !is.null(sup0)
        )

        private$inf0 <- inf0
        private$inf1 <- inf1
        private$sup1 <- sup1
        private$sup0 <- sup0

        private$checkValidity()
        private$isPositive <-
          (inf0 > -1 && inf1 > -1 && sup1 > -1 && sup0 > -1)
      },

      #' @description
      #' This method gives the inf0 attribute of the 'TrapezoidalFuzzyNumber'.
      #'
      #' @details See examples.
      #'
      #' @return The inf0 attribute of the TrapezoidalFuzzyNumber object.
      #'
      #' @examples
      #' TrapezoidalFuzzyNumber$new(1,2,3,4)$getInf0()
      getInf0 = function()
      {
        return(private$inf0)
      },

      #' @description
      #' This method gives the inf1 attribute of the 'TrapezoidalFuzzyNumber'.
      #'
      #' @details See examples.
      #'
      #' @return The inf1 attribute of the TrapezoidalFuzzyNumber object.
      #'
      #' @examples
      #' TrapezoidalFuzzyNumber$new(1,2,3,4)$getInf1()
      getInf1 = function()
      {
        return(private$inf1)
      },

      #' @description
      #' This method gives the sup1 attribute of the 'TrapezoidalFuzzyNumber'.
      #'
      #' @details See examples.
      #'
      #' @return The sup1 attribute of the TrapezoidalFuzzyNumber object.
      #'
      #' @examples
      #' TrapezoidalFuzzyNumber$new(1,2,3,4)$getSup1()
      getSup1 = function()
      {
        return(private$sup1)
      },

      #' @description
      #' This method gives the sup0 attribute of the 'TrapezoidalFuzzyNumber'.
      #'
      #' @details See examples.
      #'
      #' @return The sup0 attribute of the TrapezoidalFuzzyNumber object.
      #'
      #' @examples
      #' TrapezoidalFuzzyNumber$new(1,2,3,4)$getSup0()
      getSup0 = function()
      {
        return(private$sup0)
      },

      #' @description
      #' This method gives information whether the 'TrapezoidalFuzzyNumber' is valid
      #' regarding its attributes and checking conditions.
      #'
      #' @details See examples.
      #'
      #' @return TRUE whether the TrapezoidalFuzzyNumber object has non-decreasing
      #' attributes, otherwise FALSE.
      #'
      #' @examples
      #' # Example 1:
      #' TrapezoidalFuzzyNumber$new(1,2,3,4)$is_valid()
      #'
      #' # Example 2:
      #' TrapezoidalFuzzyNumber$new(-8,-6,-4,-2)$is_valid()
      #'
      #' # Example 3:
      #' TrapezoidalFuzzyNumber$new(1,0,2,3)$is_valid()
      #'
      #' # Example 4:
      #' TrapezoidalFuzzyNumber$new(-1,0,-0.5,0)$is_valid()
      is_valid = function()
      {
        return(private$isValid)
      },


      #' @description
      #' This method gives information whether the 'TrapezoidalFuzzyNumber' is positive
      #' regarding its attributes.
      #'
      #' @details See examples.
      #'
      #' @return TRUE whether the TrapezoidalFuzzyNumber object has all its attributes
      #' greater than -1, otherwise FALSE.
      #'
      #' @examples
      #' # Example 1:
      #' TrapezoidalFuzzyNumber$new(1,2,3,4)$is_positive()
      #'
      #' # Example 2:
      #' TrapezoidalFuzzyNumber$new(-8,-6,-4,-2)$is_positive()
      #'
      #' # Example 5:
      #' TrapezoidalFuzzyNumber$new(1,0,2,3)$is_positive()
      #'
      #' # Example 6:
      #' TrapezoidalFuzzyNumber$new(-1,0,-0.5,0)$is_positive()
      is_positive = function()
      {
        return(private$isPositive)
      }
    )
  )
