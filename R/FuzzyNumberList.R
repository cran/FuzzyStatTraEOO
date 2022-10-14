#' @title 'FuzzyNumberList' is a child class of 'StatList'. It contains 'FuzzyNumbers'.
#'
#' @description
#' 'FuzzyNumberList' can contain valid and not valid 'FuzzyNumbers'.
#' This class implements a version of the empty 'StatList' methods.
#'
#' @note In case you find (almost surely existing) bugs or have recommendations
#' for improving the method, comments are welcome to the above mentioned mail addresses.
#'
#' @author(s) Andrea Garcia Cernuda <uo270115@uniovi.es>, Asun Lubiano <lubiano@uniovi.es>,
#' Sara de la Rosa de Saa
#'
#' @references
#' [1] Blanco-Fernandez, A.; Casals, R.M.; Colubi, A.; Corral, N.; Garcia-Barzana, M.; Gil, M.A.;
#' Gonzalez-Rodriguez, G.; Lopez, M.T.; Lubiano, M.A.; Montenegro, M.; Ramos-Guajardo, A.B.;
#' de la Rosa de Saa, S.; Sinova, B.: Random fuzzy sets: A mathematical tool to develop
#' statistical fuzzy data analysis, Iranian Journal on Fuzzy Systems 10(2), 1-28 (2013)
#'
#' [2] Diamond, P.; Kloeden, P.: Metric spaces of fuzzy sets, Fuzzy Sets and Systems 35,
#' 241-249 (1990)
#'
#' [3] Sinova, B.; de la Rosa de Saa, S.; Gil, M.A.: A generalized L1-type metric between fuzzy
#' numbers for an approach to central tendency of fuzzy data, Information Sciences 242, 22-34
#' (2013)
#'
#' [4] Sinova, B.; Gil, M.A.; Van Aelst, S.: M-estimates of location for the robust central
#' tendency of fuzzy data, IEEE Transactions on Fuzzy Systems 24(4), 945-956 (2016)
#'
#' @import R6
#'
#' @export FuzzyNumberList
FuzzyNumberList <- R6::R6Class(
  classname = "FuzzyNumberList",
  inherit = StatList,
  private = list (
    # auxiliary method used in the dthetaphi public method
    # it calculates integrals by hand as sums
    # x is a vector (first column of fuzzy set)
    l2dist = function(x = NA) {
      k <- length(x) - 1
      delta <- 1 / k
      y <- x[1:k] + x[2:(k + 1)]
      values <- x[1:k] + x[2:(k + 1)] + 2 * y
      integral <- sum(values) * delta / 6
      invisible(integral)
    },

    # auxiliary method used in the rho1 public method
    # it calculates integrals by hand by Simpson's rule
    # x is a vector (first column of fuzzy set)
    rho1dist = function(x = NA) {
      k <- length(x) - 1
      delta <- 1 / k
      y <- x[1:k] + x[2:(k + 1)]
      values <- abs(x[1:k]) + abs(x[2:(k + 1)]) + 2 * abs(y)
      integral <- sum(values) * delta / 6
      invisible(integral)
    },

    # auxiliary method used in the dthetaphi, dwablphi and rho1 public methods
    # list is the second FuzzyNumberList that is going to be compared
    checkAlphaLevels = function(list = NA, verbose = TRUE) {
      aux <- self$numbers[[1]]$getFNAlphaLevels()[, 1]
      result <- TRUE
      for (val in 1:length(list$numbers)) {
        if (!identical(aux,
                       list$numbers[[val]]$getFNAlphaLevels()[, 1])) {
          if (verbose) {
            print("the fuzzy numbers of the two FuzzyNumberLists must have the same alpha-levels")
          }
          result <- FALSE
        }
        if (!result) {
          break
        }
      }
      return (result)
    }
  ),
  public = list(
    #' @field numbers is a collection of fuzzy numbers.
    numbers = NULL,

    #' @description
    #' This method creates a 'FuzzyNumberList' object with the columns and dimensions
    #' attributes set.
    #'
    #' @param numbers is a list of dimension nl x 3 x n which contains n
    #' fuzzy numbers. nl is the number of considered \eqn{\alpha}-levels and 3 is the number of
    #' columns of the list. The first column represents the number of considered
    #' \eqn{\alpha}-levels, the second one represents their infimum values and the
    #' third and last column represents their supremum values.
    #'
    #' @details See examples.
    #'
    #' @return The FuzzyNumberList object created with the columns and dimensions
    #' attributes set.
    #'
    #' @examples
    #' # Example 1:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0,-1.5,-1.0,-1.0, 2.0, 1.5, 1.0), dim =
    #' c(3, 3))), FuzzyNumber$new(array(c(0.0, 0.5, 1.0,-1.5,-1.25,-1.0, 3.0, 2.0,
    #' 1.0), dim = c(3, 3)))))
    #'
    #' # Example 2:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.5, -1.0, 2, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1.5, -1.0, -1.0, 2.0, 1.5, 1.0),
    #' dim = c(3, 3)))))
    initialize = function(numbers = NA) {
      stopifnot(is.list(numbers))
      for (val in 1:length(numbers)) {
        stopifnot(class(numbers[[val]])[1] == "FuzzyNumber")
      }

      private$dimensions <- length(numbers)
      private$columns <- ncol(numbers[[val]]$getFNAlphaLevels())
      self$numbers <- numbers
    },

    #' @description
    #' This method checks that the numbers that contain this class are given in the correct format.
    #' First, it checks that all individual 'FuzzyNumber' are valid according its conditions.
    #' Second and last, it checks that all the 'FuzzyNumbers' have the same column of \eqn{\alpha}-levels.
    #' It also set the rows attribute.
    #'
    #' @param verbose if TRUE the messages are written to the console unless the
    #' user actively decides to set verbose=FALSE.
    #'
    #' @details See examples.
    #'
    #' @return TRUE if all FuzzyNumbers are valid and have the same column of \eqn{\alpha}-levels.
    #' Otherwise, FALSE.
    #'
    #' @examples
    #' # Example 1:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.5, -1.0, 2, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1.5, -1.0, -1.0, 2.0, 1.5, 1.0),
    #' dim = c(3, 3)))))$checking()
    #'
    #' # Example 2:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(1, 2, 3, 4), dim = c(2, 1))),
    #' FuzzyNumber$new(array(c(0, 0.1, 0.5, 1, 2, 3), dim = c(3, 2)))))$checking()
    #'
    #' # Example 3:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0,-1.5,-1.0,-1.0, 2.0, 1.5, 1.0), dim =
    #' c(3, 3))),FuzzyNumber$new(array(c(0.0, 0.5, 1.0,-1.5,-1.25,-1.0, 3.0, 2.0,
    #' 1.0), dim = c(3, 3)))))$checking()
    checking = function(verbose = TRUE) {
      for (val in 1:length(self$numbers)) {
        if (!self$numbers[[val]]$is_valid()) {
          if (verbose) {
            print("Not all fuzzy numbers are valid")
          }
          return (FALSE)
        }
      }

      aux <- self$numbers[[1]]$getFNAlphaLevels()[, 1]
      if (length(self$numbers) > 1) {
        for (val in 2:length(self$numbers)) {
          if (!identical(aux,
                         self$numbers[[val]]$getFNAlphaLevels()[, 1])) {
            if (verbose) {
              print("all fuzzy numbers must have the same alpha-levels")
            }
            return (FALSE)
          }
        }
      }

      private$rows <- length(aux)

      return (TRUE)
    },

    #' @description
    #' This method calculates the mid/spr distance between the FuzzyNumbers contained
    #' in the current object and the one passed as parameter, which should be given
    #' in the desired format. For this, the method first checks if the input 'FuzzyNumberList'
    #' s is in the correct form (tested by the checking method) and if the \eqn{\alpha}-levels
    #' of all 'FuzzyNumbers' coincide.
    #' See Blanco-Fernandez et al. (2013) [1].
    #'
    #' @param s FuzzyNumberList containing FuzzyNumbers characterized by means of
    #' nl \eqn{\alpha}-levels each. The method first calls checking to check if the
    #' FuzzyNumberList s has the correct format. Moreover, the \eqn{\alpha}-levels
    #' of the FuzzyNumberList s should coincide with the ones of the current
    #' FuzzyNumberList (the method checks this condition).
    #' @param a real number > 0, by default a=1. It is the first parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1].
    #' @param b real number > 0, by default b=1. It is the second parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1].
    #' @param theta real number > 0, by default theta=1. It is the weight of the
    #' spread in the mid/spr distance.
    #'
    #' @details See examples.
    #'
    #' @return a matrix containing the mid/spr distances between the two previous
    #' mentioned FuzzyNumberLists. If the body's method inner conditions are not met,
    #' NA will be returned.
    #'
    #' @examples
    #' # Example 1:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.5, -1.0, 2, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.0, -1.0, 1.5, 1.0), dim = c(2, 3)))
    #' ))$dthetaphi(
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -0.5, 0, 1.5, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, 1, 1.5, 1.5, 1.5), dim = c(2, 3))))),
    #' 1,5,1)
    #'
    #' # Example 2:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1.5, -1.0, -1.0, 2.0, 1.5, 1.0), dim =
    #' c(3, 3))),FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1.5, -1.25, -1.0, 3.0, 2.0,
    #' 1.0), dim = c(3, 3))), FuzzyNumber$new(array(c(0.0, 0.5, 1.0, 0, 1.0, 1.0, 2.5,
    #' 2.0, 1.5), dim = c(3, 3))),FuzzyNumber$new(array(c(0.0, 0.5, 1.0, 0.5 , 1, 1.5,
    #' 3, 2.0, 2), dim = c(3, 3)))))$dthetaphi(FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0,1,1.25,1.5, 2, 1.75, 1.5), dim = c(3, 3))),
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0,-1,-0.5,0, 1.5, 1.25, 1), dim = c(3, 3))))
    #' ), 1, 1, 1/3)
    #'
    #' # Example 3:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.5, -1.0, 2, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.0, -1.0, 1.5, 1.0), dim = c(2, 3)))
    #' ))$dthetaphi(
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -0.5, 0, 1.5, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, 1, 1.5, 1.5, 1.0), dim = c(2, 3))))),
    #' 2,1,1)
    #'
    #' # Example 4:
    #' F=Simulation$new()$simulCase1(10L)
    #' S=Simulation$new()$simulCase1(20L)
    #' F=F$transfTra()
    #' S=S$transfTra()
    #' F$dthetaphi(S,1,5,1)
    #'
    #' # Example 5:
    #' F=Simulation$new()$simulCase1(10L)
    #' S=Simulation$new()$simulCase1(10L)
    #' F$dthetaphi(S,2,1,1/3)
    #'
    #' # Example 6:
    #' F=Simulation$new()$simulCase1(10L)
    #' S=Simulation$new()$simulCase1(10L)
    #' F=F$transfTra()
    #' S=S$transfTra(50L)
    #' F$dthetaphi(S,2,1,1)
    dthetaphi = function(s = NA,
                         a = 1,
                         b = 1,
                         theta = 1) {
      stopifnot(class(s)[1] == "FuzzyNumberList")
      super$dthetaphi(s, a, b, theta)

      if (self$checking() &&
          s$checking() && private$checkAlphaLevels(s)) {
        r <- length(self$numbers)
        p <- length(s$numbers)
        alpha <- self$numbers[[1]]$getFNAlphaLevels()[, 1]
        dthetaphicua <- matrix(nrow = r, ncol = p)

        for (i in 1:r) {
          for (j in 1:p) {
            mid <- (((
              self$numbers[[i]]$getFNAlphaLevels()[, 2] +
                self$numbers[[i]]$getFNAlphaLevels()[, 3]
            ) -
              (
                s$numbers[[j]]$getFNAlphaLevels()[, 2] +
                  s$numbers[[j]]$getFNAlphaLevels()[, 3]
              )
            ) / 2) ^ 2 * dbeta(alpha, a, b)

            spr <- (((
              self$numbers[[i]]$getFNAlphaLevels()[, 3] -
                self$numbers[[i]]$getFNAlphaLevels()[, 2]
            ) -
              (
                s$numbers[[j]]$getFNAlphaLevels()[, 3] -
                  s$numbers[[j]]$getFNAlphaLevels()[, 2]
              )
            ) / 2) ^ 2 * dbeta(alpha, a, b)

            dthetaphicua[i, j] <-
              private$l2dist(mid) + theta * private$l2dist(spr)
          }
        }

        dthetaphi <- sqrt(dthetaphicua)
        return(dthetaphi)
      }

      return (NA)

    },

    #' @description
    #' This method calculates the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance between
    #' the 'FuzzyNumbers' contained in two 'FuzzyNumberLists', which should be given
    #' in the desired format. For this, the method first checks if the two 'FuzzyNumberLists'
    #' are in the correct form (tested by the checking method) and if the \eqn{\alpha}-levels
    #' of all 'FuzzyNumbers' coincide.
    #' See Sinova et al. (2013) [3] and Sinova et al. (2016) [4].
    #'
    #' @param s FuzzyNumberList containing FuzzyNumbers characterized by means of nl
    #' \eqn{\alpha}-levels each. The method first calls checking to check if it is
    #' in the correct format. Moreover, the \eqn{\alpha}-levels should coincide with
    #' ones of the other FuzzyNumberList (the method checks this condition).
    #' @param a real number > 0, by default a=1. It is the first parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1].
    #' @param b real number > 0, by default b=1. It is the second parameter of a
    #' beta distribution which corresponds to a weighting measure on [0,1].
    #' @param theta real number > 0, by default theta=1. It is the weight of the
    #' ldev and rdev in the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distance.
    #'
    #' @details See examples.
    #'
    #' @return a matrix containing the (\eqn{\phi},\eqn{\theta})-wabl/ldev/rdev distances
    #' between the two previous mentioned FuzzyNumberLists. If the body's
    #' method inner conditions are not met, NA will be returned.
    #'
    #' @examples
    #' # Example 1:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.5, -1.0, 2, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.0, -1.0, 1.5, 1.0), dim = c(2, 3)))
    #' ))$dwablphi(
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -0.5, 0, 1.5, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, 1, 1.5, 1.5, 1.5), dim = c(2, 3))))),
    #' 1,5,1)
    #'
    #' # Example 2:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1.5, -1.0, -1.0, 2.0, 1.5, 1.0), dim =
    #' c(3, 3))),FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1.5, -1.25, -1.0, 3.0, 2.0,
    #' 1.0), dim = c(3, 3))), FuzzyNumber$new(array(c(0.0, 0.5, 1.0, 0, 1.0, 1.0, 2.5,
    #' 2.0, 1.5), dim = c(3, 3))),FuzzyNumber$new(array(c(0.0, 0.5, 1.0, 0.5 , 1, 1.5,
    #' 3, 2.0, 2), dim = c(3, 3)))))$dwablphi(FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0,1,1.25,1.5, 2, 1.75, 1.5), dim = c(3, 3))),
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0,-1,-0.5,0, 1.5, 1.25, 1), dim = c(3, 3))))
    #' ), 1, 1, 1/3)
    #'
    #' # Example 3:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.5, -1.0, 2, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.0, -1.0, 1.5, 1.0), dim = c(2, 3)))
    #' ))$dwablphi(
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -0.5, 0, 1.5, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, 1, 1.5, 1.5, 1.0), dim = c(2, 3))))),
    #' 2,1,1)
    #'
    #' # Example 4:
    #' F=Simulation$new()$simulCase1(3L)
    #' S=Simulation$new()$simulCase1(4L)
    #' F=F$transfTra()
    #' S=S$transfTra()
    #' F$dwablphi(S,2,1,1)
    #'
    #' # Example 5:
    #' F=Simulation$new()$simulCase1(10L)
    #' S=Simulation$new()$simulCase1(10L)
    #' F$dwablphi(S)
    #'
    #' # Example 6:
    #' F=Simulation$new()$simulCase1(10L)
    #' S=Simulation$new()$simulCase1(10L)
    #' F=F$transfTra()
    #' S=S$transfTra(50L)
    #' F$dwablphi(S,2,1,1)
    dwablphi  = function(s = NA,
                         a = 1,
                         b = 1,
                         theta = 1) {
      stopifnot(class(s)[1] == "FuzzyNumberList")
      super$dwablphi(s, a, b, theta)

      if (self$checking() &&
          s$checking() && private$checkAlphaLevels(s)) {
        r <- length(self$numbers)
        p <- length(s$numbers)
        alpha <- self$numbers[[1]]$getFNAlphaLevels()[, 1]
        wablR <- vector()
        wablS <- vector()
        dwablphi <- matrix(nrow = r, ncol = p)

        for (i in 1:r) {
          midR <-
            (self$numbers[[i]]$getFNAlphaLevels()[, 2] + self$numbers[[i]]$getFNAlphaLevels()[, 3]) /
            2
          integrandWablR <- midR * dbeta(alpha, a, b)
          wablR[i] <- private$l2dist(integrandWablR)
        }

        for (j in 1:p) {
          midS <-
            (s$numbers[[j]]$getFNAlphaLevels()[, 2] + s$numbers[[j]]$getFNAlphaLevels()[, 3]) /
            2
          integrandWablS <- midS * dbeta(alpha, a, b)
          wablS[j] <- private$l2dist(integrandWablS)
        }

        for (i in 1:r) {
          ldevR <- wablR[i] - self$numbers[[i]]$getFNAlphaLevels()[, 2]
          rdevR <-
            self$numbers[[i]]$getFNAlphaLevels()[, 3] - wablR[i]
          for (j in 1:p) {
            ldevS <- wablS[j] - s$numbers[[j]]$getFNAlphaLevels()[, 2]
            rdevS <-
              s$numbers[[j]]$getFNAlphaLevels()[, 3] - wablS[j]
            integrandldev <- abs(ldevR - ldevS) * dbeta(alpha, a, b)
            integrandrdev <- abs(rdevR - rdevS) * dbeta(alpha, a, b)
            dwablphi[i, j] <-
              abs(wablR[i] - wablS[j]) + (theta / 2) * private$l2dist(integrandldev) +
              (theta / 2) * private$l2dist(integrandrdev)
          }
        }

        return (dwablphi)
      }

      return (NA)

    },

    #' @description
    #' This method calculates the 1-norm distance between the 'FuzzyNumbers' contained
    #' in two 'FuzzyNumberLists', which should be given in the desired format. For
    #' this, the method first checks if the two 'FuzzyNumberLists' are in the correct
    #' form (tested by the checking method) and if the \eqn{\alpha}-levels of all
    #' 'FuzzyNumbers' coincide.
    #' See Diamond and Kloeden. (1990) [2].
    #'
    #' @param s FuzzyNumberList containing FuzzyNumbers characterized by means of nl
    #' \eqn{\alpha}-levels each. The method first calls checking to check if it is
    #' in the correct format. Moreover, the \eqn{\alpha}-levels should coincide with
    #' ones of the other FuzzyNumberList (the method checks this condition).
    #'
    #' @details See examples.
    #'
    #' @return a matrix containing the 1-norm distances between the two previous
    #' mentioned FuzzyNumberLists. If the body's method inner conditions are not met,
    #' NA will be returned.
    #'
    #' @examples
    #' # Example 1:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.5, -1.0, 2, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.0, -1.0, 1.5, 1.0), dim = c(2, 3)))
    #' ))$rho1(
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -0.5, 0, 1.5, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, 1, 1.5, 1.5, 1.5), dim = c(2, 3))))))
    #'
    #' # Example 2:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1.5, -1.0, -1.0, 2.0, 1.5, 1.0), dim =
    #' c(3, 3))),FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1.5, -1.25, -1.0, 3.0, 2.0,
    #' 1.0), dim = c(3, 3))), FuzzyNumber$new(array(c(0.0, 0.5, 1.0, 0, 1.0, 1.0, 2.5,
    #' 2.0, 1.5), dim = c(3, 3))),FuzzyNumber$new(array(c(0.0, 0.5, 1.0, 0.5 , 1, 1.5,
    #' 3, 2.0, 2), dim = c(3, 3)))))$rho1(FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0,1,1.25,1.5, 2, 1.75, 1.5), dim = c(3, 3))),
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0,-1,-0.5,0, 1.5, 1.25, 1), dim = c(3, 3))))))
    #'
    #' # Example 3:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.5, -1.0, 2, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.0, -1.0, 1.5, 1.0), dim = c(2, 3)))
    #' ))$rho1(
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -0.5, 0, 1.5, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 1.0, 1, 1.5, 1.5, 1.0), dim = c(2, 3))))))
    #'
    #' # Example 4:
    #' F=Simulation$new()$simulCase1(4L)
    #' S=Simulation$new()$simulCase1(5L)
    #' F=F$transfTra()
    #' S=S$transfTra()
    #' F$rho1(S)
    #' S$rho1(F)
    #'
    #' # Example 6:
    #' F=Simulation$new()$simulCase1(4L)
    #' S=Simulation$new()$simulCase1(5L)
    #' F=F$transfTra()
    #' S=S$transfTra(10L)
    #' F$rho1(S)
    #' S$rho1(F)
    rho1 = function(s = NA) {
      stopifnot(class(s)[2] == "StatList")
      stopifnot(class(s)[1] == "FuzzyNumberList")

      if (self$checking() &&
          s$checking() && private$checkAlphaLevels(s)) {
        r <- length(self$numbers)
        p <- length(s$numbers)
        rho <- matrix(nrow = r, ncol = p)

        for (i in 1:r) {
          for (j in 1:p) {
            inf <- self$numbers[[i]]$getFNAlphaLevels()[, 2] -
              s$numbers[[j]]$getFNAlphaLevels()[, 2]

            sup <- self$numbers[[i]]$getFNAlphaLevels()[, 3] -
              s$numbers[[j]]$getFNAlphaLevels()[, 3]
            rho[i, j] <-
              (private$rho1dist(inf) + private$rho1dist(sup)) * 0.5
          }
        }

        return(rho)
      }

      return (NA)

    },

    #' @description
    #' This method adds a 'FuzzyNumber' to the current collection of fuzzy numbers.
    #' Therefore, the dimensions' field is increased in a unit.
    #'
    #' @param n is the FuzzyNumber to be added to the current collection of fuzzy
    #' numbers.
    #' @param verbose if TRUE the messages are written to the console unless the
    #' user actively decides to set verbose=FALSE.
    #'
    #' @details See examples.
    #'
    #' @return NULL.
    #'
    #' @examples
    #' # Example 1:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.5, -1.0, 2, 1), dim = c(2, 3)))))$addFuzzyNumber(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1, -0.5, 0, 1.5, 1.25, 1), dim = c(3, 3))))
    #'
    #' # Example 2:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1, -0.5, 0, 1.5, 1.25, 1), dim = c(3, 3)))
    #' ))$addFuzzyNumber( FuzzyNumber$new(array(c(0.0, 0.5, 1.0, 1, 1.25, 1.5, 2, 1.75,
    #' 1.5), dim = c(3, 3))))
    addFuzzyNumber = function(n = NA, verbose = TRUE) {
      stopifnot(class(n)[1] == "FuzzyNumber")
      private$dimensions <- private$dimensions + 1
      self$numbers <- append(self$numbers, n)
      if (verbose) {
        cat("numbers updated, current dimension is", private$dimensions)
      }
    },

    #' @description
    #' This method removes a 'FuzzyNumber' to the current collection of fuzzy numbers.
    #' Therefore, the dimensions' field is decreased in a unit.
    #'
    #' @param i is the position of the FuzzyNumber to be removed in the current
    #' collection of fuzzy numbers.
    #' @param verbose if TRUE the messages are written to the console unless the
    #' user actively decides to set verbose=FALSE.
    #'
    #' @details See examples.
    #'
    #' @return NULL.
    #'
    #' @examples
    #' # Example 1:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 1.0, -1.5, -1.0, 2, 1), dim = c(2, 3))),
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1, -0.5, 0, 1.5, 1.25, 1), dim = c(3, 3)))
    #' ))$removeFuzzyNumber(1L)
    #'
    #' # Example 2:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1, -0.5, 0, 1.5, 1.25, 1), dim = c(3, 3))),
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, 1, 1.25, 1.5, 2, 1.75, 1.5), dim = c(3, 3)))
    #' ))$removeFuzzyNumber(2L)
    removeFuzzyNumber = function(i = NA, verbose = TRUE) {
      stopifnot(typeof(i) == "integer" &&
                  i > 0 && i <= private$dimensions)
      private$dimensions <- private$dimensions - 1
      self$numbers <- self$numbers[-i]
      if (verbose) {
        cat("numbers updated, current dimension is", private$dimensions)
      }
    },

    #' @description
    #' This method gives the number contained in the dimension passed as parameter
    #' when the dimension is greater than 0 and not greater than the dimensions
    #' of the 'FuzzyNumberList's' numbers array.
    #'
    #' @param i is the dimension of the FuzzyNumber wanted to be retrieved.
    #'
    #' @details See examples.
    #'
    #' @return The FuzzyNumber contained in the dimension passed as parameter or
    #' an error if the dimension is not valid.
    #'
    #' @examples
    #' # Example 1:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1, -0.5, 0, 1.5, 1.25, 1), dim = c(3, 3))),
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, 1, 1.25, 1.5, 2, 1.75, 1.5), dim = c(3, 3)))
    #' ))$getDimension(1L)
    #'
    #' # Example 2:
    #' FuzzyNumberList$new(c(
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, -1, -0.5, 0, 1.5, 1.25, 1), dim = c(3, 3))),
    #' FuzzyNumber$new(array(c(0.0, 0.5, 1.0, 1, 1.25, 1.5, 2, 1.75, 1.5), dim = c(3, 3)))
    #' ))$getDimension(2L)
    getDimension = function(i = NA) {
      stopifnot(typeof(i) == "integer" &&
                  i > 0 && i <= private$dimensions)
      self$numbers[[i]]
    }
  )
)
