#' Question 1: R function to Compute Add-4 Odds Ratio
#'
#' Computes the add-4 odds ratio, which adds 1 to each cell of a 2x2 contingency table
#' to stabilize the estimate, especially in small samples or zero counts.
#'
#' @param exposure A logical vector indicating exposure status (e.g., TRUE = exposed, FALSE = unexposed).
#' @param outcome A logical vector indicating outcome status (e.g., TRUE = event occurred, FALSE = no event).
#'
#' @return A numeric value representing the add-4 odds ratio.
#'
#' @details
#' This function calculates the odds ratio using the add-4 method: 1 is added to each
#' cell in the 2x2 table to prevent division by zero and to reduce bias in small samples.
#'
#' @examples
#' exposure <- c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
#' outcome  <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
#' add4_or(exposure, outcome)
#'
#' @export
add4_or <- function(exposure, outcome) {
  # Ensure logical vectors
  if (!is.logical(exposure) || !is.logical(outcome)) {
    stop("Both exposure and outcome must be logical vectors.")
  }
  if (length(exposure) != length(outcome)) {
    stop("Exposure and outcome vectors must be of equal length.")
  }

  # Create 2x2 contingency table
  a <- sum(exposure & outcome)      # Exposed & Outcome
  b <- sum(exposure & !outcome)     # Exposed & No Outcome
  c <- sum(!exposure & outcome)     # Unexposed & Outcome
  d <- sum(!exposure & !outcome)    # Unexposed & No Outcome

  # Apply Add-4 method: add 1 to each cell
  a <- a + 1
  b <- b + 1
  c <- c + 1
  d <- d + 1

  # Calculate odds ratio
  (a * d) / (b * c)
}


#' Question 2: R function to Adjust and Filter P-values for Multiple Comparisons
#'
#' Applies a multiple testing correction to a vector of p-values and filters out those that do not meet the alpha threshold.
#'
#' @param p_values A numeric vector of raw p-values.
#' @param method A string specifying the p-value adjustment method (e.g., "holm", "bonferroni", "BH"). Default is "holm".
#' @param alpha A numeric significance threshold to filter adjusted p-values. Default is 0.05.
#'
#' @return A numeric vector of adjusted p-values; values that do not meet the alpha threshold are returned as \code{NA}.
#'
#' @details
#' This function uses \code{p.adjust()} to correct for multiple comparisons and returns adjusted p-values.
#' Any value greater than \code{alpha} is replaced with \code{NA} to indicate it is not statistically significant after adjustment.
#'
#' @examples
#' raw_p <- c(0.01, 0.04, 0.20, 0.03)
#' adjust_and_filter_p(raw_p, method = "BH", alpha = 0.05)
#'
#' @export
adjust_and_filter_p <- function(p_values, method = "holm", alpha = 0.05) {
  if (!is.numeric(p_values)) {
    stop("p_values must be a numeric vector.")
  }
  adjusted <- p.adjust(p_values, method = method)
  adjusted[adjusted > alpha] <- NA
  return(adjusted)
}

#' Question 3: Coalesce First Non-Missing, Non-Empty String
#'
#' Return the First Non-Missing, Non-Empty String
#'
#' Returns the first element in a character vector that is neither \code{NA} nor an empty string (\code{""}).
#'
#' @param x A character vector.
#'
#' @return A single character string: the first non-missing, non-empty element of \code{x}, or \code{NA} if none found.
#'
#' @details
#' This function is useful when you want to select the first meaningful value from a set of fallback options,
#' such as metadata fields or alternate labels.
#'
#' @examples
#' coalesce_nonempty(c(NA, "", "apple", "banana"))  # returns "apple"
#' coalesce_nonempty(c(NA, "", NA))                # returns NA
#'
#' @export
coalesce_nonempty <- function(x) {
  if (!is.character(x)) {
    stop("Input must be a character vector.")
  }
  result <- x[!is.na(x) & nzchar(x)]
  if (length(result) == 0) NA_character_ else result[1]
}

#' Question 4: Fit Bernoulli Model via Maximum Likelihood
#'
#' Estimates the probability of success (p) from a Bernoulli distribution using maximum likelihood estimation (MLE).
#'
#' @param x A logical or binary numeric vector (values must be 0/1 or TRUE/FALSE).
#'
#' @return A list with one named element:
#' \describe{
#'   \item{p}{The estimated probability of success.}
#' }
#'
#' @details
#' This function computes the sample mean, which is the MLE for the probability of success under a Bernoulli model.
#'
#' @examples
#' fit_bernoulli(c(TRUE, FALSE, TRUE, TRUE))     # returns p = 0.75
#' fit_bernoulli(c(1, 0, 1, 1, 0))               # returns p = 0.6
#'
#' @export
fit_bernoulli <- function(x) {
  if (!is.logical(x) && !all(x %in% c(0, 1))) {
    stop("Input must be a logical vector or numeric vector with 0s and 1s.")
  }
  p_hat <- mean(as.numeric(x), na.rm = TRUE)
  list(p = p_hat)
}

#' Question 5: Fit Gaussian Model via Maximum Likelihood
#'
#' Computes the maximum likelihood estimates (MLE) of the mean and variance for a Gaussian (normal) distribution.
#'
#' @param x A numeric vector.
#'
#' @return A list with two named elements:
#' \describe{
#'   \item{mean}{The estimated mean of the distribution.}
#'   \item{variance}{The estimated variance of the distribution.}
#' }
#'
#' @details
#' The mean is estimated as the sample mean. The variance is estimated as the sample variance using MLE,
#' which divides by \code{n} rather than \code{n - 1}.
#'
#' @examples
#' fit_gaussian(c(2.3, 4.1, 3.9, 5.0))
#'
#' @export
fit_gaussian <- function(x) {
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector.")
  }
  mu_hat <- mean(x, na.rm = TRUE)
  var_hat <- mean((x - mu_hat)^2, na.rm = TRUE)  # MLE variance divides by n
  list(mean = mu_hat, variance = var_hat)
}

#' Question 6: Truncate Negative Values to Zero
#'
#' Sets all negative numeric values in a vector to zero.
#'
#' @param x A numeric value or numeric vector.
#'
#' @return A numeric vector with all values less than zero replaced by zero.
#'
#' @details
#' This function is useful for enforcing non-negativity constraints in models, predictions, or outputs
#' where negative values are not meaningful (e.g., counts, proportions, etc.).
#'
#' @examples
#' gt_zero(c(-2, 0, 3.5, -1))  # returns 0 0 3.5 0
#' gt_zero(-5)                # returns 0
#'
#' @export
gt_zero <- function(x) {
  if (!is.numeric(x)) {
    stop("Input must be numeric.")
  }
  pmax(x, 0)
}


#' Question 7: Convert Variable Names to Flat Case
#'
#' Converts variable names to flat lowercase by removing separators (such as spaces, underscores, and punctuation).
#'
#' @param var_names A character vector of variable names to clean.
#'
#' @return A character vector of cleaned names in flat lowercase with no separators.
#'
#' @details
#' This function is useful for standardizing variable names, especially for downstream uses like matching or automated parsing.
#' All non-alphanumeric characters are removed, and the resulting string is converted to lowercase.
#'
#' @examples
#' make_flat_case(c("Full Name", "Age_in_Years", "e-mail.Address"))
#' # Returns: "fullname", "ageinyears", "emailaddress"
#'
#' @export
make_flat_case <- function(var_names) {
  if (!is.character(var_names)) {
    stop("Input must be a character vector.")
  }
  var_names_clean <- gsub("[^[:alnum:]]+", "", tolower(var_names))
  return(var_names_clean)
}


#' Question 8: Print Column Names Line-by-Line
#'
#' Prints each column name of a data frame or tibble on a new line.
#'
#' @param df A data frame or tibble.
#'
#' @return Invisibly returns a character vector of column names.
#'
#' @details
#' This utility function is helpful for inspecting or documenting variable names during data cleaning or analysis.
#' Column names are printed to the console line-by-line. The function also returns the names invisibly for optional reuse.
#'
#' @examples
#' df <- data.frame(a = 1:3, b = 4:6)
#' print_colnames(df)
#'
#' @export
print_colnames <- function(df) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame or tibble.")
  }
  col_names <- colnames(df)
  for (name in col_names) {
    cat(name, "\n")
  }
  invisible(col_names)
}


#' Question 9: Compute Group-Wise Classification Accuracy
#'
#' Calculates classification accuracy separately for each group.
#'
#' @param truth A binary vector of ground truth labels (0/1 or FALSE/TRUE).
#' @param pred A binary vector of predicted labels (same length as \code{truth}).
#' @param group A vector indicating group membership (same length as \code{truth}).
#'
#' @return A named numeric vector where each element represents the classification accuracy for a group.
#'
#' @details
#' This function is useful for evaluating fairness or group-based performance metrics.
#' Accuracy is defined as the proportion of correctly predicted labels within each group.
#'
#' @examples
#' truth <- c(1, 0, 1, 0, 1, 1)
#' pred  <- c(1, 0, 0, 0, 1, 0)
#' group <- c("A", "A", "B", "B", "B", "B")
#' fair_accuracy(truth, pred, group)
#'
#' @export
fair_accuracy <- function(truth, pred, group) {
  if (length(truth) != length(pred) || length(truth) != length(group)) {
    stop("All input vectors must be of the same length.")
  }
  split_indices <- split(seq_along(truth), group)
  sapply(split_indices, function(idx) {
    mean(truth[idx] == pred[idx], na.rm = TRUE)
  })
}

#' Question 10: Compute Confusion Matrix Metrics
#'
#' Computes classification metrics including accuracy, sensitivity (recall), and specificity from binary truth and prediction vectors.
#'
#' @param truth A binary vector of ground truth labels (0/1 or FALSE/TRUE).
#' @param pred A binary vector of predicted labels (same length as \code{truth}).
#'
#' @return A list containing:
#' \describe{
#'   \item{accuracy}{Proportion of correct predictions.}
#'   \item{sensitivity}{True positive rate (recall).}
#'   \item{specificity}{True negative rate.}
#' }
#'
#' @details
#' Assumes binary classification with labels 1 = positive, 0 = negative.
#'
#' @examples
#' truth <- c(1, 0, 1, 1, 0, 0, 1)
#' pred  <- c(1, 0, 1, 0, 0, 1, 1)
#' confusion_metrics(truth, pred)
#'
#' @export
confusion_metrics <- function(truth, pred) {
  if (length(truth) != length(pred)) {
    stop("truth and pred must be the same length.")
  }

  truth <- as.logical(truth)
  pred  <- as.logical(pred)

  TP <- sum(truth & pred)
  TN <- sum(!truth & !pred)
  FP <- sum(!truth & pred)
  FN <- sum(truth & !pred)

  acc <- (TP + TN) / length(truth)
  sens <- if ((TP + FN) == 0) NA else TP / (TP + FN)
  spec <- if ((TN + FP) == 0) NA else TN / (TN + FP)

  list(
    accuracy = acc,
    sensitivity = sens,
    specificity = spec
  )
}

#' Question 11: Check if a Vector is Binary
#'
#' Determines whether a vector contains only two unique values.
#'
#' @param x A vector of any type.
#' @param strict Logical; if \code{TRUE}, only numeric 0 and 1 or logical TRUE/FALSE are allowed.
#'
#' @return A logical value: \code{TRUE} if the input is binary, otherwise \code{FALSE}.
#'
#' @details
#' In non-strict mode, the function checks whether there are exactly two unique, non-missing values.
#' In strict mode, it ensures that the values are strictly numeric (0, 1) or logical (TRUE, FALSE).
#'
#' @examples
#' is_binary(c(0, 1, 0, 1))             # TRUE
#' is_binary(c("yes", "no", "yes"))    # TRUE
#' is_binary(c(0, 1, 2))               # FALSE
#' is_binary(c(0, 1), strict = TRUE)   # TRUE
#' is_binary(c("yes", "no"), strict = TRUE)  # FALSE
#'
#' @export
is_binary <- function(x, strict = FALSE) {
  if (strict) {
    if (is.logical(x)) {
      return(all(x %in% c(TRUE, FALSE, NA)))
    } else if (is.numeric(x)) {
      return(all(x %in% c(0, 1, NA)))
    } else {
      return(FALSE)
    }
  } else {
    return(length(unique(na.omit(x))) == 2)
  }
}

#' Question 12: Return the Most Frequent Value (Majority Label)
#'
#' Returns the most frequently occurring value in a vector, optionally excluding \code{NA}.
#'
#' @param x A vector of any type (character, numeric, factor, etc.).
#' @param na.rm Logical; if \code{TRUE}, \code{NA} values are removed before calculation. Default is \code{TRUE}.
#'
#' @return The most frequent value in the vector. If all values are \code{NA} or input is empty, returns \code{NA}.
#'
#' @details
#' If there is a tie for the most frequent value, the function returns the first one encountered in the mode.
#'
#' @examples
#' majority_label(c("apple", "banana", "apple"))      # "apple"
#' majority_label(c(1, 1, 2, 2, 2, NA), na.rm = TRUE)  # 2
#' majority_label(c(NA, NA))                          # NA
#'
#' @export
majority_label <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  if (length(x) == 0) return(NA)
  freq <- table(x)
  names(freq)[which.max(freq)]
}





