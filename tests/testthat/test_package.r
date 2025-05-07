# Question 1
test_that("add4_or returns correct odds ratio", {
  exposure <- c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
  outcome  <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)

  # Manually computed Add-4 OR:
  # a = 2 + 1 = 3
  # b = 1 + 1 = 2
  # c = 1 + 1 = 2
  # d = 2 + 1 = 3
  # OR = (3 * 3) / (2 * 2) = 9 / 4 = 2.25

  expect_equal(add4_or(exposure, outcome), 2.25)
})

# Question 2
test_that("adjust_and_filter_p returns correct adjusted values", {
  raw_p <- c(0.01, 0.04, 0.20, 0.03)
  result <- adjust_and_filter_p(raw_p, method = "BH", alpha = 0.05)

  expected <- p.adjust(raw_p, method = "BH")
  expected[expected > 0.05] <- NA

  expect_equal(result, expected)
})

# Question 3
test_that("coalesce_nonempty returns the first non-empty, non-NA string", {
  x <- c(NA, "", "apple", "banana")
  expect_equal(coalesce_nonempty(x), "apple")
})

# Question 4
test_that("fit_bernoulli returns correct probability for logical input", {
  x <- c(TRUE, FALSE, TRUE, TRUE)
  result <- fit_bernoulli(x)
  expect_equal(result$p, 0.75)
})

# Question 5
test_that("fit_gaussian returns correct mean and variance", {
  x <- c(2, 4, 6, 8)
  result <- fit_gaussian(x)

  expected_mean <- mean(x)
  expected_variance <- mean((x - expected_mean)^2)

  expect_equal(result$mean, expected_mean)
  expect_equal(result$variance, expected_variance)
})

# Question 6
test_that("gt_zero correctly truncates negative values", {
  expect_equal(gt_zero(c(-2, 0, 3.5, -1)), c(0, 0, 3.5, 0))
  expect_equal(gt_zero(-5), 0)
})

# Question 7
test_that("make_flat_case removes non-alphanumerics and converts to lowercase", {
  input <- c("Full Name", "Age_in_Years", "e-mail.Address")
  expected <- c("fullname", "ageinyears", "emailaddress")
  expect_equal(make_flat_case(input), expected)
})

# Question 8
test_that("print_colnames prints names and returns them invisibly", {

  df <- data.frame(a = 1:2, b = 3:4)

  # Check return value
  result <- print_colnames(df)
  expect_equal(result, c("a", "b"))
})

# Question 9
test_that("fair_accuracy computes correct group-wise accuracy", {
  truth <- c(1, 0, 1, 0, 1, 1)
  pred  <- c(1, 0, 0, 0, 1, 0)
  group <- c("A", "A", "B", "B", "B", "B")

  result <- fair_accuracy(truth, pred, group)

  expect_named(result, c("A", "B"))
  expect_equal(result[["A"]], 1)
  expect_equal(result[["B"]], 0.5)
})

# Question 10
test_that("confusion_metrics returns correct accuracy, sensitivity, and specificity", {
  truth <- c(1, 0, 1, 1, 0, 0, 1)
  pred  <- c(1, 0, 1, 0, 0, 1, 1)

  result <- confusion_metrics(truth, pred)

  # TP = 3 (1&1), TN = 2 (0&0), FP = 1, FN = 1
  # accuracy = (3 + 2)/7 = 5/7
  # sensitivity = 3 / (3+1) = 0.75
  # specificity = 2 / (2+1) = 0.666...

  expect_equal(result$accuracy, 5 / 7)
  expect_equal(result$sensitivity, 0.75)
  expect_equal(result$specificity, 2 / 3)
})

# Question 11
test_that("is_binary correctly identifies binary vectors", {
  expect_true(is_binary(c(0, 1, 1, 0)))
  expect_true(is_binary(c("yes", "no", "yes")))
  expect_false(is_binary(c(0, 1, 2)))

  expect_true(is_binary(c(0, 1), strict = TRUE))
  expect_false(is_binary(c("yes", "no"), strict = TRUE))
})

# Question 12
test_that("majority_label returns the most frequent value", {
  expect_equal(majority_label(c("apple", "banana", "apple")), "apple")
  expect_equal(majority_label(c(1, 1, 2, 2, 2, NA), na.rm = TRUE), "2")
  expect_true(is.na(majority_label(c(NA, NA))))
})


