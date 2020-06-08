context('math')

test_that('roll_stat computes rolling stats', {
  x <- c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE)
  y <- c(2, 1, 0, 1, 1, 2)
  
  expect_equal(roll_stat(x, 3, sum), y)
})

test_that('roll_stat fills vector when requested', {
  x <- c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE)
  y <- c(2, 1, 0, 1, 1, 2, NA, NA)
  
  expect_equal(roll_stat(x, 3, sum, fill.length = NA), y)
})

test_that('log1pneg computes scale for negative values and NA', {
  part <- c(20, 10, 5, 1, 0)
  x <- c(part, NA, -rev(part))
  y <- c(log1p(part), NA, -log1p(rev(part)))
  
  expect_equal(log1pneg(x), y)
})
