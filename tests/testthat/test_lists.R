context('lists')

test_that('lists shortcut actually returns a list', {
  a_list <- l(a = 1, b = 2, c = 3)
  expect_equal(a_list, list(a = 1, b = 2, c = 3))
})

test_that('lists shortcut evaluates expressions sequentially', {
  a_list <- l(a = 1, b = a + 1, c = b + 1)
  expect_equal(a_list, list(a = 1, b = 2, c = 3))
})
