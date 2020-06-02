context('dplyr-extras')

test_that('with_colnames renames columns as expected', {
  a <- tibble(v1 = c(20, 21), v2 = c(8.5, 5)) %>%
    with_colnames('age', 'grade')
  
  expect_equal(colnames(a), c('age', 'grade'))
})