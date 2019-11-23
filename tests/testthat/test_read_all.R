context('read_all')

test_that('it reads a set of files from the filesystem', {
  path <- system.file('extdata', 'read_all', package = 'megautils')
  
  schema <- list(a = readr::col_integer(), b = readr::col_integer())
  
  f1 <- readr::read_csv(file.path(path, 'f1.csv'), col_types = schema) 
  f2 <- readr::read_csv(file.path(path, 'f2.csv'), col_types = schema)
  
  tbl <- read_all() %>%
    using(col_types = schema) %>%
    from_fs(path) %>%
    into_tibble()
  
  expect_equal(tbl, rbind(
    f1 %>% mutate(origin = 'f1'), 
    f2 %>% mutate(origin = 'f2')
  ))
  
  env <- read_all() %>%
    using(col_types = schema) %>%
    from_fs(path) %>%
    into_env()
  
  expect_equal(env$f1, f1)
  expect_equal(env$f2, f2)
})
