context('db tables')
library(megautils)

conn <- DBI::dbConnect(
  RMySQL::MySQL(),
  host = '127.0.0.1',
  port = 3307,
  user = 'root',
  db = 'world', 
  password = 'pass',
  encoding = 'latin1'
)

test_that('db_table gets right size', {
  info <- size(db_table(name = 'city', conn = conn))
  expect_equal(info$tbl, 'city')
  expect_equal(info$size, 0.52)
})

test_that('db_table pulls whole table', {
  table <- db_table(name = 'city', conn = conn) %>% 
    materialize() %>% 
    collect() %>%
    arrange(Name)
  
  expect_equal(nrow(table), 4079)
  expect_equal(ncol(table), 5)
  
  # Takes two random rows without encoding issues to check.
  expect_equal(table[3,]$Name, 'Aachen')
  expect_equal(table[3050,]$Name, 'Sahiwal')
})

test_that('caching works', {
  import(db_table(name = 'city', conn = conn))
  expect_equal(nrow(city), 4079)
  expect_equal(ncol(city), 5)
  
  rm(city)
  city <- cached_table(name = 'city') %>% materialize()
  expect_equal(nrow(city), 4079)
  expect_equal(ncol(city), 5)
})

test_that('table_references works', {
  import_all(
    table_references(
      ref_type = db_table,
      ref_parameters = l(conn = conn),
      'city',
      'country',
      'countrylanguage'
    )
  )
  
  expect_equal(nrow(city), 4079)
  expect_equal(nrow(country), 239)
  expect_equal(nrow(countrylanguage), 984)
})

test_that('null connection loads cached table', {
  import(query_table(conn = conn, name = 'city', query = 'SELECT * FROM city'))
  expect_false(is.null(.GlobalEnv$city))
  expect_equal(nrow(city), 4079)
  
  rm(city, envir = .GlobalEnv)
  expect_true(is.null(.GlobalEnv$city))

  import(query_table(conn = NULL, name = 'city', query = 'SELECT * FROM city'))
  expect_equal(nrow(city), 4079)
})

test_that('parametric table does not barf with null connection', {
  table_ref <- pquery_table(
    NULL,
    name = 'city',
    'SELECT * FROM ?table',
    query_parameters = l(table = 'city')
  )
  
  # But it will barf if you try to materialize it.
  result <- tryCatch(
    materialize(table_ref),
    error = function(err) 'oops, I barfed!'
  )
  
  expect_equal(result, 'oops, I barfed!')
})