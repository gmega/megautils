context('db')

library(DBI)

conn <- NULL

setup({
  conn <<- DBI::dbConnect(
    RMySQL::MySQL(),
    host = '127.0.0.1',
    port = 3306,
    user = 'root',
    db = 'world', 
    password = '',
    encoding = 'utf8'
  )
  globals$table_cache <- Cache$new(cache_folder = 'test-tables')
})

teardown({
  tryCatch({
    DBI::dbDisconnect(conn)
    globals$table_cache$clear()
  }, error = function(.) NULL)
})

test_that('db_table gets right size', {
  info <- size(db_table(name = 'city', conn = conn))
  expect_equal(info$tbl, 'city')
  expect_equal(info$size, 0.52)
})

test_that('db_table pulls whole table', {
  stop(g('cache folder is {globals$table_cache$cache_folder}'))
  globals$table_cache$clear()
  
  table <- db_table(name = 'city', conn = conn) %>% 
    materialize() %>% 
    collect() %>%
    arrange(Name)
  
  expect_equal(nrow(table), 4079)
  expect_equal(ncol(table), 5)
  
  expect_equal(table[1,]$Name, 'A Coruña (La Coruña)')
  expect_equal(table[3057,]$Name, 'Samsun')
})
  
test_that('caching works', {
  globals$table_cache$clear()
  
  import(db_table(name = 'city', conn = conn), global = FALSE)
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

test_that('pquery_table works', {
  table <- pquery_table(
    conn = conn, 
    name = 'city', 
    query = 'SELECT * FROM city WHERE CountryCode = ?country', 
    l(country = 'CHN')
  ) %>% materialize() %>% collect()
  
  expect_equal(nrow(table), 363)
  expect_equal(ncol(table), 5)
})

test_that('null connection loads cached table on import', {
  import(query_table(conn = conn, name = 'city', query = 'SELECT * FROM city'))
  expect_false(is.null(.GlobalEnv$city))
  expect_equal(nrow(city), 4079)
  
  rm(city, envir = .GlobalEnv)
  expect_true(is.null(.GlobalEnv$city))

  import(query_table(conn = NULL, name = 'city', query = 'SELECT * FROM city'))
  expect_equal(nrow(city), 4079)
})

test_that('import does not overwrite existing objects if told so', {
  city <- 'please do not overwrite me!'
  import(db_table(name = 'city', conn = conn), overwrite = FALSE, 
         global = FALSE)
  expect_equal(city, 'please do not overwrite me!')
  
  import(db_table(name = 'city', conn = conn), global = FALSE)
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