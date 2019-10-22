context('db')

library(DBI)

setup({
  db_register(
    name = 'test-db',
    driver = RMySQL::MySQL(),
    user = 'root',
    host = '127.0.0.1',
    port = 3306,
    db = 'world', 
    password = '',
    encoding = 'utf8'
  )
  globals$table_cache <- Cache$new(cache_folder = 'test-tables')
})

teardown({
  tryCatch({
    db_close('test-db')
    globals$table_cache$clear()
  }, error = function(.) NULL)
})

test_that('db_table gets right size', {
  info <- size(db_table(name = 'city', conn = db_conn('test-db')))
  expect_equal(info$tbl, 'city')
  expect_equal(info$size, 0.52)
})

test_that('db_table pulls whole table', {
  globals$table_cache$clear()
  
  table <- db_table(name = 'city', conn = db_conn('test-db')) %>% 
    materialize() %>% 
    collect() 
  
  # Because handling of encoding in R manages to be more confusing
  # in Python 2. 
  Encoding(table$Name) <- 'UTF-8'

  table <- table %>% arrange(Name)
  
  expect_equal(table$Name[3], 'A Coruña (La Coruña)')
  expect_equal(table$Name[500], 'Borås')
  expect_equal(table$Name[4079], 'Zytomyr')
  
  expect_equal(nrow(table), 4079)
  expect_equal(ncol(table), 5)
})
  
test_that('caching works', {
  globals$table_cache$clear()
  
  import(db_table(name = 'city', conn = db_conn('test-db')), global = FALSE)
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
      ref_parameters = l(conn = db_conn('test-db')),
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
    conn = db_conn('test-db'), 
    name = 'city', 
    query = 'SELECT * FROM city WHERE CountryCode = ?country', 
    l(country = 'CHN')
  ) %>% materialize() %>% collect()
  
  expect_equal(nrow(table), 363)
  expect_equal(ncol(table), 5)
})

test_that('null connection loads cached table on import', {
  import(query_table(conn = db_conn('test-db'), 
                     name = 'city', query = 'SELECT * FROM city'))
  expect_false(is.null(.GlobalEnv$city))
  expect_equal(nrow(city), 4079)
  
  rm(city, envir = .GlobalEnv)
  expect_true(is.null(.GlobalEnv$city))

  import(query_table(conn = NULL, name = 'city', query = 'SELECT * FROM city'))
  expect_equal(nrow(city), 4079)
})

test_that('import does not overwrite existing objects if told so', {
  city <- 'please do not overwrite me!'
  import(db_table(name = 'city', conn = db_conn('test-db')), overwrite = FALSE, 
         global = FALSE)
  expect_equal(city, 'please do not overwrite me!')
  
  import(db_table(name = 'city', conn = db_conn('test-db')), global = FALSE)
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

test_that('import post-processing works', {
  import(
    db_table(
      name = 'city', 
      conn = db_conn('test-db')
    ),
    {
      Encoding(city$Name) <- 'UTF-8'
      city %>% mutate(Name = tolower(Name)) %>% arrange(Name)
    },
    global = FALSE
  )
  
  expect_equal(city$Name[3], 'a coruña (la coruña)')
  expect_equal(city$Name[500], 'borås')
  expect_equal(city$Name[4079], 'zytomyr')
})