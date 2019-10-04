context('gcs dataset utils')
library(megautils)

setup({
  # We'll eventually need a service key to run these tests on travis.
  skip_on_travis()
  gcs_auth(email = Sys.getenv('GCS_USER'))
  globals$table_cache <- Cache$new(cache_folder = 'test-tables')
})

teardown({
  skip_on_travis()
  globals$table_cache$clear()
})

check_table <- function(cities) {
  expect_equal(nrow(cities), 4079)
  expect_equal(ncol(cities), 5)
  
  # Tests two random points to see we haven't read garbage.
  expect_equal(cities[1,]$Name, 'Kabul')
  expect_equal(cities[2585,]$Name, 'Campeche')
}

test_that('gcs_data pulls files from Cloud Storage', {
  skip_on_travis()
  check_table(
    gcs_data(
      bucket = Sys.getenv('TEST_GCS_BUCKET'),
      path = 'playax-data-science/playax-R-utils/testdata/cities.csv',
      loader = read_csv
    )
  )
})

test_that('import works with GCS', {
  skip_on_travis()
  globals$table_cache$clear()
  expect_false(globals$table_cache$exists('cities.rds'))
  
  import(
    gcs_table(
      bucket = Sys.getenv('TEST_GCS_BUCKET'),
      path = 'playax-data-science/playax-R-utils/testdata/cities.csv',
      loader = read_csv
    ),
    global = FALSE
  )
  
  check_table(cities)
  
  # Table must have been cached.
  expect_true(globals$table_cache$exists('cities.rds'))
})