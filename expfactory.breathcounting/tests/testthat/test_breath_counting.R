context('Breath counting')

test_that("bc_process_expfactory_file() can process a CSV file", {
  expect_silent(bc_process_expfactory_file('../fixtures/1/1/1_bc.csv', p=1, json=FALSE))
})

test_that("bc_process_expfactory() works", {
  expect_silent(bc_process_expfactory('../fixtures/1', 1))
})

## JSON data
json_p = data.frame(token = c('00b96573-0f35-4f17-88e4-2836ca7deec1',
                              '03e32ecf-8939-4338-ae43-ba545c15601a',
                              '06285401-fc7c-41ce-be54-9b03bf26c69f'))

fixture_dir <- '../fixtures/2'
path <- paste0(fixture_dir,'/',json_p[1,1],'_finished/breath-counting-task-results.json')
test_that("bc_process_expfactory_file() can process a JSON file", {
  expect_silent(bc_process_expfactory_file(path, p=1))
})
bc <- bc_process_expfactory_file(path, p=1)
test_that("bc_process_expfactory_file() parses data correctly", {
  expect_equal(bc[[1,'rt']],14085)
})

bc <- expand.grid(token = json_p$token) %>%
  group_by(token) %>%
  mutate(path=paste0(fixture_dir, '/', token, '_finished/breath-counting-task-results.json')) %>%
  do(., bc_process_expfactory_file(.$path, .$token)) %>%
  ungroup()

df <- bc %>%
  filter(p == '00b96573-0f35-4f17-88e4-2836ca7deec1')
test_that("bc_accuracy() returns a data frame", {
  expect_is(bc_accuracy(df), "data.frame")
})
foo <- bc_accuracy(df)
test_that("bc_accuracy() is accurate", {
  expect_equal(foo[[1,'correct']],12)
})

test_that('bc_process_eprime_file() returns a data frame', {
  expect_is(bc_process_eprime_file('../fixtures/3/bc15_converted-2-1.txt'), 'data.frame')
})


