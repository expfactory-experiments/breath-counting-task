context('Breath counting')

test_that("process_expfactory_bc_file() can process a CSV file", {
  expect_silent(process_breath_counting('../fixtures/1/1/1_bc.csv', p=1, json=FALSE))
})

test_that("expfactory_breath_counting_to_csv() works", {
  expect_silent(expfactory_breath_counting_to_csv('../fixtures/1', 1, c(2,6,7)))
})

## JSON data
json_p = data.frame(token = c('00b96573-0f35-4f17-88e4-2836ca7deec1','03e32ecf-8939-4338-ae43-ba545c15601a',
                              '06285401-fc7c-41ce-be54-9b03bf26c69f'))
fixture_dir <- '../fixtures/2'
path <- paste0(fixture_dir,'/',json_p[1,1],'_finished/breath-counting-task-results.json')
test_that("process_breath_counting() can process a JSON file", {
  expect_silent(process_breath_counting(path, p=1))
})
bc <- process_breath_counting(path, p=1)
test_that("process_breath_counting() parses data correctly", {
  expect_equal(bc[[1,'rt']],14085)
})

bc <- expand.grid(token = json_p$token) %>%
  rowwise() %>%
  mutate(path=paste0(fixture_dir, '/', token, '_finished/breath-counting-task-results.json')) %>%
  do(., process_breath_counting(.$path, .$token))

df <- bc %>%
  filter(p == '00b96573-0f35-4f17-88e4-2836ca7deec1')
test_that("breath_counting_accuracy() returns a data frame", {
  expect_is(breath_counting_accuracy(df), "data.frame")
})
foo <- breath_counting_accuracy(df)
test_that("breath_counting_accuracy() is accurate", {
  expect_equal(foo[[1,'correct']],12)
})
