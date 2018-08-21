library(tidyverse)
context('Breath counting')

test_that("process_expfactory_bc_file() can process a JSON file", {
  expect_silent(efbreathr::process_breath_counting('../fixtures/breath-counting-task-results.json', p=1))
})

test_that("process_expfactory_bc_file() can process a CSV file", {
  expect_silent(efbreathr::process_breath_counting('../fixtures/1/1/1_bc.csv', p=1))
})

test_that("expfactory_breath_counting_to_csv() works", {
  expect_silent(efbreathr::expfactory_breath_counting_to_csv('../fixtures/1', 1, c(2,6,7)))
})

# fixture setup
bc_df       <- read_csv('../fixtures/1/bc.csv')
bc_accuracy <- bc_df %>%
  select(p) %>%
  unique() %>%
  dplyr::rename(participant = p) %>%
  mutate(total=0,correct=0,incorrect=0)

test_that("fixture is a data frame", {
  expect_is(bc_df, "data.frame")
})

test_that("breath_counting_accuracy() returns a data frame", {
  expect_is(efbreathr::breath_counting_accuracy(p = bc_df[, 'p'], bc_df = bc_df), "data.frame")
})

test_that("breath_counting_accuracy() returns a data frame via do()", {
  bc_accuracy <- expand.grid(p=bc_accuracy$participant) %>%
    do(., efbreathr::breath_counting_accuracy(.$p, bc_df)) %>%
    rowwise %>%
    arrange(p)
  expect_is(bc_accuracy, "data.frame")
})

test_that("summarise_breath_counting_accuracy() returns a data frame", {
  expect_is(efbreathr::summarise_breath_counting_accuracy(bc_df, data.frame(p = c(1,3,5,8:10))), 'data.frame')
})
