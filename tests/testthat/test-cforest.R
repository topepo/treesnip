context("cforest engine for decision trees")

## -----------------------------------------------------------------------------

data("cells", package = "modeldata")
data("biomass", package = "modeldata")

biomass <- biomass %>% dplyr::select(-sample, -dataset)
cells <- cells %>% dplyr::select(-case)

## -----------------------------------------------------------------------------

same_ensemble <- function(x, y) {
  isTRUE(all.equal(x@ensemble, y$fit@ensemble))
}

## -----------------------------------------------------------------------------

test_that("classification", {
  set.seed(1)
  direct_mod <- party::cforest(class ~ ., data = cells,
                               controls = party::cforest_unbiased(ntree = 3))
  direct_prob <- predict(direct_mod, newdata = cells, type = "prob")
  direct_prob <- matrix(unlist(direct_prob), ncol = 2, byrow = TRUE)
  direct_class <- predict(direct_mod, newdata = cells, type = "response")


  expect_error({
    set.seed(1)
    baseline_fit <-
      rand_forest(trees = 3) %>%
      set_engine("party") %>%
      set_mode("classification") %>%
      fit(class ~ ., data = cells)
  },
  regexp = NA
  )

  expect_error(
    baseline_class <- predict(baseline_fit, cells),
    regexp = NA
  )

  expect_error(
    baseline_prob <- predict(baseline_fit, cells, type = "prob"),
    regexp = NA
  )

  expect_equal(direct_prob[,1], baseline_prob[[1]])
  expect_equal(direct_class, baseline_class[[1]])

  ### test main argument min_n

  expect_error(
    min_n_fit <-
      rand_forest(min_n = 150, trees = 3) %>%
      set_engine("party") %>%
      set_mode("classification") %>%
      fit(class ~ ., data = cells),
    regexp = NA
  )

  expect_error(
    min_n_class <- predict(min_n_fit, cells),
    regexp = NA
  )

  expect_error(
    min_n_prob <- predict(min_n_fit, cells, type = "prob"),
    regexp = NA
  )

  expect_false(same_ensemble(direct_mod, min_n_fit))

  ### test main argument mtry


  expect_error(
    mtry_fit <-
      rand_forest(mtry = 3, trees = 3) %>%
      set_engine("party") %>%
      set_mode("classification") %>%
      fit(class ~ ., data = cells),
    regexp = NA
  )

  expect_error(
    mtry_class <- predict(mtry_fit, cells),
    regexp = NA
  )

  expect_error(
    mtry_prob <- predict(mtry_fit, cells, type = "prob"),
    regexp = NA
  )

  expect_false(same_ensemble(direct_mod, mtry_fit))

  ### test control_usage

  expect_error(
    stump_fit <-
      rand_forest(trees = 3) %>%
      set_engine("cforest", controls = party::cforest_control(stump = TRUE)) %>%
      set_mode("classification") %>%
      fit(class ~ ., data = cells),
    regexp = NA
  )

  expect_error(
    stump_class <- predict(stump_fit, cells),
    regexp = NA
  )

  expect_error(
    stump_prob <- predict(stump_fit, cells, type = "prob"),
    regexp = NA
  )

  expect_false(same_ensemble(direct_mod, stump_fit))
})


## -----------------------------------------------------------------------------

test_that("regression", {
  set.seed(1)
  direct_mod <- party::cforest(HHV ~ ., data = biomass,
                               controls = party::cforest_unbiased(ntree = 3))
  direct_num <- predict(direct_mod, newdata = biomass, type = "response")

  expect_error({
    set.seed(1)
    baseline_fit <-
      rand_forest(trees = 3) %>%
      set_engine("party") %>%
      set_mode("regression") %>%
      fit(HHV ~ ., data = biomass)
    },
    regexp = NA
  )

  expect_error(
    baseline_num <- predict(baseline_fit, biomass),
    regexp = NA
  )

  expect_equal(direct_num[,1], baseline_num[[1]])

})
