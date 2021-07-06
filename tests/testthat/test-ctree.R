context("ctree engine for decision trees")
suppressPackageStartupMessages(library(dplyr))

## -----------------------------------------------------------------------------

data("cells", package = "modeldata")
data("biomass", package = "modeldata")

biomass <- biomass %>% dplyr::select(-sample, -dataset)
cells <- cells %>% dplyr::select(-case)

## -----------------------------------------------------------------------------

num_nodes <- function(x) {
  length(unique(party::where(x)))
}

same_model <- function (x, y) {
  isTRUE(all.equal(x$fit@tree, y$fit@tree))
}

## -----------------------------------------------------------------------------

test_that("classification", {
  set.seed(1)
  direct_mod <- party::ctree(class ~ ., data = cells)
  direct_prob <- predict(direct_mod, cells, type = "prob")
  direct_prob <- matrix(unlist(direct_prob), ncol = 2, byrow = TRUE)
  direct_class <- predict(direct_mod, cells, type = "response")

  expect_error(
    baseline_fit <-
      decision_tree() %>%
      set_engine("party") %>%
      set_mode("classification") %>%
      fit(class ~ ., data = cells),
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
      decision_tree(min_n = 150) %>%
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

  expect_true(!same_model(baseline_fit, min_n_fit))

  ### test main argument tree_depth

  expect_error(
    depth_fit <-
      decision_tree(tree_depth = 3) %>%
      set_engine("party") %>%
      set_mode("classification") %>%
      fit(class ~ ., data = cells),
    regexp = NA
  )

  expect_error(
    depth_class <- predict(depth_fit, cells),
    regexp = NA
  )

  expect_error(
    depth_prob <- predict(depth_fit, cells, type = "prob"),
    regexp = NA
  )

  expect_true(!same_model(baseline_fit, depth_fit))

  ### test control_usage

  expect_error(
    stump_fit <-
      decision_tree() %>%
      set_engine("ctree", controls = party::ctree_control(stump = TRUE)) %>%
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

  expect_true(num_nodes(stump_fit$fit) == 2)

  ### tree_depth
  expect_error(
    stump_fit <-
      decision_tree() %>%
      set_engine("ctree", controls = party::ctree_control(stump = TRUE)) %>%
      set_mode("classification") %>%
      fit(class ~ ., data = cells),
    regexp = NA
  )
  expect_true(!same_model(baseline_fit, stump_fit))

  ### teststat engine arg
  expect_error(
    max_stat <-
      decision_tree() %>%
      set_engine("ctree", teststat = "max") %>%
      set_mode("classification") %>%
      fit(class ~ ., data = cells),
    regexp = NA
  )
  expect_true(!same_model(baseline_fit, max_stat))

  ### testtype engine arg
  expect_error(
    uni_testtype <-
      decision_tree() %>%
      set_engine("ctree", testtype = "Univariate") %>%
      set_mode("classification") %>%
      fit(class ~ ., data = cells),
    regexp = NA
  )
  expect_true(!same_model(baseline_fit, uni_testtype))

  ### mincriterion engine arg
  expect_error(
    smol_crit <-
      decision_tree() %>%
      set_engine("ctree", mincriterion = .5) %>%
      set_mode("classification") %>%
      fit(class ~ ., data = cells),
    regexp = NA
  )
  expect_true(!same_model(baseline_fit, smol_crit))
})


## -----------------------------------------------------------------------------

test_that("regression", {
  set.seed(1)
  direct_mod <- party::ctree(HHV ~ ., data = biomass)
  direct_num <- predict(direct_mod, biomass, type = "response")

  expect_error(
    baseline_fit <-
      decision_tree() %>%
      set_engine("party") %>%
      set_mode("regression") %>%
      fit(HHV ~ ., data = biomass),
    regexp = NA
  )

  expect_error(
    baseline_num <- predict(baseline_fit, biomass),
    regexp = NA
  )

  expect_equal(direct_num[,1], baseline_num[[1]])
})
