#' A wrapper function for conditional inference trees
#'
#' This function is a slightly different API for [party::ctree()] that has
#' several important arguments as top-level arguments (as opposed to being
#' specified in [party::ctree_control()]).
#' @param formula a symbolic description of the model to be fit. Note that
#' symbols like \code{:} and \code{-} will not work and the tree will make use
#' of all variables listed on the right-hand side of \code{formula}.
#' @param data a data frame containing the variables in the model.
#' @param teststat a character specifying the type of the test statistic to be
#' applied.
#' @param testtype a character specifying how to compute the distribution of
#' the test statistic.
#' @param mincriterion the value of the test statistic (for \code{testtype ==
#' "Teststatistic"}), or 1 - p-value (for other values of \code{testtype}) that
#' must be exceeded in order to implement a split.
#' @param minsplit the minimum sum of weights in a node in order to be
#' considered for splitting.
#' @param maxdepth maximum depth of the tree. The default \code{maxdepth = 0}
#' means that no restrictions are applied to tree sizes.
#' @param ... Other options to pass to [party::ctree()].
#' @return An object of class `BinaryTree-class`.
#' @export
cond_inference_tree <-
  function(formula,
           data,
           minsplit = 20,
           maxdepth = 0,
           teststat = "quad",
           testtype = "Bonferroni",
           mincriterion = 0.95,
           ...) {
    opts <- rlang::list2(...)

    # Edit the control options to put some argument values into the controls
    # function. This is a challenge since the control objects are S4 objects
    # of S4 objects. If the top-level tuning parameter disagrees with an
    # existing value, we over-write it an issue a warning.
    if (any(names(opts) == "controls")) {
      opts$controls <- resub_party_arg(opts$controls, "splitctrl", "minsplit", minsplit)
      opts$controls <- resub_party_arg(opts$controls, "tgctrl",    "maxdepth", maxdepth)
      opts$controls <- resub_party_arg(opts$controls, "varctrl",   "teststat", teststat)
      opts$controls <- resub_party_arg(opts$controls, "gtctrl",    "testtype", testtype)
      opts$controls <- resub_party_arg(opts$controls, "gtctrl",    "mincriterion", mincriterion)

    } else {
      opts$controls <-
        rlang::call2(
          "ctree_control",
          .ns = "party",
          !!!list(
            minsplit = minsplit,
            maxdepth = maxdepth,
            teststat = teststat,
            testtype = testtype,
            mincriterion = mincriterion
          )
        )
    }

    tree_call <-
      rlang::call2(
        "ctree",
        .ns = "party",
        formula = rlang::expr(formula),
        data = rlang::expr(data),!!!opts
      )
    rlang::eval_tidy(tree_call)
  }

#' A wrapper function for conditional inference forests
#'
#' This function is a slightly different API for [party::cforest()] that has
#' several important arguments as top-level arguments (as opposed to being
#' specified in [party::cforest_control()]).
#' @inheritParams cond_inference_tree
#' @param ... Other options to pass to [party::cforest()].
#' @param mtry number of input variables randomly sampled as candidates at each
#' node for random forest like algorithms. The default \code{mtry = 0} means
#' that no random selection takes place.
#' @param replace a logical indicating whether sampling of observations is done
#' with or without replacement.
#' @param fraction fraction of number of observations to draw without
#' replacement (only relevant if \code{replace = FALSE}).
#' @param ntree number of trees to grow in a forest.
#' @details
#' Note that, although [party::cforest_unbiased()] is not directly used, the
#' defaults for `cond_inference_forest()` mirror its default values. However,
#' [party::cforest_unbiased()] does not allow several tuning parameters to be
#' optimized (`teststat`, `testtype`, `mincriterion`, `replace`, and `fraction`).
#' If you set pass a [party::cforest_unbiased()] object to
#' `cond_inference_forest()` and modify those arguments, their values will be
#' overwritten.
#' @return An object of class `RandomForest-class`.
#' @export
cond_inference_forest <-
  function(formula,
           data,
           minsplit = 20L,
           maxdepth = 0L,
           teststat = "quad",
           testtype = "Univariate",
           mincriterion = 0,
           replace = FALSE,
           fraction = 0.632,
           mtry = 5L,
           ntree = 500L,
           ...) {
    opts <- rlang::list2(...)

    mtry <- min(ncol(data) - 1, mtry)
    minsplit <- min(nrow(data), minsplit)

    # Edit the control options to put some argument values into the controls
    # function. This is a challenge since the control objects are S4 objects
    # of S4 objects. If the top-level tuning parameter disagrees with an
    # existing value, we over-write it an issue a warning.
    if (any(names(opts) == "controls")) {
      opts$controls <-
        resub_party_arg(opts$controls, "splitctrl", "minsplit", minsplit, warn = FALSE)
      opts$controls <-
        resub_party_arg(opts$controls, "tgctrl",    "maxdepth", maxdepth, warn = FALSE)
      opts$controls <-
        resub_party_arg(opts$controls, "varctrl",   "teststat", teststat, warn = FALSE)
      opts$controls <-
        resub_party_arg(opts$controls, "gtctrl",    "testtype", testtype, warn = FALSE)
      opts$controls <-
        resub_party_arg(opts$controls, "gtctrl",    "mincriterion", mincriterion, warn = FALSE)
      opts$controls <-
        resub_party_arg(opts$controls, "gtctrl",    "mtry", mtry, warn = FALSE)

      # Not in seperate S4 objects
      if (!isTRUE(all.equal(methods::slot(opts$controls, "replace"), replace))) {
        methods::slot(opts$controls, "replace") <- replace
      }
      if (!isTRUE(all.equal(methods::slot(opts$controls, "fraction"), fraction))) {
        methods::slot(opts$controls, "fraction") <- fraction
      }
      if (!isTRUE(all.equal(methods::slot(opts$controls, "ntree"), as.integer(ntree)))) {
        methods::slot(opts$controls, "ntree") <- as.integer(ntree)
      }

    } else {
      opts$controls <-
        rlang::call2(
          "cforest_control",
          .ns = "party",
          !!!list(
            minsplit = minsplit,
            maxdepth = maxdepth,
            teststat = teststat,
            testtype = testtype,
            mincriterion = mincriterion,
            mtry = mtry,
            ntree = ntree,
            replace = replace,
            fraction = fraction
          )
        )
    }

    forest_call <-
      rlang::call2(
        "cforest",
        .ns = "party",
        formula = rlang::expr(formula),
        data = rlang::expr(data),!!!opts
      )
    rlang::eval_tidy(forest_call)
  }


## -----------------------------------------------------------------------------
## helpers

resub_party_arg <- function(x, slot, arg, value, warn = TRUE) {
  y <- methods::slot(x, slot)
  z <- methods::slot(y, arg)

  ## -----------------------------------------------------------------------------

  if (is.integer(z)) {
    value <- as.integer(value)
  }

  if (is.factor(z)) {
    value <- factor(value, levels = levels(z))
  }

  ## -----------------------------------------------------------------------------

  if (!isTRUE(all.equal(z, value))) {
    msg <- paste0("For the user's ", class(x)[1], " object, the option '",
                  arg, "' was changed from ", z, " to ", value, ".")
    if (warn) {
      rlang::warn(msg)
    }
    methods::slot(y, arg) <- value
    methods::slot(x, slot) <- y
  }
  x
}

## -----------------------------------------------------------------------------

add_decision_tree_party <- function() {

  parsnip::set_model_engine("decision_tree", mode = "regression", eng = "party")
  parsnip::set_model_engine("decision_tree", mode = "classification", eng = "party")
  parsnip::set_dependency("decision_tree", eng = "party", pkg = "party")
  parsnip::set_dependency("decision_tree", eng = "party", pkg = "treesnip")

  parsnip::set_fit(
    model = "decision_tree",
    eng = "party",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "treesnip", fun = "cond_inference_tree"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "decision_tree",
    mode = "regression",
    eng = "party",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_encoding(
    model = "decision_tree",
    mode = "classification",
    eng = "party",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "decision_tree",
    eng = "party",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = function(results, object) results[,1],
      func = c(fun = "predict"),
      args = list(object = quote(object$fit), newdata = quote(new_data), type = "response")
    )
  )

  parsnip::set_pred(
    model = "decision_tree",
    eng = "party",
    mode = "regression",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(object = quote(object$fit), newdata = quote(new_data))
    )
  )

  parsnip::set_fit(
    model = "decision_tree",
    eng = "party",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "treesnip", fun = "cond_inference_tree"),
      defaults = list()
    )
  )

  parsnip::set_pred(
    model = "decision_tree",
    eng = "party",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = NULL, fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "response"
      )
    )
  )

  parsnip::set_pred(
    model = "decision_tree",
    eng = "party",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = function(x, object) {
        x <- matrix(unlist(x), ncol = length(object$lvl), byrow = TRUE)
        colnames(x) <- object$lvl
        tibble::as_tibble(x, .name_repair = "minimal")
      },
      func = c(pkg = NULL, fun = "predict"),
      args = list(object = quote(object$fit), newdata = quote(new_data), type = "prob")
    )
  )

  parsnip::set_pred(
    model = "decision_tree",
    eng = "party",
    mode = "classification",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(object = quote(object$fit), newdata = quote(new_data))
    )
  )

  # model args ----------------------------------------------------

  parsnip::set_model_arg(
    model = "decision_tree",
    eng = "party",
    parsnip = "tree_depth",
    original = "maxdepth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "decision_tree",
    eng = "party",
    parsnip = "min_n",
    original = "minsplit",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = TRUE
  )
  parsnip::set_model_arg(
    model = "decision_tree",
    eng = "party",
    parsnip = "mtry",
    original = "mtry",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )

}


## -----------------------------------------------------------------------------

add_rand_forest_party <- function() {

  parsnip::set_model_engine("rand_forest", mode = "regression", eng = "party")
  parsnip::set_model_engine("rand_forest", mode = "classification", eng = "party")
  parsnip::set_dependency("rand_forest", eng = "party", pkg = "party")
  parsnip::set_dependency("rand_forest", eng = "party", pkg = "treesnip")

  parsnip::set_fit(
    model = "rand_forest",
    eng = "party",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "treesnip", fun = "cond_inference_forest"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "rand_forest",
    mode = "regression",
    eng = "party",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_encoding(
    model = "rand_forest",
    mode = "classification",
    eng = "party",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "rand_forest",
    eng = "party",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = function(results, object) results[,1],
      func = c(fun = "predict"),
      args = list(object = quote(object$fit), newdata = quote(new_data), type = "response")
    )
  )

  parsnip::set_pred(
    model = "rand_forest",
    eng = "party",
    mode = "regression",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(object = quote(object$fit), newdata = quote(new_data))
    )
  )

  parsnip::set_fit(
    model = "rand_forest",
    eng = "party",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "treesnip", fun = "cond_inference_forest"),
      defaults = list()
    )
  )

  parsnip::set_pred(
    model = "rand_forest",
    eng = "party",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = NULL, fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "response"
      )
    )
  )

  parsnip::set_pred(
    model = "rand_forest",
    eng = "party",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = function(x, object) {
        x <- matrix(unlist(x), ncol = length(object$lvl), byrow = TRUE)
        colnames(x) <- object$lvl
        tibble::as_tibble(x, .name_repair = "minimal")
      },
      func = c(pkg = NULL, fun = "predict"),
      args = list(object = quote(object$fit), newdata = quote(new_data), type = "prob")
    )
  )

  parsnip::set_pred(
    model = "rand_forest",
    eng = "party",
    mode = "classification",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(object = quote(object$fit), newdata = quote(new_data))
    )
  )

  # model args ----------------------------------------------------

  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "party",
    parsnip = "trees",
    original = "ntree",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "party",
    parsnip = "min_n",
    original = "minsplit",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = TRUE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "party",
    parsnip = "mtry",
    original = "mtry",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )

}



