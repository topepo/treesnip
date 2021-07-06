.onLoad <- function(libname, pkgname){
  add_decision_tree_tree()
  add_boost_tree_catboost()
  add_boost_tree_lightgbm()
  add_decision_tree_party()
  add_rand_forest_party()
}




