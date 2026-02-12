test_that("mod_overview_ui returns a shiny tag", {
  ui <- mod_overview_ui("test")
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
})

test_that("mod_evidence_ui returns a shiny tag", {
  ui <- mod_evidence_ui("test")
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
})

test_that("mod_indicators_ui returns a shiny tag", {
  ui <- mod_indicators_ui("test")
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
})

test_that("mod_pillar_detail_ui returns a shiny tag", {
  ui <- mod_pillar_detail_ui("test")
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
})

test_that("mod_sensitivity_ui returns a shiny tag", {
  ui <- mod_sensitivity_ui("test")
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
})

test_that("r4sub_app returns a shiny.appobj", {
  skip_if_not_installed("r4subcore")
  skip_if_not_installed("r4subscore")
  app <- r4sub_app()
  expect_s3_class(app, "shiny.appobj")
})

test_that("r4sub_app accepts pre-loaded evidence", {
  skip_if_not_installed("r4subcore")
  skip_if_not_installed("r4subscore")
  ev <- generate_demo_evidence(n_rows = 10)
  app <- r4sub_app(evidence = ev)
  expect_s3_class(app, "shiny.appobj")
})
