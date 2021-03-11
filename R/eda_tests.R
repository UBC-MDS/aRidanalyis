test_that('Categorical plot should be denisty plots based on selected features ', {
  p <- arid_eda(penguins, 'species', 'categorical', c('body_mass_g'))
  expect_true('species' == p[[1]]$labels$fill)
  expect_true('density' == p[[1]]$labels$y)
  expect_true(p[[1]]$labels$x %in% colnames(penguins))
})

test_that('Numeric response should produce histogram of features', {
  p <- arid_eda(penguins, 'body_mass_g', 'numeric', c('flipper_length_mm'))
  expect_true('flipper_length_mm' == p[[1]]$labels$title)
  expect_true(is_null(p[[1]]$labels$x) == TRUE)
  expect_true('count' == p[[1]]$labels$y)
})

test_that('Number of histograms should be equivalent to features', {
  p <- arid_eda(penguins, 'body_mass_g', 'numeric', colnames(select(penguins, is.numeric)))
  expect_true(length(p) == (length(colnames(select(penguins, is.numeric))) + 1))
  expect_true('count' == p[[length(colnames(select(penguins, is.numeric)))]]$labels$y)
})

test_that('Number of histograms should be equivalent to features', {
  p <- arid_eda(penguins, 'body_mass_g', 'numeric')
    expect_true(p[[length(p)]]$labels$title == 'Correlation Matrix')
    expect_true(length(colnames(p[[length(p)]]$data)) == 4)
    expect_true(p[[length(p)]]$labels$fill == 'coefficient')
    expect_true(p[[length(p)]]$labels$label == 'label')
})

test_that('arguments must be in data frame and consistent with each other', {
    expect_error(arid_eda(penguins, 'sex', 'numeric'))
    expect_error(arid_eda(penguins, 'body_mass_g', 'categorical', c()))
    expect_error(arid_eda(penguins, 'body_mass_g', 'numeric', c('body_mass_g')))
    expect_error(arid_eda(penguins, 'fake-column'))
    expect_error(arid_eda(penguins, 'body_mass_g', 'numeric', c('not in df')))
    expect_error(arid_eda(penguins, 'body_mass_g', 'numeric', c('sex', 'not in df')))
})

