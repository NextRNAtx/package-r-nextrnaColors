context('colors')
library(ggplot2)

test_that("scale_color_nextrna can be applied to a ggplot", {
  expect_error(ggplot(data=iris, aes(x = Sepal.Length, y = Sepal.Width)) +
                 geom_point(aes(color=Species, shape=Species)) +
                 scale_color_nextrna() , NA)

})

test_that("scale_fill_nextrna can be applied to a ggplot", {
  expect_error(ggplot(data=iris, aes(x=Species, y=Sepal.Length)) +
                 geom_boxplot(aes(fill=Species)) +
                 scale_color_nextrna() , NA)
})
