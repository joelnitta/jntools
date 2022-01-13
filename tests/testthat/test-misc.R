test_that("Conversion of numers to English works", {
  # Should work on default limit of 10
  expect_equal(english2(9), "nine")
  expect_equal(english2(10), "10")
  expect_equal(english2(11), "11")
})
