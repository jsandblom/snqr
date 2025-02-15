test_that("skanesjh() extracts", {
  expect_equal(skanesjh(c("Lunds", "Kristianstads centralsjukhus")), c("Lund", "Kristianstad"))
})
