# This test the basic app
# 20190805 by JJAV
# # # # # # # # # # # # # # #

context("apps_testbasic")

library(shinytest)

test_that("Test basic application workd", {
#kip_on_cran()
expect_pass(testApp("apps/testbasic", compareImages = FALSE))
})
