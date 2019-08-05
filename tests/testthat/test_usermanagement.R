# This test the user management
# 20190802 by JJAV
# # # # # # # # # # # # # # # # #
library(DBI)
library(pool)
context("User management")

test_that("get_con works" , {
  con <- get_con(driver = RSQLite::SQLite)
  expect_true(dbIsValid(con))
  poolClose(con)
})


test_that("functions to create user works", {
  con <- get_con(driver = RSQLite::SQLite)
  salt <-  config::get("salt")
  create_users_table(con,salt)
  expect_true(exists_user(con,"admin"))
  expect_true(exists_user(con,"guest"))
  expect_true(is_authorized(con, salt,"admin","admin"))
  expect_true(is_authorized(con, salt, "guest", "guest"))
  expect_false(exists_user(con, "nouser"))
  expect_false(is_authorized(con, salt, "admin","guest"))
  expect_false(exists_user(con,"newuser"))
  add_user(con, salt, "newuser","newpass")
  expect_true(exists_user(con,"newuser"))
  expect_true(is_authorized(con, salt,"newuser","newpass"))
  modify_password(con,salt, "newuser" ,"otherpass")
  expect_false(is_authorized(con, salt,"newuser","newpass"))
  expect_true(is_authorized(con, salt,"newuser","otherpass"))
  delete_user(con,"newuser")
  expect_false(exists_user(con,"newuser"))
  poolClose(con)
})
