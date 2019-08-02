# Functions to help with user management and a database
# 20190802 by JJAV
# # # # # # # # # # # # # # # # # # # # # # # # # # # #
library(openssl)
library(DBI)

# get Database connection ####

#' Get a connection string
#'
#' Read a \code{config.yml} file to obtain the
#' connection parameter string to create a \code{\link[pool]{dbPool}}
#' connection object
#' @param configname section of the \code{config.yml} file with the parameters
#' @param driver the function that return an \code{DBI} driver
#' @return a \code{\link[pool]{dbPool}} connection object
#' @importFrom config get
#' @importFrom pool dbPool
#' @export
get_con <- function(configname = "defaultdb",
                    driver = odbc::odbc) {
  dw <- config::get(configname)
  drv <- driver()
  theargs <- c(list(drv = drv), dw)
  return(do.call(dbPool, theargs))
}


#' Creates the user table
#'
#' A Helper function to create the user table in the database
#' It adds also the admin and guest users with admin and guest passwords
#' respectively.
#' @param con \code{\link[pool]{dbPool}} connection object
#' @param salt a string to improve masking of passwords
#' @return the result of the transaction
#' @importFrom pool poolWithTransaction
#' @importFrom DBI dbExecute
#' @export
create_users_table <- function(con, salt) {
  sqlsentence1 <-
    "CREATE TABLE IF NOT EXISTS users (
         id INTEGER PRIMARY KEY,
         user CHAR(60) NOT NULL,
         hash CHAR(60) NOT NULL
       );"

  sqlsentence2 <-
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_user on users (user);"
  poolWithTransaction(con,
                      function(con) {
                        dbExecute(con, sqlsentence1)
                        dbExecute(con, sqlsentence2)
                        add_user(con, salt, "admin", "admin")
                        add_user(con, salt, "guest", "guest")
                      })
}

#' Adds an user to the database
#'
#' @param con \code{\link[pool]{dbPool}} connection object
#' @param salt a string to improve masking of passwords
#' @param username the username
#' @param password the password
#' @return the result of the query or -1 if user already exists
#' @export
#' @importFrom openssl sha256
#' @importFrom DBI sqlInterpolate
#' @importFrom DBI dbExecute
add_user <- function(con, salt, username, password) {
  if (!exists_user(con, username)) {
    salthash <- sha256(paste(salt, username, password))
    sql <- "INSERT INTO USERS (user,hash) VALUES( ?user, ?hash);"
    query <-
      sqlInterpolate(con, sql, user = username, hash = salthash)
    dbExecute(con, query)
  }
  else {
    return(-1)
  }
}

#' Deletes an user from the database
#'
#' @param con \code{\link[pool]{dbPool}} connection object
#' @param username the username
#' @return the result of the executed sentence or -1 if the user does not exists
#' @importFrom openssl sha256
#' @importFrom DBI sqlInterpolate
#' @importFrom DBI dbExecute
#' @export
delete_user <- function(con, username) {
  if (exists_user(con, username)) {
    sql <- "DELETE FROM users WHERE user = ?user;"
    query <- sqlInterpolate(con, sql, user = username)
    dbExecute(con, query)
  } else {
    return(-1)
  }
}


#' Modifies the password for an user
#'
#' @param con \code{\link[pool]{dbPool}} connection object
#' @param salt a string to improve masking of passwords
#' @param username the username
#' @param password the password
#' @return the result of the executed sentence or -1 if the user does not exists
#' @importFrom openssl sha256
#' @importFrom DBI sqlInterpolate
#' @importFrom DBI dbExecute
#' @export
modify_password <- function(con, salt, username, password) {
  if (exists_user(con, username)) {
    thehash <- sha256(paste(salt, username, password))
    sql <- "UPDATE users SET hash = ?hash WHERE user = ?user;"
    query <-
      sqlInterpolate(con, sql, hash = thehash, user = username)
    dbExecute(con, query)
  }
  else {
    return(-1)
  }
}

#' Test if the user exists
#'
#' @param con \code{\link[pool]{dbPool}} connection object
#' @param username the username
#' @return the result of the executed sentence or -1 if the user does not exists
#' @importFrom DBI sqlInterpolate
#' @importFrom DBI dbExecute
#' @export
exists_user <- function(con, username) {
  sql <- "SELECT user FROM users WHERE user = ?user;"
  query <- sqlInterpolate(con, sql, user = username)
  resdf <- dbGetQuery(con, query)
  return(nrow(resdf) >= 1)
}


#' Is the user authorized?
#'
#' Return true if the user/password combination is found in the database
#'
#' @param con  \code{\link[pool]{dbPool}} connection object
#' @param salt a string to improve masking of passwords
#' @param username the username
#' @param password the password
#' @return TRUE if the user is authorized, FALSE otherwise
#' @export
#' @importFrom openssl sha256
#' @importFrom DBI sqlInterpolate
#' @importFrom DBI dbGetQuery
is_authorized <- function(con, salt, username, password) {
  salthash <- sha256(paste(salt, username, password))
  sql <- "SELECT * FROM users where user = ?user AND hash = ?hash;"
  query <- sqlInterpolate(con, sql, user = username, hash = salthash)
  resultdf <- dbGetQuery(con, query)
  return(nrow(resultdf) >= 1)
}

# This may change in the futer with the roles definition

#' Test if the user is admin
#'
#' @param username the username
#' @return TRUE if the user is admin
#' @export
is_admin <- function(username) {
  return(username == "admin")
}

#' Test if the user is guest
#'
#' @param username the username
#' @return TRUE if the user is guest
#' @export
is_guest <- function(username) {
  return(username == "guest")
}


# # Test ####
# salt <- config::get("salt")
# mycon <- get_con()
# create_users_table(mycon)
# exists_user(mycon, salt, "yo")
# add_user(mycon, salt, "yo")
# exists_user(mycon, salt, "yo")
# modify_password(mycon, salt, "yo","newpass")
# add_user(mycon, salt, "yo")
# is_authorized(mycon, salt, "yo", "newpass")
# is_authorized(mycon, salt, "tu", "newpass")
# is_authorized(mycon, salt, "el", "newpass")
# add_user(mycon, salt, "tu")
# modify_password(mycon, salt, "tu", "newpass")
# is_authorized(mycon, salt, "tu", "newpass")
# dbDisconnect(mycon)
# rm(mycon)
