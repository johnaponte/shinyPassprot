% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user_management.R
\name{exists_user}
\alias{exists_user}
\title{Test if the user exists}
\usage{
exists_user(con, username)
}
\arguments{
\item{con}{\code{\link[pool]{dbPool}} connection object}

\item{username}{the username}
}
\value{
the result of the executed sentence or -1 if the user does not exists
}
\description{
Test if the user exists
}
