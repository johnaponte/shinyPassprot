# The UI for the wrap
# 20190702 by JJAV
# # # # # # # # # # # #

#' Creates the UI
#'
#' Returns a UI page with the user management UI. The user application
#' UI should be defined in the \code{appmoduleUI}
#'
#' @param apptitle application title
#' @param apptheme \code{Boostrap} theme from \code{shinythemes}
#' @param appmoduleUI a module UI object
#' @param ... Other parameters for the \code{appModuleUI}
#' @return a \code{\link[shiny]{fluidPage}} object
#' @export
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shinythemes shinytheme
#' @importFrom shinyjs hidden
#' @importFrom shinyBS bsButton
passprotUI <-
  function(apptitle, apptheme = "cerulean", appmoduleUI , ...) {
    fluidPage(
      useShinyjs(),
      theme = shinytheme(apptheme),
      # Application title
      titlePanel(apptitle),
      hidden(div(
        id = "app",
        fluidRow(column(12, appmoduleUI("applogic",...))),
        fluidRow(column(1, actionLink("lnk_exit","Exit")))
      )),
      div(
        id = "login",
        fluidRow(column(4,wellPanel(
          textInput("username", "User ID", ""),
          passwordInput("password", "Password", ""),
          bsButton("btn_login", "Login", disabled = T)
        )))
      )
    )
  }
