# This produce a function that should be use as wrap for the shiny app
# 20190802 by JJAV
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ##

#' Return a function that can be use as Server for Shiny app
#'
#' This wrap have the logic for the user management and authorization
#' The logic of the user application should be defined in the \code{appmodule}
#'
#' @param con \code{\link[pool]{dbPool}} connection object
#' @param salt a string to improve masking of passwords
#' @param appmodule a \code{Shiny module server} object
#' @param ... extra parameters for \code{appmodule}
#' @import shiny
#' @importFrom shinyjs hideElement
#' @importFrom shinyjs showElement
#' @importFrom shinyBS updateButton
#' @return a function with \code{input}, \code{output}, \code{session} arguments
#' @export
passprotServer <-  function(con, salt, appmodule, ...) {
  return(
    function(input, output, session ){
    #Here comes the logic of the app
    callModule(appmodule, "applogic", ...)

    #Here comes the logic of the change password
    callModule(chgpwd, "chgpassword", con, salt, reactive(input$username) )

    #Here comes the logic of the user management
    rv <- reactiveValues(attemps = 3)

    observeEvent(input$lnk_exit,
                 {
                   updateTextInput(session, "username", value = "")
                   updateTextInput(session, "password", value = "")
                   hideElement("app")
                   hideElement("lnk_exit")
                   showElement("login")
                   rv$attemps = 3
                 })

    observe({
      if ( trimws(input$username,"both") == "" | trimws(input$password,"both") == "")
        updateButton(session,"btn_login", disabled = T)
      else
        updateButton(session, "btn_login", disabled = F)
    })

    observeEvent(
      input$btn_login,
      {
        if (rv$attemps <= 0) {
          updateButton(session,
                       "btn_login",
                       label = "Too many attemps",
                       disabled = T)
          hideElement("username")
          hideElement("password")
        }
        else {
          if (is_authorized(con,
                            salt,
                            trimws(input$username,"both"),
                            trimws(input$password,"both"))) {
            hideElement("login")
            showElement("app")
            showElement("lnk_exit")
          }
          else {
            rv$attemps <<- rv$attemps - 1
          }
        }
      }
    )

    observeEvent(
      input$lnk_pwd,
      {
        hideElement("app")
        showElement("chgpwd")
      }
    )

    observeEvent(
      input$lnk_backapp,
      {
        hideElement("chgpwd")
        showElement("app")
      }
    )

    }
)}
