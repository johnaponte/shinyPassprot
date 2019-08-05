# Here the modules for the change of password
# 20190802 by JJAV
# # # # # # # # # # # # # # # # # # # # # # # #

#' Change password module, UI part
#'
#' @param id id for namespace
#' @import shiny
#' @importFrom shinyBS bsButton
#' @export
chgpwdUI <- function(id){
  ns <- NS(id)
  tagList(
    h4(textOutput(ns("prompt"))),
    passwordInput(ns("oldpwd"),label = "Old Password"),
    passwordInput(ns("newpwd1"), label = 'New Password'),
    passwordInput(ns("newpwd2"), label = 'Confirm Password'),
    bsButton(ns("btn_pwd"), "Change password", disabled = T),
    textOutput(ns("msg"))
  )
}


#' Change password module, Server part
#'
#' @param input for the \code{callmodule}
#' @param output for the \code{callmodule}
#' @param session for the \code{callmodule}
#' @param con \code{\link[pool]{dbPool}} connection object
#' @param salt a string to improve masking of passwords
#' @param username the username
#' @import shiny
#' @importFrom shinyBS updateButton
#' @importFrom shinyjs hideElement
#' @export
chgpwd <- function(input, output, session, con, salt, username) {
  ns <- session$ns

  output$prompt <- renderText(paste("Change password for user:",username()))

  observe({
    cat("Username: ", username(),"\n")
    cat(input$oldpwd,"\n")
    cat(input$newpwd1,"\n")
    cat(input$newpwd2,"\n")
    cat(is_authorized(con,salt,trimws(username(),"both"), trimws(input$oldpwd,"both")), "\n")

    if (
     is_authorized(con,salt,trimws(username(),"both"), trimws(input$oldpwd,"both")) &
      trimws(input$newpwd1, "both") == trimws(input$newpwd2, "both") &
      nchar(trimws(input$newpwd1, "both")) > 6
    ) {
      updateButton(session, ns("btn_pwd"), disabled = F)
    }
    else {
      updateButton(session, ns("btn_pwd"), disabled = T)
    }
  }
  )

  observeEvent(
    input$btn_pwd,
    {
      modify_password(con, salt, trimws(username(),"both"), trimws(input$newpwd1, "both"))
      hideElement("oldpwd")
      hideElement("newpwd1")
      hideElement("newpwd2")
      hideElement("btn_pwd")
      output$msg <- renderText("Password changed with success")
    }
  )

}
