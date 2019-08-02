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
    h3("Change of password"),
    passwordInput(ns("oldpwd"),label = "Old Password"),
    passwordInput(ns("newpwd1"), label = 'New Password'),
    passwordInput(ns("newpwd2"), label = 'Confirm Password'),
    bsButton(ns("btn_pwd"), "Change password", disabled = T),
    textOutput(ns("msg"))
  )
}


#' Change passwod module, Server part
#'
#' @param input for the callmodule
#' @param output for the callmodule
#' @param session for the callmodule
#' @param con \code{\link[pool]{dbPool}} connection object
#' @param salt a string to improve masking of passwords
#' @param username the username
#' @import shiny
#' @importFrom shinyBS updateButton
#' @export
chgpwd <- function(input, output, session, con, salt, username) {
  ns <- session$ns
  observe(
    cat(username(),"\n"),
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
  )

  observeEvent(
    input$btn_pwd,
    {
      modify_password(con, salt, trimws(username(),"both"), trimws(input$newpwd1, "both"))
      output$msg <- renderText("Password changed with success")
      updateTextInput(session, ns("oldpwd"),value = "")
      updateTextInput(session, ns("newpwd1"),value = "")
      updateTextInput(session, ns("newpwd2"),value = "")
    }
  )

}
