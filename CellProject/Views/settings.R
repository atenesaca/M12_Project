settingsView <- function(){
  conditionalPanel(
    condition= "input.sidebar == 'settings'",
    h1("Settings"),
    p("You can change the theme page with this selector",br(),"Have fun!!!"),
    br(),
    uiChangeThemeDropdown()
  )
}