sidebarView <- function(){
  
  sidebarMenu(id="sidebar",
              menuItem("Home", tabName = "home", icon = icon("home")),
              menuItem("Data input", tabName = "queries", icon = icon("keyboard")),
              menuItem("Plot", tabName = "plot", icon = icon("chart-bar")),
              menuItem("Data", tabName = "data", icon = icon("table")),
              menuItem("Page Settings", tabName = "settings", icon = icon("cog"))
              #uiChangeThemeDropdown()
  )
}