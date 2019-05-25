sidebarView <- function(){
  # Create a menu where we can add multiples menu items
  sidebarMenu(id="sidebar", # set an id to sidebar menu
              # Create an item where we can add an id and an icon
              menuItem("Home", tabName = "home", icon = icon("home")),
              menuItem("Queries", tabName = "queries", icon = icon("keyboard")),
              menuItem("Plot", tabName = "plot", icon = icon("chart-bar")),
              menuItem("Data", tabName = "data", icon = icon("table")),
              uiChangeThemeDropdown()
  )
}