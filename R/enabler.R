#==============================================================================|
#                      ---- UI enabler/disabler utils ----
#==============================================================================|
# This script contains functions used to disable/enable "en masse" the UI
# elements of the two teams in the "Player" page.

#' Enable/disable UI elements in the player page
#'
#' @param team String ("home" or "away") indicating for which team the UI must
#' be enabled or disabled.
#' @returns
#' @keywords internal
#'
#' @examples
disable_players_ui <- function(team) {
  if (team == "home") {
    shinyjs::disable("new_player_name_home")
    shinyjs::disable("new_player_role_home")
    shinyjs::disable("new_player_num_home")
    shinyjs::disable("remove_player_home")
    shinyjs::disable("home_players_tbl")
    shinyjs::disable("validate_player_home")
  } else if (team == "away") {
    shinyjs::disable("new_player_name_away")
    shinyjs::disable("new_player_role_away")
    shinyjs::disable("new_player_num_away")
    shinyjs::disable("remove_player_away")
    shinyjs::disable("away_players_tbl")
    shinyjs::disable("validate_player_away")
  }
}


#' @rdname disable_players_ui

enable_players_ui <- function(team) {
  if (team == "home") {
    shinyjs::enable("new_player_name_home")
    shinyjs::enable("new_player_role_home")
    shinyjs::enable("new_player_num_home")
    shinyjs::enable("remove_player_home")
    shinyjs::enable("home_players_tbl")
    shinyjs::enable("validate_player_home")
  } else if (team == "away") {
    shinyjs::enable("new_player_name_away")
    shinyjs::enable("new_player_role_away")
    shinyjs::enable("new_player_num_away")
    shinyjs::enable("remove_player_away")
    shinyjs::enable("away_players_tbl")
    shinyjs::enable("validate_player_away")
  }
}
