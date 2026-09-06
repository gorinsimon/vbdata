#' Get the two team scores at the end of each rotation for a given set
#'
#' @param set Numeric scalar indicating the set for which to extract the scores.
#' @inherit make_score_rotation_input input
#'
#' @returns A list with two numeric vectors containing the score of each team at
#' the end of each rotation. The vector are named ("home" and "away") and the
#' first vector is always the "serving" team, while the second is the
#' "receiving" team. The "serving" team is the one that starts serving in the
#' set.
#' @keywords internal
#'
#' @examples
#' \donotrun{get_rotation_scores_input(1, input)}

get_rotation_scores_input <- function(set, input) {
  serving <- get_set_serving_team(input, set)
  # Extract from the app input the scores for each rotation during the set and
  # remove NA's from the vectors.
  scores_home <- get_raw_rotation_scores(input, "home", set)
  scores_away <- get_raw_rotation_scores(input, "away", set)
  scores_home <- scores_home[!is.na(scores_home)]
  scores_away <- scores_away[!is.na(scores_away)]

  # If the "home" team started serving, we append NA at the beginning of the
  # rotation score for the away team since they did not serve on first rotation.
  if (serving == "home") {
    scores_away <- c(NA, scores_away)
    scores <- list(home = scores_home, away = scores_away)
  } else if (serving == "away") {
    scores_home <- c(NA, scores_home)
    scores <- list(away = scores_away, home = scores_home)
  }

  attr(scores, "serving") <- ifelse(
    set < 5,
    input$serving_set_1,
    input$serving_set_5
  )
  attr(scores$home, "team") <- input$home_team
  attr(scores$away, "team") <- input$away_team

  return(scores)
}

