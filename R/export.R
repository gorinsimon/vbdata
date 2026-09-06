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

#' Extract score series from rotation scores
#'
#' The function takes the scores of the two team at the end of each rotation and
#' create for each team a numeric vector with the score of the team after each
#' point played.
#'
#' @param serving Numeric vector with the score of the serving team at the end
#' of each rotation (i.e. at the end of each series of service). The serving
#' team is the one starting serving at the beginning of the set.
#' @param receiving Numeric vector with the score of the receiving team at the
#' end of each rotation (i.e. at the end of each series of service). The
#' receiving team is the one starting receiving at the beginning of the set.
#'
#' @returns A list with two numeric vector. The first numeric vector contains
#' the score of the serving team for each point played in the set. The second
#' vector contains the score of the receiving team for each point played.
#' @keywords internal
#'
#' @examples
#' serving_scores <- c(0, 5, 12, 17, 25)
#' receiving_scores <- c(4, 10, 11, 15)
#' set_scores <- extract_score_series(serving_scores, receiving_scores)

extract_score_series <- function(serving, receiving) {
  receiving[1] <- 0
  # Loop over the rotation scores of the receiving team (the one likely to have
  # the most rotations).
  for (i in 1:length(receiving)) {
    # When this is the first rotation, the serving team has a score series
    # starting from 1 to the number of points scored, or just 0 if it lost the
    # first point. For the receiving team, we repeat 0 as many times the serving
    # team scored.

    if (i == 1) {
      # When iterating, it is easier to start from 0 (included) and then
      # removing it by deleting the first index of the vector.
      team_s <- c(0:serving[i])[-1]
      team_r <- rep(0, serving[i])

      # When this is the last rotation and only the receiving team had that
      # rotation, we create for the receiving team a series of scores iterating
      # from the score in the previous rotation to the score at the end of this
      # rotation. For the serving team, we just repeat the score at the end of
      # the previous rotation.
    } else if (i == length(receiving) && length(receiving) > length(serving)) {
      scored <- receiving[i] - receiving[i - 1]
      team_s <- c(team_s, rep(serving[i - 1], scored))
      team_r <- c(team_r, c(receiving[i - 1]:receiving[i])[-1])

      # As long as it is not the last rotation and the receiving team had not
      # one more rotation, we iterate  for the receiving team from the score of
      # the previous rotation to the score of the current rotation, then repeat
      # that score as many times as the serving team as served. For the serving
      # team, we repeat the score at the end of the last rotation as many times
      # as the receiving team served, then we iterate from the score of the
      # previous rotation to the score of the current rotation.
    } else if (i <= length(receiving)) {
      scored <- c(receiving[i] - receiving[i - 1], serving[i] - serving[i - 1])
      team_s <- c(
        team_s,
        rep(serving[i - 1], scored[1]),
        c(serving[i - 1]:serving[i])[-1]
      )
      team_r <- c(
        team_r,
        c(receiving[i - 1]:receiving[i])[-1],
        rep(receiving[i], scored[2])
      )
    }
  }

  return(list(serving = team_s, receiving = team_r))
}


# This file contains functions to easily access the inputs available in the app.
# They behaviour is too simply extract the information, with only a minimum of
# extra cleaning.

#' Extract from the app the score input by rotation
#'
#' @param input The app input object.
#' @param team A scalar string indicating the team for which extract the scores.
#' Only "home" or "away" are accepted.
#' @param set An integer scalar with the set for which extract the scores.
#' @param n_rotation
#'
#' @returns A raw vector with the score at the end of each rotation for the team
#' and set indicated. NULL values are replaced by NA's to maintain the mapping
#' between score and rotation.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' get_raw_rotation_scores(input, "home", 1)
#' }

get_raw_rotation_scores <- function(input, team, set, n_rotation = 8) {
  unlist(
    lapply(
      1:n_rotation,
      \(x) {
        unlist(
          lapply(
            1:6,
            \(y) {
              input[[paste0("set_", set, "_r_", x, "_p_", y, "_", team)]] %||%
                NA
            }
          )
        )
      }
    )
  )
}
