#==============================================================================|
#                           ---- Summary utils ----
#==============================================================================|
# This script contains functions used to calculate scores, number of
# substitutions and time-outs, by the two teams during the game. These will be
# used to populate the "Summary" tab.


## Scores ----------------------------------------------------------------------

#' Get the score of a team across all sets
#'
#' This is a rapper around `calculate_score()` that extract the score of a team
#' for a given set.
#'
#' @inheritParams get_set_score
#'
#' @returns An integer vector with the score of the team across all sets.
#' @keywords internal

get_set_scores <- function(team, input) {
  unlist(lapply(1:5, \(x) get_set_score(x, team, input)))
}

#' Get the score of a team for a given set
#'
#' Extract from the list of score-position pairs in the UI the score of the
#' indicated team for the given set. When all inputs are empty, the score is
#' set to NA.
#'
#' @param set A integer with the set number (between 1 and 5).
#' @param team A string with the name of the team ("home" or "away" only).
#' @param input The input from the (Shiny) app.
#'
#' @returns An integer scalar with the score of the team in the given set.
#' @keywords internal

get_set_score <- function(set, team, input) {
  # Access all scores input for the set and team parameters, this over the eight
  # rotations and six positions per rotation.
  scores <- lapply(
    1:8,
    \(x) {
      unlist(
        lapply(
          1:6,
          \(y) {
            input[[paste0("set_", set, "_r_", x, "_p_", y, "_", team)]]
          }
        )
      )
    }
  )
  # Clean scores input by replacing all NA (empty inputs) with 0's
  scores <- unlist(replace(scores, is.na(scores), 0))

  # If all scores are 0's, it means they are all empty and score is set to NA
  if (all(scores == 0)) {
    score <- NA
  } else {
    score <- max(scores, na.rm = TRUE)
  }

  # Return the maximum score for the team during the set
  return(score)
}


#' Validate scores across sets
#'
#' Make sure that the scores of the game are valid. This is used to define
#' whether to render or not the scores in the summary page. Scores are thus
#' displayed only when they are valid.
#'
#' @param home An integer vector with the maximum score, for each set, of the
#' home team.
#' @param away An integer vector with the maximum score, for each set, of the
#' away team.
#' @returns A named list of length two ("home" and "away") with the current score
#' by set for the two team. Each element in the list is a vector with the score
#' by team for the team in the vector name.
#' @keywords internal

validate_set_scores <- function(home, away) {
  # Default for the `scores` object to return
  scores <- NA
  attr(scores, "is_valid") <- FALSE
  attr(scores, "n_sets") <- 0
  attr(scores, "is_over") <- FALSE

  # Initiate sets won
  home_set <- 0
  away_set <- 0

  # Loop over each set score
  for (i in 1:5) {
    # If there is any NA, return `scores` which is still `NA`
    if (any(is.na(c(home[i], away[i])))) {
      return(scores)
    }
    # Set the score to reach to win a set (25 or 15 for set 5)
    set_end <- ifelse(i < 5, 25, 15)
    # If the score difference is at least of 2 and the maximum of the two scores
    # is at least 25 (or 15 for the 5th set), then return update sets won.
    if (
      (any(c(home[i], away[i]) >= set_end) && (abs(home[i] - away[i]) >= 2))
    ) {
      # Update the number of sets won for the home and away teams
      home_set <- home_set + (home[i] > away[i])
      away_set <- away_set + (home[i] < away[i])
      # Set the "n_sets" attribute indicating how many sets have been played
      attr(scores, "n_sets") <- home_set + away_set
    } else {
      # If the conditions to end a set are not met, return `scores` which is
      # still `NA`.
      return(scores)
    }
    # Break the loop is there is a team with 3 sets won
    if (max(home_set, away_set) == 3) {
      # If a team won the game, we indicate it is over
      attr(scores, "is_over") <- TRUE
      break
    }
  }
  # Prepare the object to return by making sure that potentially "extra" data
  # are removed.
  scores <- list(
    home = home[1:sum(home_set, away_set)],
    away = away[1:sum(home_set, away_set)]
  )
  # If we made it so far, then the scores are valid
  attr(scores, "is_valid") <- TRUE
  return(scores)
}


## Time-outs -------------------------------------------------------------------

#' Get the number of time-outs in the game
#'
#' @inheritParams get_set_score
#'
#' @returns A named list containing two vectors of a length equal to the number
#' of sets played in the game. The vector contains for each set the number of
#' time-outs requested for the team in the vector name.
#' @keywords internal

get_n_time_out <- function(input) {
  # Access the time-out inputs across all sets for the home team and get the sum
  home <- lapply(
    1:5,
    \(s) {
      sum(
        unlist(
          lapply(1:2, \(x) nzchar(input[[paste0("set_", s, "_home_to_", x)]]))
        )
      )
    }
  )
  # Access the time-out inputs across all sets for the away team and get the sum
  away <- lapply(
    1:5,
    \(s) {
      sum(
        unlist(
          lapply(1:2, \(x) nzchar(input[[paste0("set_", s, "_away_to_", x)]]))
        )
      )
    }
  )
  # Create a named list with the number of time-out per set for the two teams
  time_outs <- list(home = unlist(home), away = unlist(away))

  return(time_outs)
}

## Substitutions ---------------------------------------------------------------

#' Get the number of substitutions in the game
#'
#' @inheritParams get_set_score
#'
#' @returns A named list containing two vectors of a length equal to the number
#' of sets played in the game. The vector contains for each set the number of
#' substitutions requested for the team in the vector name.
#' @keywords internal

get_n_substitutions <- function(input) {
  # Access the substitutions input across all sets for the home team and get the
  # sum.
  home <- lapply(
    1:5,
    \(s) {
      sum(
        unlist(
          lapply(
            1:6,
            \(p) {
              lapply(
                1:2,
                \(x) {
                  nzchar(
                    input[[paste0(
                      "set_",
                      s,
                      "_home_p",
                      p,
                      "_sub_",
                      x,
                      "_score"
                    )]]
                  )
                }
              )
            }
          )
        )
      )
    }
  )
  # Access the substitutions input across all sets for the away team and get the
  # sum.
  away <- lapply(
    1:5,
    \(s) {
      sum(
        unlist(
          lapply(
            1:6,
            \(p) {
              lapply(
                1:2,
                \(x) {
                  nzchar(
                    input[[paste0(
                      "set_",
                      s,
                      "_away_p",
                      p,
                      "_sub_",
                      x,
                      "_score"
                    )]]
                  )
                }
              )
            }
          )
        )
      )
    }
  )
  # Create a named list with the number of substitutions per set for the two
  # teams.
  subs <- list(home = unlist(home), away = unlist(away))

  return(subs)
}
