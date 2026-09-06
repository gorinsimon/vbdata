#==============================================================================|
#                             ---- Misc utils ----
#==============================================================================|
# This script contains utility functions used mainly to get raw input from the
# app, such as players in base rotations, substitutions (players and scores),
# time-out scores, serving team,
#
#' Extract from the app the base rotation (players' starting position)
#'
#' @param input The app input object.
#' @param team A scalar string indicating the team for which extract the scores.
#' Only "home" or "away" are accepted.
#' @param set An integer scalar with the set for which extract data.
#'
#' @returns A raw vector with the score at the end of each rotation for the team
#' and set indicated. NULL values are replaced by NA's to maintain the mapping
#' between score and rotation.
#' @keywords internal

get_base_rotation <- function(input, team, set) {
  unlist(
    lapply(
      # Loop across the six positions
      1:6,
      # We get the input (set to NA if NULL)
      \(x) input[[paste0("set_", set, "_", team, "_p", x)]] %||% NA
    )
  )
}

#' Extract from the app the substitutions (sub player position)
#'
#' @details
#' Note that in the app substitutions are entered as a second base rotation
#' line. If a sub player is indicated under the player in the second position of
#' the base rotation, this means the sub player replaced the player that started
#' at the second position in the base rotation.
#'
#' @inheritParams get_base_rotation
#'
#' @returns A raw vector with the score at the end of each rotation for the team
#' and set indicated. NULL values are replaced by NA's to maintain the mapping
#' between score and rotation.
#' @keywords internal

get_substitution_players <- function(input, team, set) {
  unlist(
    lapply(
      # Loop across the six positions
      1:6,
      # We get the input (set to NA if NULL)
      \(x) input[[paste0("set_", set, "_", team, "_p", x)]] %||% NA
    )
  )
}

#' Extract from the app the substitutions scores
#'
#' @details
#' Based on the type argument ("in" or "ou"), the functions extracts the scores
#' when substitutions took place. The scores are expected to be in a format
#' "XX:XX" (e.g. "10:05").
#'
#' @inheritParams get_base_rotation
#' @param type A string indicating whether the scores of when the sub players
#' entered ("in") or got out ("out") of the court should be extracted.
#'
#' @returns A list of length 6. Each position in the list contains a length
#' 2 numeric vector with the score when the substitution took place. The first
#' element in these vectors is always the score of the team that requested the
#' substitution. Positions in the list match the positions of the base rotation
#' and substitution players in the UI.
#' @keywords internal

get_substitution_scores <- function(input, team, set, type) {
  # "in" corresponds to the score when the player on the bench replaced a player
  # from the base roration.
  if (type == "in") {
    res <- lapply(
      # Loop across the six positions
      1:6,
      \(x) {
        # We get the score input (if any)
        str <- input[[paste0("set_", set, "_", team, "_p", x, "_sub_1_score")]]
        # We extract and return the score using regex (the format is always,
        # "XX:XX", with the first two XX's being the score of the team that
        # requested the change and the last two XX's the score of the other).
        c(
          as.numeric(gsub("^(\\d\\d):\\d\\d$", "\\1", str %||% NA)),
          as.numeric(gsub("^\\d\\d:(\\d\\d)$", "\\1", str %||% NA))
        )
      }
    )
    # "out" corresponds to the score when a player from the base rotation that
    # has been replaced re-enter the court.
  } else if (type == "out") {
    res <- lapply(
      # Loop across the six positions
      1:6,
      \(x) {
        # We get the score input (if any)
        str <- input[[paste0("set_", set, "_", team, "_p", x, "_sub_2_score")]]
        # We extract and return the score using regex (the format is always,
        # "XX:XX", with the first two XX's being the score of the team that
        # requested the change and the last two XX's the score of the other).
        c(
          as.numeric(gsub("^(\\d\\d):\\d\\d$", "\\1", str %||% NA)),
          as.numeric(gsub("^\\d\\d:(\\d\\d)$", "\\1", str %||% NA))
        )
      }
    )
  }

  return(res)
}

#' Extract from the app the score of time-outs
#'
#' @details
#' The scores are expected to be in a format "XX:XX" (e.g. "10:05").
#'
#' @inheritParams get_base_rotation
#'
#' @returns A list of length 6. Each position in the list contains a length
#' 2 numeric vector with the score when the substitution took place. The first
#' element in these vectors is always the score of the team that requested the
#' substitution. Positions in the list match the positions of the base rotation
#' and substitution players in the UI.
#' @keywords internal

get_time_out_scores <- function(input, team, set) {
  lapply(
    1:2,
    \(x) {
      str <- input[[paste0("set_", set, "_", team, "_to_", x)]]
      c(
        as.numeric(gsub("^(\\d\\d):\\d\\d$", "\\1", str %||% NA)),
        as.numeric(gsub("^\\d\\d:(\\d\\d)$", "\\1", str %||% NA))
      )
    }
  )
}


#' Extract from the app the team that started the set serving
#'
#' @inheritParams get_base_rotation
#'
#' @returns A string with the name ("home" or "away") of the team that started
#' serving for the set indicated.
#' @keywords internal

get_set_serving_team <- function(input, set) {
  # First let's determine which teeam started serving in set 1
  if (input$serving_set_1 == input$home_team) {
    serving_set_1 <- "home"
  } else if (input$serving_set_1 == input$away_team) {
    serving_set_1 <- "away"
  }

  # Now we can create the series of "starting serving" teams for all sets
  if (serving_set_1 == "home") {
    serving <- rep(c("home", "away"), 2)
  } else if (serving_set_1 == "away") {
    serving <- rep(c("away", "home"), 2)
  }

  # If we are not checking for set 5, we set the result to a vector with the
  # name ("home" or "away") of the team that started serving for each set. If
  # we check for set 5, we simply determine which team started to serve and
  # set the answer to its name.
  if (set < 5) {
    serving_team <- serving[set]
  } else if (set == 5) {
    if (input$serving_set_5 == input$home_team) {
      serving_set_5 <- "home"
    } else if (input$serving_set_5 == input$away_team) {
      serving_set_5 <- "away"
    }
    serving_team <- serving_set_5
  }

  return(serving_team)
}

#' Check that the base rotation for the two teams are complete
#'
#' @inheritParams get_base_rotation
#'
#' @returns A logical indicating if the base rotations are completed (`TRUE`) or
#' not (`FALSE`).
#' @keywords internal

are_base_rotations_complete <- function(input, team, set) {
  all(nzchar(get_base_rotation(input, team, set)))
}
