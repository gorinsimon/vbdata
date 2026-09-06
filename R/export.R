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


#' Add to information about time-out to a set data frame
#'
#' For each score during the set, the functions flags if a time-out was
#' requested which team requested it. The flags are added to a data frame
#' already wrangled.
#'
#' @param dat Data frame with the evolution of the score for a given set, as
#' created within the function `wrangle_set_data()`.
#' @inheritParams wrangle_set_data
#'
#' @returns The data frame passed to the function (`dat`) with flags indicating
#' for each point whether a time-out was requested or not.
#' @keywords internal

add_time_out <- function(dat, time_outs) {
  # Retrieve the name of the "home" and "away" team saved as attributes of the
  # "time_outs" objects. The team names are necessary to index the data frame
  # with the scores.
  home_t <- attr(time_outs$home, "team")
  away_t <- attr(time_outs$away, "team")

  # Checking that the time-outs are properly formatted (should not happen
  # since it is returned by the app, but it is safer...).
  chk_time_outs(time_outs)

  # NOTE: the time-out are always assigned to the "next point". In other words,
  # when a time-out is request at 6-6, that is, after the 12th point is over,
  # the time-out is assigned to the next point, that is, the 13th.

  # Time-out data on time-outs requested by the home are skipped if the list is
  # NULL.
  if (!all(is.na(unlist(time_outs$home)))) {
    # Iterate over the time-out vectors of the home team
    for (i in seq_along(time_outs$home)) {
      # If the vector does not contain NA, we identify the score entry in the
      # data and set the time-out flags accordingly. Keep in mind that the
      # first element in the score vector is the score of the requesting team,
      # that is, in this case, the home team.
      if (all(!is.na(time_outs$home[[i]]))) {
        dat <- mutate(
          dat,
          time_out = if_else(
            lag(
              dat[[home_t]] == time_outs$home[[i]][1] &
                dat[[away_t]] == time_outs$home[[i]][2],
              default = FALSE
            ),
            TRUE,
            time_out
          ),
          asked_time_out = if_else(
            lag(
              dat[[home_t]] == time_outs$home[[i]][1] &
                dat[[away_t]] == time_outs$home[[i]][2],
              default = FALSE
            ),
            home_t,
            asked_time_out
          )
        )
      }
    }
  }

  # Time-out data on time-outs requested by the away are skipped if the list is
  # NULL.
  if (any(!is.na(unlist(time_outs$away)))) {
    # Iterate over the time-out vectors of the away team
    for (i in seq_along(time_outs$away)) {
      # If the vector does not contain NA, we identify the score entry in the
      # data and set the time-out flags accordingly. Keep in mind that the
      # first element in the score vector is the score of the requesting team,
      # that is, in this case, the away team.
      if (all(!is.na(time_outs$away[[i]]))) {
        dat <- mutate(
          dat,
          time_out = if_else(
            lag(
              dat[[away_t]] == time_outs$away[[i]][1] &
                dat[[home_t]] == time_outs$away[[i]][2],
              default = FALSE
            ),
            TRUE,
            time_out
          ),
          asked_time_out = if_else(
            lag(
              dat[[away_t]] == time_outs$away[[i]][1] &
                dat[[home_t]] == time_outs$away[[i]][2],
              default = FALSE
            ),
            away_t,
            asked_time_out
          )
        )
      }
    }
  }

  return(dat)
}

#' Checker function for time-out lists
#'
#' This function checks that the list of time-outs to add to a data frame with
#' the scores are properly formatted.
#'
#' @inheritParams wrangle_set_data
#'
#' @returns Nothing is returned, it is used only for side-effect.
#' @keywords internal

chk_time_outs <- function(time_outs) {
  if (!is.list(time_outs) && length(time_outs) != 2) {
    stop("`time_outs` must be a list of length 2.")
  }

  if (!setequal(names(time_outs), c("home", "away"))) {
    stop("Names of `time_outs` must be match \"home\" and \"away\"")
  }

  if (length(time_outs[["home"]]) != 2) {
    stop("`time_outs$home` must be a list of length 2.")
  }

  if (length(time_outs[["away"]]) != 2) {
    stop("`time_outs$away` must be a list of length 2.")
  }

  if (!all(lapply(time_outs[["home"]], length) == 2)) {
    stop("The two vectors in `time_outs$home` must be of length 2.")
  }

  if (!all(lapply(time_outs[["away"]], length) == 2)) {
    stop("The two vectors in `time_outs$away` must be of length 2.")
  }

  to_home_num_or_na <- lapply(
    time_outs$home,
    \(x) all(is.numeric(x) & !is.na(x)) | all(is.na(x))
  )

  to_away_num_or_na <- lapply(
    time_outs$away,
    \(x) all(is.numeric(x) & !is.na(x)) | all(is.na(x))
  )

  if (!all(unlist(to_home_num_or_na))) {
    stop(
      "Vectors in `time_outs$home` can contain only numeric values or only NA's."
    )
  }
  if (!all(unlist(to_away_num_or_na))) {
    stop(
      "Vectors in `time_outs$away` can contain only numeric values or only NA's."
    )
  }

  invisible()
}

#' Add substitutions to a set data frame
#'
#' For each substitution made by the two teams, the function flags in the data
#' on which point a substitution was made and the team who requested it. The
#' players in the rotation are also updated accordingly. Note that the point for
#' which a substitution is flagged is always the next one, meaning that when a
#' substitution is asked after the 10th point, it is flagged in the 11th one.
#'
#' @inherit add_time_out dat
#' @inheritParams wrangle_set_data
#'
#' @returns The data frame passed to the function (`dat`), which contains
#' already the sequence of player numbers at each position, updated with the
#' substitutions and flags indicating for which point a substitution happened.
#'
#' @keywords internal

add_substitutions <- function(dat, substitutions) {
  home_t <- attr(substitutions$home, "team")
  away_t <- attr(substitutions$away, "team")

  # NOTE: the substitutions are always assigned to the "next point". In other
  # words, when a time-out is request at 6-6, that is, after the 12th point is
  # over, the time-out is assigned to the next point, that is, the 13th.

  # Home team substitutions

  # Only update "dat" for the home team if the substitutions list is not empty
  if (!is.null(substitutions$home)) {
    # Going along the six substitution positions possible
    for (i in seq_along(substitutions$home$score_in)) {
      # If for the current position, there was no substitution, then nothing is
      # done.
      if (!all(is.na(substitutions$home$score_in[[i]]))) {
        # Shorter access to the score of the first and second change (if any)
        # for the current position.
        s_h_in <- substitutions$home$score_in[[i]]
        s_h_out <- substitutions$home$score_out[[i]]

        # For each score, if it is equal to the home-away team score for the
        # current change (first or second) for the position, then we set the
        # "substitution" value to TRUE and indicate the team that asked for
        # a substitution.
        dat <- mutate(
          dat,
          substitution = case_when(
            lag(
              dat[[home_t]] == s_h_in[1] & dat[[away_t]] == s_h_in[2],
              default = FALSE
            ) ~ TRUE,
            lag(
              dat[[home_t]] == s_h_out[1] & dat[[away_t]] == s_h_out[2],
              default = FALSE
            ) ~ TRUE,
            .default = substitution
          ),
          asked_substitution = case_when(
            lag(
              dat[[home_t]] == s_h_in[1] & dat[[away_t]] == s_h_in[2],
              default = FALSE
            ) ~ home_t,
            lag(
              dat[[home_t]] == s_h_out[1] & dat[[away_t]] == s_h_out[2],
              default = FALSE
            ) ~ home_t,
            .default = asked_substitution
          )
        )

        # Now, we make the substitution in all positions and rotations that
        # occurred after the substitution.

        # Take the subset of home team positions where the score is equal or
        # higher to the current substitutions.
        temp_hp <- dat[
          lag(
            dat[[home_t]] >= s_h_in[1] & dat[[away_t]] >= s_h_in[2],
            default = FALSE
          ),
          paste0("home_P", 1:6)
        ]
        # Replace all instances of the played currently replaced in the temp
        # subset of positions
        temp_hp[
          temp_hp == substitutions$home$rotation[i]
        ] <- substitutions$home$sub[i]
        # We add back the temp positions to the "dat" to include substitutions
        dat[
          lag(
            dat[[home_t]] >= s_h_in[1] & dat[[away_t]] >= s_h_in[2],
            default = FALSE
          ),
          paste0("home_P", 1:6)
        ] <- temp_hp
        # We now repeat the same but with final substitution (i.e. when a player
        # previously substituted enter back).
        if (!all(is.na(s_h_out))) {
          temp_hp_2 <- dat[
            lag(
              dat[[home_t]] >= s_h_out[1] & dat[[away_t]] >= s_h_out[2],
              default = FALSE
            ),
            paste0("home_P", 1:6)
          ]

          temp_hp_2[
            temp_hp_2 == substitutions$home$sub[i]
          ] <- substitutions$home$rotation[i]

          dat[
            lag(
              dat[[home_t]] >= s_h_out[1] & dat[[away_t]] >= s_h_out[2],
              default = FALSE
            ),
            paste0("home_P", 1:6)
          ] <- temp_hp_2
        }
      }
    }
  }

  # Away team substitutions

  # Only update "dat" for the away team if the substitutions list is not empty
  if (!is.null(substitutions$away)) {
    # Going along the the six substitution positions possible
    for (i in seq_along(substitutions$away$score_in)) {
      # If for the current position, there was no substitution, then nothing is
      # done.
      if (!any(is.na(substitutions$away$score_in[[i]]))) {
        # Shorter access to the score of the first and second change (if any)
        # for the current position.
        s_a_in <- substitutions$away$score_in[[i]]
        s_a_out <- substitutions$away$score_out[[i]]

        # For each score, if it is equal to the home-away team score for the
        # current change (first or second) for the position, then we set the
        # "substitution" value to TRUE and indicate the team that asked for
        # a substitution.
        dat <- mutate(
          dat,
          substitution = case_when(
            lag(
              dat[[away_t]] == s_a_in[1] & dat[[home_t]] == s_a_in[2],
              default = FALSE
            ) ~ TRUE,
            lag(
              dat[[away_t]] == s_a_out[1] & dat[[home_t]] == s_a_out[2],
              default = FALSE
            ) ~ TRUE,
            .default = substitution
          ),
          asked_substitution = case_when(
            lag(
              dat[[away_t]] == s_a_in[1] & dat[[home_t]] == s_a_in[2],
              default = FALSE
            ) ~ away_t,
            lag(
              dat[[away_t]] == s_a_out[1] & dat[[home_t]] == s_a_out[2],
              default = FALSE
            ) ~ away_t,
            .default = asked_substitution
          )
        )

        # Now, we make the substitution in all positions and rotations that
        # occurred after the substitution.

        # Take the subset of away team positions where the score is equal or
        # higher to the current substitutions.
        temp_ap <- dat[
          lag(
            dat[[away_t]] >= s_a_in[1] & dat[[home_t]] >= s_a_in[2],
            default = FALSE
          ),
          paste0("away_P", 1:6)
        ]
        # Replace all instances of the played currently replaced in the temp
        # subset of positions
        temp_ap[
          temp_ap == substitutions$away$rotation[i]
        ] <- substitutions$away$sub[i]
        # We add back the temp positions to the "dat" to include substitutions
        dat[
          lag(
            dat[[away_t]] >= s_a_in[1] & dat[[home_t]] >= s_a_in[2],
            default = FALSE
          ),
          paste0("away_P", 1:6)
        ] <- temp_ap
        # We now repeat the same but with final substitution (i.e. when a player
        # previously substituted enter back).
        if (!any(is.na(s_a_out))) {
          temp_ap_2 <- dat[
            lag(
              dat[[away_t]] >= s_a_out[1] & dat[[home_t]] >= s_a_out[2],
              default = FALSE
            ),
            paste0("away_P", 1:6)
          ]
          temp_ap_2[
            temp_ap_2 == substitutions$away$sub[i]
          ] <- substitutions$away$rotation[i]
          dat[
            lag(
              dat[[away_t]] >= s_a_out[1] & dat[[home_t]] >= s_a_out[2],
              default = FALSE
            ),
            paste0("away_P", 1:6)
          ] <- temp_ap_2
        }
      }
    }
  }
  return(dat)
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
