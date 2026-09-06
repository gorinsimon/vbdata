#==============================================================================|
#                            ---- Score utils ----
#==============================================================================|
# This script contains functions used check if scores by rotations are valid and
# scores for a substitution are not missing.

#' Check that the scores by rotation are valid for the two teams
#'
#' The function checks that the score by rotation for each team is valid by
#' running `is_scores_seq_valid()`. Then, it is checked if the number of
#' rotations and the end score of the team that won the set are valid.
#'
#' @param set An integer indicating the score from the set to check.
#' @param input The list of input from the Shiny app.
#'
#' @returns Logical indicating if the scores are coherent (`TRUE`) or not
#' (`FALSE`).
#' @keywords internal

check_scores_rotations <- function(set, input) {
  # Check if the scores entered for the two teams are valid
  home_scores_valid <- is_scores_seq_valid(set, "home", input)
  away_scores_valid <- is_scores_seq_valid(set, "away", input)

  # Extract the "score" attribute from the object returned by
  # `is_scores_seq_valid()`.
  home_scores <- attr(home_scores_valid, "score")
  away_scores <- attr(away_scores_valid, "score")

  # Extract the "len" attribute from the object returned by
  # `is_scores_seq_valid()`. The length allows to determine how many rotation
  # per team there were.
  home_n_rotations <- attr(home_scores_valid, "len")
  away_n_rotations <- attr(away_scores_valid, "len")

  # Check if the number of rotation is valid (the difference between the
  # number of rotations by the receiving and the serving team scan only be of 0
  # or 1).
  are_rotations_valid <- ifelse(
    ((input$serving_set_1 == input$home_team) && (set %in% c(1, 3))) ||
      ((set == 5) && (input$serving_set_5 == input$home_team)),
    (away_n_rotations - home_n_rotations) %in% 0:1,
    (home_n_rotations - away_n_rotations) %in% 0:1
  )

  # Check that that the score of the winning team is valid
  is_max_valid <- max(c(home_scores, away_scores)) >= ifelse(set == 5, 15, 25)

  # Check that that the difference between the two team is coherent
  if (max(c(home_scores, away_scores)) == ifelse(set == 5, 15, 25)) {
    is_diff_valid <- abs(home_scores - away_scores) >= 2
  } else if (max(c(home_scores, away_scores)) > ifelse(set == 5, 15, 25)) {
    is_diff_valid <- abs(home_scores - away_scores) == 2
  } else {
    is_diff_valid <- FALSE
  }

  # Check that all checks are TRUE
  are_all_valid <- all(
    home_scores_valid,
    away_scores_valid,
    is_max_valid,
    is_diff_valid,
    are_rotations_valid
  )

  return(are_all_valid)
}

#' Check if the scores entered in the UI are valid
#'
#' Take the input from the app and check if for a given set and team, the
#' sequence of score by rotation is valid.
#'
#' @inheritParams check_scores_rotations
#' @param team A string with the name of the team ("home" or "away" only).
#'
#' @returns A logical indicating whether the scores are valid (`TRUE`) or not
#' (`FALSE`). The logical also has attributes indicating what is the final
#' score ("score" attribute) and the number of rotation ("len" attribute).
#' @keywords internal

is_scores_seq_valid <- function(set, team, input) {
  # Access all scores input for the given set and team
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
  # Unlist the input
  scores <- unlist(scores)

  # If the team started receiving during the set, we remove the first element
  # of "scores" as it should be empty (not available in the UI).
  if (
    ((input[[paste0(team, "_team")]] != input$serving_set_1) &&
     (set %in% c(1, 3))) ||
    ((input[[paste0(team, "_team")]] == input$serving_set_1) && (set %in% c(2, 4)))
  ) {
    scores <- scores[-1]
  }
  # Same as if statement above but for the 5th set if there was one
  if ((team != input$serving_set_5) && (set == 5)) {
    scores <- scores[-1]
  }

  # Is there "multiple max" (e.g. two point in time with 25)?
  has_multiple_max <- sum(scores == max(scores, na.rm = TRUE), na.rm = TRUE) > 1
  # If the rotation scores sequence has "multiple max", the function returns
  # "FALSE".
  if (has_multiple_max) {
    res <- FALSE
    attr(res, "len") <- 0
    return(FALSE)
  }

  # Check that the score sequence is increasing (i.e. after each rotation, the
  # new score is higher than the one in the previous rotation).
  is_increasing <- all(diff(scores[!is.na(scores)], 1) > 0)
  # If the rotation scores sequence is not increasing, the function returns
  # "FALSE".
  if (!is_increasing) {
    res <- FALSE
    attr(res, "len") <- 0
    return(FALSE)
  }

  # Check if there are any NAs in the rotation scores sequence. This is done by
  # selecting the scores from the first rotation to the rotation with the
  # highest score. At this stage, the sequence is increasing and with a
  # single maximum. If there are any NAs, they should be located between the
  # first element and the last one (where the maximum score is).
  scores <- scores[1:which.max(scores)]
  has_na <- sum(is.na(scores)) > 0
  # If the rotation scores sequence has any NAs, the function returns "FALSE"
  if (has_na) {
    res <- FALSE
    attr(res, "len") <- 0
    return(FALSE)
  }

  # If the all conditions above are met, we set the results (`res`) to TRUE and
  # add attributes with the score ("score") in the set and the number of
  # rotation ("len").
  res <- TRUE
  attr(res, "len") <- length(scores)
  attr(res, "score") <- max(scores)

  # Since we removed one score element when the team is receiving at the
  # beginning of the set, we need to add one additional rotation to compensate
  # for that before returning `res`.
  if (
    ((input[[paste0(team, "_team")]] != input$serving_set_1) &&
     (set %in% c(1, 3))) ||
    ((input[[paste0(team, "_team")]] == input$serving_set_1) && (set %in% c(2, 4)))
  ) {
    attr(res, "len") <- attr(res, "len") + 1
  }
  if ((team != input$serving_set_5) && (set == 5)) {
    attr(res, "len") <- attr(res, "len") + 1
  }

  return(res)
}

#' Verify scores for a substitution are not missing
#'
#' Check if there are any substitution score missing. This can happen when a
#' substitution is indicated in the UI (the player entering the court is
#' indicated), but the corresponding score when the substitution happened is
#' empty.
#'
#' @inheritParams check_scores_rotations
#' @inheritParams is_scores_seq_valid
#'
#' @returns Logical indicating if the scores are missing (`TRUE`) or not
#' (`FALSE`).
#' @keywords internal

is_sub_score_missing <- function(team, set, input) {
  # Access each substitution input and corresponding score, then check if the
  # former is not empty and the later is empty. When TRUE, this means there is
  # a missing score.
  missings <- lapply(
    1:6,
    \(x) {
      nzchar(input[[paste0("set_", set, "_", team, "_p", x, "_sub")]]) &
        !nzchar(
          input[[paste0("set_", set, "_", team, "_p", x, "_sub_1_score")]]
        )
    }
  )
  are_missings <- any(unlist(missings))
}
