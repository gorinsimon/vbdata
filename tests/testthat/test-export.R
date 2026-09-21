# In this series of tests, four different "scores" scenarios were tested:
# - scenario 1: serving team won / serving team served last
# - scenario 2: serving team won / receiving team served last
# - scenario 3: receiving team won / receiving team served last
# - scenario 4: receiving team won / serving team served last

for (scenario in as.character(1:4)) {
  # Select the scores based on current scenario
  scores <- switch(
    scenario,
    "1" = list(
      home = c(5, 8, 10, 15, 20, 22, 25),
      away = c(NA, 3, 5, 9, 12, 13, 16)
    ),
    "2" = list(
      home = c(5, 8, 10, 15, 20, 24, 25),
      away = c(NA, 3, 5, 9, 12, 13, 16)
    ),
    "3" = list(
      home = c(0, 3, 5, 9, 12, 13),
      away = c(NA, 5, 8, 10, 15, 20, 25)
    ),
    "4" = list(
      home = c(0, 3, 5, 9, 12, 13),
      away = c(NA, 5, 8, 10, 15, 24, 25)
    )
  )
  # For all scenario the serving team is the "home" team (all manipulations
  # are in "scores").
  attr(scores, "serving") <- "home"

  # Select the time-outs based on current scenario
  time_outs <- switch(
    scenario,
    "1" = list(
      home = list(c(10, 9), c(NA, NA)),
      away = list(c(0, 4), c(9, 15))
    ),
    "2" = list(
      home = list(c(10, 9), c(15, 12)),
      away = list(c(0, 4), c(NA, NA))
    ),
    "3" = list(
      home = list(c(0, 5), c(12, 17)),
      away = list(c(10, 8), c(15, 12))
    ),
    "4" = list(
      home = list(c(0, 5), c(12, 17)),
      away = list(c(NA, NA), c(NA, NA))
    )
  )

  # Set the name of the "home" and "away" teams
  attr(time_outs$home, "team") <- "Dev"
  attr(time_outs$away, "team") <- "Test"

  # Select the substitutions based on current scenario
  substitutions <- list(
    home = list(
      rotation = c(4, 6, 10, 7, 3, 13),
      sub = c(14, 2, 15, NA, NA, NA),
      score_in = list(
        # fmt: skip
        switch(scenario, "1" = c(5, 3), "2" = c(5, 3), "3" = c(0, 4), "4" = c(0, 4)),
        # fmt: skip
        switch(scenario, "1" = c(15, 12), "2" = c(15, 12), "3" = c(12, 18), "4" = c(12, 18)),
        # fmt: skip
        switch(scenario, "1" = c(23, 16), "2" = c(24, 16), "3" = c(9, 14), "4" = c(9, 14)),
        c(NA, NA),
        c(NA, NA),
        c(NA, NA)
      ),
      score_out = list(
        # fmt: skip
        switch(scenario, "1" = c(NA, NA), "2" = c(NA, NA), "3" = c(3, 7), "4" = c(3, 7)),
        c(NA, NA),
        c(NA, NA),
        c(NA, NA),
        c(NA, NA),
        c(NA, NA)
      )
    ),
    away = list(
      rotation = c(4, 6, 10, 7, 3, 13),
      sub = c(14, 2, 15, NA, NA, NA),
      score_in = list(
        # fmt: skip
        switch(scenario, "1" = c(3, 8), "2" = c(3, 8), "3" = c(8, 4), "4" = c(8, 4)),
        # fmt: skip
        switch(scenario, "1" = c(9, 12), "2" = c(9, 12), "3" = c(15, 9), "4" = c(15, 9)),
        # fmt: skip
        switch(scenario, "1" = c(13, 20), "2" = c(13, 20), "3" = c(23, 13), "4" = c(23, 12)),
        c(NA, NA),
        c(NA, NA),
        c(NA, NA)
      ),
      score_out = list(
        c(NA, NA),
        c(NA, NA),
        c(NA, NA),
        c(NA, NA),
        c(NA, NA),
        c(NA, NA)
      )
    )
  )

  # Set the attributes expected for the "scores" object
  attr(scores, "serving") <- "home"
  attr(substitutions$home, "team") <- "Dev"
  attr(substitutions$away, "team") <- "Test"
  attr(scores$home, "team") <- "Dev"
  attr(scores$away, "team") <- "Test"

  # Wrangling the data to be tested
  wrangled_data <- wrangle_set_data(
    1,
    scores,
    time_outs,
    substitutions
  )

  # Create a alternate version of `wrangled_data` where the columns with the
  # score of the two teams are renamed as "home" and "away".
  alt_wrangled_data <- rename(
    wrangled_data,
    "home" := attr(time_outs$home, "team"),
    "away" := attr(time_outs$away, "team")
  )

  # Replace the first NA score of the receiving team with 0
  if (attr(scores, "serving") == "home") {
    scores[["away"]][1] <- 0
    serving <- "home"
    receiving <- "away"
  } else {
    scores[["home"]][1] <- 0
    serving <- "away"
    receiving <- "home"
  }

  test_that("Check wrangling works as expected for time-outs", {
    # Check that the time-outs indicated in `time_outs-out` are correctly
    # reflected in the wrangled data.
    are_time_outs_correct <- lapply(
      time_outs,
      \(x) {
        lapply(
          x,
          \(y) {
            # If time-out score is not NA, the sum of score + 1 should
            # correspond to the point where the time-out happened. In the
            # wrangled data, the point must be marked as with a time-out and the
            # requesting team should be the team in the attribute.
            if (!is.na(sum(y))) {
              (isTRUE(wrangled_data$time_out[sum(y) + 1]) &&
                isTRUE(
                  wrangled_data$asked_time_out[sum(y) + 1] == attr(x, "team")
                ))
            } else {
              TRUE
            }
          }
        )
      }
    )

    expect_all_true(unlist(are_time_outs_correct))

    # Second check that the time-outs indicated in `time_outs-out` are correctly
    # reflected in the wrangled data. Here, we filter the wrangled data using the
    # exact time-out score instead that the point when it was requested. The
    # focus in on the "home" team.
    are_time_outs_home_correct <- lapply(
      time_outs[["home"]],
      \(x) {
        # Here we do the same as above, but we do not select the point based on
        # the sum of the time-out score, but we select the row based on the
        # exact score of the two teams.
        if (!is.na(sum(x))) {
          pt <- pull(
            filter(alt_wrangled_data, home == x[1], away == x[2]),
            point
          ) +
            1
          isTRUE(wrangled_data$time_out[[pt]]) &&
            isTRUE(
              wrangled_data$asked_time_out[[pt]] ==
                attr(time_outs[["home"]], "team")
            )
        } else {
          TRUE
        }
      }
    )

    expect_all_true(unlist(are_time_outs_home_correct))

    # Same as above but with a focus in on the "away" team
    are_time_outs_away_correct <- lapply(
      time_outs[["away"]],
      \(x) {
        if (!is.na(sum(x))) {
          pt <- pull(
            filter(alt_wrangled_data, away == x[1], home == x[2]),
            point
          ) +
            1
          isTRUE(wrangled_data$time_out[[pt]]) &&
            isTRUE(
              wrangled_data$asked_time_out[[pt]] ==
                attr(time_outs[["away"]], "team")
            )
        } else {
          TRUE
        }
      }
    )

    expect_all_true(unlist(are_time_outs_away_correct))
  })

  test_that("Check wrangling works as expected for substitutions", {
    # Check that the substitutions indicated in the "score_in" part of the
    # `substitutions` object are correctly reflect in the wrangled data. The
    # focus is on the "in" scores.
    are_sub_in_correct <- lapply(
      substitutions,
      \(x) {
        lapply(
          x$score_in,
          \(y) {
            # If substitution score is not NA, the sum of score + 1 should
            # correspond to the point where the substitution happened. In the
            # wrangled data, the point must be marked as with a substitution and
            # requesting team should be the team in the attribute.
            if (!is.na(sum(y))) {
              (isTRUE(wrangled_data$substitution[sum(y) + 1]) &&
                isTRUE(
                  wrangled_data$asked_substitution[sum(y) + 1] ==
                    attr(x, "team")
                ))
            } else {
              TRUE
            }
          }
        )
      }
    )

    expect_all_true(unlist(are_sub_in_correct))

    # Same as above but with a focus is on the "in" scores
    are_sub_out_correct <- lapply(
      substitutions,
      \(x) {
        lapply(
          x$score_out,
          \(y) {
            if (!is.na(sum(y))) {
              (isTRUE(wrangled_data$substitution[sum(y) + 1]) &&
                isTRUE(
                  wrangled_data$asked_substitution[sum(y) + 1] ==
                    attr(x, "team")
                ))
            } else {
              TRUE
            }
          }
        )
      }
    )

    expect_all_true(unlist(are_sub_out_correct))

    # Second check that the substitutions indicated in `substitutions` are
    # correctly reflected in the wrangled data. Here, we filter the wrangled
    # data using the exact substitution score instead that the point when it was
    # requested. The focus in on the "home" team.
    are_substitution_home_correct <- lapply(
      c(
        substitutions[["home"]][["score_in"]],
        substitutions[["home"]][["score_out"]]
      ),
      \(x) {
        # Here we do the same as above, but we do not select the point based on
        # the sum of the substitution score, but we select the row based on the
        # exact score of the two teams.
        if (!is.na(sum(x))) {
          pt <- pull(
            filter(alt_wrangled_data, home == x[1], away == x[2]),
            point
          ) +
            1
          isTRUE(wrangled_data$substitution[[pt]]) &&
            isTRUE(
              wrangled_data$asked_substitution[[pt]] ==
                attr(time_outs[["home"]], "team")
            )
        } else {
          TRUE
        }
      }
    )

    expect_all_true(unlist(are_substitution_home_correct))

    # Same as above but with a focus in on the "away" team
    are_substitution_away_correct <- lapply(
      c(
        substitutions[["away"]][["score_in"]],
        substitutions[["away"]][["score_out"]]
      ),
      \(x) {
        if (!is.na(sum(x))) {
          pt <- pull(
            filter(alt_wrangled_data, away == x[1], home == x[2]),
            point
          ) +
            1
          isTRUE(wrangled_data$substitution[[pt]]) &&
            isTRUE(
              wrangled_data$asked_substitution[[pt]] ==
                attr(time_outs[["away"]], "team")
            )
        } else {
          TRUE
        }
      }
    )

    expect_all_true(unlist(are_substitution_away_correct))
  })

  test_that("Check wrangling works as expected for scores and services", {
    # The point sequence is as expected
    expect_identical(
      wrangled_data$point,
      seq_len(max(scores$home, na.rm = TRUE) + max(scores$away, na.rm = TRUE))
    )

    # The number of rows in wrangled data is as expected
    expect_identical(
      as.integer(nrow(wrangled_data)),
      as.integer(
        max(scores$home, na.rm = TRUE) + max(scores$away, na.rm = TRUE)
      )
    )

    # Create the sequence of points served by the "serving" team
    service_serving <- scores[[serving]] - lag(scores[[serving]], default = 0)
    service_serving[1] <- service_serving[1] + 1
    if (max(unlist(scores)) == max(scores[[serving]])) {
      service_serving[length(service_serving)] <- service_serving[length(
        service_serving
      )] -
        1
    }
    # Create the sequence of points served by the "receiving" team
    service_receiving <- scores[[receiving]] -
      lag(scores[[receiving]], default = 0)
    if (max(unlist(scores)) == max(scores[[receiving]])) {
      service_receiving[length(service_receiving)] <- service_receiving[length(
        service_receiving
      )] -
        1
    }

    # Initiate an empty vector to create the sequence of serving team
    vec_service_team <- vector("character")
    # Filling-in the sequence of serving team
    for (i in seq_along(service_receiving)) {
      vec_service_team <- c(
        vec_service_team,
        rep("receiving", service_receiving[i])
      )
      if (length(service_serving) >= i) {
        vec_service_team <- c(
          vec_service_team,
          rep("serving", service_serving[i])
        )
      }
    }

    # Replace "serving" and "receiving" with the actual teams' name
    vec_service_team[vec_service_team == "serving"] <- serving
    vec_service_team[vec_service_team == "receiving"] <- receiving
    vec_service_team[vec_service_team == "home"] <- attr(scores$home, "team")
    vec_service_team[vec_service_team == "away"] <- attr(scores$away, "team")

    expect_identical(wrangled_data$service, vec_service_team)

    # Create the sequence of points won by the "serving" team
    won_serving <- scores[[serving]] - lag(scores[[serving]], default = 0) - 1
    won_serving[1] <- won_serving[1] + 1
    # Create the sequence of points won by the "receiving" team
    won_receiving <- (scores[[receiving]] -
      lag(scores[[receiving]], default = 0) -
      1)
    won_receiving[1] <- won_receiving[1] + 1

    # Initiate an empty vector to create the sequence of winning team
    vec_won_team <- vector("character")
    # Filling-in the sequence of winning team
    for (i in seq_along(won_receiving)) {
      if (i > 1 && length(won_serving) >= i) {
        vec_won_team <- c(
          vec_won_team,
          rep("receiving", won_receiving[i]),
          "serving"
        )
      } else if (i > 1 && length(won_serving) < i) {
        vec_won_team <- c(
          vec_won_team,
          rep("receiving", won_receiving[i])
        )
      }
      if (i < length(won_receiving)) {
        vec_won_team <- c(
          vec_won_team,
          rep("serving", won_serving[i]),
          "receiving"
        )
      } else if (i > length(won_serving)) {
        vec_won_team <- vec_won_team
      } else {
        vec_won_team <- c(vec_won_team, rep("serving", won_serving[i]))
      }
    }

    # Replace "serving" and "receiving" with the actual teams' name
    vec_won_team[vec_won_team == "serving"] <- serving
    vec_won_team[vec_won_team == "receiving"] <- receiving
    vec_won_team[vec_won_team == "home"] <- attr(scores$home, "team")
    vec_won_team[vec_won_team == "away"] <- attr(scores$away, "team")

    expect_identical(wrangled_data$won, vec_won_team)
  })

  test_that("Check wrangling works as expected for rotations", {
    # Create the sequence of rotations by the "serving" team
    rotation_serving <- scores[[serving]] - lag(scores[[serving]], default = 0)
    rotation_serving[1] <- rotation_serving[1] + 1
    if (max(unlist(scores)) == max(scores[[serving]])) {
      rotation_serving[length(rotation_serving)] <- rotation_serving[length(
        rotation_serving
      )] -
        1
    }
    # Create the sequence of rotations by the "receiving" team
    rotation_receiving <- scores[[receiving]] -
      lag(scores[[receiving]], default = 0)
    if (max(unlist(scores)) == max(scores[[receiving]])) {
      rotation_receiving[length(
        rotation_receiving
      )] <- rotation_receiving[length(
        rotation_receiving
      )] -
        1
    }

    # Initiate an empty vector to create the sequence of rotations for each team
    vec_rotation_serving_team <- vector("numeric")
    vec_rotation_receiving_team <- vector("numeric")
    # Filling-in the sequence of serving team
    for (i in seq_along(rotation_receiving)) {
      if (
        length(rotation_receiving) == length(rotation_serving) &&
          i == length(rotation_receiving)
      ) {
        vec_rotation_serving_team <- c(
          vec_rotation_serving_team,
          rep(i, rotation_serving[i])
        )
      } else if (i < length(rotation_receiving)) {
        vec_rotation_serving_team <- c(
          vec_rotation_serving_team,
          rep(i, rotation_serving[i]),
          rep(i, rotation_receiving[i + 1])
        )
      }

      if (
        length(rotation_receiving) == length(rotation_serving) &&
          i == length(rotation_receiving)
      ) {
        vec_rotation_receiving_team <- c(
          vec_rotation_receiving_team,
          rep(i, rotation_serving[i])
        )
      } else if (i < length(rotation_receiving)) {
        vec_rotation_receiving_team <- c(
          vec_rotation_receiving_team,
          rep(i, rotation_serving[i]),
          rep(i + 1, rotation_receiving[i + 1])
        )
      }
    }

    expect_identical(wrangled_data$rotations_s, vec_rotation_serving_team)
    expect_identical(wrangled_data$rotations_r, vec_rotation_receiving_team)
  })

  test_that("Check wrangling works as expected for players position", {
    # Create the sequence of rotations by the "serving" team
    rotation_serving <- scores[[serving]] - lag(scores[[serving]], default = 0)
    rotation_serving[1] <- rotation_serving[1] + 1
    if (max(unlist(scores)) == max(scores[[serving]])) {
      rotation_serving[length(rotation_serving)] <- rotation_serving[length(
        rotation_serving
      )] -
        1
    }
    # Create the sequence of rotations by the "receiving" team
    rotation_receiving <- scores[[receiving]] -
      lag(scores[[receiving]], default = 0)
    if (max(unlist(scores)) == max(scores[[receiving]])) {
      rotation_receiving[length(
        rotation_receiving
      )] <- rotation_receiving[length(
        rotation_receiving
      )] -
        1
    }

    # Initiate an empty vector to create the sequence of rotations for each team
    vec_rotation_serving_team <- vector("numeric")
    vec_rotation_receiving_team <- vector("numeric")
    # Filling-in the sequence of serving team
    for (i in seq_along(rotation_receiving)) {
      if (
        length(rotation_receiving) == length(rotation_serving) &&
          i == length(rotation_receiving)
      ) {
        vec_rotation_serving_team <- c(
          vec_rotation_serving_team,
          rep(i, rotation_serving[i])
        )
      } else if (i < length(rotation_receiving)) {
        vec_rotation_serving_team <- c(
          vec_rotation_serving_team,
          rep(i, rotation_serving[i]),
          rep(i, rotation_receiving[i + 1])
        )
      }

      if (
        length(rotation_receiving) == length(rotation_serving) &&
          i == length(rotation_receiving)
      ) {
        vec_rotation_receiving_team <- c(
          vec_rotation_receiving_team,
          rep(i, rotation_serving[i])
        )
      } else if (i < length(rotation_receiving)) {
        vec_rotation_receiving_team <- c(
          vec_rotation_receiving_team,
          rep(i, rotation_serving[i]),
          rep(i + 1, rotation_receiving[i + 1])
        )
      }
    }

    rotations_order <- c(1:6)
    vec_rotation_serving_team <- ((vec_rotation_serving_team - 1) %% 6) + 1

    vec_p_1_serving_team <- c(
      substitutions[[serving]]$rotation,
      substitutions[[serving]]$rotation
    )[1:6][vec_rotation_serving_team]

    vec_p_2_serving_team <- c(
      substitutions[[serving]]$rotation,
      substitutions[[serving]]$rotation
    )[2:7][vec_rotation_serving_team]

    vec_p_3_serving_team <- c(
      substitutions[[serving]]$rotation,
      substitutions[[serving]]$rotation
    )[3:8][vec_rotation_serving_team]

    vec_p_4_serving_team <- c(
      substitutions[[serving]]$rotation,
      substitutions[[serving]]$rotation
    )[4:9][vec_rotation_serving_team]

    vec_p_5_serving_team <- c(
      substitutions[[serving]]$rotation,
      substitutions[[serving]]$rotation
    )[5:10][vec_rotation_serving_team]

    vec_p_6_serving_team <- c(
      substitutions[[serving]]$rotation,
      substitutions[[serving]]$rotation
    )[6:11][vec_rotation_serving_team]

    serving_team_positions <- dplyr::as_tibble(
      cbind(
        point = seq_along(vec_p_1_serving_team),
        P1 = vec_p_1_serving_team,
        P2 = vec_p_2_serving_team,
        P3 = vec_p_3_serving_team,
        P4 = vec_p_4_serving_team,
        P5 = vec_p_5_serving_team,
        P6 = vec_p_6_serving_team
      )
    )

    for (i in 1:6) {
      sub_in <- NA
      sub_out <- NA
      if (!any(is.na(substitutions[[serving]]$score_in[[i]]))) {
        sub_in <- sum(substitutions[[serving]]$score_in[[i]]) + 1
        if (!any(is.na(substitutions[[serving]]$score_out[[i]]))) {
          sub_out <- sum(substitutions[[serving]]$score_out[[i]])
        } else {
          sub_out <- nrow(serving_team_positions)
        }

        serving_team_positions <- serving_team_positions |>
          dplyr::mutate(
            dplyr::across(
              !point,
              \(x) {
                dplyr::if_else(
                  x == substitutions[[serving]]$rotation[i] &
                    point >= sub_in &
                    point <= sub_out,
                  substitutions[[serving]]$sub[i],
                  x
                )
              }
            )
          )
      }
    }

    expect_identical(
      dplyr::select(wrangled_data, dplyr::starts_with(serving)) |>
        dplyr::rename_with(\(x) gsub(paste0(serving, "_"), "", x)),
      dplyr::select(serving_team_positions, !point)
    )

    vec_rotation_receiving_team <- ((vec_rotation_receiving_team - 1) %% 6) + 1

    vec_p_1_receiving_team <- c(
      substitutions[[receiving]]$rotation,
      substitutions[[receiving]]$rotation
    )[1:6][vec_rotation_receiving_team]

    vec_p_2_receiving_team <- c(
      substitutions[[receiving]]$rotation,
      substitutions[[receiving]]$rotation
    )[2:7][vec_rotation_receiving_team]

    vec_p_3_receiving_team <- c(
      substitutions[[receiving]]$rotation,
      substitutions[[receiving]]$rotation
    )[3:8][vec_rotation_receiving_team]

    vec_p_4_receiving_team <- c(
      substitutions[[receiving]]$rotation,
      substitutions[[receiving]]$rotation
    )[4:9][vec_rotation_receiving_team]

    vec_p_5_receiving_team <- c(
      substitutions[[receiving]]$rotation,
      substitutions[[receiving]]$rotation
    )[5:10][vec_rotation_receiving_team]

    vec_p_6_receiving_team <- c(
      substitutions[[receiving]]$rotation,
      substitutions[[receiving]]$rotation
    )[6:11][vec_rotation_receiving_team]

    receiving_team_positions <- dplyr::as_tibble(
      cbind(
        point = seq_along(vec_p_1_receiving_team),
        P1 = vec_p_1_receiving_team,
        P2= vec_p_2_receiving_team,
        P3 = vec_p_3_receiving_team,
        P4 = vec_p_4_receiving_team,
        P5 = vec_p_5_receiving_team,
        P6 = vec_p_6_receiving_team
      )
    )

    for (i in 1:6) {
      sub_in <- NA
      sub_out <- NA
      if (!any(is.na(substitutions[[receiving]]$score_in[[i]]))) {
        sub_in <- sum(substitutions[[receiving]]$score_in[[i]]) + 1
        if (!any(is.na(substitutions[[receiving]]$score_out[[i]]))) {
          sub_out <- sum(substitutions[[receiving]]$score_out[[i]])
        } else {
          sub_out <- nrow(receiving_team_positions)
        }

        receiving_team_positions <- receiving_team_positions |>
          dplyr::mutate(
            dplyr::across(
              !point,
              \(x) {
                dplyr::if_else(
                  x == substitutions[[receiving]]$rotation[i] &
                    point >= sub_in &
                    point <= sub_out,
                  substitutions[[receiving]]$sub[i],
                  x
                )
              }
            )
          )
      }
    }

    expect_identical(
      dplyr::select(wrangled_data, dplyr::starts_with(receiving)) |>
        dplyr::rename_with(\(x) gsub(paste0(receiving, "_"), "", x)),
      select(receiving_team_positions, !point)
    )
  })
}
