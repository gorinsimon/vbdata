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
}
