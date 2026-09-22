test_that("Test error when running the app", {
  expect_error(run_vb_game_encoder(), ".+file.+does.+not.+exist")
  expect_error(run_vb_game_encoder("abc.yaml"), ".+file.+does.+not.+exist")
  expect_error(run_vb_game_encoder("abc.csv"), ".+must.+indicate.+yaml")
})

test_that("Test check of season params", {
  yaml <- list(team = "", season = "", players = "")

  # Check missing fields throw an error
  expect_error(
    lapply(
      1:3,
      \(x) {
        check_season_params(yaml[-x], "NA.yaml")
      }
    ),
    ".+field.+is.+missing.+"
  )

  # Check extra fields throw an error
  yaml <- list(team = "", season = "", players = "", extra = "")
  expect_error(check_season_params(yaml, "NA.yaml"), ".+not.+a.+recognized.+")
  yaml <- list(team = "", season = "", players = "", extra1 = "", extra2 = "")
  expect_error(check_season_params(yaml, "NA.yaml"), ".+not.+a.+recognized.+")

  # Valid YAML that will be modified for further checks
  yaml_complete <- list(
    team = "",
    season = "",
    players = list(
      "Player 1" = list(id = 1, role = "Setter"),
      "Player 2" = list(id = 2, role = "Middle blocker"),
      "Player 3" = list(id = 3, role = "Opposite")
    )
  )

  # Check that missing id/role fields in players throw an error
  yaml <- yaml_complete
  yaml$players$`Player 1`$id <- NULL
  expect_error(check_season_params(yaml, "NA.yaml"), ".+has.+no.+id.+field.+")
  yaml <- yaml_complete
  yaml$players$`Player 1`$role <- NULL
  expect_error(check_season_params(yaml, "NA.yaml"), ".+has.+no.+role.+field.+")

  # Check that empty/NA id fields in players throw an error
  yaml <- yaml_complete
  yaml$players$`Player 1`$id <- ""
  expect_error(check_season_params(yaml, "NA.yaml"), "is.+NULL.+NA.+or.+empty")
  yaml <- yaml_complete
  yaml$players$`Player 1`$id <- NA
  expect_error(check_season_params(yaml, "NA.yaml"), "is.+NULL.+NA.+or.+empty")
  # Check that not unique id's in players throw an error
  yaml <- yaml_complete
  yaml$players$`Player 1`$id <- 2
  expect_error(check_season_params(yaml, "NA.yaml"), ".+id's.+are.+not.+unique")

  # Check that empty/NA role fields in players throw an error
  yaml <- yaml_complete
  yaml$players$`Player 1`$role <- ""
  expect_error(check_season_params(yaml, "NA.yaml"), "is.+NULL.+NA.+or.+empty")
  yaml <- yaml_complete
  yaml$players$`Player 1`$role <- NA
  expect_error(check_season_params(yaml, "NA.yaml"), "is.+NULL.+NA.+or.+empty")
  # Check that not valid roles in players throw an error
  yaml <- yaml_complete
  yaml$players$`Player 1`$role <- "other"
  expect_error(check_season_params(yaml, "NA.yaml"), ".+role.+is.+not.+valid.+")

  # Check that extra fields in players throw an error
  yaml <- yaml_complete
  yaml$players$`Player 1`$extra <- "other"
  expect_error(check_season_params(yaml, "NA.yaml"), ".+unrecognized.+field.+")

  # Test that valid YAML does not throw an error
  expect_no_error(check_season_params(yaml_complete))
})
