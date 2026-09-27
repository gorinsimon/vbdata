new_season_project <- function(path, ...) {
  # Extract the options set in the Project Wizard
  dots <- list(...)

  # Try to set-up the project (and remove all content if it fails)
  tryCatch(
    {
      # Create the project (without opening it) and set it as active project
      usethis::create_project(path, open = FALSE, rstudio = TRUE)
      usethis::proj_set(path, force = TRUE)
      proj_path <- usethis::proj_get()

      # Delete the "R" folder created by default by `usethis` and create a
      # "game" folder where the games data will be saved.
      unlink(file.path(proj_path, "R"))
      dir.create(file.path(proj_path, "games"))

      # Write the YAML file with season parameters
      yaml::write_yaml(
        list(
          team = dots[["user_team"]],
          season = dots[["season"]],
          players = list("Player 1" = list(role = "", id = ""))
        ),
        file = file.path(proj_path, "season.yaml")
      )

      # Create a main script with just the code to run the app
      writeLines(
        paste0(
          "# Start the game encoder app\n",
          "vbdata::run_vb_game_encoder(params_path = \"season.yaml\")"
        ),
        file.path(proj_path, "main.R")
      )
    },
    error = function(e) {
      message(paste("Error:", e$message))
      unlink(proj_path, recursive = TRUE)
      message(
        "An error occured during the creation of the season project ",
        sprintf("and the directory located at `%s` was deleted", path)
      )
    }
  )
}
