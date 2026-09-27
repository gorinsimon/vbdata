#' Run the `vbdata` game encoding app
#'
#' @param params_path The path to the YAML file where the season parameters
#' (team name, season, players details) are stored. The default is set to
#' "season.yaml", assuming the file is located at the root of the project.
#'
#' @returns Nothing is return, used only for side-effect (running the app)
#' @export
#'
#' @examples
#' \dontrun{
#' run_vb_game_encoder()
#' }

run_vb_game_encoder <- function(params_path = "season.yaml") {
  # Check that Quarto is available before running the app
  invisible(quarto::quarto_available(error = TRUE))

  # Check `params`
  if (!is.null(params_path) && !grepl("^.+\\.yaml", params_path)) {
    stop(
      "`params_path` must indicate the path to a file with a `.yaml` extension."
    )
  } else if (!file.exists(params_path)) {
    stop(sprintf("The file '%s' does not exist.", params_path))
  } else {
    params <- yaml::read_yaml(params_path)
    check_season_params(params, params_path)
  }

  # Locate the vbdata package files
  vbdata_files <- system.file(package = "vbdata")
  if (!nzchar(vbdata_files)) {
    stop("The folder with the files of `vbdata` cannot be found.")
  }

  # Create a temporary .qmd file where the vbdata encoding app will be copied
  # to avoid that the content is rendered where the package is installed.
  vbd_tmp_dir <- file.path(tempdir(), "vbdata_dashboard")
  dir.create(vbd_tmp_dir, showWarnings = FALSE)
  tmp_app_loc <- tempfile(tmpdir = vbd_tmp_dir, fileext = ".qmd")

  # Copy the app to its temporary location
  invisible(file.copy(
    file.path(vbdata_files, "dashboard.qmd"),
    vbd_tmp_dir,
    overwrite = TRUE
  ))
  invisible(file.copy(params_path, vbd_tmp_dir, overwrite = TRUE))
  # Move also the .qmd files that are included with the app because they need
  # to be available.
  invisible(file.copy(
    file.path(vbdata_files, "position_chunks"),
    recursive = TRUE,
    vbd_tmp_dir
  ))
  invisible(file.copy(
    file.path(vbdata_files, "score_rotations_chunks"),
    recursive = TRUE,
    vbd_tmp_dir
  ))

  # Render and serve the app
  quarto::quarto_serve(file.path(vbd_tmp_dir, "dashboard.qmd"), render = TRUE)

  invisible()
}

#' @keywords internal
check_season_params <- function(params, yaml_path) {
  # Check that expected fields are present in the season parameters
  expected_names <- c("team", "season", "players", "division")
  name_is_missing <- !(expected_names %in% names(params))
  if (any(name_is_missing)) {
    n_miss <- sum(name_is_missing)
    stop(
      sprintf("The following field%s", ifelse(n_miss > 1, "s are ", " is ")),
      sprintf("missing in '%s': \"", yaml_path),
      paste0(expected_names[name_is_missing], collapse = "\", \""),
      "\"."
    )
  }

  # Check that there is no extra fields in the season parameters
  name_is_extra <- !(names(params) %in% expected_names)
  if (any(name_is_extra)) {
    n_extra <- sum(name_is_extra)
    stop(
      sprintf(
        "The following field%s in '%s' %s recognized field%s: \"",
        ifelse(n_extra > 1, "s", ""),
        yaml_path,
        ifelse(n_extra > 1, "are not", "is not a"),
        ifelse(n_extra > 1, "s", "")
      ),
      paste0(names(params)[name_is_extra], collapse = "\", \""),
      "\".\nOnly the fields \"",
      paste0(expected_names, collapse = "\", \""),
      "\" are recognized."
    )
  }

  # The season field must follow a specific format ("YYYY - YYYY")
  if (!grepl("^\\d{4}\\s-\\s\\d{4}$", params$season)) {
    stop(
      "The field `season` is not properly formatted. It should follow the ",
      "format: \"YYYY - YYYY\"."
    )
  }
   # Extract the players for the season YAML then check their formatting
  players <- params$players
  check_player_params(players, yaml_path)
  check_player_id(players, yaml_path)
  check_player_role(players, yaml_path)
  check_player_number(players, yaml_path)
}

#' @keywords internal
check_player_params <- function(players, yaml_path) {
  player_names <- lapply(players, \(x) names(x))

  has_player_role <- mapply(\(x) "role" %in% x, player_names)
  if (!all(has_player_role)) {
    stop(
      sprintf(
        "The following player%s no \"role\" field in '%s': \"",
        ifelse(sum(!has_player_role) > 1, "s have", " has"),
        yaml_path
      ),
      paste0(names(player_names)[!has_player_role], collapse = "\", \""),
      "\"."
    )
  }

  has_player_id <- mapply(\(x) "id" %in% x, player_names)
  if (!all(has_player_id)) {
    stop(
      sprintf(
        "The following player%s no \"id\" field in '%s': \"",
        ifelse(sum(!has_player_id) > 1, "s have", " has"),
        yaml_path
      ),
      paste0(names(player_names)[!has_player_id], collapse = "\", \""),
      "\"."
    )
  }

  has_player_number <- mapply(\(x) "number" %in% x, player_names)
  if (!all(has_player_number)) {
    stop(
      sprintf(
        "The following player%s no \"number\" field in '%s': \"",
        ifelse(sum(!has_player_number) > 1, "s have", " has"),
        yaml_path
      ),
      paste0(names(player_names)[!has_player_number], collapse = "\", \""),
      "\"."
    )
  }

  has_player_extra <- mapply(
    \(x) !all(x %in% c("role", "id", "number")),
    player_names
  )
  if (any(has_player_extra)) {
    stop(
      sprintf(
        "The following player%s in '%s' %s unrecognized field(s): \"",
        ifelse(sum(has_player_extra) > 1, "s", ""),
        yaml_path,
        ifelse(sum(has_player_extra) > 1, "have", "has")
      ),
      paste0(names(player_names)[has_player_extra], collapse = "\", \""),
      "\".\nOnly the fields \"role\" and \"id\" are recognized."
    )
  }
}

#' @keywords internal
check_player_id <- function(players, yaml_path) {
  players_id <- mapply(\(x) x[["id"]], players)
  has_null_id <- mapply(is.null, players_id)
  has_empty_id <- !nzchar(players_id)
  has_na_id <- is.na(players_id)
  any_incorrect_id <- has_null_id | has_empty_id | has_na_id

  if (any(any_incorrect_id)) {
    stop(
      sprintf(
        "In '%s', the `id` field of the following player%s is ",
        yaml_path,
        ifelse(sum(any_incorrect_id) > 1, "s", "")
      ),
      "NULL, NA, or empty: \"",
      paste0(names(players)[any_incorrect_id], collapse = "\", \""),
      "\".\nFor each player, the `id` field must be populated with a unique ",
      "numeric id."
    )
  }

  if (length(unique(players_id)) < length(players_id)) {
    stop(
      sprintf("The player id's in '%s' are not unique.", yaml_path)
    )
  }
}

#' @keywords internal
check_player_number <- function(players, yaml_path) {
  players_num <- mapply(\(x) x[["number"]], players)
  has_null_num <- mapply(is.null, players_num)
  has_not_num <- !is.numeric(players_num)
  has_na_num <- is.na(players_num)
  any_incorrect_id <- has_null_num | has_not_num | has_na_num

  if (any(any_incorrect_id)) {
    stop(
      sprintf(
        "In '%s', the `number` field of the following player%s is ",
        yaml_path,
        ifelse(sum(any_incorrect_id) > 1, "s", "")
      ),
      "NULL, NA, or not a number: \"",
      paste0(names(players)[any_incorrect_id], collapse = "\", \""),
      "\".\nFor each player, the `number` field must be populated with a ",
      "number."
    )
  }
}

#' @keywords internal
check_player_role <- function(players, yaml_path) {
  players_role <- mapply(\(x) x[["role"]], players)
  has_null_role <- mapply(is.null, players_role)
  has_empty_role <- !nzchar(players_role)
  has_na_role <- is.na(players_role)
  any_incorrect_role <- has_null_role | has_empty_role | has_na_role

  if (any(any_incorrect_role)) {
    stop(
      sprintf(
        "In '%s', the `role` field of the following player%s is ",
        yaml_path,
        ifelse(sum(any_incorrect_role) > 1, "s", "")
      ),
      "NULL, NA, or empty: \"",
      paste0(names(players)[any_incorrect_role], collapse = "\", \""),
      "\".\nFor each player, the `role` field must be populated with a valid ",
      "role (\"Setter\", \"Outside hitter\", \"Middle blocker\", ",
      "\"Opposite\", or \"Libero\")."
    )
  }

  roles <- c(
    "Setter",
    "Outside hitter",
    "Middle blocker",
    "Opposite",
    "Libero"
  )

  has_valid_role <- mapply(\(x) x %in% roles, players_role)

  if (any(!has_valid_role)) {
    stop(
      sprintf(
        "In '%s', the `role` field of the following player%s is ",
        yaml_path,
        ifelse(sum(!has_valid_role) > 1, "s", "")
      ),
      "not valid: \"",
      paste0(names(players)[!has_valid_role], collapse = "\", \""),
      "\".\nOnly the following roles are accepted: ",
      "\"Setter\", \"Outside hitter\", \"Middle blocker\", ",
      "\"Opposite\", or \"Libero\"."
    )
  }
}
