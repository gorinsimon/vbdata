#' Run the `vbdata` game encoding app
#'
#' @returns Nothing is return, used only for side-effect (running the app)
#' @export
#'
#' @examples
#' \dontrun{
#' run_vb_game_encoder()
#' }

run_vb_game_encoder <- function() {
  # Check that Quarto is available before running the app
  invisible(quarto::quarto_available(error = TRUE))

  # Locate the vbdata package files
  vbdata_files <- system.file(package = "vbdata")
  if (!nzchar(vbdata_files)) {
    stop("The folder with the files of `vbdata` cannot be found.")
  }

  # Create a temporary .qmd file where the vbdata encoding app will be copied
  # to avoid that the content is rendered where the package is installed.
  tmp_app_loc <- tempfile(fileext = ".qmd")

  # Copy the app to its temporary location
  invisible(file.copy(file.path(vbdata_files, "dashboard.qmd"), tmp_app_loc))
  # Move also the .qmd files that are included with the app because they need
  # to be available.
  invisible(file.copy(
    file.path(vbdata_files, "position_chunks"),
    recursive = TRUE,
    tempdir()
  ))
  invisible(file.copy(
    file.path(vbdata_files, "score_rotations_chunks"),
    recursive = TRUE,
    tempdir()
  ))

  # Render and serve the app
  quarto::quarto_serve(tmp_app_loc, render = TRUE)

  invisible()
}
