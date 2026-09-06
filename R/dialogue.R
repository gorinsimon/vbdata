#==============================================================================|
#                           ---- Dialogue utils ----
#==============================================================================|
# This script contains functions used to prepare messages displayed in the app
# via `shiny::showModal()`. The modals are mainly displayed at the end of a set
# when validating its input.

#' Prepare message to display in modal dialogue when changing set
#'
#' @param set An integer with the set number that is complete.
#' @param input The list of input from the Shiny app.
#'
#' @returns A call to `shiny::showModal()` with the message to display in the UI
#' when changing set.
#' @keywords internal

next_set_message <- function(set, input = NULL) {
  # The modal dialogue displayed for set 1 to 4
  if (set %in% 1:4) {
    showModal(
      modalDialog(
        title = paste0("Proceed to set ", set, "?"),
        "No further modifications are possible after starting the next set.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(paste0("next_to_set_", set, "_confirm"), "OK"),
        ),
        easyClose = TRUE
      )
    )
    # When a 5th set is played, the modal dialogue includes a selector to
    # indicate which team started serving.
  } else if (set == 5) {
    showModal(
      modalDialog(
        title = paste0("Proceed to set ", set, "?"),
        "No further modifications are possible after starting the next set.",
        selectInput(
          "serving_set_5",
          "Which team started serving in set 5?",
          choices = c("", input$home_team, input$away_team)
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(paste0("next_to_set_", set, "_confirm"), "OK"),
        ),
        easyClose = TRUE
      )
    )
    # If `set` is set to NULL, it means this was the last set and a different
    # modal dialogue is thus displayed.
  } else if (set == "summary") {
    showModal(
      modalDialog(
        title = paste0("Proceed to summary page?"),
        "No further modifications are possible after starting the next set.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("to_summary_confirm", "OK"),
        ),
        easyClose = TRUE
      )
    )
  }
}
