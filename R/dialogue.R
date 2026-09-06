#==============================================================================|
#                           ---- Dialogue utils ----
#==============================================================================|
# This script contains functions used to prepare messages displayed in the app
# via `shiny::showModal()`. The modals are displayed when validating a page and
# before moving to the next page.

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
        title = HTML("Go to the <i>Set", set, "</i> page?"),
        "No further modifications are possible after starting the next set.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(paste0("next_to_set_", set, "_confirm"), "Confirm"),
        ),
        easyClose = FALSE
      )
    )
    # When a 5th set is played, the modal dialogue includes a selector to
    # indicate which team started serving.
  } else if (set == 5) {
    showModal(
      modalDialog(
        title = HTML("Go to the <i>Set", set, "</i> page?"),
        "No further modifications are possible after starting the next set.",
        selectInput(
          "serving_set_5",
          "Which team started serving in set 5?",
          choices = c("", input$home_team, input$away_team)
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(paste0("next_to_set_", set, "_confirm"), "Confirm"),
        ),
        easyClose = FALSE
      )
    )
    # If `set` is set to NULL, it means this was the last set and a different
    # modal dialogue is thus displayed.
  } else if (set == "summary") {
    showModal(
      modalDialog(
        title = HTML("Go to the <i>Summary</i> page?"),
        "No further modifications are possible after reaching the summary ",
        "page.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("to_summary_confirm", "OK"),
        ),
        easyClose = FALSE
      )
    )
  }
}


#' Prepare message to display in modal dialogue when going to the player page
#'
#' @returns A call to `shiny::showModal()` with the message to display in the UI
#' when validating the game page and going to the player page.
#' @keywords internal

go_to_player_tab_message <- function() {
  showModal(
    modalDialog(
      title = tagList(HTML("Go to the <i>Player</i> page?")),
      tagList(
        HTML(
          "No further modifications are possible once the game details are ",
          "locked.",
          "<br><br>Click <code>Confirm</code> to lock the game details and ",
          "go to the next page.",
          "<br>Click <code>Cancel</code> to make modifications."
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          "go_to_player_tab",
          "Confirm",
          style = "background-color: #93c47d"
        ),
      ),
      easyClose = FALSE
    )
  )
}


#' Prepare message to display in modal dialogue when going to the first set page
#'
#' @returns A call to `shiny::showModal()` with the message to display in the UI
#' when validating the players in the game and going to the first set page.
#' @keywords internal

go_to_set_1_tab_message <- function() {
  showModal(
    modalDialog(
      title = tagList(HTML("Go to the <i>Set 1</i> page?")),
      tagList(
        HTML(
          "The list of players has been validated for the two teams. Once",
          "confirmed, no further modifications are possible.",
          "<br><br>Click <code>Confirm</code> to move to the <i>Set 1</i> ",
          "page.<br>Click <code>Cancel</code> to make modifications."
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          "go_to_set_1_tab",
          "Confirm",
          style = "background-color: #93c47d"
        ),
      ),
      easyClose = FALSE
    )
  )
}
