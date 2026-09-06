#' Create a validator for the time-out input
#'
#' @param set Integer indicating the set for which to set a validator for the
#' time-out input.
#'
#' @returns A list of validators for the time-outs of the indicated set.
#' @export

set_validator_time_out <- function(set) {
  validator <- shinyvalidate::InputValidator$new()
  lapply(
    c("home", "away"),
    \(t) {
      lapply(
        1:2,
        \(to) {
          val <- paste0("set_", set, "_", t, "_to_", to)
          validator$add_rule(val, shinyvalidate::sv_optional())
          validator$add_rule(
            val,
            \(v) {
              if (!grepl("^\\d\\d:\\d\\d$", v)) {
                "Wrong format (must be XX:XX)"
              }
            }
          )
        }
      )
    }
  )
  return(validator)
}

#' Create a validator for the substitutions input
#'
#' @param set Integer indicating the set for which to set a validator for the
#' substitutions input.
#'
#' @returns A list of validators for the substitutions of the indicated set.
#' @export

set_validator_substitution <- function(set) {
  validator <- shinyvalidate::InputValidator$new()
  lapply(
    c("home", "away"),
    \(t) {
      lapply(
        1:6,
        \(p) {
          lapply(
            1:2,
            \(n) {
              val <- paste0("set_", set, "_", t, "_p", p, "_sub_", n, "_score")
              validator$add_rule(val, shinyvalidate::sv_optional())
              validator$add_rule(
                val,
                \(v) {
                  if (!grepl("^\\d\\d:\\d\\d$", v)) {
                    "Wrong format (must be XX:XX)"
                  }
                }
              )
            }
          )
        }
      )
    }
  )
  return(validator)
}
