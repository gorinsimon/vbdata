set_validator_time_out <- function(set) {
  validator <- InputValidator$new()
  lapply(
    c("home", "away"),
    \(t) {
      lapply(
        1:2,
        \(to) {
          val <- paste0("set_", set, "_", t, "_to_", to)
          validator$add_rule(val, sv_optional())
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

set_validator_substitution <- function(set) {
  validator <- InputValidator$new()
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
              validator$add_rule(val, sv_optional())
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
