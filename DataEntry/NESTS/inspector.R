#' source("./DataEntry/NESTS/global.R")
#' source("./DataEntry/NESTS/inspector.R")
#'
#' dat = DBq('SELECT * FROM NESTS')
#' dat = dbq(q = 'SELECT * FROM NESTS', db = 'FIELD_2024_NOLAatDUMMERSEE', server = SERVER)
#' dat[, bird_inc := NA]
#' class(dat) = c(class(dat), 'NESTS')
#' ii = inspector(dat)
#' evalidators(ii)

inspector.NESTS <- function(dat, ...) {
  x <- copy(dat)
  x[, rowid := .I]

  list(
    # Mandatory values
    x[, .(author, nest, nest_state, date, time_appr, rowid)] |>
      is.na_validator(),
    x[nest_state == 'F', .(gps_id, gps_point, clutch_size, rowid)] |>
      is.na_validator("Mandatory when nest is found."),
    x[nest_state %in% c("C", "pD", "pP", "D", "P"), .(clutch_size, rowid)] |>
      is.na_validator("Mandatory when nest is checked."),
    # Re-inforce formats
    x[, .(date, rowid)] |> POSIXct_validator(),
    x[, .(time_appr, rowid)] |> hhmm_validator(),

    # Reinforce values (from existing db tables or lists)
    {
      z = x[, .(nest_state, bird_inc, behav)]

      v <- data.table(
        variable = names(z),
        set = c(
          list(c("F", "C", "I", "P", "pP", "D", "pD", "H", "notA")), # nest_state
          list(c('M', 'F', 'U', 'E')), # bird_inc
          list(c("INC", "DF", "BW", "O")) # behav
        )
      )

      is.element_validator(z, v)
    },

    x[, .(author, rowid)] |>
      is.element_validator(
        v = data.table(
          variable = "author",
          set = list(DBq("SELECT author ii FROM AUTHORS")$ii),
          reason = "entry not in the AUTHORS table"
        )
      ) |>
      try_validator(nam = "val1"),

    x[, .(gps_id, rowid)] |>
      is.element_validator(
        v = data.table(
          variable = "gps_id",
          set = list(1:20),
          reason = "GPS ID not in use"
        )
      ) |>
      try_validator(nam = "gps exists"),

    x[nest_state != "F", .(nest, rowid)] |>
      is.element_validator(
        v = data.table(
          variable = "nest",
          set = list(c(
            DBq("SELECT distinct nest FROM NESTS")$nest,
            x[nest_state == "F"]$nest
          ))
        ),
        reason = "nest_state is not F (found) but the nest does not exist in the NESTS table or is entered during this session!"
      ) |>
      try_validator(nam = "val3"),

    # COMBO should exist in CAPTURES
    {
      z <- x[, .(f_UL, f_LL, f_UR, f_LR, rowid)]
      z[,
        combo := make_combo(
          z,
          UL = "f_UL",
          LL = "f_LL",
          UR = "f_UR",
          LR = "f_LR"
        )
      ]
      z[combo == "~/~|~/~", combo := NA]

      is.element_validator(
        z,
        v = data.table(
          variable = "combo",
          set = list(
            DBq(
              "SELECT UL,LL, UR, LR FROM CAPTURES UNION SELECT UL,LL, UR, LR FROM CAPTURES_ARCHIVE"
            ) |>
              make_combo()
          )
        ),
        reason = "female combo does not exist in CAPTURES or CAPTURES_ARCHIVE."
      )
    },
    {
      z <- x[, .(m_UL, m_LL, m_UR, m_LR, rowid)]
      z[,
        combo := make_combo(
          z,
          UL = "m_UL",
          LL = "m_LL",
          UR = "m_UR",
          LR = "m_LR"
        )
      ]
      z[combo == "~/~|~/~", combo := NA]

      is.element_validator(
        z,
        v = data.table(
          variable = "combo",
          set = list(
            DBq(
              "SELECT UL,LL, UR, LR FROM CAPTURES UNION SELECT UL,LL, UR, LR FROM CAPTURES_ARCHIVE"
            ) |>
              make_combo()
          )
        ),
        reason = "male combo does not exist in CAPTURES or CAPTURES_ARCHIVE."
      )
    },

    # Values should be within given intervals
    x[, .(gps_point, rowid)] |>
      interval_validator(
        v = data.table(variable = "gps_point", lq = 1, uq = 999),
        "GPS waypoint is over 999?"
      ) |>
      try_validator(),
    x[!is.na(clutch_size), .(clutch_size)] |>
      interval_validator(
        v = data.table(variable = "clutch_size", lq = 0, uq = 4),
        reason = "Unusual clutch size."
      ) |>
      try_validator(nam = "val4"),
    # Values should be UNIQUE within their containing table
    x[nest_state == 'F', .(nest, rowid)] |>
      is.duplicate_validator(
        v = data.table(
          variable = "nest",
          set = list(DBq("SELECT distinct nest FROM NESTS")$nest)
        ),
        reason = "nest_state is  F(found) but the nest already exists in the NESTS table."
      ) |>
      try_validator(nam = "val5")
  )
}
