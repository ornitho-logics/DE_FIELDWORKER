#' source("./DataEntry/RESIGHTINGS/global.R")
#' source("./DataEntry/RESIGHTINGS/inspector.R")
#'
#' dat = DBq('SELECT * FROM RESIGHTINGS')
#' class(dat) = c(class(dat), 'RESIGHTINGS')
#' ii = inspector(dat)
#' evalidators(ii)

inspector.RESIGHTINGS <- function(dat, ...) {
  x <- copy(dat)
  x[, rowid := .I]

  list(
    # Mandatory values
    x[, .(author, gps_id, gps_point_start, rowid)] |>
      is.na_validator(),
    # Reinforce values (from existing db tables or lists)

    x[, .(author, rowid)] |>
      is.element_validator(
        v = data.table(
          variable = "author",
          set = list(DBq("SELECT author ii FROM AUTHORS")$ii),
          reason = 'entry not in the AUTHORS table'
        )
      ),

    x[, .(gps_id, rowid)] |>
      is.element_validator(
        v = data.table(
          variable = "gps_id",
          set = list(1:10),
          reason = "GPS ID not in use"
        )
      ),

    # COMBO should exist in CAPTURES
    {
      z <- x[, .(UL, LL, UR, LR, rowid)]
      z[, combo := make_combo(z)]
      z[combo == "~/~|~/~", combo := NA]

      is.element_validator(
        z,
        v = data.table(
          variable = "combo",

          set = list(
            DBq("SELECT UL,LL, UR, LR FROM CAPTURES_ALL") |> make_combo()
          )
        ),
        reason = "combo does not exist in CAPTURES (this or last seasons). "
      )
    }
  )
}
