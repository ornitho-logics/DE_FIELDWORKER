#' source("./DataEntry/CAPTURES/global.R")
#' source("./DataEntry/CAPTURES/inspector.R")
#'
#' dat = DBq('SELECT * FROM CAPTURES')
#' class(dat) = c(class(dat), 'CAPTURES')
#' ii = inspector(dat)
#' evalidators(ii)
#'
#'
# TODO: add combo validation

inspector.CAPTURES <- function(dat, ...) {
  x = copy(dat)
  x[, rowid := .I]

  list(
    # Mandatory values
    x[, .(
      species,
      date,
      cap_start,
      caught,
      released,
      capture_meth,
      form_id,
      author,
      ID,
      UL,
      UR,
      LR,
      recapture,
      tarsus,
      culmen,
      total_head,
      wing,
      weight,
      wt_w_tag,
      sex_observed,
      rowid
    )] |>
      is.na_validator() |>
      try_validator(nam = 1),

    x[recapture == 0, .(tarsus, culmen, total_head, wing, crest, rowid)] |>
      is.na_validator("Mandatory at first capture.") |>
      try_validator(nam = 2),

    x[is.na(nest), .(gps_id, gps_point, rowid)] |>
      is.na_validator() |>
      try_validator(nam = 'gps'),

    # Re-enforce formats
    x[, .(date, rowid)] |>
      POSIXct_validator() |>
      try_validator(nam = 3),
    # Reinforce values (from existing db tables or lists)
    {
      z = x[, .(capture_meth, tag_action, tagType, sex_observed, recapture)]

      v <- data.table(
        variable = names(z),
        set = c(
          list(c("T", "M", "O")), # capture_meth
          list(c("on", "off")), # tag_action
          list(c("D", "G")), # tagType
          list(c("F", "M", "U")), # sex_observed
          list(c(1, 0)) # recapture
        )
      )

      is.element_validator(z, v)
    } |>
      try_validator(nam = "fixed values"),

    x[, .(author, rowid)] |>
      is.element_validator(
        v = data.table(
          variable = "author",
          set = list(DBq("SELECT author ii FROM AUTHORS")$ii)
        ),
        reason = 'entry not in the AUTHORS table'
      ) |>
      try_validator(nam = "authors"),

    x[recapture == 0, .(tagID, rowid)] |>
      is.element_validator(
        v = data.table(
          variable = "tagID",
          set = list(
            3624:3653
          ),
          reason = 'invalid new tagID'
        )
      ) |>
      try_validator(nam = 'new_tagID'),

    x[, .(gps_id, rowid)] |>
      is.element_validator(
        v = data.table(
          variable = "gps_id",
          set = list(1:15)
        ),
        reason = "GPS ID not in use"
      ) |>
      try_validator(nam = 6),

    # time order
    x[
      cap_start >= caught,
      .(rowid, variable = "cap_start,caught", reason = 'cap_start >= caught')
    ],

    x[
      caught >= released,
      .(rowid, variable = "caught,released", reason = 'caught >= released')
    ],

    # morphometrics
    x[
      sex_observed == 'F',
      .(culmen, total_head, tarsus, wing, weight, rowid)
    ] |>
      interval_validator(
        v = fread(
          "    
      variable      lq     uq
        tarsus   43.76  56.81
        culmen   21.66  44.18
    total_head   57.76  69.51
          crest  36.00  79.00
          wing  201.00 237.00
        weight  174.30 269.10"
        ),
        reason = "Female measurement out of the typical range."
      ) |>
      try_validator(nam = 'female morphometrics'),

    x[
      sex_observed == 'M',
      .(culmen, total_head, tarsus, wing, weight, rowid)
    ] |>
      interval_validator(
        v = fread(
          "    
      variable      lq     uq
        tarsus   42.64  87.85
        culmen   20.89  26.49
    total_head   57.17  63.70
          crest  69.00 117.00
          wing  218.00 242.00
        weight  187.90 241.30"
        ),
        reason = "Male measurement out of the typical range."
      ) |>
      try_validator(nam = 'male morphometrics'),

    # Values should be UNIQUE within their containing table
    x[recapture == 0 & !is.na(ID), .(ID, rowid)] |>
      is.duplicate_validator(
        v = data.table(
          variable = "ID",
          set = list(DBq("SELECT distinct ID FROM CAPTURES")$ID)
        ),
        reason = "Metal band already in use! Is this a recapture?"
      ) |>
      try_validator(nam = "metal band"),

    x[recapture == 1, .(tagID, rowid)] |>
      is.duplicate_validator(
        v = data.table(
          variable = "tagID",
          set = list(
            DBq(
              "SELECT distinct tagID FROM CAPTURES_ARCHIVE where tagID is not NULL and CHAR_LENGTH(tagID) >0"
            )$tagID
          )
        ),
        reason = "We could not find this tagID in the existing data!"
      ) |>
      try_validator(nam = "old_tagID"),

    x[recapture == 1, .(ID, rowid)] |>
      is.duplicate_validator(
        v = data.table(
          variable = "ID",
          set = list(
            DBq(
              "SELECT distinct ID FROM CAPTURES_ARCHIVE"
            )$ID
          )
        ),
        reason = "We could not find this ID in the old capture data!"
      ) |>
      try_validator(nam = "old_ID")
  )
}
