#' shiny::runApp('./DataEntry/RESIGHTINGS', launch.browser = TRUE)

SERVER = "de_fieldworker" # dbo::my.cnf()


#! SETTINGS
sapply(
  c(
    "DataEntry", # remotes::install_github('mpio-be/DataEntry')
    "DataEntry.validation", # remotes::install_github('mpio-be/DataEntry.validation')
    "shinyjs",
    "shinyWidgets",
    "shinytoastr",
    "tableHTML",
    "glue",
    "stringr",
    "beR",
    "dbo",
    "configr"
  ),
  require,
  character.only = TRUE,
  quietly = TRUE
)

#* FUNCTIONS

DBq <- function(x) {
  con <- dbo::dbcon(server = SERVER, db = db)
  on.exit(DBI::dbDisconnect(con))

  o <- DBI::dbGetQuery(con, x)
  setDT(o)
  o
}

describeTable <- function() {
  x <- DBq(
    "SELECT UNIQUE concat(UL,LL,UR,LR) combo, pk FROM RESIGHTINGS 
              ORDER BY pk DESC"
  )

  data.table(
    N_entries = nrow(x),
    N_unique_combos = length(unique(x$combo)),
    last_entry = paste(x[1, pk], collapse = ", ")
  )
}


#! PARAMETERS
tableName = "RESIGHTINGS"
excludeColumns = c("pk", "nov")
n_empty_lines = 10
cnf = read.config(getOption("dbo.my.cnf"))[[SERVER]]
user = cnf$user
host = cnf$host
pwd = cnf$password
db = cnf$database


# UI elements
comments = column_comment(
  user = user,
  host = host,
  db = db,
  pwd = pwd,
  table = tableName,
  excludeColumns = excludeColumns
)

uitable =
  emptyFrame(
    user = user,
    host = host,
    db = db,
    pwd = pwd,
    table = tableName,
    excludeColumns = excludeColumns,
    n = n_empty_lines,
    preFilled = list(species = "NOLA")
    # preFilled      = list(UL = "M", UR = "W")
  ) |>
  rhandsontable(
    afterGetColHeader = js_hot_tippy_header(comments, "description")
  ) |>
  hot_cols(columnSorting = FALSE, manualColumnResize = TRUE) |>
  hot_col(col = "sex", type = "autocomplete", source = c("F", "M", "U")) |>
  hot_rows(fixedRowsTop = 1)
