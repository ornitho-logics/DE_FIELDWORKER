#' shiny::runApp('./DataEntry/tags_eol', launch.browser = TRUE)

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
  x <- DBq("SELECT count(*) FROM tags_eol")

  data.table(
    N_entries = nrow(x)
  )
}


#! PARAMETERS
tableName = "tags_eol"
cnf = read.config(getOption("dbo.my.cnf"))[[SERVER]]
user = cnf$user
host = cnf$host
pwd = cnf$password
db = cnf$database

backupdir = paste0('~/backup_', db)


# UI elements
comments = column_comment(
  user = user,
  host = host,
  db = db,
  pwd = pwd,
  table = tableName
)
