#+ NOTE:
#' list.files('./R', full.names = TRUE) |> lapply(source) |> invisible(); source('global.R')
#' ss = function() shiny::runApp(launch.browser = TRUE)

SERVER = "de_fieldworker" # dbo::my.cnf()


#! PACKAGES
sapply(
  c(
    "dbo",
    "sf",
    "data.table",
    "stringr",
    "forcats",
    "zip",
    "glue",
    "ggplot2",
    "ggrepel",
    "ggtext",
    "patchwork",
    "shinyWidgets",
    "bs4Dash",
    "DT",
    "leaflet",
    "leafem",
    "leaflet.extras"
  ),
  require,
  character.only = TRUE,
  quietly = TRUE
)

# other packages: wadeR, configr,shinycssloaders, ggpubr

# External data
data(OsterFeinerMoor, package = "wadeR")


#- VARIABLES

cnf = configr::read.config(getOption("dbo.my.cnf"))[[SERVER]]
user = cnf$user
host = cnf$host
pwd = cnf$password
db = cnf$database
dbbasenam = 'NOLAatDUMMERSEE'
years = c(2024, 2025, 2026)

app_nam = "DE_FIELDWORKER"
dbtabs_entry =
  c(
    "AUTHORS",
    "CAPTURES",
    "RESIGHTINGS",
    "CHICKS",
    "NESTS",
    "EGGS",
    "SAMPLES",
    "tags_eol",
    "todo_white_list"
  )
dbtabs_view =
  c(
    "AUTHORS",
    "CAPTURES",
    "CAPTURES_ARCHIVE",
    "RESIGHTINGS",
    "CHICKS",
    "NESTS",
    "EGGS",
    "SAMPLES",
    "tags_eol",
    "COMBOS"
  )
species = c("NOLA", "REDS")
studySiteCenter = c(x = 8.341151, y = 52.55065)


hatch_pred_gam = "./data/gam_float_to_hach.rds"


nest_state_cols = c(
  "F" = "#00815f",
  "C" = "#E69F00",
  "I" = "#fff023",
  "pP" = "#A50026",
  "P" = "#6405a3",
  "pD" = "#CC79A7",
  "D" = "#6A51A3",
  "H" = "#1aa9fc",
  "notA" = "#4b4b4b"
)

todo_cols = c(
  "catch M" = "#0745cc",
  "catch F" = "#f33b0c",
  "catch any" = "#f38c38"
)

todo_symbols = c(
  "nest check" = 2,
  "hatch check" = 5
)


#! OPTIONS
options(shiny.autoreload = TRUE)
options(dbo.tz = "Europe/Berlin")

options(ggrepel.max.overlaps = Inf)

#! UI DEFAULTS

ver = "3.0.0"
apptitle = "DÜMMER-SEE"
pagetitle = apptitle
set_capturedDaysAgo = 3
set_seenDaysAgo = 3
