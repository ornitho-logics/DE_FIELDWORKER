shinyServer(function(input, output, session) {
  # For debugging only (assign input values to global environment)
  observe({
    on.exit(assign('input', reactiveValuesToList(input), envir = .GlobalEnv))
  })

  # Header
  output$ref_date_text <- renderUI({
    ago = round(Sys.Date() - as.Date(input$refdate))

    if (ago == 0) {
      o = glue(
        "Reference date: {S(input$refdate,1)} today. <i>Todo-s are for tomorrow!</i>"
      )
    }

    if (ago > 0) {
      o = glue("Reference date: {S(input$refdate,2)} {abs(ago)} days ago.")
    }

    if (ago < 0) {
      o = glue("Reference date: {S(input$refdate,2)} {abs(ago)} days from now.")
    }

    HTML(o)
  })

  # Control bar: clock and hard drive status
  output$clock <- renderUI({
    invalidateLater(5000, session)
    glue('{HR()}{format(Sys.time(), "%d-%B %H:%M %Z")}') |> HTML()
  })

  output$hdd_state <- renderUI({
    dfsys_output()
  })

  # ENTER DATA
  output$new_data <- renderUI({
    startApp(
      hrefs = glue('../DataEntry/{dbtabs_entry}/'),
      labels = paste(icon("pencil"), dbtabs_entry)
    )
  })

  # GPS
  output$open_gps <- renderUI({
    startApp(
      hrefs = "../gpxui/",
      labels = p(icon("location-crosshairs"), "GPS upload/download")
    )
  })

  #* Database Interface
  output$open_db <- renderUI({
    startApp(
      hrefs = "../../../db_ui/field_db.php",
      labels = p(icon("database"), "Database interface")
    )
  })

  # VIEW DATA: Helper function to create DataTables

  output$AUTHORS_show <- TABLE_show("AUTHORS", session)
  output$CAPTURES_show <- TABLE_show("CAPTURES", session)
  output$CAPTURES_ARCHIVE_show <- TABLE_show("CAPTURES_ARCHIVE", session)
  output$RESIGHTINGS_show <- TABLE_show("RESIGHTINGS", session)
  output$CHICKS_show <- TABLE_show("CHICKS", session)
  output$NESTS_show <- TABLE_show("NESTS", session)
  output$EGGS_show <- TABLE_show("EGGS", session)
  output$SAMPLES_show <- TABLE_show("SAMPLES", session)
  output$COMBOS_show <- TABLE_show("COMBOS", session)
  output$tags_eol_show <- TABLE_show("tags_eol", session)

  #+ N: Reactive for NESTS data (only update when one of the nest-related tabs is active)
  N <- reactive({
    if (
      input$main %in% c("nests_map", "live_nest_map", "todo_list", "todo_map")
    ) {
      n <- tryCatch(
        NESTS(.refdate = input$refdate),
        error = function(e) {
          ErrToast(glue(
            "Error fetching nests data. Maybe there are no nests on {input$refdate}?"
          ))
          return(NULL)
        }
      )

      req(n)

      nolat <- n[is.na(lat)]
      if (nrow(nolat) > 0) {
        ErrToast(
          glue(
            "{paste(nolat$nest, collapse = ';')} without coordinates. Did you download all GPS units?"
          )
        )
      }
      n[, N := .N, nest]
      doubleEntry <- n[N > 1]
      if (nrow(doubleEntry) > 0) {
        WarnToast(
          glue(
            "Nests with inconsistent states: {paste(unique(doubleEntry$nest), collapse = ';')}"
          )
        )
      }
      n
    }
  })

  # STATIC NESTS MAP
  output$map_nests_show <- renderPlot({
    n <- N()
    req(n)
    map_nests(
      n[nest_state %in% input$nest_state],
      size = input$nest_size,
      grandTotal = nrow(n),
      .refdate = input$refdate
    )
  })

  output$map_nests_pdf <- downloadHandler(
    filename = "map_nests.pdf",

    content = function(file) {
      n <- N()
      req(n)
      cairo_pdf(file = file, width = 11, height = 8.5)

      print(
        map_nests(
          n[nest_state %in% input$nest_state],
          size = input$nest_size,
          grandTotal = nrow(n),
          .refdate = input$refdate
        )
      )
      dev.off()
    }
  )

  # DYNAMIC NESTS MAP
  leafmap <- leaflet_map()
  output$nest_dynmap_show <- renderLeaflet(leafmap)

  # Update dynamic map only when the "live_nest_map" tab is active
  observeEvent(input$main, {
    if (input$main == "live_nest_map") {
      n <- N()
      req(n)
      n <- st_as_sf(n[!is.na(lat)], coords = c("lon", "lat"), crs = 4326)
      if (nrow(n) > 0) {
        leafletProxy(mapId = "nest_dynmap_show") |>

          clearGroup("live_nest_markers") |>

          addCircleMarkers(
            group = "live_nest_markers",
            data = n,
            fillOpacity = 0.5,
            opacity = 0.5,
            radius = ~3,
            label = ~nest
          )
      }
    }
  })

  # TO-DO list
  output$todo_list_show <- DT::renderDataTable(
    {
      n <- N() |> extract_TODO(.refdate = input$refdate)
      req(n)
      o = n[, let(lat = NULL, lon = NULL)]
      o
    },
    server = FALSE,
    rownames = TRUE,
    escape = FALSE,
    extensions = c("Scroller", "Buttons"),
    options = list(
      dom = "Blfrtip",
      buttons = list(
        "copy",
        list(
          extend = "collection",
          buttons = c("excel", "pdf"),
          text = "Download"
        )
      ),
      scrollX = "600px",
      deferRender = TRUE,
      scrollY = 900,
      scroller = TRUE,
      searching = TRUE,
      columnDefs = list(list(className = "dt-center", targets = "_all"))
    ),
    class = c("compact", "stripe", "order-column", "hover")
  )

  # TO-DO MAP
  output$map_todo_show <- renderPlot({
    n <- N()
    req(n)
    map_todo(n, size = input$nest_size, .refdate = input$refdate)
  })

  output$map_todo_pdf <- downloadHandler(
    filename = "map_todo.pdf",
    content = function(file) {
      n <- N()
      req(n)

      cairo_pdf(file = file, width = 11, height = 8.5)
      map_todo(n, size = input$nest_size, .refdate = input$refdate)
      dev.off()
    }
  )

  # Overview
  output$overview_show <- renderPlot({
    # first egg
    x = ALL_EGGS()
    x[, year := factor(year(date))]
    x[, Date := update(min_pred_hatch_date, year = 2000) - 26 - 4]

    rdate = as.Date(input$refdate) |> update(year = 2000)

    g1 = ggplot(x, aes(x = Date, fill = year)) +
      geom_histogram(binwidth = 2, position = "dodge") +
      geom_vline(xintercept = rdate, col = "#001346", linewidth = 1.5) +
      ggtitle("First egg date") +
      xlab("") +
      ylab("") +
      scale_x_date(date_labels = "%b %d", date_breaks = "3 day") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14)) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        legend.position = "inside",
        legend.position.inside = c(1, 1),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = alpha("white", 0.7), color = NA)
      )

    # N nests by author
    x = DBq(
      "SELECT DISTINCT n.nest, a.author
                  FROM AUTHORS a
                  LEFT JOIN NESTS n ON n.author = a.author
                    WHERE n.nest_state = 'F' OR n.nest IS NULL",
      .db = db
    )
    x = x[, .N, author]
    x[N == 1, N := 0]

    g2 =
      ggplot(x, aes(x = fct_reorder(author, -N), y = N)) +
      geom_col(fill = "#ad7100", color = "black") +
      labs(x = "Author", y = "N nests") +
      theme_minimal(base_size = 14)

    # bypass validation rate
    x = DBq("SELECT author, nov FROM NESTS", .db = db)
    x = x[, .N, .(author, nov)] |> dcast(author ~ nov, value.var = "N")
    x[is.na(`1`), `1` := 0]
    x[, Bypass_validation_rate := `1` / (`1` + `0`)]

    g3 =
      ggplot(
        x,
        aes(
          x = fct_reorder(author, -Bypass_validation_rate),
          y = Bypass_validation_rate
        )
      ) +
      geom_col(fill = "#5b016d", color = "black") +
      labs(x = "Author", y = "Bypass validation rate") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme_minimal(base_size = 14)

    # hatching
    nn = NESTS(.refdate = input$refdate)
    pp = nn[, .(
      nest,
      date = as.Date(min_pred_hatch_date),
      hatching = "predicted"
    )]

    oo = CHICKS(.refdate = input$refdate)[, date := as.Date(date)]
    oo = oo[, .(date = min(as.Date(date))), by = nest][, hatching := "observed"]

    O = rbind(pp, oo)

    g4 =
      ggplot(O, aes(x = date, fill = hatching)) +
      geom_histogram(binwidth = 1, position = "dodge") +
      geom_vline(
        xintercept = as.Date(input$refdate),
        col = "#001346",
        linewidth = 1
      ) +
      ggtitle(glue(
        "Hatching: {nrow(oo)} nests hatched;{nrow(nn[nest_state%in% c('I', 'F', 'C')])} nests expected to hatch."
      )) +
      xlab("") +
      ylab("") +
      scale_x_date(date_labels = "%b %d", date_breaks = "2 day") +
      scale_fill_manual(values = c("#e76b05", "#03c9bf91")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14)) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        legend.position = "inside",
        legend.position.inside = c(1, 1),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = alpha("white", 0.7), color = NA)
      )

    (g1 + g4) / (g2 + g3)
  })

  # HATCHING
  output$hatching_est_plot <- renderPlot({
    require(mgcv)

    h = readRDS(hatch_pred_gam)

    pred =
      ggeffects::ggpredict(
        h,
        terms = c(
          glue("float_angle [{input$float_angle}]"),
          glue("surface [{input$float_height}]")
        )
      ) |>
      data.table()
    pred = pred[, .(predicted, conf.low, conf.high)]
    pred = melt(pred, measure.vars = names(pred))
    pred[, date_ := as.Date(input$refdate) + value]
    pred[, value := round(value, 1)]
    pred[,
      variable := factor(
        variable,
        labels = c(
          "Most likely [average]",
          "Earliest [95%CI-low]",
          "Latest [95%CI-high]"
        )
      )
    ]
    setnames(pred, c("", "Days to hatch", "Hatching date"))

    gtab = ggpubr::ggtexttable(
      pred,
      rows = NULL,
      theme = ggpubr::ttheme(base_size = 12)
    )

    g1 =
      ggplot(h$model, aes(x = float_angle, y = days_to_hatch)) +
      ggbeeswarm::geom_beeswarm(alpha = 0.5) +
      geom_smooth() +
      geom_vline(aes(xintercept = input$float_angle), color = '#df4306') +
      theme_minimal(base_size = 12)

    g2 =
      ggplot(h$model, aes(x = surface, y = days_to_hatch)) +
      ggbeeswarm::geom_beeswarm(alpha = 0.5) +
      geom_smooth(method = "loess", span = 1.0) +
      geom_vline(aes(xintercept = input$float_height), color = '#df4306') +
      theme_minimal(base_size = 12)

    gtab / (g1 + g2) + plot_layout(axes = "collect", heights = c(1, 2))
  })

  session$allowReconnect(TRUE)
})
