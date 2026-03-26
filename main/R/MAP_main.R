map_empty <- function(b = OsterFeinerMoor) {
  ggplot() +
    geom_sf(data = b, fill = NA, color = "#cacaca") +
    theme_minimal() +
    theme(
      panel.border = element_blank(),
      panel.grid = element_line(colour = "#ffffff", linewidth = 0),
      panel.spacing = unit(c(0, 0, 0, 0), "cm"),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    ) +
    ggspatial::annotation_scale(
      location = "bl",
      width_hint = 0.2,
      tick_height = 0.1,
      height = unit(0.1, "cm")
    )
}

#' x = NESTS()
#' x = NESTS(DB = "FIELD_2025_NOLAatDUMMERSEE", .refdate = "2025-04-26")[!is.na(lat)]
#' map_nests(x)
map_nests <- function(
  x,
  size = 2.5,
  grandTotal = nrow(x),
  .refdate = input$refdate
) {
  if (!exists('input', envir = .GlobalEnv)) {
    .refdate = as.character(Sys.Date())
    warning('input not found, using ', Sys.Date() |> dQuote(), ' as reference.')
  }

  g = map_empty()

  x[, nest := str_remove(nest, "^L")]

  x[!is.na(F_cap) & is.na(M_cap), capt := '♀']
  x[is.na(F_cap) & !is.na(M_cap), capt := '♂']
  x[!is.na(F_cap) & !is.na(M_cap), capt := "⚤"]

  x[, LAB := glue_data(.SD, "{nest}", .na = "")]
  x[!is.na(capt), LAB := glue_data(.SD, "{nest}*{{bold('{capt}')}}")]

  g =
    g +
    ggtext::geom_richtext(
      aes(
        x = -Inf,
        y = Inf,
        label = glue(
          "
      Reference date: <span style='color:#e55a34'>{.refdate}</span>   <br>
      **{grandTotal}** nests, 
      **{nrow(x)}** shown, 
      **{nrow(x[collected == 1])}** collected (◈) clutches <br>
      Text: Nest ID ♀(female caugth), ♂(male caught),⚤ (male and female caught) <br>
      <span style='color: red;'>&#x2022;</span>: last check > 3 days ago."
        )
      ),
      fill = NA,
      label.color = NA,
      hjust = -0.05,
      vjust = 1.05,
      size = 3
    ) +

    geom_point(data = x, aes(lon, lat, color = nest_state), size = size) +
    geom_point(
      data = x[lastCheck > 3],
      aes(lon, lat),
      size = 0.8,
      col = 'red',
      inherit.aes = FALSE
    ) +
    geom_point(
      data = x[collected == 1],
      aes(lon, lat),
      size = size + 0.1,
      shape = 5,
      inherit.aes = FALSE
    ) +
    geom_text_repel(
      data = x,
      aes(lon, lat, label = LAB),
      size = size,
      parse = TRUE
    ) +

    scale_color_manual(values = nest_state_cols, name = "Last\nstate") +
    scale_shape_manual(values = 5, name = "coll") +

    xlab(NULL) +
    ylab(NULL) +

    guides(
      color = guide_legend(ncol = 1),
      shape = guide_legend(ncol = 1)
    ) +

    theme(
      legend.position = "left",
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.box = "vertical",
      legend.text = element_text(size = 10),
      legend.margin = margin(0, 0, 0, 0),
      legend.spacing.x = unit(0, "cm"),
      legend.spacing.y = unit(0, "cm")
    )

  print(g)
}

#' n = NESTS()
#' map_todo(n)
map_todo <- function(n, size = 2.5, .refdate = "yyyy-mm-dd") {
  x = extract_TODO(n, .refdate)
  x = x[, todo_yn := !(is.na(todo_catch) & is.na(todo_check))]

  x[,
    lab := glue_data(
      .SD,
      "{str_remove(nest, '^L')}^{{bold('{round(min_days_to_hatch)}')}}"
    )
  ]
  # days to hatch is only relevant when catching
  x[(is.na(todo_catch)), lab := str_remove(nest, "^L")]

  g =
    map_empty() +

    ggtext::geom_richtext(
      aes(
        x = -Inf,
        y = Inf,
        label = glue(
          "
        Reference date: <span style='color:#e55a34'>{.refdate}</span>   <br>
        Catch at **{nrow(x[!is.na(todo_catch)])}** nests; 
        Check **{nrow(x[!is.na(todo_check)])}** nests. <br>
        Text: Nest ID<sup><strong>days to hatch</strong></sup>
        "
        )
      ),
      fill = NA,
      label.color = NA,
      hjust = -0.05,
      vjust = 1.05,
      size = 4
    ) +

    geom_text_repel(
      data = x,
      aes(lon, lat, label = lab, color = todo_yn),
      size = size * 0.8,
      parse = TRUE,
      show.legend = FALSE
    ) +
    scale_color_manual(values = c("grey", "black")) +

    ggnewscale::new_scale_colour() +

    geom_point(
      data = x,
      aes(lon, lat, color = todo_catch),
      size = size,
      alpha = 0.7
    ) +

    geom_point(
      data = x[!is.na(todo_check)],
      aes(lon, lat, shape = todo_check),
      size = size
    ) +

    guides(
      color = guide_legend(ncol = 1),
      shape = guide_legend(ncol = 1)
    ) +

    scale_color_manual(values = todo_cols, name = NULL, na.translate = FALSE) +
    scale_shape_manual(values = todo_symbols, name = NULL) +

    xlab(NULL) +
    ylab(NULL) +

    theme(
      legend.position = "left",
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.box = "vertical",
      legend.text = element_text(size = 10)
    )

  print(g)
}
