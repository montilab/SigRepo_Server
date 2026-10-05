# Helpers behind the Home tab (modules/home_module.R).
#
# The tab mirrors the React dashboard (web/src/pages/DashboardPage.tsx, fed by
# GET /insights): four totals, signatures per organism and per assay, and the
# five most active contributors. Everything here is a pure function of the
# searchSignature() frame so the module is only layout, and so the numbers
# and figures can be tested without Shiny or a database.

# The React data-viz palette (web/src/index.css, --viz-1 .. --viz-5), in the
# order the stat cards, donut slices and legend dots cycle through it.
home_palette <- function() {
  c("#2563eb", "#0ea5e9", "#14b8a6", "#f59e0b", "#ec4899")
}

# Text and grid colours shared by the three charts (--text-muted,
# --text-secondary, --viz-grid, --accent in index.css).
home_chart_colours <- function() {
  list(muted = "#98a1b0", secondary = "#545c6b", grid = "#e6eef9", accent = "#2563eb")
}

# The four numbers on the stat cards. Distinct counts ignore NA so a signature
# with no organism row does not count as its own organism.
home_summary_counts <- function(df) {
  distinct_in <- function(column) {
    if (!is.data.frame(df) || !column %in% names(df)) {
      return(0L)
    }
    dplyr::n_distinct(df[[column]], na.rm = TRUE)
  }
  list(
    total_signatures = if (is.data.frame(df)) nrow(df) else 0L,
    total_users = distinct_in("user_name"),
    total_organisms = distinct_in("organism"),
    total_assays = distinct_in("assay_type")
  )
}

# Signatures per value of `column`, as the name/value rows the API's
# by_organism / by_assay / top_contributors lists carry, largest first. A
# missing value is kept and labelled rather than dropped, so the slices still
# add up to the total on the stat card. `n` keeps only the top rows.
home_count_by <- function(df, column, n = NULL) {
  empty <- data.frame(name = character(0), value = integer(0), stringsAsFactors = FALSE)
  if (!is.data.frame(df) || nrow(df) == 0 || !column %in% names(df)) {
    return(empty)
  }
  values <- as.character(df[[column]])
  values[is.na(values) | !nzchar(trimws(values))] <- "Unknown"
  tally <- table(values)
  counts <- data.frame(
    name = names(tally),
    value = as.integer(tally),
    stringsAsFactors = FALSE
  )
  counts <- counts[order(-counts$value, counts$name), , drop = FALSE]
  if (!is.null(n)) {
    counts <- utils::head(counts, n)
  }
  rownames(counts) <- NULL
  counts
}

# The compact legend under the organism donut: a palette dot, the organism and
# its count for the top `n` slices, in the same order and colours as the donut.
home_legend_tags <- function(counts, n = 5) {
  counts <- utils::head(counts, n)
  palette <- home_palette()
  items <- lapply(seq_len(nrow(counts)), function(i) {
    htmltools::div(
      class = "legend-item",
      htmltools::span(
        class = "legend-dot",
        style = sprintf("background: %s;", palette[(i - 1) %% length(palette) + 1])
      ),
      htmltools::span(class = "legend-label", counts$name[i]),
      htmltools::span(class = "legend-value", counts$value[i])
    )
  })
  htmltools::div(class = "legend legend-compact", items)
}

# ---- charts -----------------------------------------------------------------

# Axis breaks that stay on whole numbers, since every value is a count.
home_integer_breaks <- function(limits) {
  breaks <- unique(floor(pretty(limits)))
  breaks[breaks >= 0]
}

# recharts-like styling: transparent so the card shows through, no axis
# lines, hairline grid, muted small tick labels, no legend or axis titles.
home_chart_theme <- function() {
  colours <- home_chart_colours()
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = colours$grid),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = colours$muted, size = 9),
      legend.position = "none",
      plot.margin = ggplot2::margin(6, 8, 4, 4)
    )
}

# What a chart shows when there is nothing to count, instead of an empty axis.
home_empty_plot <- function() {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text", x = 0, y = 0, label = "No signatures to show",
      colour = home_chart_colours()$muted, size = 3.6
    ) +
    ggplot2::theme_void()
}

# Colours for the rows of `counts`, cycling through the palette in row order
# so slice i, bar i and legend dot i all match.
home_fill_scale <- function(counts) {
  palette <- home_palette()
  fills <- palette[(seq_len(nrow(counts)) - 1) %% length(palette) + 1]
  ggplot2::scale_fill_manual(values = stats::setNames(fills, counts$name))
}

# Keeps the largest-first order from home_count_by() on the axis.
home_ordered <- function(counts) {
  counts$name <- factor(counts$name, levels = counts$name)
  counts
}

# By organism: a donut (inner radius ~55%, outer ~82%, a thin white gap
# between slices, like the recharts Pie on the React page).
home_donut_plot <- function(counts) {
  if (nrow(counts) == 0) {
    return(home_empty_plot())
  }
  counts <- home_ordered(counts)
  ggplot2::ggplot(counts, ggplot2::aes(x = 1.7, y = value, fill = name)) +
    ggplot2::geom_col(width = 0.6, colour = "white", linewidth = 0.9) +
    ggplot2::coord_polar(theta = "y", direction = -1) +
    ggplot2::xlim(0.5, 2) +
    home_fill_scale(counts) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA)
    )
}

# By assay: vertical bars, one palette colour each, with slanted labels.
home_bar_plot <- function(counts) {
  if (nrow(counts) == 0) {
    return(home_empty_plot())
  }
  counts <- home_ordered(counts)
  colours <- home_chart_colours()
  ggplot2::ggplot(counts, ggplot2::aes(x = name, y = value, fill = name)) +
    ggplot2::geom_col(width = 0.62) +
    home_fill_scale(counts) +
    ggplot2::scale_y_continuous(
      breaks = home_integer_breaks,
      expand = ggplot2::expansion(mult = c(0, 0.08))
    ) +
    home_chart_theme() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 15, hjust = 1, vjust = 1, colour = colours$secondary)
    )
}

# Top contributors: horizontal accent-coloured bars, largest at the top.
home_hbar_plot <- function(counts) {
  if (nrow(counts) == 0) {
    return(home_empty_plot())
  }
  colours <- home_chart_colours()
  counts$name <- factor(counts$name, levels = rev(counts$name))
  ggplot2::ggplot(counts, ggplot2::aes(x = value, y = name)) +
    ggplot2::geom_col(width = 0.5, fill = colours$accent) +
    ggplot2::scale_x_continuous(
      breaks = home_integer_breaks,
      expand = ggplot2::expansion(mult = c(0, 0.08))
    ) +
    home_chart_theme() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(colour = colours$secondary, size = 9.5)
    )
}
