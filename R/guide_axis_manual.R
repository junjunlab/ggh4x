# Constructor -------------------------------------------------------------

#' Manual axis
#'
#' This axis allows a greater degree of control than the default axes guides. In
#' particular, this axis allows setting break positions and labels independently
#' from the scale and is not bound by the same constraints as secondary axes.
#' Additionally, label attributes may be set in parallel to the labels
#' themselves, circumventing the unsupported vectorised input to
#' `element_text()`.
#'
#' @inheritParams guide_axis_truncated
#' @param breaks One of the following ways to parametrise the tick and label
#'   positions:
#' * `NULL` to draw no ticks and labels.
#' * `waiver()` for the default breaks computed by the scale (default).
#' * A `numeric` vector for continuous or discrete scales, or a `character`
#'   vector for discrete scales.
#' * A `function` that takes the limits as input and returns breaks as output.
#'   Also accepts rlang [lambda][rlang::as_function()] notation.
#' * A [`unit`][grid::unit()] vector for setting data-independent
#'   breaks.
#' @param labels One of the following ways to dictate the labels:
#' * `NULL` to draw no labels.
#' * `waiver()` for the default labels computed by the scale on the breaks
#'   (default). Note that a scale with non-identity transformation is unlikely
#'   to graciously handle breaks defined in grid-units.
#' * A `character` vector giving
#' * A `function` that takes the breaks as input and returns labels as output.
#'   Also accepts rlang [lambda][rlang::as_function()] notation.
#' @param
#'   label_family,label_face,label_colour,label_size,label_hjust,label_vjust,label_lineheight,label_color,label_margin
#'    Arguments passed down to the label constructor. See
#'   [`element_text()`][ggplot2::element_text()] arguments, which these
#'   arguments mirror with the `label_`-prefix. With the exception of
#'   `label_margin`, the other `label_*` arguments are assumed be parallel to
#'   (the result of) the `labels` argument and will be recycled with
#'   [`rep_len()`][base::rep_len()] as necessary. By default, these parameters
#'   are taken from the theme.
#'
#' @return An *axis_manual* guide class object.
#' @export
#' @family axis-guides
#' @md
#'
#' @examples
# Using the manual axis for greater control over labels
#' ggplot(iris, aes(Species, Sepal.Width)) +
#'   geom_boxplot(aes(fill = Species)) +
#'   guides(x = guide_axis_manual(
#'     label_colour = scales::hue_pal()(3),
#'     label_face = c("bold", "italic", "plain"),
#'     labels = toupper
#'   ))
#'
#' # Using the manual axis to annotate some specific point
#' ggplot(pressure, aes(temperature, pressure)) +
#'   geom_point() +
#'   geom_hline(yintercept = 300, linetype = 2, colour = "blue") +
#'   guides(y.sec = guide_axis_manual(breaks = 300, labels = "some\nthreshold",
#'                                    label_colour = "blue"))
guide_axis_manual <- function(
  title = waiver(),
  breaks = waiver(),
  labels = waiver(),
  label_family = NULL,
  label_face = NULL,
  label_colour = NULL,
  label_size = NULL,
  label_hjust = NULL,
  label_vjust = NULL,
  label_lineheight = NULL,
  label_color = NULL,
  label_margin = NULL,
  check.overlap = FALSE,
  angle = NULL,
  n.dodge = 1,
  order = 0,
  colour = NULL,
  color = NULL,
  trunc_lower = NULL,
  trunc_upper = NULL,
  position = waiver()
) {
  colour <- color %||% colour
  label_colour <- label_color %||% label_colour
  check_trunc_arg(trunc_lower, trunc_upper)

  # Allow rlang lambda expressions
  if (is_formula(breaks)) {
    breaks <- as_function(breaks)
  }
  if (is_formula(labels)) {
    labels <- as_function(labels)
  }

  # Compact label params
  label_params <- list(
    family = label_family,
    face = label_face,
    colour = label_colour,
    size = label_size,
    hjust = label_hjust,
    vjust = label_vjust,
    lineheight = label_lineheight,
    margin = label_margin
  )
  label_params <- label_params[!vapply(label_params, is.null, logical(1))]

  structure(
    list(
      title = title,
      breaks = breaks,
      labels = labels,
      check.overlap = check.overlap,
      angle = angle,
      n.dodge = n.dodge,
      order = order,
      trunc_lower = trunc_lower,
      trunc_upper = trunc_upper,
      colour = colour,
      label_params = label_params,
      position = position,
      available_aes = c("x", "y"),
      name = "axis"
    ),
    class = c("guide", "axis_manual", "axis_ggh4x", "axis")
  )
}

# Methods -----------------------------------------------------------------

#' @export
#' @method guide_train axis_manual
guide_train.axis_manual <- function(guide, scale, aesthetic = NULL) {
  aesthetic <- aesthetic %||% scale$aesthetics[1]

  if (inherits(guide$breaks, "waiver")) {
    breaks <- scale$get_breaks()
    breaks <- breaks[!is.na(breaks)]
    transform_breaks <- FALSE
  } else {
    breaks <- guide$breaks
    transform_breaks <- !scale$is_discrete() & !is.unit(breaks)
  }
  if (is.function(breaks)) {
    limits <- scale$get_limits()
    if (transform_breaks) {
      # Function is expected to work on untransformed data
      breaks <- breaks(scale$scale$trans$inverse(limits))
    } else {
      breaks <- breaks(limits)
    }
  }

  # Warn when a transformation tries to auto-label grid units
  if (is.unit(breaks) && inherits(guide$labels, "waiver")) {
    if (!scale$is_discrete() && scale$scale$trans$name != "identity") {
      warning("Setting units for breaks might not work with default ",
              "scale labelling.", call. = FALSE)
    }
  }

  empty_ticks <- .int$new_data_frame(list(
    aesthetic = numeric(0),
    .value = numeric(0),
    .label = character(0)
  ))
  names(empty_ticks) <- c(aesthetic, ".value", ".label")
  if (length(intersect(scale$aesthetics, guide$available_aes)) == 0) {
    warn("axis guide needs appropriate scales.")
    guide$key <- empty_ticks
  } else if (length(breaks) == 0) {
    guide$key <- empty_ticks
  } else {
    if (scale$is_discrete() & !is.unit(breaks)) {
      mapped_breaks <- scale$map(breaks)
    } else {
      if (transform_breaks) {
        mapped_breaks <- scale$scale$trans$transform(breaks)
      } else {
        mapped_breaks <- breaks
      }
    }
    ticks <- .int$new_data_frame(setNames(list(mapped_breaks), aesthetic))
    ticks$.value <- breaks

    if (inherits(guide$labels, "waiver")) {
      if (scale$is_discrete()) {
        labels <- scale$get_labels(breaks)
      } else {
        labels <- scale$get_labels(mapped_breaks)
      }
    } else {
      labels <- guide$labels
    }
    if (is.function(labels)) {
      labels <- labels(breaks)
    }
    ticks$.label <- labels
    n <- nrow(ticks)
    extra <- intersect(c("family", "face", "colour", "size", "hjust", "vjust",
                         "lineheight"),
                       names(guide$label_params))
    extra <- lapply(guide$label_params[extra], rep_len, length.out = n)
    if (sum(lengths(extra)) > 0) {
      names(extra) <- paste0(".", names(extra))
      ticks <- cbind.data.frame(ticks, extra)
    }
    guide$key <- ticks[is.finite(ticks[[aesthetic]]), ]
  }
  guide$name <- paste0(guide$name, "_", aesthetic)
  guide$hash <- digest::digest(list(guide$title, guide$key$.value,
                                    guide$key$.label, guide$name))
  guide <- truncate_guide(guide, scale, aesthetic)
  guide

}

#' @export
#' @method guide_transform axis_manual
guide_transform.axis_manual <- function(guide, coord, panel_params) {
  if (is.null(guide$position) || nrow(guide$key) == 0) {
    return(guide)
  }
  aesthetics <- names(guide$key)[!grepl("^\\.", names(guide$key))]
  if (all(c("x", "y") %in% aesthetics)) {
    i <- vapply(guide$key[aesthetics], is.unit, logical(1))
    i <- setdiff(names(guide$key), aesthetics[i])
    guide$key[, i] <- coord$transform(
      guide$key[, i, drop = FALSE], panel_params
    )
  } else {
    other_aesthetic <- setdiff(c("x", "y"), aesthetics)
    override_value <- if (guide$position %in% c("bottom", "left")) -Inf else Inf
    guide$key[[other_aesthetic]] <- override_value
    if (is.unit(guide$key[[aesthetics]])) {
      i <- setdiff(names(guide$key), aesthetics)
      guide$key[, i] <- coord$transform(guide$key[, i, drop = FALSE],
                                        panel_params)
    } else {
      guide$key <- coord$transform(guide$key, panel_params)
    }
    .int$warn_for_guide_position(guide)
  }
  guide$trunc <- transform_truncated(guide$trunc, coord, panel_params)
  guide
}

#' @export
#' @method guide_gengrob axis_manual
guide_gengrob.axis_manual <- function(guide, theme) {
  aesthetic <- names(guide$key)[!grepl("^\\.", names(guide$key))][1]
  draw_axis_manual(
    key = guide$key,
    axis_position = guide$position,
    theme = theme,
    check.overlap = guide$check.overlap,
    angle = guide$angle,
    n.dodge = guide$n.dodge,
    trunc = guide$trunc,
    colour = guide$colour,
    label_params = guide$label_params
  )
}


# Helpers -----------------------------------------------------------------


draw_axis_manual <- function(
  key,
  axis_position,
  theme,
  check.overlap,
  angle = NULL,
  n.dodge = 1,
  trunc,
  colour = NULL,
  label_params
) {
  axis_position <- match.arg(substr(axis_position, 1, 1),
                             c("t", "b", "r", "l"))
  elements <- build_axis_elements(axis_position, angle, theme, colour)
  params <- setup_axis_params(axis_position)
  params$margin <- label_params$margin
  line_grob <- build_trunc_axis_line(elements$line, params, trunc)

  if (nrow(key) == 0) {
    out <- gTree(
      children = gList(line_grob),
      width    = grobWidth(line_grob),
      height   = grobHeight(line_grob),
      cl = "absoluteGrob"
    )
    return(out)
  }

  label_grobs <- build_axis_labels(
    elements, key = key,
    dodge = n.dodge, check.overlap = check.overlap, params = params
  )

  sizes <- unit.c(elements$tick_length)
  tick_grob <- build_axis_ticks(
    elements$ticks, sizes, key[[params$aes]], params
  )
  elements$tick_length <- max(sizes)
  assemble_axis_grobs(
    ticks = tick_grob,
    labels = label_grobs,
    lines = line_grob,
    elements = elements,
    params = params
  )
}
