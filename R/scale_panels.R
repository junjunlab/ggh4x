#' Position scales for facets
#'
#' These functions are for adjusting the position scales in particular panels
#' of a faceted plot. They should be called *after* the facets have been added
#' to a plot *and* the facets should have free scales.
#'
#' @param condition An expression that evaluates to a `logical` vector in the
#'   context of the panel layout.
#' @param ... Arguments to pass to the position scale.
#' @param type A `character(1)` to note the type of scale, such that
#'   `scale_{x/y}_{type}` is a function that constructs a position scale.
#'   Examples are `"continuous"`, `"discrete"` or `"date"`.
#' @param .env The environment for which to look for the `type` function.
#'
#' @details For the `condition` argument, the expression is evaluated in the
#'   tidy evaluation framework whereby the `data.frame` with the plot's layout
#'   is given priority over the environment in which the expression was created.
#'   As a consequence, variables (columns) that define faceting groups can be
#'   referenced directly.
#'
#'   Other than the syntactic courtesy of directly adding a scale, the mechanism
#'   of adding a panel specific scale is the same as in [`facetted_pos_scales`].
#'   The same limitations apply: the `oob` argument isn't working and
#'   transformations occur *before* stat layer computations.
#'
#' @return A `facetted_pos_scales` object, instructing a ggplot how to adjust
#'   the scales per facet.
#' @export
#' @name scale_panels
#' @seealso The [`facetted_pos_scales()`] function.
#'
#' @examples
#' # A generic facetted plot
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   facet_grid(cyl ~ year, scales = "free")
#'
#' # Condition based on the facetting variables
#' p + scale_x_panels(year == 1999, labels = scales::number_format(0.001))
#'
#' # Condition based on the layout positions
#' p + scale_y_panels(ROW %in% 2:3, limits = c(10, 30))
#'
#' # Use TRUE condition last for a default scale that applies to all others
#' p + scale_y_panels(cyl %in% c(5, 6), limits = c(10, 30)) +
#'   scale_y_panels(TRUE, limits = c(10, 50))
scale_x_panels <- function(condition, ..., type = "continuous",
                          .env = parent.frame()) {
  scale_facetted(rlang::enquos(condition), ...,
                 type = type, .env = .env, aes = "x")
}

#' @rdname scale_panels
#' @export
scale_y_panels <- function(condition, ..., type = "continuous",
                           .env = parent.frame()) {
  scale_facetted(rlang::enquos(condition), ...,
                 type = type, .env = .env, aes = "y")
}

# Helper ------------------------------------------------------------------

scale_facetted <- function(condition, ..., type = "continuous",
                           .env = parent.frame(), aes = "x") {
  fun <- paste0("scale_", aes, "_", type)
  if (exists(fun, envir = .env, mode = "function")) {
    fun <- get(fun, envir = .env, mode = "function")
  }
  if (!is.function(fun)) {
    nsenv <- asNamespace("ggplot2")
    if (exists(fun, envir = nsenv, mode = "function")) {
      fun <- get(fun, envir = nsenv, mode = "function")
    } else {
      rlang::abort(paste0("Cannot find a scale function named: `", fun, "()`."))
    }
  }
  scale <- fun(...)
  check <- check_facetted_scale(list(scale), aes, allow_null = FALSE)
  if (!isTRUE(check)) {
    rlang::abort(paste0("The scale appears to be invalid."))
  }
  alt <- chartr("xy", "yx", aes)

  structure(rlang::exec(
    list,
    !!aes := structure(list(scale), lhs = condition, class = "list"),
    !!alt := list(NULL)
  ), class = "facetted_pos_scales")
}
