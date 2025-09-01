#' Wordcloud
#'
#' Draw a wordcloud.
#'
#' @inheritParams e_bar
#' @param word,freq Terms and their frequencies.
#' @param color Word color.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#' @param ... Additional parameters passed to the wordcloud series, including:
#'   \describe{
#'     \item{shape}{Shape of the wordcloud. Can be 'circle', 'cardioid', 'diamond', 'rectangle', 'triangle-forward', 'triangle', 'pentagon', 'star'. The 'rectangle' shape uses horizontal orientation and fills the available width.}
#'     \item{rectangleRatio}{Aspect ratio for rectangle shape (width:height). Default is 2.0. Only used when shape = "rectangle". The rectangle shape maintains this ratio even with equal sizeRange values.}
#'     \item{sizeRange}{Vector of two numbers specifying the range of font sizes. Equal values (e.g., c(20, 20)) are supported and rectangle shapes will maintain their aspect ratio.}
#'     \item{gridSize}{Grid size for word placement collision detection. Default is 8, but larger values (10-16) can prevent word overlapping, especially with larger fonts.}
#'     \item{drawOutOfBound}{Logical. Whether to allow words to be drawn outside the shape bounds. Default FALSE. Set to TRUE when using large font sizes.}
#'     \item{shrinkToFit}{Logical. Whether to shrink words that don't fit within the shape. Default FALSE. Alternative to drawOutOfBound for large fonts.}
#'     \item{width}{Character or numeric. Width of the wordcloud area. Defaults to '90%' (95% for rectangle shapes). Can be percentage string like '100%' or absolute pixels.}
#'     \item{height}{Character or numeric. Height of the wordcloud area. Defaults to '85%' (90% for rectangle shapes). Can be percentage string like '100%' or absolute pixels.}
#'   }
#'
#' @examples
#' words <- function(n = 5000) {
#'   a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
#'   paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
#' }
#'
#' tf <- data.frame(
#'   terms = words(100),
#'   freq = rnorm(100, 55, 10)
#' ) |>
#'   dplyr::arrange(-freq)
#'
#' tf |>
#'   e_color_range(freq, color) |>
#'   e_charts() |>
#'   e_cloud(terms, freq, color, shape = "circle", sizeRange = c(3, 15))
#'
#' # Rectangle shape example - uses horizontal orientation and fills available width
#' tf |>
#'   e_color_range(freq, color) |>
#'   e_charts() |>
#'   e_cloud(terms, freq, color, shape = "rectangle", sizeRange = c(3, 15))
#'
#' # Rectangle shape with custom aspect ratio (3:1 instead of default 2:1)
#' tf |>
#'   e_color_range(freq, color) |>
#'   e_charts() |>
#'   e_cloud(terms, freq, color, shape = "rectangle", rectangleRatio = 3.0, sizeRange = c(3, 15))
#'
#' # For large font sizes, use drawOutOfBound = TRUE to allow words outside shape bounds
#' tf |>
#'   e_color_range(freq, color) |>
#'   e_charts() |>
#'   e_cloud(terms, freq, color, shape = "rectangle", sizeRange = c(10, 100), drawOutOfBound = TRUE)
#'
#' # To prevent word overlapping, increase gridSize (especially useful with larger fonts)
#' tf |>
#'   e_color_range(freq, color) |>
#'   e_charts() |>
#'   e_cloud(terms, freq, color, shape = "rectangle", sizeRange = c(5, 25), gridSize = 12)
#'
#' # Equal sizeRange values now work correctly with rectangle shapes (maintains aspect ratio)
#' tf |>
#'   e_color_range(freq, color) |>
#'   e_charts() |>
#'   e_cloud(terms, freq, color, shape = "rectangle", rectangleRatio = 4.0, sizeRange = c(20, 20))

# Control wordcloud size - use full available space
tf |>
  e_color_range(freq, color) |>
  e_charts() |>
  e_cloud(terms, freq, color, shape = "rectangle", width = "100%", height = "95%")

# Or specify exact pixel dimensions
tf |>
  e_color_range(freq, color) |>
  e_charts() |>
  e_cloud(terms, freq, color, shape = "circle", width = 600, height = 400)
#' @seealso \href{https://github.com/ecomfe/echarts-wordcloud}{official documentation}
#'
#' @rdname e_cloud
#' @export
e_cloud <- function(e, word, freq, color, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  if (!missing(color)) {
    cl <- deparse(substitute(color))
  } else {
    cl <- NULL
  }

  e_cloud_(e, deparse(substitute(word)), deparse(substitute(freq)), cl, rm_x, rm_y, ...)
}

#' @rdname e_cloud
#' @export
e_cloud_ <- function(e, word, freq, color = NULL, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  data <- .build_data(e, freq)
  data <- .add_bind(e, data, word)

  if (!is.null(color)) {
    color <- .get_data(e, color)
    for (i in 1:length(data)) {
      col <- list(
        color = color[i]
      )
      data[[i]]$textStyle <- col
    }
  }

  serie <- list(
    type = "wordCloud",
    data = data,
    ...
  )

  e$x$opts$series <- append(e$x$opts$series, list(serie))

  # add dependency
  path <- system.file("htmlwidgets/lib/echarts-4.8.0/plugins", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-wordcloud",
    version = "1.0.0",
    src = c(file = path),
    script = "echarts-wordcloud.min.js"
  )

  e$dependencies <- append(e$dependencies, list(dep))

  e
}

#' Liquid fill
#'
#' Draw liquid fill.
#'
#' @inheritParams e_bar
#' @param color Color to plot.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#'
#' @examples
#' df <- data.frame(val = c(0.6, 0.5, 0.4))
#'
#' df |>
#'   e_charts() |>
#'   e_liquid(val) |>
#'   e_theme("dark")
#' @seealso \href{https://github.com/ecomfe/echarts-liquidfill}{official documentation}
#'
#' @rdname e_liquid
#' @export
e_liquid <- function(e, serie, color, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  if (!missing(color)) {
    cl <- deparse(substitute(color))
  } else {
    cl <- NULL
  }

  e_liquid_(e, deparse(substitute(serie)), cl, rm_x, rm_y, ...)
}

#' @rdname e_liquid
#' @export
e_liquid_ <- function(e, serie, color = NULL, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  data <- .get_data(e, serie) |>
    unlist() |>
    unname()

  serie <- list(
    type = "liquidFill",
    data = data,
    ...
  )

  if (!is.null(color)) {
    serie$color <- .get_data(e, color) |>
      unlist() |>
      unname()
  }

  e$x$opts$series <- append(e$x$opts$series, list(serie))

  # add dependency
  path <- system.file("htmlwidgets/lib/echarts-4.8.0/plugins", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-liquidfill",
    version = "1.0.0",
    src = c(file = path),
    script = "echarts-liquidfill.min.js"
  )

  e$dependencies <- append(e$dependencies, list(dep))

  e
}

#' Modularity
#'
#' Graph modularity extension will do community detection and partian a graph's vertices in several subsets.
#' Each subset will be assigned a different color.
#'
#' @inheritParams e_bar
#' @param modularity Either set to \code{TRUE}, or a \code{list}.
#'
#' @section Modularity:
#' \itemize{
#'   \item{\code{resolution} Resolution}
#'   \item{\code{sort} Whether to sort to comunities}
#' }
#'
#' @examples
#' nodes <- data.frame(
#'   name = paste0(LETTERS, 1:100),
#'   value = rnorm(100, 10, 2),
#'   stringsAsFactors = FALSE
#' )
#'
#' edges <- data.frame(
#'   source = sample(nodes$name, 200, replace = TRUE),
#'   target = sample(nodes$name, 200, replace = TRUE),
#'   stringsAsFactors = FALSE
#' )
#'
#' e_charts() |>
#'   e_graph() |>
#'   e_graph_nodes(nodes, name, value) |>
#'   e_graph_edges(edges, source, target) |>
#'   e_modularity(
#'     list(
#'       resolution = 5,
#'       sort = TRUE
#'     )
#'   )
#' @note Does not work in RStudio viewer, open in browser.
#'
#' @seealso \href{https://github.com/ecomfe/echarts-graph-modularity}{Official documentation}
#'
#' @export
e_modularity <- function(e, modularity = TRUE) {
  clu <- list(
    modularity = modularity
  )

  e$x$opts$series[[length(e$x$opts$series)]]$modularity <- clu

  # add dependency
  path <- system.file("htmlwidgets/lib/echarts-4.8.0/plugins", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-modularity",
    version = "1.0.0",
    src = c(file = path),
    script = "echarts-graph-modularity.min.js"
  )

  e$dependencies <- append(e$dependencies, list(dep))

  e
}
