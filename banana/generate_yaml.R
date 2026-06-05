# Wrapper functions to programmatically generate bananarama YAML configs.
# Not sure yet if it should be a standalone function (as there are two steps: YAML generation and Image generation)
# Or it should be embeded into bananarama.

#' Drop NULL entries from a list
compact <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

#' Error handling, DRY from parser.R - should be in a shared file!

check_aspect_ratio <- function(value, name) {
  valid <- c(
    "1:1",
    "2:3",
    "3:2",
    "3:4",
    "4:3",
    "4:5",
    "5:4",
    "9:16",
    "16:9",
    "21:9"
  )
  if (!value %in% valid) {
    cli::cli_abort(c(
      "Image {.val {name}} has invalid {.field aspect-ratio} {.val {value}}.",
      i = "Must be one of {.or {.val {valid}}}."
    ))
  }
}

check_resolution <- function(value, name) {
  valid <- c("1K", "2K", "4K")
  if (!value %in% valid) {
    cli::cli_abort(c(
      "Image {.val {name}} has invalid {.field resolution} {.val {value}}.",
      i = "Must be one of {.or {.val {valid}}}."
    ))
  }
}



# copied from readme....

#' Build a single image entry for the `images` list
#'
#' @param name        Character. Required. Used as the output filename stem.
#' @param description Character. Required (unless a default description is set).
#'                    Prompt for image generation. Use `[name]` to reference
#'                    images in the same directory.
#' @param style       Character or NULL. Per-image style override.
#' @param aspect_ratio Character or NULL. Per-image aspect-ratio override.
#'                    One of "1:1","2:3","3:2","3:4","4:3","4:5","5:4","9:16","16:9","21:9".
#' @param resolution  Character or NULL. Per-image resolution override ("1K","2K","4K").
#' @param n           Integer or NULL. Number of variants to generate.
#' @param model       Character or NULL. Gemini model override.
#' @param force       Logical or NULL. If TRUE, regenerate even if image exists.
#' @param seed        Integer or NULL. RNG seed for more deterministic output.
#'
#' @return A named list representing one image entry.

bnn_image <- function(name,
                      description,
                      style = NULL,
                      aspect_ratio = NULL,
                      resolution = NULL,
                      n = NULL,
                      model = NULL,
                      force = NULL,
                      seed = NULL) {

  if (missing(name) || !is.character(name) || nchar(name) == 0) {
    stop("'name' is required and must be a non-empty string.", call. = FALSE)
  }
  if (missing(description) || !is.character(description) || nchar(description) == 0) {
    stop("'description' is required and must be a non-empty string.", call. = FALSE)
  }

  if (!is.null(aspect_ratio)) check_aspect_ratio(aspect_ratio)
  if (!is.null(resolution))   check_resolution(resolution)
  if (!is.null(n))            stopifnot(is.numeric(n), n >= 1)
  if (!is.null(force))        stopifnot(is.logical(force))
  if (!is.null(seed))         stopifnot(is.numeric(seed))

# compact ensures that empty parameters are ommited from final yaml.
# the image will inherit from defaults.
  compact(list(
    name           = name,
    description    = description,
    style          = style,
    `aspect-ratio` = aspect_ratio,
    resolution     = resolution,
    n              = n,
    model          = model,
    force          = force,
    seed           = seed
  ))
}


#' Generate a bananarama YAML configuration file
#'
#' Builds a YAML file compatible with [bananarama::bananarama()] from R objects.
#'
#' @param images      A character vector of image names (e.g. \code{"robot"} or
#'                    \code{c("robot", "hero")}), or a list of entries created
#'                    by [bnn_image()] for full per-image control. When a
#'                    character vector is given, each name becomes an image
#'                    entry that inherits its description from the defaults.
#' @param style       Character or NULL. Default style prompt appended to every
#'                    image description.
#' @param description Character or NULL. Default description applied to every
#'                    image that doesn't specify its own. Use \code{[name]}
#'                    notation to reference images (e.g. \code{"Draw [robot]"}).
#' @param aspect_ratio Character. Default aspect-ratio. Default: "16:9".
#' @param resolution  Character. Default resolution. Default: "1K".
#' @param n           Integer. Default number of variants per image. Default: 1.
#' @param model       Character. Default Gemini model. Default:
#'                    "gemini-3.1-flash-image-preview".
#' @param force       Logical. If TRUE, regenerate images even if they exist.
#'                    Default: FALSE.
#' @param seed        Integer or NULL. Default RNG seed.
#' @param output_dir  Character or NULL. Output directory for generated images,
#'                    relative to the YAML file. If NULL, bananarama defaults to
#'                    a directory named after the YAML file.
#' @param output_file Character. Path to write the YAML file. Default:
#'                    "bananarama.yaml".
#'
#' @return The path to the generated YAML file (invisibly).
#'
#' @examples
#' \dontrun{
#' # Minimal usage — image names as a character vector,
#' # description inherited from defaults:
#' generate_bananarama_yaml(
#'   description = "Draw a picture of [robot] doing something cool.",
#'   images      = "robot"
#' )
#'
#' # Full control — per-image overrides via bnn_image():
#' generate_bananarama_yaml(
#'   images = list(
#'     bnn_image("hero", "A futuristic cityscape at sunset"),
#'     bnn_image("chart", "A bar chart made of bananas", resolution = "4K")
#'   ),
#'   style       = "Flat vector illustration with muted colors.",
#'   resolution  = "2K",
#'   model       = "gemini-3-pro-image-preview",
#'   output_file = "my_deck.yaml"
#' )
#' }
generate_bananarama_yaml <- function(
    images,
    style        = NULL,
    description  = NULL,
    aspect_ratio = "16:9",
    resolution   = "1K",
    n            = 1L,
    model        = "gemini-3.1-flash-image-preview",
    force        = FALSE,
    seed         = NULL,
    output_dir   = NULL,
    output_file  = "bananarama.yaml"
) {

  # --- Coerce images ------------------------------------------------------
  # Accept a character vector of names (e.g. "robot" or c("robot", "hero"))
  # and convert each to a minimal image entry: list(name = x).
  # The description is inherited from the defaults block.
  if (is.character(images)) {
    images <- lapply(images, function(nm) list(name = nm))
  }

  if (!is.list(images) || length(images) == 0) {
    stop("'images' must be a character vector of names or a list of bnn_image() entries.",
         call. = FALSE)
  }

  # --- Build defaults block -----------------------------------------------
  # final yaml build. if werent provided and are not defaults - ommit from final build
  defaults <- compact(list(
    style          = style,
    description    = description,
    `aspect-ratio` = aspect_ratio,
    resolution     = resolution,
    n              = as.integer(n),
    model          = model,
    force          = force,
    seed           = seed
  ))

  # --- Build top-level config ---------------------------------------------
  # here compact removes output_dir if not provided.
  config <- compact(list(
    defaults     = defaults,
    `output-dir` = output_dir,
    images       = images
  ))

  # --- Ensure output directory exists -------------------------------------
  out_dir <- dirname(output_file)
  if (nzchar(out_dir) && out_dir != "." && !dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  # --- Write YAML ---------------------------------------------------------
  if (!requireNamespace("yaml12", quietly = TRUE)) {
    stop("The 'yaml12' package is required. Install it with install.packages('yaml12').",
         call. = FALSE)
  }

  yaml12::write_yaml(config, path = output_file)

  message("Wrote bananarama config to: ", normalizePath(output_file, mustWork = FALSE))
  invisible(normalizePath(output_file, mustWork = FALSE))
  # probably should add a banana imoji.. i mean... 🍌🍌🍌
}
