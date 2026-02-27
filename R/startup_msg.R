metafrontieRStartupMessage <- function() {
  art_lines <- c(
    "MetafrontieR. v 1.0.0"
  )

  base_msg <- paste0(
    "\n\n* Please cite the 'metafrontieR' package as:\n",
    "Owili S. (2026). metafrontieR: Metafrontier Analysis in R. R package version 1.0.0.\n\n",
    "See also: citation(\"metafrontieR\")\n\n",
    "* For any questions, suggestions, or comments on the 'metafrontieR' package, you can contact the authors directly or visit:\n",
    "  https://github.com/SulmanOlieko/metafrontieR/issues\n"
  )

  console_width <- getOption("width")
  art_width <- max(nchar(art_lines))

  if (console_width < art_width) {
    # 1. Truncate all lines to fit the console width
    trimmed_art <- substr(art_lines, 1, console_width - 5)

    # 2. Add the ellipsis strictly to the last row, calculating exact length
    ellipsis <- "..."
    e_len <- nchar(ellipsis)
    cut_point <- max(0, console_width - e_len)

    last_line_index <- length(trimmed_art)
    trimmed_art[last_line_index] <- paste0(
      substr(art_lines[last_line_index], 1, cut_point),
      ellipsis
    )

    # 3. Set the version alignment target
    align_width <- console_width
  } else {
    trimmed_art <- art_lines
    align_width <- art_width
  }

  art_msg <- paste(trimmed_art, collapse = "\n")

  # 4. Pad the version text so it hits the exact right edge
  version_msg <- paste0(
    "\n",
    sprintf(paste0("%", align_width, "s"), "version 1.0.0")
  )

  return(paste0(art_msg, version_msg, base_msg))
}

.onAttach <- function(lib, pkg) {
  msg <- metafrontieRStartupMessage()
  packageStartupMessage(msg)
  invisible()
}
