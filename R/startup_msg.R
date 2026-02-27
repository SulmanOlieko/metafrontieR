metafrontieRStartupMessage <- function() {
  art_lines <- c(
"                       888              .d888                          888    d8b          8888888b. ",
"                       888             d88P\"                           888    Y8P          888   Y88b",
"                       888             888                             888                 888    888",
"88888b.d88b.   .d88b.  888888  8888b.  888888 888d888 .d88b.  88888b.  888888 888  .d88b.  888   d88P",
"888 \"888 \"88b d8P  Y8b 888        \"88b 888    888P\"  d88\"\"88b 888 \"88b 888    888 d8P  Y8b 8888888P\" ",
"888  888  888 88888888 888    .d888888 888    888    888  888 888  888 888    888 88888888 888 T88b  ",
"888  888  888 Y8b.     Y88b.  888  888 888    888    Y88..88P 888  888 Y88b.  888 Y8b.     888  T88b ",
"888  888  888  \"Y8888   \"Y888 \"Y888888 888    888     \"Y88P\"  888  888  \"Y888 888  \"Y8888  888   T88b"
  )
  base_msg <- paste0(
    "\n\n* Please cite the 'metafrontieR' package as:\n",
    "Owili, SO. (2026). metafrontieR: Metafrontier Analysis in R. R package version 1.0.0.\n\n",
    "See also: citation(\"metafrontieR\")\n\n",
    "* For any questions, suggestions, or comments on the 'metafrontieR' package, you can contact the authors directly or visit:\n",
    "  https://github.com/SulmanOlieko/metafrontieR/issues\n"
  )
  
  console_width <- getOption("width")
  art_width <- max(nchar(art_lines))
  
  if (console_width < art_width) {
    # 1. Truncate all lines to fit the console width
    trimmed_art <- substr(art_lines, 1, console_width - 5)
    
    # 2. Add the ASCII ellipsis strictly to the last row
    ellipsis <- "   88 88 88"
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
  # We still skip printing during the CRAN check to keep the test logs clean
  in_chk <- Sys.getenv("_R_CHECK_PACKAGE_NAME_") != ""
  
  if (!in_chk) {
    msg <- metafrontieRStartupMessage()
  } else {
    msg <- paste0(
      "\n* Please cite the 'metafrontieR' package as:\n",
      "Owili, SO. (2026). metafrontieR: Metafrontier Analysis in R. R package version 1.0.0.\n\n",
      "See also: citation(\"metafrontieR\")\n\n",
      "* For any questions, suggestions, or comments on the 'metafrontieR' package, you can contact the authors directly or visit:\n",
      "  https://github.com/SulmanOlieko/metafrontieR/issues\n"
    )
  }
  
  packageStartupMessage(msg)
  invisible()
}
