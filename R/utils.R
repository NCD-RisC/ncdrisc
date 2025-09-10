#' Coloured printing function
print <- function(x, col = "white", indent = 0) insight::print_color(paste0(strrep(" ", indent), paste(x, collapse = ", "), "\n"), color = col)