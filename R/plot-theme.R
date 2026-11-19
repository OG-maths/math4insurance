# Shared base-R colours, matching custom.scss. No plotting package is required.
insurance_colours <- c(
  background = "#111820", raised = "#18222d", shade = "#202d3a",
  grid = "#2c3a49", axis = "#7d91a4", text = "#c0c9d2",
  title = "#f1f4f7", curve = "#8fb6d8", accent = "#d9ba82"
)

insurance_par <- function(...) {
  old <- par(no.readonly = TRUE)
  settings <- modifyList(list(
    bg = getOption("insurance.plot.background", "transparent"), fg = insurance_colours[["axis"]],
    col = insurance_colours[["text"]], col.axis = insurance_colours[["text"]],
    col.lab = insurance_colours[["text"]], col.main = insurance_colours[["title"]],
    col.sub = insurance_colours[["text"]], family = "sans", las = 1,
    bty = "l", tcl = -0.22, mgp = c(2.4, 0.65, 0),
    cex.axis = 0.9, cex.lab = 0.95, cex.main = 1.05,
    font.main = 1, mar = c(4, 4.3, 2.8, 1)
  ), list(...))
  do.call(par, settings)
  invisible(old)
}
