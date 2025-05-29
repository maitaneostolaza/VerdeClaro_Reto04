Save_pdf <- function(nombre_archivo, plot_expr) {
  pdf(file = paste0("Graficos/", nombre_archivo, ".pdf"), width = 10, height = 6)
  print(eval(plot_expr))
  dev.off()
}