source("src/globals.R")

DEFAULT_PLOT_WIDTH  = 5
DEFAULT_PLOT_HEIGHT = 3
options(warnPartialMatchArgs = FALSE) #to avoid the warning with ggsave about file and filename
save_plot <- function(obj, file_name, dir_name="plots", plot_width=DEFAULT_PLOT_WIDTH, plot_height=DEFAULT_PLOT_HEIGHT)
{
  dir.create(dir_name, showWarnings = FALSE)
  file_path = file.path(dir_name, file_name)
  #, width=plot_width, height=plot_height, units="in"
  ggsave(filename=file_path, obj)
  flog.info("created files %s...", file_path)
}

save_csv <- function(obj, file_name, dir_name="data")
{
  dir.create(dir_name, showWarnings = FALSE)
  file_path = file.path(dir_name, file_name)
  write.csv(obj, file_path, row.names=F)
  flog.info("created files %s...", file_path)
}