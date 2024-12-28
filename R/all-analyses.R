#' Outputs the following:
#' 1. a three tab (.xlsx) spreadsheet with length-adjusted averages
#' 2. a six tab (.xlsx) spreadsheet with summary tables
#' 3. bar plots in (.jpeg) image files for mercury results in sport fish and prey fish
#' 4. scatterplots in (.jpeg) image files for every sport fish species present in the dataset with mercury results
#'
#' @param filename Filename (or filepath, if necessary) for CEDEN AQT download data (.tsv) file.
#' @export

all_analyses <- function(filename) {
  summary_tables(filename);
  length_adjusted_averages(filename);
  mercury_by_species_barplot(filename,"sport");
  mercury_by_species_barplot(filename,"prey");
  df<-load_dataset(filename)%>%get_mercury_vs_length_scatterplot_subset();
  species<-unique(df$Species);
  for (i in 1:length(species)) {
    mercury_vs_length_scatterplot(filename,species[i]);
  }
}
