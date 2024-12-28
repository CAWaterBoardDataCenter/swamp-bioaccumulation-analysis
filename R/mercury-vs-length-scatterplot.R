#' Outputs a (.jpeg file) scatterplot of fish mercury concentration vs fish length in (mm) for a given sourcefile and species
#'
#' @param filename Filename and realtive path for CEDEN AQT download data (.tsv) file.
#' @param species_common_name common name of fish species to be plotted (CommonName field in the CEDEN AQT download)
#' @export

mercury_vs_length_scatterplot <- function(filename,species_common_name) {
  export_filename=paste(stringr::str_extract(filename,'^[^.]+'),"_scatterplot_",species_common_name,gsub(" ","",gsub(":","",timestamp(Sys.time()))),".jpeg", sep = "");

  #setup export image 8.5x11 landscape equivalent
  jpeg(export_filename,11,8.5,'in',res=300,quality=100);

  # read in dataset from source
  dataset_CEDEN_format<-load_dataset(filename);
  #extract source table for mercury conc vs length
  dataset<-get_mercury_vs_length_scatterplot_subset(dataset_CEDEN_format)%>%dplyr::filter(Species==species_common_name);

  waterbodies<-sort(unique(dataset$Waterbody));

  waterbodies_chunked<-waterbodies%>%get_chunks(10);
  #counter for filenames
  n=1;
  for(i in waterbodies_chunked) {
      if (length(waterbodies)>10) {
        export_filename=paste(stringr::str_extract(filename,'^[^.]+'),"_scatterplot_",species_common_name,as.character(n),"_",gsub(" ","",gsub(":","",timestamp(Sys.time()))),".jpeg", sep = "");
        png(export_filename);
        n<-n+1;
        }
    dataset_chunk<-dataset%>%dplyr::filter(Waterbody %in% i)
    #set max length value at a close multipile of 100 above highest length
    maxLength<-100+round(max(dataset_chunk$TLAvgLength.mm., na.rm = T),-2)
    #minLength<-floor(min(dataset_chunk$TLAvgLength.mm., na.rm = T))

    if(min(dataset$TLAvgLength.mm., na.rm = T)<100) {
      minLength<-0
    } else{
      minLength<-100
    }

    #get nearest above max value for ylim (with some buffer added)
    if (max(dataset$Result, na.rm = T)<0.5) {
      maxConc<-0.4+round(max(dataset$Result, na.rm = T),1)

    } else{
      #get nearest integer above max value for ylim (with some buffer added)
      maxConc<-1.45+ceiling(max(dataset$Result, na.rm = T))
    }
    minConc<-0

    mercuryLabel<-dplyr::first(dataset_chunk$AnalyteUnit, na_rm = TRUE);

    #error message for bad species common name input
    if(nrow(dataset_chunk) == 0){
      return("There are no records in dataset for the species common name provided.")
    }
    car::scatterplot(Result~TLAvgLength.mm.| Waterbody,
                     reg=FALSE, smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5,
                     by.groups=TRUE, xlim=c(minLength,maxLength), ylim=c(minConc,maxConc), xlab="Total Length (mm)",
                     ylab=mercuryLabel, main=species_common_name,
                     legend=c(coords="topright",pt.cex=.8),cex=1.5,lwd=0.1, data=dataset_chunk)
    while (!is.null(dev.list())) dev.off();
  }
  while (!is.null(dev.list())) dev.off();
}
