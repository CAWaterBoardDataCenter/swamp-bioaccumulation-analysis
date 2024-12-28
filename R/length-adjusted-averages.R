#' Outputs a three tab (.xlsx) spreadsheet with length-adjusted averages, length-adjusted averages with additional summary calculations, and intermediate length-adjusted result calculations
#'
#' @param filename Filename (or filepath, if necessary) for CEDEN AQT download data (.tsv) file.
#' @export

length_adjusted_averages <- function(filename) {

  #set working directory to the directory the dataset .tsv file is in prior to running script

   # read in dataset from source
  dataset<-load_dataset(filename);
  #extract mercury in individual fish results
  df<-get_mercury_in_individuals_subset(dataset);

  #get length adjusted results
  lar<-get_length_adjusted_results(df);

  #get a new dataframe with averages of the Length adjusted result for each station (and other summary calculations)
  laa<-summarize_length_adjusted_results(lar);

  #set up sheet for report table (appendix table 4 format)
  modelAvgs<-laa%>% dplyr::select(ProjectCode,"Sample Date"=SampleDateMax,Station=StationName,"Common Name"=CommonName,"Number of Fish in Sample"=NumberInComp,Result,"Sample Type"=ResultType)

  #set up sheet for review with Standard error (se) of LAA and Significance of F for model
  laa_details<-laa%>% dplyr::select(ProjectCode,StationCode,StationName,LocationCode,CommonName,Result,ResultType,LengthAdjustedAverage,'se (length adj)'=seLengthAdjusted,Significance,"SimpleMean >=305mm and <395 mm"=SimpleMean_Between305mmAnd395mm,'2*se >=305mm'=TwoXse_Between305mmAnd395mm,SimpleMean,'2*se'=TwoXse,'N in Avg'=NumberInComp)

  #Get excel export filename
  export_filename=paste(stringr::str_extract(filename,'^[^.]+'),"_LengthAdjAvg_",gsub(" ","",gsub(":","",timestamp(Sys.time()))),".xlsx", sep = "")

  #export, include file showing composite-level calculated values used for final averages
  wb <- openxlsx2::wb_workbook() %>%
  openxlsx2::wb_add_worksheet(sheet = "LengthAdjustedAverages") %>%
  openxlsx2::wb_add_data("LengthAdjustedAverages", modelAvgs,na.strings=NULL) %>%
  openxlsx2::wb_add_worksheet(sheet = "LengthAdjustedAveragesDetails") %>%
  openxlsx2::wb_add_data("LengthAdjustedAveragesDetails", laa_details,na.strings=NULL) %>%
  openxlsx2::wb_add_worksheet(sheet = "SupportingCalculations") %>%
  openxlsx2::wb_add_data("SupportingCalculations", lar,na.strings=NULL) %>%
  openxlsx2::wb_save(export_filename);
}

