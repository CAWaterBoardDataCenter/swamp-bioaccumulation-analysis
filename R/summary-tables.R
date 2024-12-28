#' Outputs a six tab (.xlsx) spreadsheet with summary tables for your dataset
#'
#' @param filename Filename (or filepath, if necessary) for CEDEN AQT download data (.tsv) file.
#' @export

summary_tables <- function(filename) {

  #read in dataset from source
  dataset<-load_dataset(filename);

  dataset_og<-dataset;
  #remove exact duplicates
  dataset<-dataset%>% dplyr::distinct();

  preyfish<-dataset%>%get_preyfish_subset();
  sportfish<-dataset%>%get_sportfish_subset();
  limit<-sportfish$smallfish_limit
  length<-sportfish$TLAvgLength.mm.

  #keep only the first analysis of a given composite/parameter
  dataset<-dataset%>%dplyr::group_by(CompositeCompositeID,CompositeReplicate,Analyte,Unit)%>%dplyr::filter(AnalysisDate==min(AnalysisDate))%>%dplyr::ungroup();
  #keep only the first sample date of a given composite
  dataset<-dataset%>%dplyr::group_by(CompositeCompositeID,CompositeReplicate)%>%dplyr::filter(CompositeSampleDate==min(CompositeSampleDate))%>%dplyr::ungroup();

  #get unique stations in dataset with WB Region
  stations<-get_stations_info(dataset);

  #extract mercury in individual fish results for laa input
  merc_ind<-get_mercury_in_individuals_subset(dataset);

  #get summary table with counts of observations, % detections (not ND), and % reporting (not ND or ComplianceCode=Rej)
  analytes_observs_detects<-dataset%>%add_sum_groups()%>%
    # dplyr::filter(SumFlag==1)%>%
    dplyr::group_by(Laboratory=AnalyzingAgency,Class=SumGroup,	Analyte,	Unit,	Method)%>%
    remove_moisture_lipid()%>%dplyr::filter(Unit!="% recovery")%>%
    dplyr::mutate(Class=ifelse((SumFlag =="1" & !is.na(SumFlag)),SumGroup,Analyte))%>%
    dplyr::summarize("Detection Limit"=mean(MDL),"Number of Observations"=dplyr::n(),"Frequency of Detection (%)"=round((sum(ResQualCode != 'ND')/dplyr::n())*100,0),"Frequency of Reporting (%)"=round((sum(ResQualCode!='ND' & ComplianceCode!='Rej')/dplyr::n())*100,0));

  #get mercury in individual fish results in appendix format
  merc_ind_summary<-dataset%>%add_LocationCode()%>%
    dplyr::filter(Analyte %in% c('Mercury','Age') & NumberFishPerComp==1)%>%
    dplyr::mutate(AnalyteUnit=paste(Analyte," (",Unit,")", sep = ""))%>%
    dplyr::mutate(SampleYear=as.numeric(format(lubridate::mdy(LatestDateSampled), "%Y")))%>%
    dplyr::select(Region=rb_number,SampleYear,"Waterbody Name"=CompositeStationName,LocationCode,"Common Name"=CompositeCommonName,SampleID=CompositeCompositeID,AnalyteUnit,Result,"Total Length Avg (mm)"=TLAvgLength.mm.)%>%
    tidyr::spread(AnalyteUnit,Result)%>%
    dplyr::select(-"Total Length Avg (mm)","Total Length Avg (mm)")

  #get preyfish composite summarytable
  preyfish_comp<-dataset%>%add_LocationCode()%>%
    remove_moisture_lipid()%>%dplyr::filter(Unit!="% recovery")%>%get_composites_subset()%>%get_preyfish_subset()%>%
    add_sum_groups()%>%
    dplyr::filter(is.na(SumGroup)|!(SumGroup=='PCB' & is.na(SumFlag)))%>%
    dplyr::mutate(SumGroupDisplay=ifelse((SumFlag =="1" & !is.na(SumFlag)),SumGroupDisplay,Analyte))%>%
    dplyr::mutate(AnalyteUnit=paste(SumGroupDisplay," (",Unit,")", sep = ""))%>%
    dplyr::mutate(SampleYear=as.numeric(format(lubridate::mdy(LatestDateSampled), "%Y")))%>%
    dplyr::select(Region=rb_number,"Waterbody Name"=CompositeStationName,LocationCode,SampleYear,"Common Name"=CompositeCommonName,SampleID=CompositeCompositeID,AnalyteUnit,Result)%>%
    tidyr::spread(AnalyteUnit,Result)%>%
    dplyr::relocate(dplyr::any_of(dplyr::starts_with("Sum of", ignore.case = TRUE)),.after=SampleID)%>%
    dplyr::relocate(dplyr::any_of(dplyr::starts_with("Selenium", ignore.case = TRUE)),.after=SampleID)%>%
    dplyr::relocate(dplyr::any_of(dplyr::starts_with("Mercury", ignore.case = TRUE)),.after=SampleID);

  #get sportfish composite summary table and calculate sum groups
  sportfish_comp<-dataset%>%add_LocationCode()%>%get_composites_subset()%>%get_sportfish_subset()%>%
    remove_moisture_lipid()%>%dplyr::filter(Unit!="% recovery")%>%
    add_sum_groups()%>%
    dplyr::filter(is.na(SumGroup)|!(SumGroup=='PCB' & is.na(SumFlag)))%>%
    dplyr::mutate(SumGroupDisplay=ifelse((SumFlag =="1" & !is.na(SumFlag)),SumGroupDisplay,Analyte))%>%
    dplyr::mutate(AnalyteUnit=paste(SumGroupDisplay," (",Unit,")", sep = ""))%>%
    dplyr::mutate(SampleType=paste(CompositeSampleTypeCode," ",LocationCode))%>%
    dplyr::mutate(SampleYear=as.numeric(format(lubridate::mdy(LatestDateSampled), "%Y")))%>%
    dplyr::group_by(Region=rb_number,"Waterbody Name"=CompositeStationName,LocationCode,SampleType,SampleYear,"Common Name"=CompositeCommonName,SampleID=CompositeCompositeID,AnalyteUnit)%>%dplyr::summarize(Result=sum(Result, na.rm=TRUE));

  #get length adjusted results
  lar<-get_length_adjusted_results(merc_ind);

  #get a new dataframe with averages of the Length adjusted result for each station (and other summary calculations)
  laa<-summarize_length_adjusted_results(lar);

  #get sportfish laa in composite summary table format (Replace hard-coded analyteunit once we get feedback from Jay)
  sportfish_laa<-laa%>%add_stations_info(stations)%>%
    dplyr::mutate(AnalyteUnit=paste(Analyte," (",Unit,")", sep = ""))%>%
    dplyr::mutate(SampleID="NA")%>%
    dplyr::mutate(SampleType=paste(ResultType," ",LocationCode))%>%
    dplyr::mutate(SampleYear=as.numeric(format(lubridate::mdy(SampleDateMax), "%Y")))%>%
    dplyr::select(Region,"Waterbody Name"=StationName,LocationCode,SampleType,SampleYear,"Common Name"=CommonName,SampleID,AnalyteUnit,Result);

  sportfish_comp_laa<-dplyr::bind_rows(sportfish_comp,sportfish_laa)%>%
    dplyr::arrange(Region,"Waterbody Name",SampleType,,SampleYear,"Common Name",SampleID,AnalyteUnit)%>%
    tidyr::spread(AnalyteUnit,Result)%>%
    dplyr::relocate(dplyr::any_of(dplyr::starts_with("Sum of", ignore.case = TRUE)),.after=SampleID)%>%
    dplyr::relocate(dplyr::any_of(dplyr::starts_with("Selenium", ignore.case = TRUE)),.after=SampleID)%>%
    dplyr::relocate(dplyr::any_of(dplyr::starts_with("Mercury", ignore.case = TRUE)),.after=SampleID);

  preyfish_summary<-dataset%>%get_preyfish_subset()%>%
    dplyr::distinct(CompositeStationCode,CompositeCompositeID,CompositeFinalID,CompositeCommonName,NumberFishPerComp,TLMin.mm.,TLAvgLength.mm.,TLMax.mm.)%>%
    dplyr::group_by(CompositeFinalID,CompositeCommonName)%>%
    dplyr::summarise("Number of Fish"=sum(NumberFishPerComp),	"Composites - Number of Samples"=dplyr::n_distinct(CompositeCompositeID[NumberFishPerComp>1]),	"Composites - Number of Locations"=dplyr::n_distinct(CompositeStationCode[NumberFishPerComp>1]),	"Individuals - Number of Samples"=dplyr::n_distinct(CompositeCompositeID[NumberFishPerComp== 1]),"Individuals - Number of Locations"=dplyr::n_distinct(CompositeStationCode[NumberFishPerComp== 1]),"Total Number of Locations Sampled"=dplyr::n_distinct(CompositeCompositeID), "Min Length (mm)"=min(TLMin.mm.),	"Median Length (mm)"=median(TLAvgLength.mm.),	"Max Length (mm)"=max(TLMax.mm.),"Analyzed as Composites"=any(NumberFishPerComp>1),"Analyzed as Individuals"=any(NumberFishPerComp==1))%>%
    dplyr::select("Species Name"=CompositeFinalID,	"Common Name"=CompositeCommonName,	"Number of Fish",	"Composites - Number of Samples",	"Composites - Number of Locations",	"Individuals - Number of Samples","Individuals - Number of Locations","Total Number of Locations Sampled", "Min Length (mm)",	"Median Length (mm)",	"Max Length (mm)"	,"Analyzed as Composites"	,"Analyzed as Individuals");

  sportfish_summary<-dataset%>%get_sportfish_subset()%>%
    dplyr::distinct(CompositeStationCode,CompositeCompositeID,CompositeFinalID,CompositeCommonName,NumberFishPerComp,TLMin.mm.,TLAvgLength.mm.,TLMax.mm.)%>%
    dplyr::group_by(CompositeFinalID,CompositeCommonName)%>%
    dplyr::summarise("Number of Fish"=sum(NumberFishPerComp),	"Composites - Number of Samples"=dplyr::n_distinct(CompositeCompositeID[NumberFishPerComp>1]),	"Composites - Number of Locations"=dplyr::n_distinct(CompositeStationCode[NumberFishPerComp>1]),	"Individuals - Number of Samples"=dplyr::n_distinct(CompositeCompositeID[NumberFishPerComp== 1]),"Individuals - Number of Locations"=dplyr::n_distinct(CompositeStationCode[NumberFishPerComp== 1]),"Total Number of Locations Sampled"=dplyr::n_distinct(CompositeCompositeID), "Min Length (mm)"=min(TLMin.mm.),	"Median Length (mm)"=median(TLAvgLength.mm.),	"Max Length (mm)"=max(TLMax.mm.),"Analyzed as Composites"=any(NumberFishPerComp>1),"Analyzed as Individuals"=any(NumberFishPerComp==1))%>%
    dplyr::select("Species Name"=CompositeFinalID,	"Common Name"=CompositeCommonName,	"Number of Fish",	"Composites - Number of Samples",	"Composites - Number of Locations",	"Individuals - Number of Samples","Individuals - Number of Locations","Total Number of Locations Sampled", "Min Length (mm)",	"Median Length (mm)",	"Max Length (mm)"	,"Analyzed as Composites"	,"Analyzed as Individuals");

  #Get excel export filename
  export_filename=paste(stringr::str_extract(filename,'^[^.]+'),"_SummaryTables_",gsub(" ","",gsub(":","",timestamp(Sys.time()))),".xlsx", sep = "")

  #export, include file showing composite-level calculated values used for final averages
  wb <- openxlsx2::wb_workbook() %>%
  #Counts of observations, detections and over reporting limit by analytes
  openxlsx2::wb_add_worksheet(sheet = "AnalytesObservsDetects") %>%
  openxlsx2::wb_add_data("AnalytesObservsDetects", analytes_observs_detects,na.strings=NULL) %>%
  #Mercury results in individual sport fish
  openxlsx2::wb_add_worksheet(sheet = "SportfishMercuryIndiv") %>%
  openxlsx2::wb_add_data("SportfishMercuryIndiv", merc_ind_summary,na.strings=NULL) %>%
  #Composites results or means of individual fish at each location (grouped by species) for sport fish.  Means are length-adjusted, if possible.
  openxlsx2::wb_add_worksheet(sheet = "SportfishCompsAndMeans") %>%
  openxlsx2::wb_add_data("SportfishCompsAndMeans", sportfish_comp_laa,na.strings=NULL) %>%
  #Composite results for prey fish at each location
  openxlsx2::wb_add_worksheet(sheet = "PreyfishComposites") %>%
  openxlsx2::wb_add_data("PreyfishComposites", preyfish_comp,na.strings=NULL) %>%
  #Counts and summary length information by species for prey fish
  openxlsx2::wb_add_worksheet(sheet = "PreyfishCounts") %>%
  openxlsx2::wb_add_data("PreyfishCounts", preyfish_summary,na.strings=NULL) %>%
  #Counts and summary length information by species for sport fish
  openxlsx2::wb_add_worksheet(sheet = "SportfishCounts") %>%
  openxlsx2::wb_add_data("SportfishCounts", sportfish_summary,na.strings=NULL) %>%
  openxlsx2::wb_save(export_filename);
}

