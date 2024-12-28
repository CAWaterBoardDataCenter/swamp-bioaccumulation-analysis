load_dataset <- function(filename) {
  read.csv(paste(filename,sep=""), header = T, stringsAsFactors=FALSE, sep="\t")
}

get_stations_info <- function(df) {
  df<- df%>%dplyr::group_by(Region=rb_number,StationCode=CompositeStationCode)%>%dplyr::summarize();
}

add_stations_info <- function(df,station_df) {
  dplyr::left_join(df, station_df,by = dplyr::join_by(StationCode==StationCode))
}

remove_moisture_lipid <- function(df) {
  df<- df%>%dplyr::filter(! Analyte %in% c('Moisture','Lipid'))
}

add_LocationCode <- function(df) {
df<- df%>% dplyr::mutate(LocationCode = dplyr::case_when(grepl("L1", CompositeCompositeID) ~ "L1",grepl("L2", CompositeCompositeID)  ~"L2",grepl("L3", CompositeCompositeID) ~ "L3",grepl("L4", CompositeCompositeID)  ~"L4",.default= as.character("NA")))
}

get_composites_subset<- function(df) {
  composites<-df%>%dplyr::filter(NumberFishPerComp>1)
}

get_individual_subset<- function(df) {
  composites<-df%>%dplyr::filter(NumberFishPerComp==1)
}

get_mercury_in_individuals_subset <- function(df) {
  mercury_in_individuals<- df%>%add_LocationCode()%>% dplyr::filter(Analyte =='Mercury' & NumberFishPerComp==1) %>% dplyr::select(ProjectCode=CompositeProjectCode,SampleDateMax=LatestDateSampled,StationCode=CompositeStationCode,StationName=CompositeStationName,CommonName=CompositeCommonName,CompositeID=CompositeCompositeID,LocationCode,NumberinComposite=NumberFishPerComp,Analyte=Analyte,Result,Unit,TotalLengthAvg=TLAvgLength.mm.) %>% dplyr::arrange(ProjectCode,StationCode,LocationCode,CommonName)
}

get_mercury_vs_length_scatterplot_subset <- function(df) {
  mercury_vs_length_source<-df %>%
    dplyr::filter(NumberFishPerComp==1 & Analyte=='Mercury')%>%dplyr::mutate(AnalyteUnit=paste(Analyte," (",Unit,")", sep = ""))%>%
    dplyr::select(Waterbody=CompositeStationName, Species=CompositeCommonName,TLAvgLength.mm., AnalyteUnit, Result)
}

get_chunks<- function(list, chunkSize) {
  chunks<-split(list, ceiling(seq_along(list)/chunkSize))
}

add_fish_type <- function(df) {
  filepath <- system.file("extdata", "species-type-lookup-table.csv", package="swampBioaccumulationAnalysis")
  lookup_table <- read.csv(filepath, header = T, stringsAsFactors=FALSE )
  dplyr::left_join(df, lookup_table,by = dplyr::join_by(CompositeCommonName==commonname))
}

add_unit_conversions <- function(df) {
  filepath <- system.file("extdata", "mercury-units.csv", package="swampBioaccumulationAnalysis")
  lookup_table <- read.csv(filepath, header = T, stringsAsFactors=FALSE )
  dplyr::left_join(df, lookup_table,by = dplyr::join_by(UnitName==Unit))
}

add_sum_groups <- function(df) {
  filepath <- system.file("extdata", "summing-lookup.csv", package="swampBioaccumulationAnalysis")
  lookup_table <- read.csv(filepath, header = T, stringsAsFactors=FALSE )
  dplyr::left_join(df, lookup_table,by = dplyr::join_by(Analyte==AnalyteName))
}

get_preyfish_subset <- function(df) {
  preyfish<-df%>%add_fish_type%>%dplyr::filter(fishtype=="small"|(fishtype=="both" & TLAvgLength.mm.< as.numeric(smallfish_limit)))
}

get_sportfish_subset <- function(df) {
  sportfish<-df%>%add_fish_type%>%dplyr::filter(fishtype=="sport"|(fishtype=="both" & TLAvgLength.mm.> as.numeric(smallfish_limit)))
}

get_mercury_by_species_barplot_subset <- function(df) {
  mercury_by_species_source<-add_fish_type(df) %>%
    dplyr::mutate(SampleTypeCode=dplyr::case_when(NumberFishPerComp==1 ~"Individual",NumberFishPerComp>1 ~ "Composite")) %>%
    dplyr::select(CompositeID=CompositeCompositeID, Type=fishtype, smallfish_limit, StationName=CompositeStationName, OldStationName=CompositeStationName, StationCode=CompositeStationCode, CommonName=CompositeCommonName, OldCommonName=CompositeCommonName, Parameter=Analyte, TissueCode=CompositeTissueName , Result, UnitName=Unit, AvgOfTotalLength_mm="TLAvgLength.mm.", SampleType=CompositeSampleTypeCode, "Number Of Fish In Sample"=NumberFishPerComp,)%>%dplyr::filter(Parameter=="Mercury")%>%dplyr::arrange(desc(CommonName))
}

#function for getting the p-value of f-statistic
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  if ( !is.numeric(f)) return(NA) else
    p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

get_length_adjusted_results <- function(df) {
  #get residuals (within a Station, Location, species group for the linear model) for all composites
  res = df %>%
    dplyr::group_by(ProjectCode,StationCode,StationName,LocationCode,CommonName,Analyte,Unit) %>%
    dplyr::mutate(resids=residuals(lm(Result ~ TotalLengthAvg))) %>%
    dplyr::ungroup() %>%
    dplyr::select(ProjectCode,StationCode,LocationCode,CommonName, Analyte,Unit,resids)

  #Add to dataframe in Residuals field
  df$Residuals=res$resids

  #get predictions at 350mm and significance of regression for Station, Location, species grouping
  #misleading predictions warnings suppressed. non-significant predictions are not used in final output.
  suppressWarnings(
    predictions<-df %>%
      dplyr::group_by(ProjectCode,StationCode,StationName,LocationCode,CommonName,Analyte,Unit) %>%
      dplyr::summarize(PredictedAt350=predict(lm(Result ~ TotalLengthAvg), list(TotalLengthAvg = 350))
                       ,Significance=lmp(lm(Result ~ TotalLengthAvg))
                       ,.groups = "drop") %>%
      data.frame()
  );

  #Join the predicted concentrations dataframe back to the main dataframe
  df= merge(df, predictions, by = c("ProjectCode","StationCode","StationName","LocationCode","CommonName","Analyte","Unit"))

  #Sum the residuals and predicted concentrations to get a Length adjusted result for each composite
  df$LengthAdjustedResult=df$Residuals+df$PredictedAt350

  df;
}

summarize_length_adjusted_results <- function(df) {
  #get a new dataframe with averages of the Length adjusted result for each station
  laa<-df %>%
    dplyr::group_by(ProjectCode,StationCode,StationName,LocationCode,CommonName,Analyte,Unit,Significance) %>%
    dplyr::summarize(LengthAdjustedAverage = mean(LengthAdjustedResult, na.rm=TRUE)
                     ,seLengthAdjusted=sd(LengthAdjustedResult)/sqrt(sum(!is.na(LengthAdjustedResult)))
                     ,SampleDateMax=max(SampleDateMax)
                     ,N=sum(!is.na(Result))
                     ,N_Between305mmAnd395mm=sum(!is.na(ifelse((TotalLengthAvg>=305 & TotalLengthAvg <395),Result,NA)))
                     ,SimpleMean=mean(Result, na.rm=TRUE)
                     ,SimpleMean_Between305mmAnd395mm=mean(ifelse((TotalLengthAvg>=305 & TotalLengthAvg <395),Result,NA), na.rm=TRUE)
                     ,TwoXse=2*(sd(Result, na.rm=TRUE)/sqrt(sum(!is.na(Result))))
                     ,TwoXse_Between305mmAnd395mm=2*(sd(ifelse((TotalLengthAvg>=305 & TotalLengthAvg <395),Result,NA), na.rm=TRUE)/sqrt(sum(!is.na(ifelse((TotalLengthAvg>=305 & TotalLengthAvg <395),Result,NA)))))
    )%>%
    data.frame()

  BassLAASpecies<-c("Largemouth Bass","Smallmouth Bass","Spotted Bass")

  #Conditionally populate Result,ResultType, and Num in Avg in output
  laa$ResultType <- ifelse(laa$LengthAdjustedAverage>=0 & laa$Significance <= 0.05 & !is.na(laa$Significance) & laa$N>=7 & laa$CommonName %in% BassLAASpecies,"350 mm Length-Adjusted"
                           ,ifelse(laa$CommonName %in% BassLAASpecies,"Average of Individuals >=305 mm and <395 mm","Average of Individuals"))

laa$Result <- ifelse(laa$LengthAdjustedAverage>=0 & laa$Significance <= 0.05 & !is.na(laa$Significance) & laa$N>=7 & laa$CommonName %in% BassLAASpecies,laa$LengthAdjustedAverage
                       ,ifelse(laa$CommonName %in% BassLAASpecies,laa$SimpleMean_Between305mmAnd395mm,laa$SimpleMean))

  laa$NumberInComp <- ifelse(laa$LengthAdjustedAverage>=0 & laa$Significance <= 0.05 & !is.na(laa$Significance) & laa$N>=7 & laa$CommonName %in% BassLAASpecies,laa$N
                             ,ifelse(laa$CommonName %in% BassLAASpecies,laa$N_Between305mmAnd395mm,laa$N))
  laa;
}

now <- Sys.time()

timestamp <- function(time) format(time, "%Y-%m-%d_%H%M%S")
