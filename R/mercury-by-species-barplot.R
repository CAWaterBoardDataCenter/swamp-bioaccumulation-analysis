#' Outputs a (.jpeg file) bar plot for mercury concentration by species, for a given source file and type of fish (sport or prey)
#'
#' @param filename Filename (or filepath, if necessary) for CEDEN AQT download data (.tsv) file.
#' @param fish_class Type of fish (either "sport" or "prey")
#' @export

mercury_by_species_barplot <- function(filename,fish_class) {

  ###########################
  # import raw RMP/BOG Data #
  ###########################

  fish1<-load_dataset(filename)
  fish1<-get_mercury_by_species_barplot_subset(fish1)%>%add_unit_conversions();
  #Unit not in lookup list. Add unit with conversion multiplier to ppm to ~/inst/extdata/MercuryUnits.csv file.
  noUnitConversion <-fish1 %>% dplyr::filter(is.na(ppmConversionMultiplier))
  #exit with error message for unit with no conversion to ppm in mercury-units.csv file.
  if (noUnitConversion%>%nrow()>0) {
    return("Mercury unit not in lookup list. Add unit with conversion multiplier to ppm to extdata/mercury-units.csv file, or contact package authors.");
  }

  #setup export filename
  export_filename=paste(stringr::str_extract(filename,'^[^.]+'),"_barplot_",fish_class,gsub(" ","",gsub(":","",timestamp(Sys.time()))),".jpeg", sep = "");

  #setup export image 8.5x11 landscape equivalent
  jpeg(export_filename,11,8.5,'in',res=300,quality=100);

  #convert Result to ppm so it can be plotted with the ppm thresholds
  fish1<-fish1%>%dplyr::mutate(Result_Orig=Result)%>%dplyr::mutate(Unit_Orig=UnitName)%>%dplyr::mutate(Result=Result*as.numeric(ppmConversionMultiplier))

  # add a space to the end of the common name, for plotting prettiness purposes
  fish1$CommonName <- paste(fish1$CommonName[]," ")

  #sport fish and prey fish handled separately below here

  ###### Conditional handling for prey fish ######
  if (fish_class=="prey") {
    fish1<- fish1 %>% dplyr::filter(Type=="small"|(Type=="both" & AvgOfTotalLength_mm<as.numeric(smallfish_limit)))

    # color gradient: green (lowest), yellow2, orange, red
    # no red line if no consumption ATL not on plot)

    # FCG <- xx # FCG options PCB = "3.6", Hg = "xx"
    # TMDL <- 10 # TMDL Target options (PCB = "10"; Hg = "0.3"
    # threeATL <- xx # ATL - 3 servings/wk options PCB = "21"; Hg = "xx"
    twoATL <- 0.05 # ATL - 2 servings/wk options PCB = "42"; Hg = "xx"
    # zeroATL <- 0.44 # ATL - no consumption options: PCB = "120"; Hg = "0.44"

    #########
    # Means #
    #########


    # select raw data for just species and concentrations
    plot_data<-data.frame(species=fish1$CommonName, contam=fish1$Result)
    # order dataframe by species names
    plot_data_o<-plot_data%>%dplyr::arrange(desc(species))

    ###### Calculate species averages ######

    # calculate average Contaminant concentration for each species (and order by common name)
    mean_spp <- aggregate(fish1$Result,fish1[c('CommonName')],mean)%>%dplyr::arrange(desc(CommonName))


    ###### Create means figure ######

    ## set the margin sizes to a standard - units = 1 line ##
    par(mar = c(5,10.1,2.1,2.1)) # order of margins sizes: bottom, left, top, right - adjust as needed

    # set x-axis limits for figure
    #get nearest integer above max value for xlim (with some buffer for legend)
    maxConc<-0.199+ceiling(max(plot_data$contam, na.rm = T))
    x_lim <- c(0,maxConc)
    #x_lim <- c(0,1.0)

    # Set species names for figure (use mean_spp df that is grouped and ordered by common name)
    spp_names1 <- mean_spp$CommonName

    # create x-axis label
    xname <- "Mercury Concentration (ppm)"

    #yname <- sort(spp_names1, decreasing=TRUE)   # if names are sorted then need to sort barplot results
    ynames <- spp_names1

    ## select orientation of x-axis labels (type A: horizontal, type B: angled) ##
    # A #
    # mean_fig <- barplot(mean_spp$x,ylim=y_lim, ylab=list(yname, cex = 1.3), axes=FALSE, names.arg=spp_names1, cex.names = 1.2);

    # B #
    mean_fig <- gplots::barplot2(mean_spp$x, xlim=x_lim, xlab=list(xname, cex = 1.2), names.arg=ynames, horiz=TRUE, axes=TRUE, axisnames=FALSE);
    text(0, mean_fig, labels = ynames, srt = 0, adj = 1, xpd = TRUE, cex = 1.0)

    # to change the angle of the text, vary the "srt" parameter between 0 and 90 (degrees).

    ## comment out thresholds as needed ##
    # abline(h=threeATL, col="green", lty=1, lwd=2);
    abline(v=twoATL, col="black",xpd=FALSE ,lty=1, );
    # abline(h=TMDL, col="yellow2", lty=1)
    # abline(v=FCG, col="green", lty=1);
    # abline(v=zeroATL, col="red", lty=1,lwd=2)

    ##### adjust legend names and colors to thresholds in use #####
    legend("topright", c("0.05 ppm"), cex=1.1, lty=1, col=c("black"));

    ##### set the position of the points on top of the bars #####
    plot_data_2 <- plot_data_o
    for (i in 1:length(spp_names1))
    {
      #replace species with the bar plot height for the species, to use for points() y value
      plot_data_2<-plot_data_2%>%dplyr::mutate(species=replace(species,spp_names1[i]==species, mean_fig[i]))
    }

    # add points on top of bars for each species #
    points(plot_data_2$contam, plot_data_2$species, pch=20, col="black", cex=1);
    # axis(2, cex.axis=1.8)
  }
  #end preyfish

  ###### Conditional handling for sport fish######
  else if (fish_class=="sport") {
    #get sportfish subset
    fish1<- fish1 %>% dplyr::filter(Type=="sport"|(Type=="both" & AvgOfTotalLength_mm>as.numeric(smallfish_limit)))

    # color gradient: green (lowest), yellow2, orange, red
    # no red line if no consumption ATL not on plot)

    # FCG <- xx # FCG options PCB = "3.6", Hg = "xx"
    # TMDL <- 10 # TMDL Target options (PCB = "10"; Hg = "0.3"
    # threeATL <- xx # ATL - 3 servings/wk options PCB = "21"; Hg = "xx"
    twoATL <- 0.2 # ATL - 2 servings/wk options PCB = "42"; Hg = "xx"
    zeroATL <- 0.44 # ATL - no consumption options: PCB = "120"; Hg = "0.44"

    #########
    # Means #
    #########

    ###### Analysis Dataset ######

    # select raw data for just species and concentrations
    plot_data<-data.frame(species=fish1$CommonName, contam=fish1$Result)
    # order dataframe by species names
    plot_data_o<-plot_data%>%dplyr::arrange(desc(species))

    ###### Calculate species averages ######

    # calculate average Contaminant concentration for each species
    mean_spp <- aggregate(fish1$Result,fish1[c('CommonName')],mean)%>%dplyr::arrange(desc(CommonName))

    ###### Create means figure ######

    ## set the margin sizes to a standard - units = 1 line ##
    par(mar = c(5,10.1,2.1,2.1)) # order of margins sizes: bottom, left, top, right - adjust as needed

    # set x-axis limits for figure
    #get nearest integer above max value for xlim (with some buffer for legend)
    maxConc<-0.499+ceiling(max(plot_data$contam, na.rm = T))
    x_lim <- c(0,maxConc)

    # Set species names for figure
    spp_names1 <- mean_spp$CommonName

    # create x-axis label
    xname <- "Mercury Concentration (ppm)"

    # create y-axis labels
    ynames <- spp_names1

    ## select orientation of x-axis labels (type A: horizontal, type B: angled) ##
    # A #
    # mean_fig <- barplot(mean_spp$x,ylim=y_lim, ylab=list(yname, cex = 1.3), axes=FALSE, names.arg=spp_names1, cex.names = 1.2);

    # B #
    mean_fig <- gplots::barplot2(mean_spp$x, xlim=x_lim, xlab=list(xname, cex = 1.2), names.arg=ynames, horiz=TRUE, axes=TRUE, axisnames=FALSE)
    text(0, mean_fig, labels = ynames, srt = 0, adj = 1, xpd = TRUE, cex = 1.0)

    # to change the angle of the text, vary the "srt" parameter between 0 and 90 (degrees).

    ## comment out thresholds as needed ##
    # abline(h=threeATL, col="green", lty=1, lwd=2);
    abline(v=twoATL, col="purple", lty=1);
    #abline(h=TMDL, col="yellow2", lty=1)
    # abline(v=FCG, col="green", lty=1);
    abline(v=zeroATL, col="orange", lty=1,lwd=1)

    ##### adjust legend names and colors to thresholds in use #####
    legend("topright", c("0.2 ppm", "0.44 ppm"), cex=1.1, lty=1, col=c("purple", "orange"));

    ##### set the position of the points on top of the bars #####
    plot_data_2 <- plot_data_o

    for (i in 1:length(spp_names1))
     {
       #replace species with the bar plot height for the species to use for points y value
       plot_data_2<-plot_data_2%>%dplyr::mutate(species=replace(species,spp_names1[i]==species, mean_fig[i]))
    }

    # add points on top of bars for each species #
    points(plot_data_2$contam, plot_data_2$species, pch=20, col="black", cex=1);

  }
  dev.off();
}
