## ----RCflux_pkg, eval=TRUE------------------------------------------------
#' RCflux calculates fluxes from closed chamber data
#' 
#' Gas concentration data may come from a GC or other gas analysers
#' Data formats for several sources can be read in:
#' [list here] 
#' Output is written to CSV and PNG files.
#' Calculates gas fluxes from closed chamber data.
#'
#' The only function you're likely to need from \pkg{RCflux} is
#' \code{\link{calcFlux}}. Refer to the vignettes for details
#' of how to use it - use \code{vignette()}.
"_PACKAGE"
#> [1] "_PACKAGE"

## ----read_GC_output----
#' This function reads Agilent GC output.
#'
#' Reads Agilent GC output
#' @param fname A GC output file from the Agilent GC.
#' @keywords Agilent GC output
#' @export
#' @seealso \code{\link{convert_GC_output}} for the higher-level function which calls this.

read_GC_output <- function(fname){
#fname <- "c:/GC_data/CINAG_2016_06_16b_CM/CINAG_7-16b 2016-08-01 09-56-13/CINAG_2016_06_16b_CM.xls"
#fname <- "F:/GC_data/CINAG_2016_06_16c_CM/CINAG_1-16a_1-6b 2016-08-01 16-42-58/CINAG_2016_06_16c_CM.xls"
  df <- read_excel(fname, sheet="Sheet1", 
    col_types = c("text", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "text"))
  names(df) <- make.names(names(df))
  df <- subset(df,  !is.na(df$Area))

  # fname_out <- paste(file_path_sans_ext(fname), ".csv", sep="")
  
  # # convert GC_output to a csv file "Sheet1.csv"
  # f <- excelToCsv(file_path = fname, target_dir = "./")
  # gcfile <- readLines(file("Sheet1.csv", "r+"))

  # # remove unwanted rows 1-4
  # gcfile <- gcfile[-1:-4]
  # # identify lines with data versus blanks and headers
  # isDataLine <- logical(length = length(gcfile))
  # isDataLine <- TRUE # so header line is kept

  # for (i in 2:length(gcfile)){ # start at 2 to keep first header
  # #i <- 113
    # # if second element is numeric data, it's a DataLine, set to TRUE 
    # isDataLine[i] <- as.numeric(strsplit(gcfile[i], ",")[[1]][2]) > 0
  # }

  # # subset to only include the Data Lines
  # gcfile <- subset(gcfile,  isDataLine))
  # # write text out to a file
  # writeLines(gcfile, con = fname_out)
  # # read back in as a data frame 
  # df <- read.csv(fname_out, stringsAsFactors = FALSE)
  # #summary(df)
  # #str(df)
  # # remove unwanted cols X, X.1, X.2
  # df$X <- NULL
  # df$X.1 <- NULL
  # df$X.2 <- NULL

  # need to parse Sample Name data into the df from a list
  sampleData <- strsplit(df$Sample.Name, "_")
  sd_df <- data.frame(matrix(unlist(sampleData), nrow=length(sampleData), byrow=T),stringsAsFactors=FALSE)
  colnames(sd_df) <- c("site", "year", "mon", "mday", "chamberID", "time_label")
  #str(sd_df)
  df <- data.frame(df, sd_df)
  
  #### Simplest quick fix:
  #### make colnames(df) same as the old names
  #Compound Name,Line#,Vial,RT [min],Amount,Unit,Peak Height,Area,Sample Name,,,
  #Compound.Name,Line.,Vial,RT..min.,Amount,Unit,Peak.Height,Area,Sample.Name,site,year,mon,mday,chamberID,time_since_closure_mins,fname
  #GasName,      Line#,Vial,RT [min],Amount,Unit,Peak Height,Area,Sample Name,,,
  #Plot,Timepoint,Secs,Known conc,Valid sample,Temp,Pressure,Volume,Area,Position,Reten. Time [min],Area [mV.s],Height [mV],Area [%],Height [%],W05 [min]
  #Plot,Timepoint,Secs,Known conc,Valid sample,Temp,Pressure,Volume,Area,Position,Reten. Time [min],Area [mV.s],Height [mV],Area [%],Height [%],W05 [min]

  #### Good fix: rename to standard set of names in a function
  df <- standardiseNames(df)
  # don't need to identify standards here, as merged with st_wdf later
  #stdIndices <- substr(df$chamberID, 1, 3) == "Std"
  #df$Known.conc   <- NA
  #df$Known.conc[stdIndices]   <- df$Amount[stdIndices]
  
  # tmp need to set this, but should be in template
  #df$Valid.sample <- 1

  # add filename 
  df$fname <- fname
    
  return(df)
}

## ----standardiseNames----
#' Standardises names in a data frame
#'
#' This function standardises names in a data frame.
#' @param df A data frame.
#' @keywords GC input
#' @export
#' @seealso \code{\link{convert_GC_output}} for the higher-level function which calls this.
#' @examples
#' tdf <- data.frame(Plot = c(1, 2), Compound.Name = c("CO2", "CH4"))
#' tdf
#' names(tdf)
#' tdf <- standardiseNames(tdf)
#' tdf
#' names(tdf)

standardiseNames <- function(df){
  # common variables  
  if (exists("Year", where=df)){
    df[["year"]] <- df[["Year"]]
    df[["Year"]] <- NULL
  }  
  if (exists("Mon", where=df)){
    df[["mon"]] <- df[["Mon"]]
    df[["mon"]] <- as.numeric(df[["mon"]])
    df[["Mon"]] <- NULL
  }    
  if (exists("Mday", where=df)){
    df[["mday"]] <- df[["Mday"]]
    df[["mday"]] <- as.numeric(df[["mday"]])
    df[["Mday"]] <- NULL
  }    
  # make sure these are numeric not character
  if (exists("year", where=df))  df[["year"]] <- as.numeric(df[["year"]])
  if (exists("mon", where=df))  df[["mon"]] <- as.numeric(df[["mon"]])
  if (exists("mday", where=df))  df[["mday"]] <- as.numeric(df[["mday"]])
  
    
  # GC output variables
  if (exists("Compound.Name", where=df)){
    df[["gasName"]] <- df[["Compound.Name"]]
    df[["Compound.Name"]] <- NULL
  }
  if (exists("Sample.Name", where=df)){
    df[["sampleName"]] <- df[["Sample.Name"]]
    df[["Sample.Name"]] <- NULL
  }  
  if (exists("Area", where=df)){
    df[["peakArea"]] <- df[["Area"]]
    df[["Area"]] <- NULL
  }  

  # GC input variables
  # Standards variables 
  if (exists("Standard", where=df)){
    df[["standardID"]] <- df[["Standard"]]
    df[["Standard"]] <- NULL
  } 
  if (exists("ch4", where=df)){
    df[["CH4"]] <- df[["ch4"]]
    df[["ch4"]] <- NULL
  }  
  if (exists("co2", where=df)){
    df[["CO2"]] <- df[["co2"]]
    df[["co2"]] <- NULL
  }  
  if (exists("n2o", where=df)){
    df[["N2O"]] <- df[["n2o"]]
    df[["n2o"]] <- NULL
  }  
  # Chamber variables  
  if (exists("Site", where=df)){
    df[["site"]] <- df[["Site"]]
    df[["Site"]] <- NULL
  }
  if (exists("Plot", where=df) & !exists("chamberID", where=df) & !exists("ChamberID", where=df)){
    df[["chamberID"]] <- df[["Plot"]]
    df[["Plot"]] <- NULL
  }  
  if (exists("ChamberID", where=df) & !exists("chamberID", where=df)){
    df[["chamberID"]] <- df[["ChamberID"]]
    df[["ChamberID"]] <- NULL
  }  
  if (exists("Pressure_mb", where=df)){
    df[["Pressure"]] <- df[["Pressure_mb"]]
    df[["Pressure_mb"]] <- NULL
  } 
  if (exists("Temperature_degC", where=df)){
    df[["Temp"]] <- df[["Temperature_degC"]]
    df[["Temperature_degC"]] <- NULL
  }  
  if (exists("Volume_m3", where=df)){
    df[["volume_m3"]] <- df[["Volume_m3"]]
    df[["Volume_m3"]] <- NULL
  }  
  if (exists("Volume", where=df)){
    df[["volume_m3"]] <- df[["Volume"]]
    df[["Volume"]] <- NULL
  }  
  if (exists("Area_m2", where=df)){
    df[["area_m2"]] <- df[["Area_m2"]]
    df[["Area_m2"]] <- NULL
  }  
  # Flux Measurement variables  
  if (exists("MmntID", where=df)){
    df[["mmntID"]] <- df[["MmntID"]]
    df[["MmntID"]] <- NULL
  }
  ## Gas sample data
  if (exists("Time_mins_exact", where=df)){
    df[["time_mins"]] <- df[["Time_mins_exact"]]
    df[["Time_mins_exact"]] <- NULL
  } 
  if (exists("time_mins_exact", where=df)){
    df[["time_mins"]] <- df[["time_mins_exact"]]
    df[["time_mins_exact"]] <- NULL
  } 
  if (exists("Time_mins", where=df)){
    df[["time_mins"]] <- df[["Time_mins"]]
    df[["Time_mins"]] <- NULL
  }  
    
  return(df)
}

## ----read_GC_input----
#' Reads a GC input file
#'
#' This function reads a GC input file.
#' @param fname A GC input file.
#' @keywords GC input
#' @export
#' @seealso \code{\link{convert_GC_output}} for the higher-level function which calls this.
#' @examples
#' read_GC_input(fname)

read_GC_input <- function(fname){
#fname <- "F:/GC_data/CINAG_2016_06_16b_CM/CINAG_7-16b 2016-08-01 09-56-13/CINAG_2016_06_16b_CM.xls"
#fname <- "F:/GC_data/CINAG_2016_06_16c_CM/CINAG_1-16a_1-6b 2016-08-01 16-42-58/CINAG_2016_06_16c_CM.xls"
#fname <- "S:/DISE_Instrumentation/GC/Agilent/GC_data/Juliette/20160810_JM.xls"
  fname_GC_input <- paste(file_path_sans_ext(fname), "_GC_input.xlsx", sep="")

  # convert GC_input to a csv files named by worksheet name .csv
  #f <- excelToCsv(file_path = fname_GC_input, target_dir = "./")

  # read standards data and rename to standard set of names
  #st_df <- read.csv("Standards.csv")
  
  st_df <- read_excel(fname_GC_input, sheet="Standards")
  names(st_df) <- make.names(names(st_df))
  st_df <- standardiseNames(st_df)
  # get a list of the varying columns, = gas names
  varying <-   colnames(subset(st_df, select = -standardID))
  # convert from wide to long format
  st_wdf <- reshape(st_df, direction = "long", varying = varying,
    timevar = "gasName", v.names = "Known.conc")
  # Adds name for 1st col, in case missing
  colnames(st_wdf)[1] <- "standardID" # necessary?
  st_wdf$gasName <- varying[st_wdf$gasName]


  # read chamber data
  #ch_df <- read.csv("Chambers.csv")
  ch_df <- read_excel(fname_GC_input, sheet="Chambers")
  names(ch_df) <- make.names(names(ch_df))
  ch_df <- standardiseNames(ch_df)

  # read flux mmnt data
  #fm_df <- read.csv("Flux_Measurements.csv")
  fm_df <- read_excel(fname_GC_input, sheet="Flux_Measurements")
  names(fm_df) <- make.names(names(fm_df))
  fm_df <- standardiseNames(fm_df)

  # read sample data
  #sm_df <- read.csv("GC_samples.csv")
  sm_df <- read_excel(fname_GC_input, sheet="GC_samples")
  names(sm_df) <- make.names(names(sm_df))
  sm_df <- standardiseNames(sm_df)

  return(list(st_wdf=st_wdf, ch_df=ch_df, fm_df=fm_df, sm_df=sm_df))
}

## ----convert_GC_output----
#' Converts Agilent GC output to a standard format
#'
#' This function converts Agilent GC output.
#' @param filelistIn A file listing the Agilent GC output files to be processed.
#' @keywords GC input
#' @export
#' @seealso \code{\link{calcFlux}} for the higher-level function which calls this.
#' @examples
#' convert_GC_output("f:/0Peter/misc/stats/GCflux/filelist_aGCxls.txt")
#' convert_GC_output("S:/DISE_Instrumentation/GC/Agilent/processing/filelist_aGCxls.txt")

convert_GC_output <- function(filelistIn){
#filelistIn <- "S:/DISE_Instrumentation/GC/Agilent/processing/filelist_aGCxls.txt"
#filelistIn <- "./filelist_aGCxls.txt"
  filelistOut <- "./filelist.txt"

  ### if mixed single digit numbers & text, we need extra check / padding:
  zeroPad <- function(ch, len.out, num.zeros = len.out[1] - nchar(ch)){
  len.out = 3
  num.zeros = len.out[1] - nchar(ch)
    rep("0", num.zeros)
    paste0(paste(rep("0", num.zeros), collapse = ""), ch)
  }

  # add yaml header lines
  yamlHeader1 <- "dataSource: aGC  # Options are aGC, GC, QCL, GodChamber, EGM, VaisalaChamber"
  yamlHeader2 <- "gasName: [N2O] # not used if dataSource = aGC"
  write(yamlHeader1, file = filelistOut)
  write(yamlHeader2, file = filelistOut, append=TRUE)

  files_df <-  read.csv(file = filelistIn, header = FALSE, strip.white = TRUE, stringsAsFactors = FALSE)
  #nGas <- length(files_df)
  nFileSet <- length(files_df[,1])

  for (i in 1:nFileSet){
  #i <- 1
    files_df[i,2] <- paste(file_path_sans_ext(files_df[i,1]), ".csv", sep="")
    df      <- read_GC_output(files_df$V1[i])
    gcinput <- read_GC_input(files_df$V1[i])
    
    df <- merge(df, gcinput$st_wdf, by.x=c("gasName", "chamberID"), sort = TRUE,
              all.x = TRUE,   by.y=c("gasName", "standardID")) 

    # convert numbers to characters, keeping leadiong zero for single digits, as in Sample Name
    gcinput$ch_df$chamberID <- str_pad(gcinput$ch_df$chamberID, width = 2, side = "left", pad = "0")  
    
    df <- merge(df, gcinput$ch_df, by.x=c("site", "chamberID"), sort = TRUE, all.x = TRUE,  
                                   by.y=c("site", "chamberID"))
    # implicitly match by.x=c("site", "chamberID", "year", "mon", "mday", "sampleName")
    # convert numbers to characters, keeping leadiong zero for single digits, as in Sample Name
    gcinput$fm_df$chamberID <- str_pad(gcinput$fm_df$chamberID, width = 2, side = "left", pad = "0")  
       
    df <- merge(df, gcinput$fm_df, sort = TRUE, all.x = TRUE)

    # match by "sampleName", so remove duplicate columns
    gcinput$sm_df <- subset(gcinput$sm_df, select = -c(site, chamberID, year, mon, mday, time_label))
    df <- merge(df, gcinput$sm_df, sort = TRUE, all.x = TRUE, # suffixes = c("",".y"),
      by.x=c("sampleName"), 
      by.y=c("sampleName"))
      
    #df %>% View
    #nrow(df)
    #nrow(distinct(df))
    
    # final merge adds sm_df records for all standard samples, producing duplicate rows
    # remove duplicates with dplyr::distinct
    df <- distinct(df)
    #summary(df)
    #nrow(df)
    ### change names in template to match lower case etc
    df <- standardiseNames(df)
    
    ### some variables names in RCflux should be changed, but consistency with old version just now
    # need to give different names to distinguish peak area (mV.s) and chamber area (m2)
    # peakArea and area_m2
    ###df$Area <- df$Area_m2

    df$Sec <- as.numeric(df$time_mins) * 60
    #df$Plot <- df$chamberID

    # put in original order (Vial no. is order in autosampler)
    df <- arrange(df, gasName, Vial)
    #df %>% View
    names(df)  
    summary(gcinput$sm_df)  
    write.csv(df, file=files_df[i,2], row.names = FALSE)
  }
  write.table(files_df[,2], file = filelistOut, row.names=FALSE, col.names = FALSE, append=TRUE)
}

## ----calcFlux----
#' This function calculates gas fluxes from GC sample data
#'
#' This function converts Agilent GC output.
#' @param filelist A file listing the Agilent GC output files to be processed.
#' @keywords GC input
#' @export
#' @seealso \code{\link{convert_GC_output}} for the higher-level function which calls this.
#' @examples
#' calcFlux("./filelist.txt")

calcFlux <- function(filelist){
#filelist <- "./filelist.txt"
  # number of models used to fit to the Conc vs time data
  nModel <- 6  # linear, quadratic, linear 2nd deriv, quadratic 2nd deriv, asymptotic, HMR

  fileOut <- "RCfluxOutput.csv"
  header <- scan(filelist, sep = "\n", nlines = 2, strip.white = FALSE, what = character())
  readin1 <- yaml.load(header[1])
  readin2 <- yaml.load(header[2])
  v_gasName <- unique(readin2$gasName)

  files_df <-  read.csv(file = filelist, skip = 2, header = FALSE, strip.white = TRUE, stringsAsFactors = FALSE)
  nGas <- length(files_df)
  nFileSet <- length(files_df[,1])

  #rm(out_df)
  for (iFileSet in 1:nFileSet){
  #iFileSet <- 1
    print(cat("Processing ", files_df[iFileSet,1], iFileSet, "\n"))
    par(mfrow=c(3,3))
    #dev.new()
    in_df <-  read.csv(files_df[iFileSet,1], header = TRUE, na.strings = c("NA", "NAN", "*", "-99"), row.names=NULL)  

    if (readin1$dataSource == "aGC"){
      v_gasName <-   as.character(unique(in_df$gasName))
      nGas <-              length(unique(in_df$gasName))
    }
    pdf(paste(files_df[iFileSet,1], "_calibration.pdf", sep = ""))
    par(mfrow=c(3,3))
    # need to generate this df to instantiate a list of dfs
    df <- subset(in_df,  is.na(Known.conc) & Valid.sample == 1 & gasName == v_gasName[1])

    for (iGas in 1:nGas){
    #iGas <- 1
      #print(cat("Processing ", files_df[iFileSet,1], iFileSet, iGas, v_gasName[iGas], "\n"))
      if (readin1$dataSource == "GC") in_df <-  read.csv(files_df[iFileSet,iGas], header = TRUE, na.strings = c("NA", "NAN", "*", "-99"), row.names=NULL)  
      # df including standards and samples
      df_iGas <-  subset(in_df, Valid.sample == 1 & gasName == v_gasName[iGas])
      # df excluding standards
      df[[iGas]] <- subset(in_df, is.na(Known.conc) & Valid.sample == 1 & gasName == v_gasName[iGas])
      df[[iGas]]$TimeSinceClosure <- df[[iGas]]$Sec
      #df[[iGas]]$MmntNo <- df[[iGas]]$Plot
      if (exists("Plot",   where = df[[iGas]])) df[[iGas]]$mmntID <- df[[iGas]]$Plot
      if (exists("MmntNo",   where = df[[iGas]])) df[[iGas]]$mmntID <- df[[iGas]]$MmntNo
      if (exists("MmntID", where = df[[iGas]])) df[[iGas]]$mmntID <- df[[iGas]]$MmntID
      #names(df[[iGas]])   
      # calculate weightings for standards to use in regression
      densfun <- approxfun(density(df_iGas$peakArea))
      df_iGas$density_wt <- densfun(df_iGas$peakArea)

      # subset standards and do calibration regression ## need to add weighting    
      st_df <-      subset(df_iGas, !is.na(Known.conc) & Valid.sample == 1 & gasName == v_gasName[iGas])
      plot(density(df_iGas$peakArea), col=2, main = v_gasName[iGas])
      points(st_df$peakArea, st_df$density_wt)

      calib    <- with(st_df, lm(Known.conc ~ peakArea))
      calib_wt <- with(st_df, lm(Known.conc ~ peakArea, weights = density_wt))
      with(st_df, plot(Known.conc ~ peakArea, main = basename(files_df[iFileSet,1])))
      ### tmp
      #with(df[[iGas]], plot(Known.conc ~ Amount))
      abline(coef(calib))
      abline(coef(calib_wt), col=3)
      df[[iGas]]$Cobs <- predict(calib_wt, df[[iGas]])
      df[[iGas]]$Known.conc <- NULL
      with(df[[iGas]], points(Cobs ~ peakArea, col = "red"))
      with(df[[iGas]], hist(Cobs, main = NULL))
    } # nGas
    dev.off() # close calib plot png file
    #dev.new() 
    #userEntry <- readkey()
    #if (userEntry == "n") break

    nMmnt <- length(unique(df[[iGas]]$mmntID))
    #MmntID   <- unique(df[[iGas]]$MmntNo)
    #MmntID   <- sort(MmntID)
    flux   <- array(data = NA, dim = c(nMmnt,nModel+1,nGas)) #+1 to add best-fit model
    ci95lo <- array(data = NA, dim = c(nMmnt,nModel+1,nGas))
    ci95hi <- array(data = NA, dim = c(nMmnt,nModel+1,nGas))
    adjr2  <- array(data = NA, dim = c(nMmnt,nModel+1,nGas))

    pb <- txtProgressBar(min = 1, max = nGas)
    for (iGas in 1:nGas){
      #iGas <- 1
  #   info <- sprintf("Doing calculations for %s in file set %d", readin2$gasName[iGas], iFileSet)
  #    setWinProgressBar(pb, (iFileSet-1)*nGas+iGas, "Progress with regression modelling", info)
      # remove first point from each mmnt - invalid diffs
      #df <- na.omit(df)
      #df[[iGas]] <- droplevels(df[[iGas]])
      #dfg$MmntNo <- as.character(dfg$MmntNo)
      #dfg <- groupedData(Cobs ~ TimeSinceClosure | MmntNo, order.groups = TRUE, data = df[[iGas]])
      dfg <- df[[iGas]]
      dfg <- dfg[ do.call(order, dfg), ]
      
    # calculate mean air density for each mmnt, should be ~40 mol m-3
      dfg$rho <- with(dfg, Pressure*100 / (8.31447*(Temp+273.15)))
      dfg$mult <- with(dfg, rho * volume_m3/ area_m2)
      # get one rho and mult per flux - using first Timepoint, but could be average
      #mult <- with(dfg, mult[Timepoint == 1])
      dfm <- ddply(dfg, .(mmntID), numcolwise(mean, na.rm = TRUE))
      mult <- dfm$mult
      
      nobs <- ddply(dfg, .(mmntID), numcolwise(length))
      nobs <- nobs[2]; colnames(nobs) <- "nobs"
      #dfm <- ddply(dfg, .(mmntID), numcolwise(mean, na.rm = TRUE))
      #mean_df <- dfm
      mult_df <- with(dfm, data.frame(mmntID, rho, nobs))
      dfg <- merge(dfg, mult_df, by.x = "mmntID", by.y = "mmntID", all.x = TRUE) 
      dfg <- subset(dfg, !is.na(Cobs) & 
                         !is.na(TimeSinceClosure) &
                         !is.na(mmntID))
      nrow(dfg)
      # need to remove variable with missing values
      dfg <- dfg[,c("Cobs", "TimeSinceClosure", "mmntID", 
                      "nobs", "rho.y", "volume_m3", "area_m2")]
      
      # fit regression models
      lfit <-   lmList(Cobs ~ TimeSinceClosure | mmntID, na.action = na.omit, data=dfg)
      qfit <-   lmList(Cobs ~ TimeSinceClosure + I(TimeSinceClosure^2) | mmntID, na.action = na.omit, data=dfg)
      lmc <- lmeControl(niterEM=500, maxIter = 500, opt="optim")
  # doesn't work with n < 4; need if block
      if (min(dfg$nobs) > 3){ # can only do nonlinear fit with 4 or more
       nfit <- nlsList(Cobs ~ SSasymp(TimeSinceClosure,a,b,c) | mmntID, control=lmc, na.action = na.pass, data=dfg)
      } else {
        nfit <- lfit
      }

      # do differencing of concs
      dCdt <- diff(dfg$Cobs) / diff(dfg$TimeSinceClosure)
      dfg$dCdt <- c(0,dCdt)
      ## need to do fits first, then add diffs to dfg, otherwise NAs cause error in lmList ##
      #dfg$dCdt <- NULL
      dfg <- within(dfg, dCdt[TimeSinceClosure == 0] <- NA)
      # regression on 2nd derivative
      lfit2 <-   lmList(dCdt ~ TimeSinceClosure | mmntID, na.action = na.omit, data=dfg)
      qfit2 <-   lmList(dCdt ~ TimeSinceClosure + I(TimeSinceClosure^2) | mmntID, na.action = na.omit, data=dfg)
      # need to remove diffs with missing values, to make nlsList work
      #dfg$dCdt <- NULL

      # save coefs for flux calc
      lfit_coef <- coefficients(lfit)
      qfit_coef <- coefficients(qfit)
      nfit_coef <- coefficients(nfit)
      dlfit_coef <- coefficients(lfit2)
      dqfit_coef <- coefficients(qfit2)
      # calculate fluxes
      ## need to have one mult per flux, not sample as now - don't conform
      flux[ ,1,iGas] <- lfit_coef[ ,2] * mult
      flux[ ,2,iGas] = qfit_coef[ ,2] * mult
      flux[ ,3,iGas] = dlfit_coef[ ,1] * mult
      flux[ ,4,iGas] = dqfit_coef[ ,1] * mult
      # get asymptotic flux by finite difference
      deltat <- 0.01
      # is.numeric(nfit$a) will be true if fitting worked at least once
      # then don't need nobs > 3 test
      if (min(dfg$nobs) > 3 & is.numeric(coefficients(nfit)$a)){ # can only do nonlinear fit with 4 or more
        a <- nfit_coef[ ,1]; b <- nfit_coef[ ,2]; c <- nfit_coef[ ,3]	
        dC_by_dt0_nfit <- ((a+(b-a)*exp(-exp(c)*deltat)) -
                           (a+(b-a)*exp(-exp(c)*0    ))) / deltat
        flux[ ,5,iGas] =  dC_by_dt0_nfit * mult
      }
      
      # write a text file for input to HMR
      HMRinput_df <- cbind.data.frame(as.character(dfg$mmntID), (dfg$rho.y * dfg$volume_m3), dfg$area_m2, dfg$TimeSinceClosure, dfg$Cobs)
      colnames(HMRinput_df)<- c("Series", "V", "A", "Time", "Concentration") 
      write.table(HMRinput_df, file = "HMRinput.txt", sep = ",", row.names = FALSE, col.names = TRUE)	
      # run HMR using ppm input ### need to multiply v by rho
      #dev.new() # no graphics output, but removes any plots on existing device
      HMR(filename= 'HMRinput.txt', series = NA, dec = '.', sep = ',', JPG = FALSE, PS = FALSE,PHMR = FALSE, npred = 500, LR.always = TRUE, FollowHMR = TRUE, ngrid = 1000)
      #Read HMR output file ## just assign it from hmr function?
      HMRoutput_df <- read.table("HMR - HMRinput.txt", skip = 1, sep=",", 
      colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "character"))
      colnames(HMRoutput_df)<- c("mmntID","EstFlux", "StdErrEstFlux", "PValue", "Low95", "High95", "Method", "Warning", "LinEstFlux", "LinStdErrEstFlux", "LinPvalue", "LinLow95", "LinHigh95", "LinWarning")
      HMRoutput_df <- within(HMRoutput_df, EstFlux[Method != "HMR" & Method != "LR"] <- NA)
      HMRoutput_df <- within(HMRoutput_df, Method[Method == "LR"]  <- 1)
      HMRoutput_df <- within(HMRoutput_df, Method[Method == "HMR"] <- 6)
      flux[ ,6,iGas]        <- HMRoutput_df$EstFlux
      flux[ ,nModel+1,iGas] <- HMRoutput_df$EstFlux
      bestFitMethod <- HMRoutput_df$Method
      
      flux[ ,nModel+1,iGas] <- HMRoutput_df$EstFlux
      flux[bestFitMethod == "No flux", nModel+1, iGas] <- 
      flux[bestFitMethod == "No flux", 1, iGas]
      
      #flux_best[is.na(flux_best)]	<- flux_nfit[is.na(flux_best)]
      #bestFitMethod[is.na(flux_best)]	<- "Asymptotic"
      #flux_best[is.na(flux_best)]	<- flux[ ,2,1][is.na(flux_best)]
      #bestFitMethod[is.na(flux_best)]	<- "Quadratic"
      #flux_best[is.na(flux_best) & flux[ ,1,1] > flux[ ,2,1]]	<- flux[ ,1,1][is.na(flux_best) & flux[ ,1,1] > flux[ ,2,1]]
      #bestFitMethod[is.na(flux_best) & flux[ ,1,1] > flux[ ,2,1]]	<- "LR"  

      # save the HMR bestfit flux 95 % CIs in flux units
      ci95lo[ ,nModel+1,iGas] <- HMRoutput_df$Low95
      ci95hi[ ,nModel+1,iGas] <- HMRoutput_df$High95

      # save the linear flux 95 % CIs in flux units
      ci95lo[ ,1,iGas] <- intervals(lfit)[ , 1, 2] * mult
      ci95hi[ ,1,iGas] <- intervals(lfit)[ , 3, 2] * mult

      ci95lo[bestFitMethod == "No flux", nModel+1, iGas] <- 
      ci95lo[bestFitMethod == "No flux", 1, iGas]
      ci95hi[bestFitMethod == "No flux", nModel+1, iGas] <- 
      ci95hi[bestFitMethod == "No flux", 1, iGas]
      bestFitMethod[bestFitMethod == "No flux"]	<- 1


      # and the r2 values
      slfit <- summary(lfit)
      adjr2[ ,1,iGas] <- do.call(rbind, lapply(slfit$adj.r.squared, unlist))
      if (min(dfg$nobs) > 3){
        sqfit <- summary(qfit)
      } else {
        sqfit <- slfit
      }
      adjr2[ ,2,iGas] <- do.call(rbind, lapply(sqfit$adj.r.squared, unlist))
      #slfit2 <- summary(lfit2)
      #adjr2[ ,4,1] <- do.call(rbind, lapply(slfit2$adj.r.squared, unlist))
      #sqfit2 <- summary(qfit2)
      #adjr2[ ,5,1] <- do.call(rbind, lapply(sqfit2$adj.r.squared, unlist))

      # put the predictions in the data frame
      dfg$lfit_pred <- predict(lfit)
      dfg$qfit_pred <- predict(qfit)
      dfg$nfit_pred <- predict(nfit)
      #dfg$dlfit2_pred <- predict(lfit2)
      #dfg$dqfit2_pred <- predict(qfit2)
      #df[[iGas]] <- dfg # save additions

      # plot the results
      dev.new()
      p <- ggplot(dfg, aes(TimeSinceClosure, Cobs)) + theme_bw()
      p <- p + xlab(expression(paste(Enclosure~time*" / "*~s)))
      p <- p + ylab(expression(paste(Concentration*" / "*~mu~mol~mol^-1)))
      p <- p + geom_point(size = 2)
      p <- p + stat_smooth(method=lm)
      p <- p + geom_line(aes(TimeSinceClosure, predict(lfit)), colour = "red")
      p <- p + geom_line(aes(TimeSinceClosure, predict(qfit)), colour = "green")
      if (any(!is.na(predict(nfit)))){
        p <- p + geom_line(aes(TimeSinceClosure, predict(nfit)), colour = "blue")
      }
      p <- p + ggtitle(bquote(atop(.(v_gasName[iGas]), atop(.(files_df[iFileSet,1]), ""))))
      p <- p + facet_wrap( ~ mmntID, scales = "free")
      #p
      print(p)
      ggsave(p, file=paste(files_df[iFileSet,1], "_", v_gasName[iGas], ".pdf", sep=""), scale=1)

      out_thisLoop_df <- cbind.data.frame(gasName = v_gasName[iGas], 
        filename = basename(files_df[iFileSet,1]), 
        fullpathname = files_df[iFileSet,1], 
        mmntID = rownames(lfit_coef), 
        dfm$mmntID, 
        flux_linear = flux[ , 1,iGas], 
        flux_quadratic = flux[ , 2,iGas], 
        flux_linear2nd = flux[ , 3,iGas], 
        flux_quadratic2nd = flux[ , 4,iGas], 
        flux_asymptotic = flux[ , 5,iGas], 
        flux_HMR = flux[ , 6,iGas], 
        flux_bestfit = flux[ , 7,iGas], 
        ci95lo_linear  = ci95lo[ ,1,iGas],         ci95hi_linear  = ci95hi[ ,1,iGas],        # intervals on linear fit
        ci95lo_bestfit = ci95lo[ ,nModel+1,iGas], ci95hi_bestfit = ci95hi[ ,nModel+1,iGas],  # intervals on best fit
        adjr2_linear = adjr2[ ,1,iGas], adjr2_quadratic = adjr2[ ,2,iGas], 
        bestFitMethod = bestFitMethod)

        # merge with means
        out_thisLoop_df <- merge(out_thisLoop_df, dfm, all.x = TRUE) 

      if (exists("out_df")){
        out_df <- rbind(out_df, out_thisLoop_df)
      } else {
        out_df <- out_thisLoop_df
      }
      #summary(out_thisLoop_df)
      #summary(out_df)
      #summary(df[[iGas]])
      #tdf <- merge(out_df, df[[iGas]], by.x = "mmntID", by.y = "mmntID", all.x = TRUE) 
      #tdf <- distinct(tdf)
      #nrow(tdf)
      #summary(tdf)
      setTxtProgressBar(pb, iGas)
    } # iGas
  } # fileset
  close(pb)

  username <- Sys.info()[["user"]]
  out_df <- cbind(username, out_df)
  #colnames(out_df) <- c("username", "gasName", "filename", "fullpathname", "MmntID", 
  #      "flux_linear", "flux_quadratic", 
  #      "flux_linear2nd", "flux_quadratic2nd",
  #      "flux_asymptotic", "flux_HMR", "flux_bestfit",
  #      "ci95lo_linear", "ci95hi_linear",   # intervals on linear fit
  #      "ci95lo_bestfit", "ci95hi_bestfit", # intervals on best fit
  #      "adjr2_linear", "adjr2_quadratic", 
  #      "bestFitMethod")
        
  # for CEH GC mmnts, we can equate Plot = MmntID = ChamberID
  # generally, would need a table for multiple mmnts on same chamber (replicates, dark/light)
  #out_df$chamberID <- out_df$MmntID
  #out_df$MmntID <- NULL

  # write output data frame to file
  write.table(out_df, file = fileOut, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE)
}

# The MIT License (MIT)
#
# Copyright (c) 2012 Schaun Jacob Wheeler
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

## ----excelToCsv----
#' This function calculates excelToCsv
#'
#' This function converts excelToCsv.
#' @param file_path A file listing the Agilent GC output files to be processed.
#' @param keep_sheets Keep the sheets or not
#' @param target_dir Directory to save csv file in
#' @param ... Further arguments
#' @keywords GC input
#' @export
#' @seealso \code{\link{convert_GC_output}} for the higher-level function which calls this.

excelToCsv <- function(file_path, keep_sheets = NULL, target_dir = NULL, ...) {
  
  temp_already <- list.files(tempdir())
  
  if(is.null(target_dir)) {
    file_root <- gsub("([[:print:]]+(/|\\\\))[[:print:]]+", "\\1", file_path)
  } else if(!is.null(target_dir) & target_dir != FALSE) {
    file_root <- target_dir
  }
  
  file_name <- gsub("[[:print:]]+(/|\\\\)", "", file_path)
  file_ext <- gsub("[[:print:]]+(.xls.?)", "\\1", file_path)
  
  converter_file <- file(paste0(tempdir(),"/", "converter.vbs"))
  
  writeLines(
    c('rem  XLS_To_CSV.vbs',
      'rem =============================================================',
      'rem  convert all NON-empty worksheets in an Excel file to csv',
      'rem  CSV file names will default to Sheet names',
      'rem  output folder defaults to the folder where the script resides or',
      'rem  if path is specified with the input file, that path is used',
      'rem  ',
      'rem  input parameter 1:  Excel path\\file in argument 1 ',
      'rem                     (if path is not specified, the current path is defaulted)',
      'rem  ',
      'rem ============================================================',
      '',
      'Dim strExcelFileName',
      'Dim strCSVFileName',
      '',
      'strExcelFileName = WScript.Arguments.Item(0)',
      '',
      'rem get path where script is running',
      'Set fso = CreateObject ("Scripting.FileSystemObject")',
      'strScript = Wscript.ScriptFullName',
      'strScriptPath = fso.GetAbsolutePathName(strScript & "\\..")',
      '',
      'rem If the Input file is NOT qualified with a path, default the current path',
      'LPosition = InStrRev(strExcelFileName, "\\") ',
      'if LPosition = 0 Then ',
      '    strExcelFileName = strScriptPath & "\\" & strExcelFileName',
      'strScriptPath = strScriptPath & "\\" ',
      'else ',
      'strScriptPath = Mid(strExcelFileName, 1, LPosition) ',
      'End If',
      'rem msgbox LPosition & " - " & strExcelFileName & " - " & strScriptPath',
      '',
      'Set objXL = CreateObject("Excel.Application")',
      'Set objWorkBook = objXL.Workbooks.Open(strExcelFileName)',
      'objXL.DisplayAlerts = False',
      '',
      'rem loop over worksheets',
      '  For Each sheet In objWorkBook.Sheets  ',
      'if objXL.Application.WorksheetFunction.CountA(sheet.Cells) <> 0 Then ',
      'rem             sheet.Rows(1).delete',
      'sheet.SaveAs strScriptPath & sheet.Name & ".csv", 6',
      '   End If',
      '  Next',
      '',
      'rem clean up  ',
      'objWorkBook.Close ',
      'objXL.quit',
      'Set objXL = Nothing ',
      'Set objWorkBook = Nothing',
      'Set fso = Nothing',
      '',
      'rem end script'),
    con = converter_file)
  
  close(converter_file)
  
  file.copy(file_path, tempdir())
  
  orig_wd <- getwd()
  setwd(tempdir())
  
  file.rename(file_name, paste0("filetoconvert", file_ext))
  
  shell(paste("converter.vbs", 
    paste0("filetoconvert", file_ext)), intern = TRUE)
  
  setwd(orig_wd)
  
  if(is.null(keep_sheets)) {
    keep_sheets <- gsub("\\.csv", "", list.files(tempdir(), pattern = "\\.csv"))
  }
  
  file_flags <- paste0(keep_sheets, ".csv")
  
  if(is.null(target_dir) | (!is.null(target_dir) & target_dir != FALSE)) {
    for(i in 1:length(file_flags)) {
      file.copy(
        paste0(tempdir(), "/", file_flags[i]), file_root, overwrite = TRUE)
    }
  } else {
    
    all_files <- lapply(file_flags, function(x) {
      csv_file <- read.csv(paste0(tempdir(), "/", x), 
        as.is = TRUE, na.strings = c("#N/A", "NA", "N/A", "?", ""))
      
      csv_file[,sapply(csv_file, function(y) mean(is.na(y), na.rm = TRUE)) < 1]
    })
    
    if(length(all_files) == 1) {
      all_files <- all_files[[1]]
    } else {
      names(all_files) <- keep_sheets
    }
  }
  
  suppressWarnings(file.remove(
    paste0(tempdir(),
      "/",
      list.files(tempdir())[!(list.files(tempdir()) %in% temp_already)])))
  
  if(!is.null(target_dir) & target_dir == FALSE) {
    all_files
  }
}