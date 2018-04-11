
# Server function for MoistureBalanceInteractive 18.03

server <- function(input, output, session) {

  decimalTime <- function(time) {
    time = as.numeric(unlist(strsplit(time, ":")))
    time = time[1]*60+time[2]+time[3]/60
    return(time)
  }
  
  fileInfo <- reactiveValues(name = NULL, path = NULL)
  ranges2 <- reactiveValues(x = NULL, y = NULL) # Controls axes in reactive plot: plot2
  txtfileout <- NA
  txtout <- "In the beginning...\n\n"
  
  
  # ******  Attempt to read different file formats and generate a data table that can be processed by the rest of the script as rawData()
  # This will have to be reactive, using input$file1 as starting point and parsing the file based on the value of dataFormat (MB18.xx, MB45, XYZ)
  # The output will be a dataframe with columns: time, MR, model.
  #
  # For MB45 data with multiple data sets within the same file, use a conditional radiobutton to select the dataset to fit and plot.
  #
  # Maybe in order to make this more flexible, the colums will be assumed to be x, y, z where z = id for the data sets.  In the case of the XYZ files, the names for x and y
  # can be extracted from the column headers if present.
  
  
  # ************** Begin file read
  
  observe({
    if (input$dataFormat == 'MB18.xx') {
      cat("MB18.xx")
      
    } else {
      if (input$dataFormat == 'MB45') {
        cat("MB45")
        
      } else {
        cat("XYZ")
        cat("...", input$dataFormat)
      }
    }
    
  })

  
  #output$viewInfo <- renderPrint({head(rawData())})
  
  

  
    rawData <- reactive({
  #rawData <- function(){   
    # cat("\nrawData\n")
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile)) {
      fileInfo$name <- NULL
      fileInfo$path <- NULL
      # cat("rawData: return(NULL)\n")
      return(NULL)
      
    } else {
      fileInfo$name <- inFile$name
      fileInfo$path <- inFile$datapath
      summaryFit <- NA
      corNLS <- NA
      corMan <- NA
      # cat("inFile$datapath:", inFile$datapath, "\n")
      # cat("fileInfo$name:", fileInfo$name, "\n")
    }
    # cat(input$dataFormat, "\n")
    
    
    if (input$dataFormat == "MB18.xx") {
      # cat(input$dataFormat, ":\n")
      # cat("rawData read.csv\n")
      r <- as.data.frame(read.csv(inFile$datapath, header= TRUE))
    } else {
      if (input$dataFormat == 'MB45') {
        # cat(input$dataFormat, ":\n")
        # cat("Process raw MB45 data\n")
        
        
        # copied from MoistureBalance18.05
        
        if (file.info(inFile$datapath)$size > 0) {
          MBData <- read.csv(inFile$datapath, sep = "\t", header = FALSE, as.is = TRUE, strip.white = TRUE, blank.lines.skip = TRUE)
          txtfileout <- "D:/T3620 dusaire/Moisture_Biochar/85_packet/"
          txtfileout <- paste(txtfileout, strsplit(fileInfo$name, ".csv")[[1]][1], "_model.txt", sep = "")
          
          # There may still be other .txt files in the directory that do not contain Ohaus MB45 data.
          # Look for specific text in line 2 of the .txt file.  Hopefully this elimiates non-data files.
          
          if (MBData[2,] == "OHAUS MB45 SN   1122272694") {
            testID <- grep("TEST ID", MBData[,], ignore.case = TRUE)  
            switchOffMode <- grep("Switchoff Mode", MBData[,], ignore.case = TRUE)  
            dryingProfile <- grep("Drying Profile", MBData[,], ignore.case = TRUE)  
            dryingTemp <- grep("Drying Temp", MBData[,], ignore.case = TRUE)  
            resultUnits <- grep("Result Units", MBData[,], ignore.case = TRUE)  
            initialWeight <- grep("Initial Weight", MBData[,], ignore.case = TRUE)
            finalWeight <- grep("Final Weight", MBData[,], ignore.case = TRUE)
            elapsedTime <- grep("Elapsed Time", MBData[,], ignore.case = TRUE)  
            finalPanTemp <- grep("Final Pan Temp", MBData[,], ignore.case = TRUE)  
            finalResult <- grep("Final Result", MBData[,], ignore.case = TRUE)
            endFlag <- grep("--End--", MBData[,], ignore.case = TRUE)  
            nSamples <- length(endFlag)
            
            # Verify that data file is correctly formatted  
            
            if (length(initialWeight) == 2 * nSamples) {
              
              # Remove duplicate Initial Weight values
              
              initialWeight <- initialWeight[seq(1, by = 2, len = nSamples)]
              beginData <- initialWeight + 1
              endData <- elapsedTime - 1
              
              for (i in 1:nSamples) {
                
                # Verify that there are more than 2 data points in the data file
                
                if (endData[i] - beginData[i] > 1) {
                  
                  # Proceed with data processing if there are 2 or more data points
                  
                  nModels = 3  # Plot and report model results from the top nModels based on cor(moisutre, fit)
                  predictDF <- NULL
                  predictDFW <- NULL
                  predictNext <- NULL
                  corr <- NULL
                  
                  #   Extract parameter info
                  
                  testID[i] <- gsub("\\s+", "", strsplit(MBData[testID[i],], ":")[[1]][2])
                  switchOffMode[i] <- gsub("\\s+", "", strsplit(MBData[switchOffMode[i],], "Mode.")[[1]][2])
                  dryingProfile[i] <- gsub("\\s+", "", strsplit(MBData[dryingProfile[i],], "Profile.")[[1]][2])  
                  dryingTemp[i] <- gsub("\\s+", "", strsplit(MBData[dryingTemp[i],], "Temp.")[[1]][2])
                  resultUnits[i] <- gsub("\\s+", "", strsplit(MBData[resultUnits[i],], "Units.")[[1]][2])
                  
                  initialWeight[i] <- gsub("\\s+", "", strsplit(MBData[initialWeight[i],], "Weight.")[[1]][2])
                  elapsedTime[i] <- gsub("\\s+", "", strsplit(MBData[elapsedTime[i],], "Time.")[[1]][2])
                  finalWeight[i] <- gsub("\\s+", "", strsplit(MBData[finalWeight[i],], "Weight.")[[1]][2])
                  
                  initialWeight[i] <- gsub("[^0-9\\.]", "", initialWeight[i])
                  finalWeight[i] <- gsub("[^0-9\\.]", "", finalWeight[i])
                  finalPanTemp[i] <- gsub("\\s+", "", strsplit(MBData[finalPanTemp[i],], "Temp.")[[1]][2])
                  finalPanTemp[i] <- gsub("[^0-9\\.]", "", finalPanTemp[i])
                  finalResult[i] <- gsub("\\s+", "", strsplit(MBData[finalResult[i],], "Result.")[[1]][2])
                  finalResult[i] <- gsub("[^0-9\\.]", "", finalResult[i])
                  
                  rawData <- MBData[beginData[i]:endData[i],]
                  rawData <- gsub("[^0-9\\.\\:]", " ", rawData)
                  rawData <- gsub("\\s+", ",", rawData)
                  rawData <- paste(rawData, i)
                  rawData <- read.table(text = rawData, sep = ",", header = FALSE,
                                        col.names = c('time', 'temperature', 'moisture', 'sample'),
                                        as.is = TRUE)
                  
                  # Convert time from character string to time element.  This will append the current date to the date-time element.
                  # The time may need to be converted to decimal time depending on how the fitting method represents time.
                  
                  # rawData$time <- as.POSIXct(rawData$time,format="%H:%M:%S")
                  
                  dectime = sapply(rawData$time,decimalTime)
                  
                  # Calculate moisture ratio MR, and include in rawData
                  
                  MR <- round(1 - rawData$moisture / max(rawData$moisture, na.rm = TRUE), 3)
                  
                  # Calculate masses based on initialWeight(s)
                  
                  mass <- as.numeric(initialWeight[i]) - as.numeric(initialWeight[i]) * rawData$moisture / 100
                  
                  # Include mass in rawData
                  
                  rawData <- cbind(rawData[, 1:3], dectime, mass, MR, rawData[, 4])
                  
                  # For some reason this process removes the name from the sample columm.  Re-assign column names.
                  
                  names(rawData) <-  c('time', 'temperature', 'moisture', 'dectime', 'mass', 'MR', 'sample')
                  # print(c('time', 'temperature', 'moisture', 'dectime', 'mass', 'MR', 'sample'))
                  # cat("Number of samples: ", nSamples, "\n")
                  
                  
                  
                }
              }
              
              if (nSamples > 1) {
                
                updateRadioButtons(session, "sampleNumber",
                                   choices = seq(1, nSamples)
                )
                
              }
              # cat("sampleSelector:", input$sampleNumber, "\n")
              r <- subset(rawData, rawData$sample == input$sampleNumber)
              r <- data.frame(rawData$dectime, rawData$MR, "raw")
              colnames(r) <- c("time", "MR", "model")
            }
          }
        }
        
        
        
        
        
        # end MoistureBalance18.05
        
        
        
        
        
        
        
        
        # cat("Processed MB45 data\n")
        
      } else {
        # process XYZ data
        # cat("Process ", input$dataFormat, "data\n")
        
        if (file.info(inFile)$size > 0) {
          r <- as.data.frame(read.csv(inFile$datapath, header= TRUE))
          print(head(r))
        }  
      }
    }
    
    
    
    # r <- as.data.frame(read.csv(inFile$datapath, header= TRUE))
    
    rRaw <- subset(r, r$model == "raw")
    time <- rRaw$time
    MR <- rRaw$MR
    
    if (input$modelSelect == 'Exponential') {
      
      
      nonZero <- which(MR > 0)
      MRNZ <- MR[nonZero]
      timeNZ <- time[nonZero]
      rRaw <- subset(rRaw, rRaw$MR > 0)
      tryResult <- tryCatch(lm(log(MRNZ) ~ timeNZ),
                            error=function(e) {
                              print(e)
                              print(strsplit(as.character(e), ":")[[1]][2])
                            }
      )
      if (class(tryResult) == "lm") {
        modelEx <- lm(log(MRNZ) ~ timeNZ)
        summaryFit <- summary(modelEx)
        Ex <- exp(predict(modelEx))
        # plot(timeNZ, moistureNZ, pch = 16, cex = 1.3, col = "blue", main = "Exponential Fit", xlab = "Time (min)", ylab = "Moisture")
        # lines(timeNZ, Ex, lty=2, col="red", lwd=3)
        corNLS <- cor(MRNZ, Ex)
        err <- NA
      } else {
        summaryFit <- NA
        corNLS <- NA
        err <- tryResult     
      }
      
      MRManual <- exp(input$aEx * time + input$bEx)
      corMan <- cor(MR, MRManual)
      iEx <- data.frame(time, MRManual)
      iEx <- data.frame(iEx, "iEx")
      colnames(iEx) <- c("time", "MR", "model")
      r <- rbind(rRaw, iEx)
      
    }
    
    if (input$modelSelect == 'Henderson-Pabis') {
      
      tryResult <- tryCatch(nls(MR ~ a * exp(-k * time), start = list(a = input$aHP, k = input$kHP)),
                            error=function(e) {
                              print(e)
                              print(strsplit(as.character(e), ":")[[1]][2])
                            }
      )
      if (class(tryResult) == "nls") {
        modelHP <- nls(MR ~ a * exp(-k * time), start = list(a = input$aHP, k = input$kHP))
        summaryFit <- summary(modelHP)
        HP <- predict(modelHP)
        corNLS <- as.numeric(cor(MR, HP))
        # plot(time,moisture, pch = 16, cex = 1.3, col = "blue", main = "Henderson Pabis Fit", xlab = "Time (min)", ylab = "Moisture")
        # lines(time, HP,lty=2,col="red",lwd=3)
        err <- NA
      } else {
        summaryFit <- NA
        corNLS <- NA
        err <- tryResult     
      }
      MRManual <- input$aHP * exp(-input$kHP * time)
      corMan <- cor(MR, MRManual)
      iHP <- data.frame(time, MRManual)
      iHP <- data.frame(iHP, "iHP")
      colnames(iHP) <- c("time", "MR", "model")
      r <- rbind(rRaw, iHP)
      
      
    }
    
    
    if (input$modelSelect == 'Lewis') {
      
      tryResult <- tryCatch(nls(MR ~ exp(-k * time), start = list(k = input$kLe)),
                            error=function(e) {
                              print(e)
                              print(strsplit(as.character(e), ":")[[1]][2])
                            }
      )
      if (class(tryResult) == "nls") {
        modelLe <- nls(MR ~ exp(-k * time), start = list(k = input$kLe))
        summaryFit <- summary(modelLe)
        Le <- predict(modelLe)
        corNLS <- as.numeric(cor(MR, Le))
        # plot(time,moisture, pch = 16, cex = 1.3, col = "blue", main = "Lewis Fit", xlab = "Time (min)", ylab = "Moisture")
        # lines(time, Le, lty=2, col="red", lwd=3)
        err <- NA
      } else {
        summaryFit <- NA
        corNLS <- NA
        err <- tryResult
      }
      MRManual <- exp(-input$kLe * time)
      corMan <- cor(MR, MRManual)
      iLe <- data.frame(time, MRManual)
      iLe <- data.frame(iLe, "iLe")
      colnames(iLe) <- c("time", "MR", "model")
      r <- rbind(rRaw, iLe)
    }
    
    if (input$modelSelect == 'Linear') {
      
      tryResult <- tryCatch(lm(MR ~ time),
                            error=function(e) {
                              print(e)
                              print(strsplit(as.character(e), ":")[[1]][2])
                            }
      )
      if (class(tryResult) == "lm") {
        modelLi <- lm(MR ~ time)
        summaryFit <- summary(modelLi)
        Li <- predict(modelLi)
        corNLS <- cor(MR, Li)
        # plot(time,moisture, pch = 16, cex = 1.3, col = "blue", main = "Linear Fit", xlab = "Time (min)", ylab = "Moisture")
        # lines(time, Li, lty=2, col="red", lwd=3)
        err <- NA
      } else {
        
        summaryFit <- NA
        corNLS <- NA
        err <- tryResult
      }
      MRManual <- input$bLi + input$mLi * time
      corMan <- cor(MR, MRManual)
      iLi <- data.frame(time, MRManual)
      iLi <- data.frame(iLi, "iLi")
      colnames(iLi) <- c("time", "MR", "model")
      r <- rbind(rRaw, iLi)
    }
    
    if (input$modelSelect == 'Logarithm') {
      
      tryResult <- tryCatch(lm(exp(MR) ~ time),
                            error=function(e) {
                              print(e)
                              print(strsplit(as.character(e), ":")[[1]][2])
                            }
      )
      if (class(tryResult) == "lm") {
        modelLo <- lm(exp(MR) ~ time)
        summaryFit <- summary(modelLo)
        Lo <- log(predict(modelLo))
        corNLS <- cor(MR, Lo)
        # plot(time, moisture, pch = 16, cex = 1.3, col = "blue", main = "Logarithmic Fit", xlab = "Time (min)", ylab = "Moisture")
        # lines(time, Lo, lty=2, col="red", lwd=3)
        err <- NA
      } else {
        summaryFit <- NA
        corNLS <- NA
        err <- tryResult
      }
      MRManual <- log(input$aLo * time + input$bLo)
      corMan <- cor(MR, MRManual)
      iLo <- data.frame(time, MRManual)
      iLo <- data.frame(iLo, "iLo")
      colnames(iLo) <- c("time", "MR", "model")
      r <- rbind(rRaw, iLo)
    }
    
    
    if (input$modelSelect == 'Logarithmic') {
      
      tryResult <- tryCatch(nls(MR ~ a * exp(-k * time) + c, start = list(a = input$aLoga, k = input$kLoga, c = input$cLoga)),
                            error=function(e) {
                              print(e)
                              print(strsplit(as.character(e), ":")[[1]][2])
                            }
      )
      if (class(tryResult) == "nls") {
        modelLoga <- nls(MR ~ a * exp(-k * time) + c, start = list(a = input$aLoga, k = input$kLoga, c = input$cLoga))
        summaryFit <- summary(modelLoga)
        Loga <- predict(modelLoga)
        corNLS <- cor(MR, Loga)
        # plot(time,moisture, pch = 16, cex = 1.3, col = "blue", main = "Logarithmic Fit (Yagcioglu et al. 1999)", xlab = "Time (min)", ylab = "Moisture")
        # lines(time, Loga, lty=2, col="red", lwd=3)
        err <- NA
      } else {
        summaryFit <- NA
        corNLS <- NA
        err <- tryResult
      }
      
      MRManual <- input$aLoga * exp(-input$kLoga * time) + input$cLoga
      corMan <- cor(MR, MRManual)
      iLoga <- data.frame(time, MRManual)
      iLoga <- data.frame(iLoga, "iLoga")
      colnames(iLoga) <- c("time", "MR", "model")
      r <- rbind(rRaw, iLoga)
      
    }
    
    if(input$modelSelect == 'Modified Henderson-Pabis') {
      
      tryResult <- tryCatch(nls(MR ~ a * exp(-ka * time) + b * exp(-kb * time) + c * exp(-kc * time), start = list(a = input$aMHP, b = input$bMHP, c = input$cMHP,
                                                                                                                   ka = input$kaMHP, kb = input$kbMHP, kc = input$kcMHP)),
                            error=function(e) {
                              print(e)
                              print(strsplit(as.character(e), ":")[[1]][2])
                            }
      )
      if (class(tryResult) == "nls") {
        modelMHP <- nls(MR ~ a * exp(-ka * time) + b * exp(-kb * time) + c * exp(-kc * time), start = list(a = input$aMHP, b = input$bMHP, c = input$cMHP,
                                                                                                           ka = input$kaMHP, kb = input$kbMHP, kc = input$kcMHP))
        summaryFit <- summary(modelMHP)
        MHP <- predict(modelMHP)
        # plot(time,moisture, pch = 16, cex = 1.3, col = "blue", main = "Modified Henderson Pabis Fit", xlab = "Time (min)", ylab = "Moisture")
        # lines(time, MHP,lty=2,col="red",lwd=3)
        corNLS <- cor(MR, MHP)
        err <- NA
      } else {
        summaryFit <- NA
        corNLS <- NA
        err <- tryResult
      }
      
      MRManual <- input$aMHP * exp(-input$kaMHP * time) + input$bMHP * exp(-input$kbMHP * time) + input$cMHP * exp(-input$kcMHP * time)
      corMan <- cor(MR, MRManual)
      iMHP <- data.frame(time, MRManual)
      iMHP <- data.frame(iMHP, "iMHP")
      colnames(iMHP) <- c("time", "MR", "model")
      r <- rbind(rRaw, iMHP)
    }
    
    
    
    if (input$modelSelect == 'Modified Page') {
      
      tryResult <- tryCatch(nls(MR ~ exp(-(k * time) ^ n), start = list(k = input$kMP, n = input$nMP)),
                            error=function(e) {
                              print(e)
                              print(strsplit(as.character(e), ":")[[1]][2])
                            }
      )
      if (class(tryResult) == "nls") {
        modelMP <- nls(MR ~ exp(-(k * time) ^ n), start = list(k = input$kMP, n = input$nMP))
        MP <- predict(modelMP)
        summaryFit <- summary(modelMP)
        corNLS <- as.numeric(cor(MR, MP))
        err <- NA
      } else {
        modelMP <- NA
        summaryFit <- NA
        corNLS <- NA
        err <- tryResult
      }
      
      MRManual <- exp(-(input$kMP * time) ^ input$nMP)
      corMan <- cor(MR, MRManual)
      iMP <- data.frame(time, MRManual)
      iMP <- data.frame(iMP, "iMP")
      colnames(iMP) <- c("time", "MR", "model")
      r <- rbind(rRaw, iMP)
      
    }
    
    if (input$modelSelect == 'Modified Page II') {
      
      tryResult <- tryCatch(nls(MR ~ exp(-(k * (time / L ^ 2) ^ n)), start = list(k = input$kMPII, L = input$LMPII, n = input$nMPII)),
                            error=function(e) {
                              print(e)
                              print(strsplit(as.character(e), ":")[[1]][2])
                            }
      )
      if (class(tryResult) == "nls") {
        modelMPII <- nls(MR ~ exp(-(k * (time / L ^ 2) ^ n)), start = list(k = input$kMPII, L = input$LMPII, n = input$nMPII))
        summaryFit <- summary(modelMPII)
        MPII <- predict(modelMPII)
        # plot(time,moisture, pch = 16, cex = 1.3, col = "blue", main = paste("Modified Page II Fit (L = ", L, ")", sep = ""), xlab = "Time (min)", ylab = "Moisture")
        # lines(time, MPII, lty=2, col="red", lwd=3)
        corNLS <- cor(MR, MPII)
        err <- NA
      } else {
        summaryFit <- NA
        corNLS <- NA
        err <- tryResult
      }
      
      MRManual <- exp(-(input$kMPII * (time / input$LMPII ^ 2) ^ input$nMPII))
      corMan <- cor(MR, MRManual)
      iMPII <- data.frame(time, MRManual)
      iMPII <- data.frame(iMPII, "iMPII")
      colnames(iMPII) <- c("time", "MR", "model")
      r <- rbind(rRaw, iMPII)
      
    }

    if (input$modelSelect == 'Page') {
      
      tryResult <- tryCatch(nls(MR ~ exp(-k * time ^ n), start = list(k = input$kPa, n = input$nPa)),
                            error=function(e) {
                              print(e)
                              print(strsplit(as.character(e), ":")[[1]][2])
                            }
      )
      if (class(tryResult) == "nls") {
        modelPa <- nls(MR ~ exp(-k * time ^ n), start = list(k = input$kPa, n = input$nPa))
        summaryFit <- summary(modelPa)
        Pa <- predict(modelPa)
        # plot(time,moisture, pch = 16, cex = 1.3, col = "blue", main = "Page Fit", xlab = "Time (min)", ylab = "Moisture")
        # lines(time, Pa,lty=2,col="red",lwd=3)
        corNLS <- cor(MR, Pa)
        err <- NA
      } else {
        summaryFit <- NA
        corNLS <- NA
        err <- tryResult
      }
      
      MRManual <- exp(-input$kPa * time ^ input$nPa)
      corMan <- cor(MR, MRManual)
      iPa <- data.frame(time, MRManual)
      iPa <- data.frame(iPa, "iPa")
      colnames(iPa) <- c("time", "MR", "model")
      r <- rbind(rRaw, iPa)
    }
    
    
    if (input$modelSelect == 'Simplified Fick Diffusion') {
      
      tryResult <- tryCatch(nls(MR ~ a * exp((-c * time) / L ^ 2), start = list(a = input$aSF, c = input$cSF, L = input$LSF)),
                            error=function(e) {
                              print(e)
                              print(strsplit(as.character(e), ":")[[1]][2])
                            }
      )
      if (class(tryResult) == "nls") {
        modelSF <- nls(MR ~ a * exp((-c * time) / L ^ 2), start = list(a = input$aSF, c = input$cSF, L = input$LSF))
        summaryFit <- summary(modelSF)
        SF <- predict(modelSF)
        # plot(time,moisture, pch = 16, cex = 1.3, col = "blue", main = paste("Simplified Fick Diffusion Fit (L = ", L, ")", sep = ""), xlab = "Time (min)", ylab = "Moisture")
        # lines(time, SF,lty=2,col="red",lwd=3)
        corNLS <- cor(MR, SF)
        err <- NA
      } else {
        summaryFit <- NA
        corNLS <- NA
        err <- tryResult
      }
      
      MRManual <- input$aSF * exp((-input$cSF * time) / input$LSF ^ 2)
      corMan <- cor(MR, MRManual)
      iSF <- data.frame(time, MRManual)
      iSF <- data.frame(iSF, "iSF")
      colnames(iSF) <- c("time", "MR", "model")
      r <- rbind(rRaw, iSF)
    }
    
    
    if (input$modelSelect == 'Wang-Singh') {
      
      # Calculate HP model values and append to r
      
      tryResult <- tryCatch(nls(MR ~ 1 + a * time + b * time ^ 2, start = list(a = input$aWS, b = input$bWS)),
                            error=function(e) {
                              print(e)
                              print(strsplit(as.character(e), ":")[[1]][2])
                            }
      )
      if (class(tryResult) == "nls") {  
        modelWS <- nls(MR ~ 1 + a * time + b * time ^ 2, start = list(a = input$aWS, b = input$bWS))
        WS <- predict(modelWS)
        summaryFit <- summary(modelWS)
        corNLS <- as.numeric(cor(MR, WS))
        err <- NA
      } else {
        modelWS <- NA
        summaryFit <- NA
        corNLS <- NA
        err <- tryResult
      }
      MRManual <-  1 + input$aWS * time + input$bWS * time ^ 2
      corMan <- cor(MR, MRManual)
      iWS <- data.frame(time, MRManual)
      iWS <- cbind(iWS, "iWS")
      colnames(iWS) <- c("time", "MR", "model")
      r <- rbind(rRaw, iWS)
      
    }
    
    if (input$modelSelect == 'Two-term') {
      
      tryResult <- tryCatch(nls(MR ~ a * exp(-k1 * time) + b * exp(-k2 * time), start = list(a = input$aTt, k1 = input$k1Tt, b = input$bTt, k2 = input$k2Tt)),
                            error=function(e) {
                              print(e)
                              print(strsplit(as.character(e), ":")[[1]][2])
                            }
      )
      if (class(tryResult) == "nls") {
        modelTt <- nls(MR ~ a * exp(-k1 * time) + b * exp(-k2 * time), start = list(a = input$aTt, k1 = input$k1Tt, b = input$bTt, k2 = input$k2Tt))
        summaryFit <- summary(modelTt)
        Tt <- predict(modelTt)
        # plot(time,moisture, pch = 16, cex = 1.3, col = "blue", main = "Two Term Fit (Henderson 1974)", xlab = "Time (min)", ylab = "Moisture")
        # lines(time, Tt, lty=2, col="red", lwd=3)
        t <- time
        corNLS <- cor(MR, Tt)
        err <- NA
      } else {
        summaryFit <- NA
        corNLS <- NA
        err <- tryResult
      }
      
      MRManual <-  input$aTt * exp(-input$k1Tt * time) + input$bTt * exp(-input$k2Tt * time)
      corMan <- cor(MR, MRManual)
      iTt <- data.frame(time, MRManual)
      iTt <- cbind(iTt, "iTt")
      colnames(iTt) <- c("time", "MR", "model")
      r <- rbind(rRaw, iTt)
    }
    
    return(list(r, summaryFit, corNLS, corMan))
 # }
    })
  
  iMData <- reactive({
    if (is.null(rawData()[[1]])) {
      return(NULL)
    } else {
      iM <- rawData()[[1]]
      iM <- spread(iM, model, MR)
      return(iM)
    }
  })
  
  iMPlotData <- reactive({
    if (is.null(rawData()[[1]])) {
      return(NULL)
    } else {
      iM <- subset(rawData()[[1]], rawData()[[1]]$model %in% c('raw', input$modelSelect))
      return(iM)
    }
  })
  
  
  output$viewPlotData <- renderTable({
    iMData()
  })
  
  
  
  observe({
    x <- as.character(colnames(iMData()))
    plotVariables <- as.character(names(rawData()[[1]]))
    
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character()
    
    # Can also set the label and select items
    updateCheckboxGroupInput(session, "modelID",
                             label = "Sample data to plot:",
                             choices = x,
                             selected = if ('raw' %in% x) 'raw' else NULL
    )
    
    
    updateSelectInput(session, "xcol",
                      choices = plotVariables,
                      selected = "time")
    
    updateSelectInput(session, "ycol",
                      choices = plotVariables,
                      selected = "MR")
    
    
    # Copied verbatim from Plot interaction - zoom example on at shiny.rstudio.com/gallery/plot-interaction-zoom.html
    
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observe({
      brush <- input$plot1_brush
      if (!is.null(brush)) {
        ranges2$x <- c(brush$xmin, brush$xmax)
        ranges2$y <- c(brush$ymin, brush$ymax)
        
      } else {
        ranges2$x <- NULL
        ranges2$y <- NULL
      }
    })
    
    if (input$close > 0)
      stopApp()                             # stop shiny
  })
  
  plotData <- reactive({
    
    #   plotData <- subset(rawData()[[1]], rawData()[[1]]$model %in% input$modelID)
    plotData <- rawData()[[1]]
    # dataSubset <- isolate(input$datasub)
    # plotData <- subset(rawData(), rawData()$model %in% dataSubset)
    
    # subset plotData based on     
    
    sampleID <- as.character(unique(plotData$model))
    smallHPData <- NULL
    for (i in sampleID){
      if (is.null(smallHPData)){
        Xmax <- which.max(plotData[plotData$model == i, input$xcol])
        smallHPData <- plotData[plotData$model == i,][c(seq(1, sum(plotData$model == i), input$sampleRate), Xmax),]
      } else {
        Xmax <- which.max(plotData[plotData$model == i, input$xcol])
        smallHPData <- rbind(smallHPData, plotData[plotData$model == i,][c(seq(1, sum(plotData$model == i), input$sampleRate), Xmax),])
      }
    }
    plotData <- smallHPData
  })
  
  
  output$viewModelResults <- renderPrint({
    rawData()[[2]]
  })
  
  plotInput <- function(){
    wd <- as.character(getwd())
    xCol <- plotData()[, input$xcol]
    yCol <- plotData()[, input$ycol]
    id <- plotData()$model
    g <- ggplot(plotData(), aes(xCol, yCol, color = id)) +
      geom_point(size = 2) +
      ggtitle(paste(input$ycol, "vrs", input$xcol, "for", fileInfo$name)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab(input$xcol) +
      ylab(input$ycol)
    return(g)
  }
  
  
  output$plot1 <- renderPlot({
    wd <- as.character(getwd())
    xCol <- plotData()[, input$xcol]
    yCol <- plotData()[, input$ycol]
    id <- plotData()$model
    g <- ggplot(plotData(), aes(xCol, yCol, color = id)) +
      geom_point(size = 2) +
      ggtitle(paste(input$ycol, "vrs", input$xcol, "for", fileInfo$name)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab(input$xcol) +
      ylab(input$ycol)
    print(g)
  })
  
  
  
  output$corrNLS <- renderPrint({
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    if (!is.null(plotData())) {
      rawData()[[3]]
    } else {
      "No data available."
    }
  })
  
  output$corrMan <- renderPrint({
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    if (!is.null(plotData())) {
      rawData()[[4]]
    } else {
      "No data available."
    }
  })
  
  
  # Output plot using downloadHandler
  
  output$savePlot <- downloadHandler(
    filename = function() { 
      paste(appName, "_", strsplit(fileInfo$name, ".csv")[[1]][1], "sub",
            as.character(input$sampleRate), "_",
            format(Sys.time(), "%Y-%m-%d %H:%M"), ".png", sep = "") 
    },
    content = function(file) {
      ggsave(file, plot = plotInput(), device = "png")
    },
    contentType = "image/png"
  )
  
  output$saveData <- downloadHandler(
    filename = function() { 
      paste(appName, strsplit(fileInfo$name, ".csv")[[1]][1], "sub",
            as.character(input$sampleRate),  "_",
            format(Sys.time(), "%Y-%m-%d %H:%M"),".csv", sep = "") 
    },
    content = function(file) {
      write.csv(plotData(), file)
    }
  )
  
  
  observeEvent(input$saveModel, {
    modelInfo <- as.character(rawData()[[2]]$formula)
    # cat(paste("saveModel button",  modelInfo), "\n")
    txtout <- append(txtout, "\n more text...\n")
    # cat(lines(txtout))
    
    # cat("more\n", "and more\n", "and more text\n")
  })
  
  
  
}

shinyApp(ui, server)