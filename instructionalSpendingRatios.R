library(dplyr)
library(data.table)
setwd("C:/Users/19732/Desktop/IPEDS/Files")

runISnumbers <- function(year){
  
  #######################################################
  #### Here we record the names of the variables we  ####
  #### are going to use.                             ####
  #######################################################
  
  hdVariables <- c(
    "UNITID",    # Unique identification number of the institution
    "INSTNM",    # Institution (entity) name
    "STABBR",    # State abbreviation
    "SECTOR",    # Sector of institution
    "CONTROL",   # Control of institution
    "HBCU",      # Historically Black College or University
    "TRIBAL",    # Tribal college
    "OPEID"      # Office of Postsecondary Education (OPE) ID Number
  )
  if(year == 2009){
    hdVariables <- c(
      "unitid",    # Unique identification number of the institution
      "instnm",    # Institution (entity) name
      "stabbr",    # State abbreviation
      "sector",    # Sector of institution
      "control",   # Control of institution
      "hbcu",      # Historically Black College or University
      "tribal",    # Tribal college
      "opeid"      # Office of Postsecondary Education (OPE) ID Number
    )
  }
  efiaVariables <- c(
    "UNITID",    # Unique identification number of the institution
    "EFTEUG",    # Estimated full-time equivalent (FTE) undergraduate enrollment
    "EFTEGD"     # Estimated full-time equivalent (FTE) graduate enrollment
  )
  f1Variables <- c(
    "UNITID",    # Unique identification number of the institution
    "F1C011",    # Instruction
    "F1C061",    # Student Support
    "F1C051",    # Academic Support
    "F1C071",    # Institutional Support
    "F1C021",    # Research
    "F1C031",    # Public Service
    "F1B01",     # Net Tuition/Fees
    "F1E08",     # Discounts and Allowances Applied to Tuition and Fees
    "F1E06",     # Institutional Grants from Unrestricted Sources
    "F1B11",     # State Appropriations
    "F1B12"      # Local Appropriations
  )
  correvF1Variables <- c(
    "UNITID", # Unique identification number of the institution
    "F1B01",  # Tuition and fees revenues
    "F1B11",  # State government appropriation revenues
    "F1B12",  # Local government appropriation revenues
    "F1B02",  # Federal operating grants and contracts
    "F1B03",  # State operating grants and contracts
    "F1B04",  # Local operating grants and contracts
    "F1B08",  # Other operating sources
    "F1B10",  # Federal appropriations
    "F1B13",  # Federal nonoperating grants
    "F1B14",  # State nonoperating grants 
    "F1B15",  # Local nonoperating grants
    "F1B16",  # Gifts, including contributions from affiliated organizations
    "F1B17",  # Investment income
    "F1B18",  # Other nonoperating revenues
    "F1B24",  # Total other revenues and additions
    "F1B26"   # Sales and services of educational activities
  )
  if(year <= 2007){
    correvF1Variables[17] <- "F1A01"
  }
  corexpF1Variables <- c(
    "UNITID", # Unique identification number of the institution
    "F1C011", # Instruction
    "F1C021", # Research
    "F1C031", # Public service
    "F1C051", # Academic support
    "F1C061", # Student services 
    "F1C071", # Institutional support
    "F1C101", # Scholarships and fellowships expenses
    "F1C141"  # Other expenses and deductions
  )
  f2Variables <- c(
    "UNITID",    # Unique identification number of the institution
    "F2E011",    # Instruction
    "F2E051",    # Student Support
    "F2E041",    # Academic Support
    "F2E061",    # Institutional Support
    "F2E021",    # Research
    "F2E031",    # Public Service
    "F2D01",     # Net Tuition/Fees
    "F2C08",     # Discounts and Allowances Applied to Tuition and Fees
    "F2C06",     # Institutional Grants from Unrestricted Sources
    "F2D04",     # Local Appropriations
    "F2D03"      # State Appropriations
  )
  correvF2Variables <- c(
    "UNITID", # Unique identification number of the institution
    "F2D01",  # Tuition and fees revenues
    "F2D02",  # Federal appropriations
    "F2D03",  # State appropriations
    "F2D04",  # Local appropriations
    "F2D05",  # Federal grants and contracts
    "F2D06",  # State grants and contracts
    "F2D07",  # Local grants and contracts 
    "F2D08",  # Private gifts, grants, and contracts
    "F2D09",  # Contributions from affiliated entities
    "F2D10",  # Investment return
    "F2D11",  # Sales and services of educational activities
    "F2D15"   # Other revenues
  )
  corexpF2Variables <- c(
    "UNITID", # Unique identification number of the institution
    "F2E011", # Instruction
    "F2E021", # Research
    "F2E031", # Public service
    "F2E041", # Academic support
    "F2E051", # Student services
    "F2E061", # Institutional support
    "F2E081", # Net grant aid to students
    "F2E121"  # Other expenses
  )
  if(year > 2013){
    f3Variables <- c(
      "UNITID",    # Unique identification number of the institution
      "F3E011",    # Instruction
      "F3E03B1",   # Student Support
      "F3E03A1",   # Academic Support
      "F3E03C1",   # Institutional Support
      "F3E02A1",   # Research
      "F3E02B1",   # Public Service
      "F3D01",     # Net Tuition/Fees
      "F3C06",     # Discounts and Allowances Applied to Tuition and Fees
      "F3C04",     # Institutional Grants from Unrestricted Sources
      "F3D03C",    # Local Appropriations
      "F3D03A"     # State Appropriations
    )
    correvF3Variables <- c(
      "UNITID", # Unique identification number of the institution
      "F3D01",  # Tuition and fees revenues
      "F3D02",  # Federal appropriations, grants and contracts
      "F3D03",  # State and local appropriations, grants and contracts
      "F3D04",  # Private gifts, grants, and contracts 
      "F3D05",  # Investment income and investment gains (losses) included in net income
      "F3D06",  # Sales and services of educational activities
      "F3D08"   # Other revenues 
    )
    corexpF3Variables <- c(
      "UNITID",  # Unique identification number of the institution
      "F3E011",  # Instruction
      "F3E02A1", # Research 
      "F3E02B1", # Public service
      "F3E03A1", # Academic support 
      "F3E03C1", # Institutional support 
      "F3E03B1", # Student services
      "F3E051",  # Net grant aid to students
      "F3E061"   # All other expenses
    )
  }
  if(year <= 2013){
    f3Variables <- c(
      "UNITID",    # Unique identification number of the institution
      "F3E01",     # Instruction
      "F3E03",     # Student Support
      "F3A01",     # Academic Support
      "F3A02",     # Institutional Support
      "F3E02",     # Research
      "F3A03",     # Public Service
      "F3D01",     # Net Tuition/Fees
      "F3C06",     # Discounts and Allowances Applied to Tuition and Fees
      "F3C04",     # Institutional Grants from Unrestricted Sources
      "F3A04",     # Local Appropriations
      "F3D03"      # State Appropriations
    )
    correvF3Variables <- c(
      "UNITID", # Unique identification number of the institution
      "F3D01",  # Tuition and fees revenues
      "F3D02",  # Federal appropriations, grants and contracts
      "F3D03",  # State and local appropriations, grants and contracts
      "F3D04",  # Private gifts, grants, and contracts 
      "F3D05",  # Investment income and investment gains (losses) included in net income
      "F3D06",  # Sales and services of educational activities
      "F3D08"   # Other revenues 
    )
    corexpF3Variables <- c(
      "UNITID",  # Unique identification number of the institution
      "F3E01",   # Instruction
      "F3E02",   # Research 
      "F3A01",   # Public service
      "F3E03",   # Academic support 
      "F3A02",   # Institutional support 
      "F3A03",   # Student services
      "F3E05",   # Net grant aid to students
      "F3E06"    # All other expenses
    )
  }
    
  #######################################################
  #### Here we establish the correct file names      ####
  #### and make any adjustments to variable names    #### 
  #### based on the entered year.                    ####
  #######################################################
  
  if(year==2020){
    hdFile <- "hd2020.csv"
    f1File <- "f1920_f1a.csv"
    f2File <- "f1920_f2.csv"
    f3File <- "f1920_f3.csv"
    efiaFile <- "efia2020.csv"
  }
  if(year==2019){
    hdFile <- "hd2019.csv"
    f1File <- "f1819_f1a.csv"
    f2File <- "f1819_f2.csv"
    f3File <- "f1819_f3.csv"
    efiaFile <- "efia2019_rv.csv"
  }
  if(year==2018){
    hdFile <- "hd2018.csv"
    f1File <- "f1718_f1a_rv.csv"
    f2File <- "f1718_f2_rv.csv"
    f3File <- "f1718_f3_rv.csv"
    efiaFile <- "efia2018_rv.csv"
  }
  if(year==2017){
    hdFile <- "hd2017.csv"
    f1File <- "f1617_f1a_rv.csv"
    f2File <- "f1617_f2_rv.csv"
    f3File <- "f1617_f3_rv.csv"
    efiaFile <- "efia2017_rv.csv"
  }
  if(year==2016){
    hdFile <- "hd2016.csv"
    f1File <- "f1516_f1a_rv.csv"
    f2File <- "f1516_f2_rv.csv"
    f3File <- "f1516_f3_rv.csv"
    efiaFile <- "efia2016_rv.csv"
  }
  if(year==2015){
    hdFile <- "hd2015.csv"
    f1File <- "f1415_f1a_rv.csv"
    f2File <- "f1415_f2_rv.csv"
    f3File <- "f1415_f3_rv.csv"
    efiaFile <- "efia2015_rv.csv"
  }
  if(year==2014){
    hdFile <- "hd2014.csv"
    f1File <- "f1314_f1a_rv.csv"
    f2File <- "f1314_f2_rv.csv"
    f3File <- "f1314_f3_rv.csv"
    efiaFile <- "efia2014_rv.csv"
  }
  if(year==2013){
    hdFile <- "hd2013.csv"
    f1File <- "f1213_f1a_rv.csv"
    f2File <- "f1213_f2_rv.csv"
    f3File <- "f1213_f3_rv.csv"
    efiaFile <- "efia2013_rv.csv"
  }
  if(year==2012){
    hdFile <- "hd2012.csv"
    f1File <- "f1112_f1a_rv.csv"
    f2File <- "f1112_f2_rv.csv"
    f3File <- "f1112_f3_rv.csv"
    efiaFile <- "efia2012_rv.csv"
  }
  if(year==2011){
    hdFile <- "hd2011.csv"
    f1File <- "f1011_f1a_rv.csv"
    f2File <- "f1011_f2_rv.csv"
    f3File <- "f1011_f3_rv.csv"
    efiaFile <- "efia2011_rv.csv"
  }
  if(year==2010){
    hdFile <- "hd2010.csv"
    f1File <- "f0910_f1a_rv.csv"
    f2File <- "f0910_f2_rv.csv"
    f3File <- "f0910_f3_rv.csv"
    efiaFile <- "efia2010_rv.csv"
  }
  if(year==2009){
    hdFile <- "hd2009.csv"
    f1File <- "f0809_f1a_rv.csv"
    f2File <- "f0809_f2_rv.csv"
    f3File <- "f0809_f3_rv.csv"
    efiaFile <- "efia2009_rv.csv"
  }
  if(year==2008){
    hdFile <- "hd2008.csv"
    f1File <- "f0708_f1a_rv.csv"
    f2File <- "f0708_f2_rv.csv"
    f3File <- "f0708_f3_rv.csv"
    efiaFile <- "efia2008_rv.csv"
  }
  if(year==2007){
    hdFile <- "hd2007.csv"
    f1File <- "f0607_f1a_rv.csv"
    f2File <- "f0607_f2_rv.csv"
    f3File <- "f0607_f3_rv.csv"
    efiaFile <- "efia2007_rv.csv"
  }
  if(year==2006){
    hdFile <- "hd2006.csv"
    f1File <- "f0506_f1a_rv.csv"
    f2File <- "f0506_f2_rv.csv"
    f3File <- "f0506_f3_rv.csv"
    efiaFile <- "efia2006_RV.csv"
  }
  if(year==2005){
    hdFile <- "hd2005.csv"
    f1File <- "f0405_f1a_rv.csv"
    f2File <- "f0405_f2_rv.csv"
    f3File <- "f0405_f3_rv.csv"
    efiaFile <- "efia2005_rv.csv"
  }
  if(year==2004){
    hdFile <- "hd2004.csv"
    f1File <- "f0304_f1a_rv.csv"
    f2File <- "f0304_f2_rv.csv"
    f3File <- "f0304_f3_rv.csv"
    efiaFile <- "efia2004_rv.csv"
  }
  
  #######################################################
  #### Now we load in the appropriate files and set  ####
  #### conforming variable names.                    ####
  #######################################################
  
  hd <- fread(hdFile, header=TRUE, select=hdVariables)
  if(year==2009){
    names(hd) <- c("UNITID", "INSTNM", "STABBR", "SECTOR", "CONTROL", "HBCU", "TRIBAL", "OPEID")
  }
  
  efia <- fread(efiaFile, header=TRUE, select=efiaVariables)
  
  efia <- efia %>% rowwise(UNITID) %>% mutate(FTE12MN = sum(EFTEUG, EFTEGD, na.rm=TRUE))
  
  f1 <- fread(f1File, header=TRUE, select=f1Variables)
  names(f1) <- c("UNITID", "Instruction", "Student Support", "Academic Support", "Institutional Support", "Research", "Public Service", "Net Tuition/Fees", "Discounts and Allowances Applied to Tuition and Fees", "Institutional Grants from Unrestricted Sources", "State Appropriations", "Local Appropriations")
  
  f2 <- fread(f2File, header=TRUE, select=f2Variables)
  names(f2) <- c("UNITID", "Instruction", "Student Support", "Academic Support", "Institutional Support", "Research", "Public Service", "Net Tuition/Fees", "Discounts and Allowances Applied to Tuition and Fees", "Institutional Grants from Unrestricted Sources", "Local Appropriations", "State Appropriations")
  
  f3 <- fread(f3File, header=TRUE, select=f3Variables)
  names(f3) <- c("UNITID", "Instruction", "Student Support", "Academic Support", "Institutional Support", "Research", "Public Service", "Net Tuition/Fees", "Discounts and Allowances Applied to Tuition and Fees", "Institutional Grants from Unrestricted Sources", "Local Appropriations", "State Appropriations")
  if(year <= 2013){
    f3$`Academic Support` <- rep(0, nrow(f3))
    f3$`Institutional Support` <- rep(0, nrow(f3))
    f3$`Public Service` <- rep(0, nrow(f3))
    f3$`Local Appropriations` <- rep(0, nrow(f3))  
  }
  
  #######################################################
  #### The next lines add in the variables "core     ####
  #### revenues" and "core expenses" which must be   ####
  #### derived from the finance survey data.         ####
  #######################################################
  
  correvF1 <- fread(f1File, header=TRUE, select=correvF1Variables)
  if(year <= 2007){
    correvF1$F1A01 <- rep(0, nrow(correvF1))
    names(correvF1)[17] <- "F1B26"
  }
  correvF1 <- correvF1 %>% rowwise(UNITID) %>% mutate(CORREV = sum(c(F1B01, F1B11, F1B12, F1B02, F1B03, F1B04, F1B08, F1B10, F1B13, F1B14, F1B15, F1B16, F1B17, F1B18, F1B24, F1B26)))
  
  correvF2 <- fread(f2File, header=TRUE, select=correvF2Variables)
  
  correvF2 <- correvF2 %>% rowwise(UNITID) %>% mutate(CORREV = sum(c(F2D01, F2D02, F2D03, F2D04, F2D05, F2D06, F2D07, F2D08, F2D09, F2D10, F2D11, F2D15)))
  
  correvF3 <- fread(f3File, header=TRUE, select=correvF3Variables)
  
  correvF3 <- correvF3 %>% rowwise(UNITID) %>% mutate(CORREV = sum(c(F3D01, F3D02, F3D03, F3D04, F3D05, F3D06, F3D08)))
  
  corexpF1 <- fread(f1File, header=TRUE, select=corexpF1Variables)
  
  corexpF1 <- corexpF1 %>% rowwise(UNITID) %>% mutate(COREXP = sum(c(F1C011, F1C021, F1C031, F1C051, F1C061, F1C071, F1C101, F1C141)))
  
  corexpF2 <- fread(f2File, header=TRUE, select=corexpF2Variables)
  
  corexpF2 <- corexpF2 %>% rowwise(UNITID) %>% mutate(COREXP = sum(c(F2E011, F2E021, F2E031, F2E041, F2E051, F2E061, F2E081, F2E121)))
  
  corexpF3 <- fread(f3File, header=TRUE, select=corexpF3Variables)
  if(year <= 2013){
    corexpF3$F3A01 <- rep(0, nrow(corexpF3))
    corexpF3$F3A02 <- rep(0, nrow(corexpF3))
    corexpF3$F3A03 <- rep(0, nrow(corexpF3))
    names(corexpF3) <- c("UNITID", "F3E011", "F3E02A1", "F3E02B1", "F3E03A1", "F3E03C1", "F3E03B1", "F3E051", "F3E061")
  }

  corexpF3 <- corexpF3 %>% rowwise(UNITID) %>% mutate(COREXP = sum(c(F3E011, F3E02A1, F3E02B1, F3E03A1, F3E03C1, F3E03B1, F3E051, F3E061)))
  
  correvF1 <- correvF1 %>% select(UNITID, CORREV)
  correvF2 <- correvF2 %>% select(UNITID, CORREV)
  correvF3 <- correvF3 %>% select(UNITID, CORREV)
  correv <- rbind(correvF1, correvF2, correvF3)
  names(correv) <- c("UNITID", "Core Revenues")
  
  corexpF1 <- corexpF1 %>% select(UNITID, COREXP)
  corexpF2 <- corexpF2 %>% select(UNITID, COREXP)
  corexpF3 <- corexpF3 %>% select(UNITID, COREXP)
  corexp <- rbind(corexpF1, corexpF2, corexpF3)
  names(corexp) <- c("UNITID", "Core Expenses")
  
  f1$`Source Survey` <- rep("F1", nrow(f1))
  f2$`Source Survey` <- rep("F2", nrow(f2))
  f3$`Source Survey` <- rep("F3", nrow(f3))
  
  financeData <- rbind(f1, f2, f3)
  financeData <- left_join(x=financeData, y=correv, by="UNITID")
  financeData <- left_join(x=financeData, y=corexp, by="UNITID")
  
  tuitionData <- financeData %>% select(UNITID, `Net Tuition/Fees`)
  efia <- full_join(x=efia, y=tuitionData, by="UNITID")
  efia$`Tuition Per FTE` <- efia$`Net Tuition/Fees` / efia$FTE12MN
  efia <- efia %>% select(UNITID, `Tuition Per FTE`)
  financeData <- full_join(x=financeData, y=efia, by="UNITID")
  
  #######################################################
  #### Now we derive the instructional spending      ####
  #### ratios.                                       ####
  #######################################################
  
  financeData <- financeData %>% rowwise(UNITID) %>% mutate(CTFR = `Net Tuition/Fees` + `Discounts and Allowances Applied to Tuition and Fees` - `Institutional Grants from Unrestricted Sources`)
  
  financeData <- financeData %>% rowwise(UNITID) %>% mutate(`Education Share` = (`Instruction` + `Student Support`)/(`Instruction` + `Student Support` + `Research` + `Public Service`))
  
  financeData <- financeData %>% rowwise(UNITID) %>% mutate(OSS = (`Education Share` * (`Local Appropriations` + `State Appropriations`)))
  
  financeData <- financeData %>% rowwise(UNITID) %>% mutate(`E&R` = `Instruction` + `Student Support` + ((`Academic Support` + `Institutional Support`) * `Education Share`))
  
  financeData <- financeData %>% rowwise(UNITID) %>% mutate(`INSTR / Net Tuition and Fees` = `Instruction` / `Net Tuition/Fees`)
  
  financeData <- financeData %>% rowwise(UNITID) %>% mutate(`INSTR / CTFR` = Instruction / CTFR)
  
  financeData <- financeData %>% rowwise(UNITID) %>% mutate(`INSTR / CTFR + OSS` = Instruction / (CTFR + OSS))
  
  financeData <- financeData %>% rowwise(UNITID) %>% mutate(`E&R / CTFR` = `E&R` / CTFR)
  
  financeData <- financeData %>% rowwise(UNITID) %>% mutate(`E&R / CTFR + OSS` = `E&R` / (CTFR + OSS))
  
  if(year <= 2013){
    financeData$`INSTR / CTFR + OSS`[financeData$`Source Survey`=="F3"] <- NA
    financeData$`E&R / CTFR`[financeData$`Source Survey`=="F3"] <- NA
    financeData$`E&R / CTFR + OSS`[financeData$`Source Survey`=="F3"] <- NA
  }
  
  financeData <- full_join(x=hd, y=financeData, by="UNITID")
  
  return(financeData)
}

write.csv(runISnumbers(2020), "Instructional Spending 2019-20.csv", row.names=FALSE)
write.csv(runISnumbers(2019), "Instructional Spending 2018-19.csv", row.names=FALSE)
write.csv(runISnumbers(2018), "Instructional Spending 2017-18.csv", row.names=FALSE)
write.csv(runISnumbers(2017), "Instructional Spending 2016-17.csv", row.names=FALSE)
write.csv(runISnumbers(2016), "Instructional Spending 2015-16.csv", row.names=FALSE)
write.csv(runISnumbers(2015), "Instructional Spending 2014-15.csv", row.names=FALSE)
write.csv(runISnumbers(2014), "Instructional Spending 2013-14.csv", row.names=FALSE)
write.csv(runISnumbers(2013), "Instructional Spending 2012-13.csv", row.names=FALSE)
write.csv(runISnumbers(2012), "Instructional Spending 2011-12.csv", row.names=FALSE)
write.csv(runISnumbers(2011), "Instructional Spending 2010-11.csv", row.names=FALSE)
write.csv(runISnumbers(2010), "Instructional Spending 2009-10.csv", row.names=FALSE)
write.csv(runISnumbers(2009), "Instructional Spending 2008-09.csv", row.names=FALSE)
write.csv(runISnumbers(2008), "Instructional Spending 2007-08.csv", row.names=FALSE)
write.csv(runISnumbers(2007), "Instructional Spending 2006-07.csv", row.names=FALSE)
write.csv(runISnumbers(2006), "Instructional Spending 2005-06.csv", row.names=FALSE)
write.csv(runISnumbers(2005), "Instructional Spending 2004-05.csv", row.names=FALSE)
write.csv(runISnumbers(2004), "Instructional Spending 2003-04.csv", row.names=FALSE)

spending2004 <- runISnumbers(2004) %>% select(UNITID, `INSTR / Net Tuition and Fees`)
names(spending2004) <- c("UNITID", "2003-04")
spending2005 <- runISnumbers(2005) %>% select(UNITID, `INSTR / Net Tuition and Fees`)
names(spending2005) <- c("UNITID", "2004-05")
spending2006 <- runISnumbers(2006) %>% select(UNITID, `INSTR / Net Tuition and Fees`)
names(spending2006) <- c("UNITID", "2005-06")
spending2007 <- runISnumbers(2007) %>% select(UNITID, `INSTR / Net Tuition and Fees`)
names(spending2007) <- c("UNITID", "2006-07")
spending2008 <- runISnumbers(2008) %>% select(UNITID, `INSTR / Net Tuition and Fees`)
names(spending2008) <- c("UNITID", "2007-08")
spending2009 <- runISnumbers(2009) %>% select(UNITID, `INSTR / Net Tuition and Fees`)
names(spending2009) <- c("UNITID", "2008-09")
spending2010 <- runISnumbers(2010) %>% select(UNITID, `INSTR / Net Tuition and Fees`)
names(spending2010) <- c("UNITID", "2009-10")
spending2011 <- runISnumbers(2011) %>% select(UNITID, `INSTR / Net Tuition and Fees`)
names(spending2011) <- c("UNITID", "2010-11")
spending2012 <- runISnumbers(2012) %>% select(UNITID, `INSTR / Net Tuition and Fees`)
names(spending2012) <- c("UNITID", "2011-12")
spending2013 <- runISnumbers(2013) %>% select(UNITID, `INSTR / Net Tuition and Fees`)
names(spending2013) <- c("UNITID", "2012-13")
spending2014 <- runISnumbers(2014) %>% select(UNITID, `INSTR / Net Tuition and Fees`)
names(spending2014) <- c("UNITID", "2013-14")
spending2015 <- runISnumbers(2015) %>% select(UNITID, `INSTR / Net Tuition and Fees`)
names(spending2015) <- c("UNITID", "2014-15")
spending2016 <- runISnumbers(2016) %>% select(UNITID, `INSTR / Net Tuition and Fees`)
names(spending2016) <- c("UNITID", "2015-16")
spending2017 <- runISnumbers(2017) %>% select(UNITID, `INSTR / Net Tuition and Fees`)
names(spending2017) <- c("UNITID", "2016-17")
spending2018 <- runISnumbers(2018) %>% select(UNITID, `INSTR / Net Tuition and Fees`)
names(spending2018) <- c("UNITID", "2017-18")
spending2019 <- runISnumbers(2019) %>% select(UNITID, `INSTR / Net Tuition and Fees`)
names(spending2019) <- c("UNITID", "2018-19")
spending2020 <- runISnumbers(2020) %>% select(UNITID, `INSTR / Net Tuition and Fees`)
names(spending2020) <- c("UNITID", "2019-20")

ratiosOverTime <- full_join(x=spending2004, y=spending2005, by="UNITID")
ratiosOverTime <- full_join(x=ratiosOverTime, y=spending2006, by="UNITID")
ratiosOverTime <- full_join(x=ratiosOverTime, y=spending2007, by="UNITID")
ratiosOverTime <- full_join(x=ratiosOverTime, y=spending2008, by="UNITID")
ratiosOverTime <- full_join(x=ratiosOverTime, y=spending2009, by="UNITID")
ratiosOverTime <- full_join(x=ratiosOverTime, y=spending2010, by="UNITID")
ratiosOverTime <- full_join(x=ratiosOverTime, y=spending2011, by="UNITID")
ratiosOverTime <- full_join(x=ratiosOverTime, y=spending2012, by="UNITID")
ratiosOverTime <- full_join(x=ratiosOverTime, y=spending2013, by="UNITID")
ratiosOverTime <- full_join(x=ratiosOverTime, y=spending2014, by="UNITID")
ratiosOverTime <- full_join(x=ratiosOverTime, y=spending2015, by="UNITID")
ratiosOverTime <- full_join(x=ratiosOverTime, y=spending2016, by="UNITID")
ratiosOverTime <- full_join(x=ratiosOverTime, y=spending2017, by="UNITID")
ratiosOverTime <- full_join(x=ratiosOverTime, y=spending2018, by="UNITID")
ratiosOverTime <- full_join(x=ratiosOverTime, y=spending2019, by="UNITID")
ratiosOverTime <- full_join(x=ratiosOverTime, y=spending2020, by="UNITID")

write.csv(ratiosOverTime, "Ratios Over Time.csv", row.names=FALSE)





