###############################################################################
###############################################################################
#
# This script will convert data from wide to long format
# Date: Jul2020
###############################################################################
###############################################################################
###############################################################################


#----------------------------------------------------------------------
# Specify file parameters  ### Check These Parameters Before Running <---------------------------
#----------------------------------------------------------------------
# open file parameters
filePath <- "C:/Users/delos001/Desktop/"  ## remember the / at the end
fileName <- "testfile"   ##update file name <----------------
fileExt <- ".xlsx"
fileSheet <- 'Patient Status Report_EXCEL'

savePath <- "C:/Users/delos001/Desktop/"
saveName <- "e-CTS_PYAB_Patient_Visit_Date_1650_fixed"
saveExt <- '.csv'


#----------------------------------------------------------------------
# Load Packages
#----------------------------------------------------------------------

lpkgs = c('dplyr', 'data.table', 'readxl')

for(pkg in lpkgs){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}


#----------------------------------------------------------------------
# Read Files Into R Environment
#----------------------------------------------------------------------

myfile = read_excel(paste(filePath, 
                          fileName, 
                          fileExt, sep = ""), 
                    sheet = fileSheet)
vdf = data.frame(myfile)

#----------------------------------------------------------------------
# Format Columns
#----------------------------------------------------------------------

# Convert columns that have numbers but should be factors
charCols = c('Site', 'Patient', 'Gender')
vdf[charCols] = lapply(vdf[charCols], as.character)

# Convert date columns
dateCols = c('Patient.Visit.Processed.Date')
vdf[dateCols] = lapply(vdf[dateCols], as.Date, origin="1970-01-01")

# Strip dashes (except when they are between names, ie: hyphenated name)
dashCols = c('Investigator')
vdf[dashCols] = lapply(vdf[dashCols], 
                       function(y) gsub('( |^)-+|-+( |$)', '\\1', 
                                        gsub("[^ [:alnum:]'-]", '', y)))


#----------------------------------------------------------------------
# PIVOT Visit 0 and 1 and rename to corresponding visit name
#----------------------------------------------------------------------
# Site + `Site Name` + Investigator + 
#   Patient + Gender + `Patient Status`
castvdf = dcast(setDT(vdf), ...~ Visit,  ## ... gets all vars not specified
                value.var = "Patient.Visit.Processed.Date")
names(castvdf)[names(castvdf)=="0"] = 'Visit_0' 
names(castvdf)[names(castvdf)=="1"] = 'Visit_1' 
castvdf = as.data.frame(castvdf)


#----------------------------------------------------------------------
# Calculate Future Visits
#----------------------------------------------------------------------

VisDay = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 
           20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 60, 85)

for (i in VisDay){
  castvdf[paste('Visit',as.character(i), sep = "_")] = castvdf$Visit_1 + i - 1
}


#----------------------------------------------------------------------
# PIVOT Wide to long
#----------------------------------------------------------------------
statcols = c('Site', 'Site.Name', 'Investigator', 'Patient', 'Gender', 
             'Patient.Status')
meltvdf = melt(setDT(castvdf), 
           id.var = statcols,
           variable.name = 'Visit',
           measure.vars = patterns('Visit_'), ## pattern gets all visit columns
           value.name = "Visit Date"
           )

finalvdf = data.frame(meltvdf) %>%
  dplyr::mutate(Visit.Number = as.numeric(sub(".*_", "", Visit)),
                Reference.Day.Flag = ifelse(Visit.Date == as.Date(Sys.time()), 
                                            "Today",
                                      ifelse(Visit.Date == as.Date(Sys.time())+1, 
                                             "Tomorrow",
                                       ifelse(Visit.Date < as.Date(Sys.time()), 
                                              "Past", "Future"))),
                Day.of.Week = weekdays(Visit.Date),
                Week.Number = week(Visit.Date),
                Month.Number = format(Visit.Date, '%m')) %>%
  dplyr::arrange(`Site`, `Patient`, `Visit Date`)


#----------------------------------------------------------------------
# Write Melted Data to File
#----------------------------------------------------------------------

write.csv(finalvdf, file.path(savePath, 
                             paste(saveName, saveExt, sep = "")),
          row.names = FALSE)
