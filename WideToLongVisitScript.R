
###############################################################################
###############################################################################
#
# This script will convert data from wide to long format
# Date: Jul2020
###############################################################################
###############################################################################
###############################################################################


#----------------------------------------------------------------------
# Specify file parameters  ### Check These Parameters Before Running
#----------------------------------------------------------------------

# OPEN PARAMETERS----------------------------------------------------------------update this fields as needed
## Visit Data file 
vdName = "_Current_e-CTS_PYAB_Patient_Visit_Date"                               #this is the visit data file name
vdExt = '.xlsx'                                                                 #this is the visit data extension
vdSheet = 'Patient Status Report_EXCEL'                                         #this is the sheet name containing the visit data
vdPath = 'C:/Users/q713174/Desktop/LilyBlaze/ReceivedFiles/'                    #this is where you saved the file (change \ to / and end with a /)

## Zip Code file-------------------------------------------------------
ContactsName = "Site Contacts and Site Ops Assignments"                         #this is the zip code data file name
ContactsExt = '.xlsx'                                                           #this is the zip code file extension
ContactsSheet = 'SC Assignments'                                                #this is the name of the sheet that has the zip code data
ContactsPath = 'C:/Users/q713174/Desktop/LilyBlaze/ReceivedFiles/'              #this is where you saved the file (change \ to / and end with a /)


#SAVE PARAMETERS-------------------------------------------------------
## Final output save parameters----------------------------------------
savePath <- "C:/Users/q713174/Desktop/LilyBlaze/FixedVisitDateFiles/"           #this is where you want to save the file after running (always end with /)
saveName <- "e-CTS_PYAB_Patient_Visit_Date_1650_fixed"                          #this is what you want to name the file after running
saveExt <- '.csv'                                                               #this is what file type you want to save (csv is best practice)


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
## read visit data file
vdfile = read_excel(paste(vdPath, 
                          vdName, 
                          vdExt, sep = ""), 
                    sheet = vdSheet)

vdf = data.frame(vdfile) %>%
  dplyr::select('Site', 'Site.Name', 'Investigator', 'Patient', 'Gender',
                'Visit', 'Patient.Visit.Processed.Date', 'Patient.Status')

## read zip code file
Contactsfile = read_excel(paste(ContactsPath, 
                                ContactsName, 
                                ContactsExt, sep = ""), 
                          sheet = ContactsSheet)
Contactsdf = data.frame(Contactsfile)
#----------------------------------------------------------------------
# Format Columns
#----------------------------------------------------------------------

## Visit Data Columns
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


## ZipCode Data Columns
Contactsdf$Site = as.character(Contactsdf$'Site.Reference..')


#----------------------------------------------------------------------
# PIVOT Long to wide for Visit 0 and 1 and rename to corresponding visit name
#----------------------------------------------------------------------

castvdf = dcast(setDT(vdf), ...~ Visit,  ## ... gets all vars not specified
                value.var = "Patient.Visit.Processed.Date")
names(castvdf)[names(castvdf)=="0"] = 'Visit_0' 
names(castvdf)[names(castvdf)=="1"] = 'Visit_1' 
## Add column to identify Missing D1 dates
castvdf = as.data.frame(castvdf) %>%
  mutate("D1.Date.Present" = ifelse((is.na(castvdf$Visit_1)) |
                                      (is.null(castvdf$Visit_1)) | 
                                      castvdf$Visit_1 == 0 | 
                                      'Visit_1' == "" |
                                      'Visit_1' == " " |
                                      'Visit_1' == "NA", 
                                    "Missing", "Present"))

#----------------------------------------------------------------------
# Calculate Future Visits
#----------------------------------------------------------------------

VisDay = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 
           20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 60, 85)

for (i in VisDay){
  castvdf[paste('Visit', as.character(i), sep = "_")] = castvdf$Visit_1 + i - 1
}


#----------------------------------------------------------------------
# PIVOT Wide to long
#----------------------------------------------------------------------
## list the columns to keep in the df (those that aren't being pivoted)
statcols = c('Site', 'Site.Name', 'Investigator', 'Patient', 'Gender', 
             'Patient.Status', 'D1.Date.Present')
meltvdf = melt(setDT(castvdf), 
               id.var = statcols,
               variable.name = 'Visit',
               measure.vars = patterns('Visit_'), ## pattern gets all visit cols
               value.name = "Visit.Date")

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
  dplyr::left_join(Contactsdf[,c('Site',
                            'Location.State.Province', 
                            'Location.City')],
                   by = c('Site' = 'Site')) %>%
  dplyr::arrange(`Site`, `Patient`, `Visit.Date`) %>%
  dplyr::select(Site, Site.Name, Investigator, Location.State.Province, 
                Location.City, Patient, Patient.Status, Gender, 
                D1.Date.Present, Visit, Visit.Number, Visit.Date, Day.of.Week, 
                Week.Number, Month.Number, Reference.Day.Flag)


#----------------------------------------------------------------------
# Write Melted Data to File
#----------------------------------------------------------------------

write.csv(finalvdf, file.path(savePath, 
                              paste(saveName, saveExt, sep = "")),
          row.names = FALSE)
