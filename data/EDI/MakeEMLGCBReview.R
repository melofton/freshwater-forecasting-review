##MakeEMLGCBReview
##Author: Mary Lofton
##Date: 31AUG22

#good sites for step-by-step instructions
#https://ediorg.github.io/EMLassemblyline/articles/overview.html
#https://github.com/EDIorg/EMLassemblyline/blob/master/documentation/instructions.md
#and links therein

# Install devtools
install.packages("devtools")

# Load devtools
library(devtools)

# Install and load EMLassemblyline
install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)

# Load other needed packages
library(tidyverse)

#Step 1: Create a directory for your dataset
#in this case, our directory is freshwater-forecasting-review/EDI

#Step 2: Move your dataset to the directory.

#Step 3: Create an intellectual rights license
#ours is CCBY

#Step 4: Identify the types of data in your dataset
# "table"

#Step 5: Import the core metadata templates

#THIS IS ONLY NECESSARY FOR A BRAND NEW DATASET!!!!

#for our application, we will need to generate all types of metadata
#files except for taxonomic coverage and geographic location, as we have both 
#continuous and categorical variables 

# View documentation for these functions
?template_core_metadata
?template_table_attributes
?template_categorical_variables #don't run this till later

# Import templates for our dataset licensed under CCBY, with 1 table.
template_core_metadata(path = "C:/Users/Mary Lofton/Documents/RProjects/freshwater-forecasting-review/data/EDI",
                       license = "CCBY",
                       file.type = ".txt",
                       write.file = TRUE)

template_table_attributes(path = "C:/Users/Mary Lofton/Documents/RProjects/freshwater-forecasting-review/data/EDI",
                          data.path = "C:/Users/Mary Lofton/Documents/RProjects/freshwater-forecasting-review/data/EDI",
                          data.table = "freshwater-forecasting-review-results.csv",
                          write.file = TRUE)

#Step 6: Script your workflow
#that's what this is, silly!

#Step 7: Abstract - check
#copy-paste the abstract from your Microsoft Word document into abstract.txt
#if you want to check your abstract for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 8: Methods - check
#copy-paste the methods from your Microsoft Word document into methods.txt
#if you want to check your abstract for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 9: Additional information - check
#nothing mandatory for Carey Lab in this section but I use it for the notes
#about whole-ecosystem manipulations in FCR

#Step 10: Keywords - check
#DO NOT EDIT KEYWORDS FILE USING A TEXT EDITOR!! USE EXCEL!!
#see the LabKeywords.txt file for keywords that are mandatory for all Carey Lab data products

#Step 11: Personnel - check
#copy-paste this information in from your metadata document
#Cayelan needs to be listed several times; she has to be listed separately for her roles as
#PI, creator, and contact, and also separately for each separate funding source (!!)

#Step 12: Attributes - check
#grab attribute names and definitions from your metadata word document
#for units....
# View and search the standard units dictionary
view_unit_dictionary()
#put flag codes and site codes in the definitions cell
#force reservoir to categorical
dat <- read_csv("./data/EDI/freshwater-forecasting-review-results.csv") %>%
  select(-`Cited References`,-`Meeting Abstract`,-`Highly Cited Status`,-`Hot Paper Status`,-null_model)
write.csv(dat, "./data/EDI/freshwater-forecasting-review-results.csv",row.names = FALSE)

#if you need to make custom units that aren't in the unit dictionary,
#use the customunits.txt file and the directions on the EMLassemblyline Github to do so

#Step 13: Close files
#if all your files aren't closed, sometimes functions don't work

#Step 14: Categorical variables - check
# Run this function for your dataset
#THIS WILL ONLY WORK once you have filled out the attributes_FluoroProbe.txt and
#identified which variables are categorical
template_categorical_variables(path = "C:/Users/Mary Lofton/Documents/RProjects/freshwater-forecasting-review/data/EDI",
                               data.path = "C:/Users/Mary Lofton/Documents/RProjects/freshwater-forecasting-review/data/EDI",
                               write.file = TRUE)

#open the created value IN A SPREADSHEET EDITOR and add a definition for each category

#Step 15: Geographic coverage
#copy-paste the bounding_boxes.txt file that is Carey Lab specific into your working directory

## Step 16: Obtain a package.id.  ####
# Go to the EDI staging environment (https://portal-s.edirepository.org/nis/home.jsp),
# then login using one of the Carey Lab usernames and passwords.
# These are found in the Google Drive folder regarding making EMLs in the 
# workshop notes from the original May 24, 2018 workshop.

# Select Tools --> Data Package Identifier Reservations and click 
# "Reserve Next Available Identifier"
# A new value will appear in the "Current data package identifier reservations" 
# table (e.g., edi.123)
# Make note of this value, as it will be your package.id below

#Step 17: Make EML
# View documentation for this function
?make_eml

# Run this function
make_eml(
  path = "C:/Users/Mary Lofton/Documents/RProjects/freshwater-forecasting-review/data/EDI",
  data.path = "C:/Users/Mary Lofton/Documents/RProjects/freshwater-forecasting-review/data/EDI",
  eml.path = "C:/Users/Mary Lofton/Documents/RProjects/freshwater-forecasting-review/data/EDI",
  dataset.title = "State-of-the-art review of near-term freshwater forecasting literature published between 2017 and 2022",
  temporal.coverage = c("2017-01-01", "2022-02-17"),
  maintenance.description = 'complete',
  data.table = "freshwater-forecasting-review-results.csv",
  data.table.description = "Freshwater forecasting review results",
  other.entity = "EDI_data_QAQC_and_formatting.R",
  other.entity.description = "data aggregation and quality control script",
  user.id = 'melofton',
  user.domain = 'EDI',
  package.id = 'edi.960.1')

## Step 8: Check your data product! ####
# Return to the EDI staging environment (https://portal-s.edirepository.org/nis/home.jsp),
# then login using one of the Carey Lab usernames and passwords. 

# Select Tools --> Evaluate/Upload Data Packages, then under "EML Metadata File", 
# choose your metadata (.xml) file (e.g., edi.270.1.xml), check "I want to 
# manually upload the data by selecting files on my local system", then click Upload.

# Now, Choose File for each file within the data package (e.g., each zip folder), 
# then click Upload. Files will upload and your EML metadata will be checked 
# for errors. If there are no errors, your data product is now published! 
# If there were errors, click the link to see what they were, then fix errors 
# in the xml file. 
# Note that each revision results in the xml file increasing one value 
# (e.g., edi.270.1, edi.270.2, etc). Re-upload your fixed files to complete the 
# evaluation check again, until you receive a message with no errors.

## Step 9: PUBLISH YOUR DATA! ####
# Reserve a package.id for your error-free data package. 
# NEVER ASSIGN this identifier to a staging environment package.
# Go to the EDI Production environment (https://portal.edirepository.org/nis/home.jsp)
# and login using the ccarey (permanent) credentials. 

# Select Tools --> Data Package Identifier Reservations and click "Reserve Next 
# Available Identifier". A new value will appear in the "Current data package 
# identifier reservations" table (e.g., edi.518)
# This will be your PUBLISHED package.id

# In the make_eml command below, change the package.id to match your 
# PUBLISHED package id. This id should end in .1 (e.g., edi.518.1)

# ALL OTHER entries in the make_eml() command should match what you ran above,
# in step 7

make_eml(
  path = "C:/Users/Mary Lofton/Documents/RProjects/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLFluoroProbe/2021",
  data.path = "C:/Users/Mary Lofton/Documents/RProjects/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLFluoroProbe/2021",
  eml.path = "C:/Users/Mary Lofton/Documents/RProjects/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLFluoroProbe/2021",
  dataset.title = "Time-series of high-frequency profiles of fluorescence-based phytoplankton spectral groups in Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in southwestern Virginia, USA 2014-2021",
  temporal.coverage = c("2014-05-04", "2021-12-06"),
  maintenance.description = 'ongoing',
  data.table = "FluoroProbe_2014_2021.csv",
  data.table.description = "Reservoir FluoroProbe dataset",
  other.entity = "FluoroProbe_QAQC_2014_2021.R",
  other.entity.description = "data aggregation and quality control script",
  user.id = 'ccarey',
  user.domain = 'EDI',
  package.id = 'edi.272.6')

# Once your xml file with your PUBLISHED package.id is Done, return to the 
# EDI Production environment (https://portal.edirepository.org/nis/home.jsp)

# Select Tools --> Preview Your Metadata, then upload your metadata (.xml) file 
# associated with your PUBLISHED package.id. Look through the rendered 
# metadata one more time to check for mistakes (author order, bounding box, etc.)

# Select Tools --> Evaluate/Upload Data Packages, then under "EML Metadata File", 
# choose your metadata (.xml) file associated with your PUBLISHED package.id 
# (e.g., edi.518.1.xml), check "I want to manually upload the data by selecting 
# files on my local system", then click Upload.

# Now, Choose File for each file within the data package (e.g., each zip folder), 
# then click Upload. Files will upload and your EML metadata will be checked for 
# errors. Since you checked for and fixed errors in the staging environment, this 
# should run without errors, and your data product is now published! 

# Click the package.id hyperlink to view your final product! HOORAY!