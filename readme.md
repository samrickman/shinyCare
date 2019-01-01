Readme for Adult Social Care Dashboard
======================================================

## What is this repo?

This is the repo for the [Adult Social Care Finance Report Dashboard](https://samrickman.shinyapps.io/ascfr_dashboard/).

## What data is used?

Since 2017, NHS Digital have released an [Adult Social Care Activity and Finance Report]("https://digital.nhs.uk/data-and-information/publications/statistical/adult-social-care-activity-and-finance-report/2017-18"), "Adult Social Care Activity and Finance Report 2017-18".

NHS Digital state that the Gross Current Expenditure on Adult Social Care for local authorities in England is £17.9bn. 

Although replication of this figure from the NHS Digital data is possible, it is not necessarily straightforward. There are a number of non-obvious filters that needed to be set (e.g. 99 in a field means the row is a sum value). Additionally, although the main data with descriptions is a .xlsb file, it is very difficult to view as a spreadsheet as it has over 500,000 rows and takes an extremely long time to load and process data.

I decided to create a dashboard filter the data by different variables, both in graphical and tabular form. The aim is to make it easier for myself, and others if so inclined, to understand the Adult Social Care (England) expenditure data. 

## How to use this

The first thing I would recommend is to play around with the drop-down menus and see what changes!

The first two drop-down menus change both the graph and the table. The x-axis drop down menu sets the x-axis in the graph and the horizontal part of the table. The panels menu changes the panels of the graph and the vertical part of the table.

The colour and plot type drop-down menus apply only to the graph, as do the checkboxes to filter by Primary Support Reason.

The full data set can be downloaded using the button in the side bar. Summary data can be downloaded using the button in the summary table tab.

## Example

*How do I achieve the 17.9bn total Gross Current Expenditure figure?*

Plot: Simply select all Primary Support Groups. 

Table: Select “sum” as the summary statistic. 

In the table. the 17.9bn figure is the total of all the values in the table and is shown on the bottom-right. Data in this table can be exported with the button below the table.

## How is spending per person calculated?

England local authorities have populations which differ from 2000 people (Isles of Sicilly) to 1.5m people (Essex).

This makes comparing total spending across local authorities misleading.

To remedy this, I divided the total spending by the last known population estimates per local authority [ONS 2011 census data](
https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/2011censuspopulationestimatesbyfiveyearagebandsandhouseholdestimatesforlocalauthoritiesintheunitedkingdom/r12ukrttablep04ukv2_tcm77-304141.xls) to create a (very rough) spending per person estimate.

Each point on the plot represents one local authority.

## Note on reproducibility 

The code in the github repository works on the NHS Digital data file as it was when I downloaded it on 18th October 2018. This file is stored in the github repository. Source files have been known to change format at times. If you plan to run the code, I would strongly recommend using the data files in the repository, rather than downloading them again from source.

The "Support Type With Populations all PSRs incl Sicilly.rds" R data file should be exactly producible by applying the code in “code to produce the RDS files from raw data.R”. This code imports “ASCFR Data with descriptions.csv”, which (following some difficulty importing a .xlsb file into R on Linux)  was created by opening the “ASCFR Data File (with descriptions).xlsb file” in LibreOffice5.1 and saving as .csv with UTF-8 encoding, comma as field delimiter and double quotes as text delimiter. The “code to produce the RDS files from raw data.R” file also uses the UK population data in the format stored in the repo, “England population density GATESHEAD NORTHUMBERLAND NEW CODES.csv”.

### Files in repo:

1. app.R - this is the R script which generates the page.
2. include.md - this is the About page which is imported into tabs.R.
3. Support Type With Populations all PSRs incl Sicilly.rds - this is the data as read by tabs.R.
4. code to produce the RDS files from raw data.R - this is the code to produce the rds file from raw data.
5. ASCFR Data with descriptions.csv - this is the NHS Digital Data .csv that is imported into the code to generate the RDS file.
6. ASCFR Data File (with descriptions).xlsb - this is the raw NHS Digital data file. The CSV file is created by opening the XLSB file in LibreOffice5.1 and saving as .csv with UTF-8 encoding, comma as field delimiter and double quotes as text delimiter.
7. r12ukrttablep04ukv2_tcm77-304141.xls - This is the raw 2011 population data from ONS.
8. England population density GATESHEAD NORTHUMBERLAND NEW CODES.csv - This is the ONS data reformatted to be imported by the code to produce the RDS files. I have manually overwritten the codes for Gateshead and Northumberland which have changed since 2011. This is admittedly not ideal but was expedient!

## About

This dashboard was created by Sam Rickman using R version 3.2.3 (2015-12-10) on an x86, 64 bit linux-gnu PC. 


