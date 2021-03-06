## What is this?

Since 2017, NHS Digital have released an [Adult Social Care Activity and Finance Report]("https://digital.nhs.uk/data-and-information/publications/statistical/adult-social-care-activity-and-finance-report/2017-18").

NHS Digital state that the Gross Current Expenditure on Adult Social Care for local authorities in England is £17.9bn. 

Although replication of this figure from the NHS Digital data is possible, it is not necessarily straightforward. There are a number of non-obvious filters that needed to be set (e.g. 99 in a field means the row is a sum value). Additionally, although the main data with descriptions is a .xlsb file, it is very difficult to view as a spreadsheet as it has over 500,000 rows and takes an extremely long time to load and process data.

I decided to create a dashboard filter the data by different variables, both in graphical and tabular form. The aim is to make it easier for myself, and others if so inclined, to understand the Adult Social Care (England) expenditure data. 

## How to use this

The first thing I would recommend is to play around with the drop-down menus and see what changes!

The first three drop-down menus change both the graph and the table:

1. The x-axis drop down menu sets the x-axis in the graph and the horizontal part of the table. 
2. The panels menu changes the panels of the graph and the vertical part of the table.
3. The plot/table type selects whether the plot or table displays spending per 100,000 population or total spending.

The colour variable applies only to the graph, as do the checkboxes to filter by Primary Support Reason.

The full data set can be downloaded using the button in the side bar. Summary data can be downloaded using the button in the summary table tab.

## Example

*How do I achieve the 17.9bn total Gross Current Expenditure figure?*

Plot: Simply select all Primary Support Groups. 

Table: Select “sum” as the summary statistic. 

In the table. the 17.9bn figure is the total of all the values in the table and is shown on the bottom-right. Data in this table can be exported with the button below the table.

## How is spending per 100k calculated?

England local authorities have populations which differ from 2000 people (Isles of Sicilly) to 1.5m people (Essex) which makes comparisons on the basis of total spending difficult.

I divided the total spending by the last known population estimates per local authority [ONS 2011 census data](
https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/2011censuspopulationestimatesbyfiveyearagebandsandhouseholdestimatesforlocalauthoritiesintheunitedkingdom/r12ukrttablep04ukv2_tcm77-304141.xls) to create a spending per 100k population estimate.

I have since discovered that ONS produces a yearly estimate for UK population by local authority so I may update it with more recent population figures at some point. 

Each point on the plot represents one local authority Gross Current Expenditure entry.

## Note on reproducibility 

The code in the github repository works on the NHS Digital data file as it was when I downloaded it on 18th October 2018. This file is stored in the github repository. Source files have been known to change format at times. If you plan to run the code, I would strongly recommend using the data files in the repository, rather than downloading them again from source.

The "Support Type With Populations all PSRs incl Sicilly.rds" R data file should be exactly producible by applying the code in “code to produce the RDS files from raw data.R”. This code imports “ASCFR Data with descriptions.csv”, which (following some difficulty importing a .xlsb file into R on Linux)  was created by opening the “ASCFR Data File (with descriptions).xlsb file” in LibreOffice5.1 and saving as .csv with UTF-8 encoding, comma as field delimiter and double quotes as text delimiter. The “code to produce the RDS files from raw data.R” file also uses the UK population data in the format stored in the repo, “England population density GATESHEAD NORTHUMBERLAND NEW CODES.csv”.

## About

This dashboard was created by Sam Rickman using R version 3.2.3 (2015-12-10) on an x86, 64 bit linux-gnu PC. 

The code is open source and can be viewed in the [Shiny Care Github Repository](https://github.com/samrickman/shinyCare)

