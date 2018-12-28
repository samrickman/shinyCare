## OK this will be an attempt to look at the ASCFR data with the Shiny app

## OK essentially next time read this:
# https://shiny.rstudio.com/articles/reactivity-overview.html
# It's about not using if/else statements and instead using reactive values
# which will make things easier to add - as currently if/else only works
# if you do one else, otherwise it only prints final value



#setwd("./shinyCare")

library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(scales)
library(reshape2)
library(tidyr)
library(rmarkdown)



# Load the data per person - also contains the council data
spendPerPersonData <- read_rds("Support Type With Populations all PSRs incl Sicilly.rds")

# Set the options for the primary support reason checkboxes
primarySupportReasons <- unique(spendPerPersonData$PrimarySupportReason_Key)

# Set the options for the x-axis and facet values
facetChoices <- xAxisChoices <- c("Support Setting" = "supportOrCareType", "Region" = "GEOGRAPHY_NAME", "Primary Support Reason" = "PrimarySupportReason_Key", "Care Type (short/long term)" = "CareType_Key", "Age Band" = "AgeBand_Key")
# Set the options for the colour values
colValues <- c("Age Band", "Support Setting", "Primary Support Reason", "Region", "Care Type (short/long term)")

ui <- fluidPage(
        
        title = "Adult Social Care Spending Data 2017/18",
        
        titlePanel("Dashboard for Adult Social Care Finance Report 2017/18 (England)"), 
        
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(
                        
                        # Input: Select the x axis
                        selectInput(inputId = "xAxisChoice", label = "Choose x-axis (horizontal for table)", choices = xAxisChoices),
                        
                        # Select the facets
                        selectInput(inputId = "facetValue", label = "Choose panels (or vertical for table)", choices = facetChoices, selected = "PrimarySupportReason_Key"),
                        
                        # Select colours
                        selectInput(inputId = "colorValue", label = "Choose colour", choices = colValues),
                        
                        # Select plot type
                        selectInput(inputId = "plotType", label = "Plot type", choices = c("Total spending (bar graph)" = "bar", "Spending per person (each point is a local authority)" = "scatter"), selected = "scatter" )  ,
                
                        # Filter by primary support reason
                        checkboxGroupInput(inputId = "PrimarySupportReason",
                                           label = "Filter graph by Primary Support Reason",
                                           choices = primarySupportReasons,
                                           selected = primarySupportReasons[1:5][-4]),
                        
                        # br() element to introduce extra vertical spacing ----
                        br(),
                        
                        # Maybe shove a y-axis slider here
                        
                        
                        # Download full data set button
                        downloadButton(outputId = "downloadDataSet", label = "Download full data set"),
                        
                        width = 3
                        
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                        
                        # Output: Tabset w/ plot, summary, and table ----
                        tabsetPanel(type = "tabs",
                                    
                                    # Main plot
                                    tabPanel("Plot", 
                                             plotOutput(outputId = "ggplot"),
                                             uiOutput(outputId = "noteText")), 
                                    
                                    # Table
                                    tabPanel("Table", 
                                             selectInput(inputId = "aggregateFormula", label = "Choose summary statistic for table", choices = c("mean", "median", "sum"), selected = "mean"),
                                             tableOutput(outputId = "summaryTable"),
                                             downloadButton(outputId = "downloadSummaryTable", label = "Download summary table")),
                                    # About
                                    tabPanel("About", uiOutput(outputId = "outputUI"))
                                    
                                    
                        ),
                        width = 9
                )
        )
)





server <- function(input, output) {
        
        # This is the workhorse bit which takes all the inputs, selects the right data,
        # filters the data, and draws the output
        plotToRender <- reactive( { 
                
                # Remove the outliers from the spendPerPerson data if you are going to be looking at it
                # on a per person level (they squash the rest of the graph)
                if(input$plotType=="scatter"){
                        # Take out the extremely high values
                        relevantData <- filter(spendPerPersonData, SpendingPerPerson<86)
                        # Take out the 143 (out of 15k) < 0 values which must be errors
                        relevantData <- filter(relevantData, SpendingPerPerson>=0)
                        
                }
                else {
                        relevantData <- spendPerPersonData
                        
                        
                }
                
                # Filter the data on the relevant primary support reasons
                filteredDataByPSR <- filter(relevantData, PrimarySupportReason_Key %in% input$PrimarySupportReason)        
                
                
                # Define function to tell it what type of plot to make - this is called
                # in the plot building function
                plotGeom <- function(geomType = input$plotType) {
                        if(geomType=="scatter"){
                                geom_point(position=position_jitter(h=0.35,w=0.35), alpha=0.7)
                        }
                        else {
                                geom_col()
                        }
                }
                
                # Define the function to actually build the plot - arguments are yvalue, xvalue and color
                buildVariablePlot <- function(yval=SpendingPerPerson, xval=supportOrCareType, colval = AgeBand_Key, facetVal ="PrimarySupportReason_Key") {
                        
                        # This is the bit that takes the variable name from the argument and makes it understandable
                        # so that it can go in the y axis, xaxis and color value. Note not necessary with facevetVal as it's in quotes.
                        yAxis <- enexpr(yval)
                        xAxis <- enexpr(xval)
                        colField <- enexpr(colval)
                        
                        # Set the x-axis label value
                        
                        c("Support Setting" = "supportOrCareType", "Region" = "GEOGRAPHY_NAME", "Primary Support Reason" = "PrimarySupportReason_Key", "Care Type (short/long term)" = "CareType_Key")
                        if(input$xAxisChoice=="supportOrCareType"){xAxisLabel = "Support Setting"}
                        else if(input$xAxisChoice=="GEOGRAPHY_NAME"){xAxisLabel = "Region"} 
                        else if(input$xAxisChoice=="PrimarySupportReason_Key"){xAxisLabel = "Primary Support Reason"}
                        else if(input$xAxisChoice=="CareType_Key"){xAxisLabel = "Care Type"}
                        else if(input$xAxisChoice=="AgeBand_Key"){xAxisLabel = "Age Band"}
                        
                        # Set the plot title - with the total spending figure if a bar chart
                        # Otherwise just a plain title
                        if(input$plotType=="bar"){
                                plotTitle <- paste("Total spending across selected primary support groups:", dollar(sum(filteredDataByPSR$ITEMVALUE), prefix = "£"))
                        }
                        else{
                                plotTitle <- "Average spending per member of population (each point represents one local authority value)"
                        }
                                
                        
                        # This is where we tell it to build the plot with the y axis set in the line above
                        # with the enexpr() phase - note the !!yAxis, means take the evaluated expression
                        variablePlot <- ggplot(data=filteredDataByPSR) + 
                                aes(x= !!xAxis, y = !!yAxis, color=!!colField, fill=!!colField) +
                                plotGeom() +
                                facet_grid(rows=facetVal) +
                                ggtitle(plotTitle) +
                                ylab("Spending (£)") +
                                xlab(xAxisLabel)
                        
                        
                        variablePlot + theme(axis.text.x = element_text(angle = 90, hjust=1),  plot.title = element_text(hjust = 0.5), 
                                             legend.title = element_blank()) +
                                scale_y_continuous(labels = scales::dollar_format(prefix = "£"))
                        
                }
                
                
                
                
                # Send the correct arguments to the plot building function (x axis, y axis, colour)  
                # Please excuse the extremely ugly irritating nesting if if/else. Unfortunately, else if is not permitted inside the function call
                # and you cannot set the values (table columns) outside the function call as they are not known in global environment. Note if/elseif not necessary
                # where the argument to be passed is in quotes. 
                buildVariablePlot({
                        if(input$plotType=="scatter") {
                                yval=SpendingPerPerson
                        } 
                        else { 
                                yval=ITEMVALUE
                        }
                }, {
                        if(input$xAxisChoice=="supportOrCareType"){
                                xval = supportOrCareType
                        }
                        else {
                                {
                                        if(input$xAxisChoice=="CareType_Key") {
                                                xval=CareType_Key
                                        }
                                        else {
                                                {
                                                        if(input$xAxisChoice=="PrimarySupportReason_Key") {
                                                                xval=PrimarySupportReason_Key
                                                        }
                                                        else {
                                                                if(input$xAxisChoice=="AgeBand_Key") {
                                                                        xval=AgeBand_Key
                                                                }
                                                                else {
                                                                        xval=GEOGRAPHY_NAME        
                                                                }        
                                                        }
                                                        
                                                }         
                                                
                                        }
                                        
                                }         
                        }
                } ,
                {
                        if(input$colorValue=="Age Band"){                
                                colval = AgeBand_Key
                        }
                        else {
                                {
                                        if(input$colorValue=="Region") {
                                                colval=GEOGRAPHY_NAME
                                        }
                                        else {
                                                {
                                                        if(input$colorValue=="Care Type (short/long term)") {
                                                                colval=CareType_Key
                                                        }
                                                        else {
                                                                {
                                                                        if(input$colorValue=="Primary Support Reason") {
                                                                                colval=PrimarySupportReason_Key
                                                                        }
                                                                        else {
                                                                                colval = supportOrCareType      
                                                                        }
                                                                        
                                                                }         
                                                                
                                                        }
                                                        
                                                }         
                                                
                                        }
                                        
                                }         
                                
                        }
                },
                facetVal = input$facetValue
                )
        })
        
        # Define the table that we are going to render according the inputs        
        tableToRender <- reactive({
                
                # Define the function that will take the arguments
                buildTable <- function(vertical = "PrimarySupportReason_Key", horizontal = "supportOrCareType", formula = "mean") {
                        
                        # Use the correct summary formula
                        if(formula=="mean"){
                                # Calculate means
                                summaryTable <-  (dcast(spendPerPersonData, as.formula(paste(vertical, horizontal, sep="~")), value.var = "ITEMVALUE", mean, na.rm=TRUE))
                                
                                # Add row means
                                summaryTable[["RowMeans"]] <- rowMeans(summaryTable[-1], na.rm = TRUE)
                                
                                # Arrange descending
                                summaryTable <- arrange(summaryTable, desc(RowMeans))
                                
                                # Add column means
                                
                                sumMeans <- as.data.frame(lapply(summaryTable[-1], mean, na.rm = TRUE))
                                cellName <- "Column means"
                                
                                sumAdd <- cbind(cellName, sumMeans)
                                names(sumAdd) <- names(summaryTable)
                                
                                summaryTable <- rbind(summaryTable, sumAdd)
                                
                        }
                        else if(formula=="median"){
                                # Calculate median
                                summaryTable <-  (dcast(spendPerPersonData, as.formula(paste(vertical, horizontal, sep="~")), value.var = "ITEMVALUE", median, na.rm=TRUE))
                                
                                # Add row median
                                summaryTable[["RowMedians"]] <- apply(summaryTable[-1], 1, median, na.rm = TRUE)
                                
                                # Arrange descending
                                summaryTable <- arrange(summaryTable, desc(RowMedians))
                                
                                # Add column median
                                
                                sumMedians <- as.data.frame(lapply(summaryTable[-1], median, na.rm = TRUE))
                                cellName <- "Column medians"
                                
                                sumAdd <- cbind(cellName, sumMedians)
                                names(sumAdd) <- names(summaryTable)
                                
                                summaryTable <- rbind(summaryTable, sumAdd)
                                
                        }
                        else { # This is for a straight sum
                                
                                # Calculate sum 
                                summaryTable <-  (dcast(spendPerPersonData, as.formula(paste(vertical, horizontal, sep="~")), value.var = "ITEMVALUE", sum, na.rm=TRUE))
                                
                                # Add row sums
                                summaryTable[["RowSums"]] <- rowSums(summaryTable[-1], na.rm = TRUE)
                                
                                # Arrange descending
                                summaryTable <- arrange(summaryTable, desc(RowSums))
                                
                                # Add column sums
                                
                                sumSums <- as.data.frame(lapply(summaryTable[-1], sum, na.rm = TRUE))
                                cellName <- "Column sums"
                                
                                sumAdd <- cbind(cellName, sumSums)
                                names(sumAdd) <- names(summaryTable)
                                
                                summaryTable <- rbind(summaryTable, sumAdd)
                                
                                
                        }
                        
                        # Replace NAs
                        summaryTable <- replace_na(summaryTable)
                        
                        # I would like the output to be to nearest pound with £ sign and commas 
                        
                        # Take out the row names
                        summaryFirstCol <- summaryTable[1]
                        
                        # Take out the numeric bit
                        summaryDataCols <- summaryTable[-1]
                        
                        # Format the numeric bit
                        summaryDataCols <- sapply(summaryDataCols, dollar, prefix = "£")
                        
                        # Put them back together, replacing the old data
                        summaryTable <- cbind(summaryFirstCol, summaryDataCols)
                        
                        # Make any "£Nan" or "£NA" values into NA
                        
                        summaryTable <- na_if(summaryTable, "£NaN")
                        summaryTable <- na_if(summaryTable, "£NA")
                        
                        summaryTable
                }
                
                # Pass the correct arguments to the function
                buildTable(vertical = input$facetValue, horizontal = input$xAxisChoice, formula = input$aggregateFormula)
        })
        
        
        output$ggplot <- renderPlot({
                plotToRender()
        })
        
        output$summaryTable <- renderTable({
                tableToRender()}, 
                hover=TRUE, align="c"
        )
        
        # Download Summary Table data
        output$downloadSummaryTable <- downloadHandler(
                filename = function() {
                        paste(input$xAxisChoice, input$facetValue, ".csv", sep = "")
                },
                content = function(file) {
                        write.csv(tableToRender(), file, row.names = FALSE)
                }
        )
        
        # Download full data
        output$downloadDataSet <- downloadHandler(
                filename = "GrossCurrentExpenditure.csv",
                content = function(file) {
                        write.csv(spendPerPersonData[-(25:26)], file, row.names = FALSE)
                }
        )
        
        # Text below the main plot - I have just put html inside R's withTags function,
        # rather than writing R functions to be convertd into html
        output$noteText <- renderUI({withTags({
                
                        div(class="Note", checked=NA,
                            h4("Data sources:"),
                            a(href="https://digital.nhs.uk/data-and-information/publications/statistical/adult-social-care-activity-and-finance-report/2017-18", "Adult Social Care Activity and Finance Report 2017-18"),
                            br(),
                            a(href="https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/2011censuspopulationestimatesbyfiveyearagebandsandhouseholdestimatesforlocalauthoritiesintheunitedkingdom/r12ukrttablep04ukv2_tcm77-304141.xls", "UK population data: 2011 census"),
                            br(),
                            p("For further information, including links to the code used to generate this dashboard, see the About section."),
                            br(),
                            h4("A note on the population data:"),
                            p("Note: The spending per person data divides the total spend by the 2011 census population figures. There are four outliers, two from City of London and Isles of Sicilly respectively, which have been removed from the per person spending data as they make the rest of the data squashed. They still contribute towards the totals in the bar chart, and in the table, and can be viewed, if desired, by downloading the full data set using the button. There are also a very small number of negative Gross Current Expenditure figures which I removed from the Spending Per Person graph - this is why the totals when switching between graph types may differ very slightly.")
                        )

                })
        })
        
        # About section 
        output$outputUI <- renderUI({ 
                        
                        
                        includeMarkdown("include.md")
            
            
        })                        
        
}

shinyApp(ui = ui, server = server)