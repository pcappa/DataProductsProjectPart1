###############################################################################
#
# This is the UI module for the Engine Analyzer app.
#
# Author: Paul Cappa
# Date:   17 December 2015
#
###############################################################################

require(shiny)
require(shinyapps)

shinyUI(
    fluidPage(
        
        titlePanel("Simple Engine Analyzer"),

        sidebarLayout(
            
            sidebarPanel(
                selectInput("documentation", strong("Documentation"), 
                	choices=c("Synopsis", "Help", "Hide"), selected = "Synopsis"),

	          	conditionalPanel(
	                condition = "input.documentation == 'Synopsis'",
	                p(paste0(
	                	"This application is a simple engine data analyzer. It is ",
	                	"used to visually compare two sensors. It can also be used ",
	                	"to review sensor data over time.")),
	                p(),
            		p(paste0(
	                	"The 7 minutes of data, within the dataset, was captured ",
						"from a 2005 Honda Civic Hybrid. There are currently five ",
						"parameters available to choose from, Speed, ThrottlePosition, ",
						"RPM, CoolantTemp, and IntakeTemp. There is also an option for ",
						"examining each sensor over time, with the possibility of ",
						"adding a second sensor to the plot.")),
	            	p(),
	          		p(paste0(
	          			"The server, for the most part moves the data. The only true ",
	          			"calculations are performed when the applicatiion is displaying ",
	          			"a second sensor when time is on the X axis. The calculation ",
	          			"is to scale the Y data between 0 and 1. The formula used is:")),
	          		p(),
#					withMathJax(helpText("Some math here $$\\alpha+\\beta$$")),
	          		p(withMathJax("\\(\\frac{(Y - Ymin)}{(Ymax-Ymin)}\\)"), align="center"),
	          		p(),
	          		p(paste0(
	          			"This is applied to each Y value in the subset of data. The ",
	          			"\\(Ymax\\) and \\(Ymin\\) are calculated for each sensor."))
            	),
            	            	
            	conditionalPanel(
                    condition = "input.documentation == 'Help'",
	            	h3("Basic Comparison"),
            		strong("Comparing Two Sensors"),
					p(paste0("To compare a pair of sensors, select sensors from the ",
						"'X Axis' and 'Y Axis' drop downs.")), 
            		p(),
            		strong("View by 1 Minute Blocks"),
					p(paste0("To compare the sensors by 1 minute blocks, select ",
						"'View in 1 Minute Snapshots'. A slider will appear below ",
						"the plot. Use the slider to examine a particular block of ",
						"time.")), 
            		p(),
					h3("Time on X Axis"),
            		strong("Selecting Time on X Axis"),
					p(paste0("To review a single sensor over time, select 'Time ",
						"(Seconds)' on the X Axis and the sensor for the Y Axis.")), 
            		p(),
            		strong("Enabling a Second Sensor"),
					p(paste0("When 'Time (Seconds)' is selected on the X Axis, a ",
						"second sensor can be displayed on the Y Axis. Enable the ",
						"'Display Second Y Axis Sensor'.  A drop down will appear.  ",
						"Select the second sensor.  The second sensor will be ",
						"displayed in gray behind the primary sensor.")),
            		p(),
					p(paste0("When two sensors are displayed on the Y axis, the ",
						"values are scaled from 0 to 1, lowest value being 0, ",
						"highest is set to 1.")),
            		p(),
            		strong("Changing Plot Type"),
					p(paste0("A time plot can be displayed with 'Points', 'Lines', ",
						"and 'Points & Lines'.")),
            		p(),
            		em("Note: This only makes sense to use in a time plot."),
					p(),
					h3("Fun Examples"),
					strong("RPM v. Speed"),
	            	tags$ol("X Axis: 'RPM'"), p(),
					tags$ol("Y Axis: 'Speed'"), p(),
					br(),
					strong("Time v. RPM & Speed"), p(),
	            	tags$ol("X Axis: 'Time (Seconds)'"), p(),
					tags$ol("Y Axis: 'RPM'"), p(),
            		tags$ol("Display Second Y Sensor"), p(),
					tags$ol("Second Y Axis Sensor: 'Speed'"), p(),
            		tags$ol("View in 1 Minute Snapshots"), p(),
					tags$ol("Time Plot Type: 'Points & Lines'")
                ),

                selectInput("xSensor", "X Axis", choices = c(df.sensors, TimeOption), 
                	selected = "Speed"),
    
                selectInput("ySensor", label = "Y Axis", choices = df.sensors, 
                	selected = "ThrottlePosition"),
      
                conditionalPanel(condition = paste0("input.xSensor == '", TimeOption, "'"), 
                    checkboxInput("ySecondSensor", strong("Display Second Y Axis Sensor"), 
                    	value = FALSE)),

                conditionalPanel(condition = "input.ySecondSensor == true", 
                    selectInput("ySensor2", label = "Second Y Axis Sensor (in Gray)", 
                    	choices = df.sensors, selected = "RPM")),
                
                checkboxInput("snapshot", strong("View in 1 Minute Snapshots"), 
                	value = FALSE),
            	
                selectInput("colorPalette", strong("Choose the Primary Color"), 
                	choices=c("Green", "Blue", "Red"), selected = "Green"),
      
                conditionalPanel(condition = paste0("input.xSensor == '", TimeOption, "'"), 
                    selectInput("plotType", strong("Time Plot Type"), choices=c("Points", 
                    	"Lines", "Points & Lines"), selected = "Points"))
            ),
            
            mainPanel(
                plotOutput(outputId = "main_plot", height = "600px", width = "600px"),
                
                # Display this slider if it is enabled by the user.
                
                conditionalPanel(
                    condition = "input.snapshot == true",
                    sliderInput(
                        inputId = "TimeSlice", 
                        label = "Minute",
                        min = min(df$Minute), 
                        max = max(df$Minute), 
                        value = min(df$Minute), 
                        step = 1,
                        width="600px",
                        animate = TRUE
                    )
                )
            	
            )
        )
    )
)
