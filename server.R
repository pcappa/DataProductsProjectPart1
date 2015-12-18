###############################################################################
#
# Shiny Server for Simple Engine Analyzer application.
#
# Author: Paul Cappa
# Date:   17 December 2015
#
###############################################################################

require(dplyr)
require(shiny)
require(shinyapps)

shinyServer(function(input, output) {

    output$main_plot <- renderPlot({

    ###########################################################################
    #
    # Read the inputs from the UI.
    #
    ###########################################################################

    xs <- input$xSensor                     # The X Axis Sensor
    ys <- input$ySensor                     # The Y Axis Sensor
    ys2 <- input$ySensor2                   # If enabled, the 2nd Y Axis Sensor.
    multiPlot <- input$ySecondSensor        # Indicates two sensors.
    
    snapshot <- input$snapshot              # Display time slices.
    ts <- input$TimeSlice                   # The times slice to display.
    colorPalette <- input$colorPalette      # Color palalette to plot with.

    
    ###########################################################################
    #
    # Reactive function to figure out the plot type (Points, Lines, Both)
    #
    ###########################################################################

    ptype <- reactive({
        if (input$xSensor != TimeOption) {
            "p" 
        } else { 
            if (input$plotType == "Lines") { 
                "l" 
            } else if (input$plotType == "Points & Lines") { 
                "o" 
            } else { 
                "p" 
            }
        }
    })

    
    ###########################################################################
    #
    # If "Time" is on the X Axis, we have to do some different manipulations.
    # Also, we have the potential for displaying more than one sensor.
    #
    ###########################################################################

    if (xs == TimeOption) {
        if (multiPlot) {        
            d <- subset(df, (df$PID.Txt == ys) | (df$PID.Txt == ys2)) 
        } else {
            d <- subset(df, (df$PID.Txt == ys))
        }
    } else {
        d <- subset(df, (df$PID.Txt == xs) | (df$PID.Txt == ys))
    }

       
    ###########################################################################
    #
    # Remove any factors that are not in the dataset.
    #
    ###########################################################################

    d$PID <- factor(d$PID)
    d$PID.Txt <- factor(d$PID.Txt)
    

    ###########################################################################
    #
    # If we are using time on the X Axis, then plug in the Y. Otherwise, we 
    # have to go through and generate it.
    #
    ###########################################################################

    if (xs == TimeOption) {                     # Time is on the X axis.
        d$Y <- d$Value
        d$Value <- d$Secs
        
    	# if it's a multi-plot, the we have to use unity for the Y axis.
    
        if (multiPlot) {
            for (PID in levels(d$PID.Txt)) {
    
                miny <- min(d$Y[d$PID.Txt == PID])
                maxy <- max(d$Y[d$PID.Txt == PID])
                scaley <- maxy - miny
            
                x <- which(d$PID.Txt == PID)

                if (scaley != 0) {
                    d$Y[x] <- (d$Y[x] - miny) / scaley
                } else {
                    d$Y[x] <- 0.5
                }
            
            }
            miny <- 0
            maxy <- 1
        } else {
            miny <- min(d$Y)
            maxy <- max(d$Y) 
        }

    } else {                                    # A sensor is on the X axis.

        # Find the first occurance of the Y Factor and start from there.
    
        j <- which(d$PID.Txt == ys)[1]
        curry <- d$Value[j]
        
        # Back fill the value (we are assuming it is the same value up to that point
        
        for (i in 1:j) {
            d$Y[i] <- curry
        }
        
        # Fill in the rest of the values.
        
        for (i in j:nrow(d)) {
            if (d$PID.Txt[i] == ys) { 
                curry <- d$Value[i]
            }
            d$Y[i] <- curry
        }
        
        # Keep only what we are going to use.
        
        d <- subset(d, d$PID.Txt == xs)
        miny <- min(d$Y)
        maxy <- max(d$Y)
   	}

    
    ###########################################################################
    #
    # This is a bit of a brute force method to find the end points for the X
    # scale.  If we are in snapshot mode then it behaves differently than
    # in normal mode.  Then within snapshot mode, we have to know if time is
    # displayed on the X axis.  If we are displaying time, we want to scale
    # the axis based on what we are displaying.
    #
    ###########################################################################

    if (snapshot) {
        if (xs == TimeOption) {
            d <- subset(d, d$Minute == ts)
            minx <- min(d$Value)
            maxx <- max(d$Value)
        } else {
            minx <- min(d$Value)
            maxx <- max(d$Value)
            d <- subset(d, d$Minute == ts)
        }
    } else {
        minx <- min(d$Value)
        maxx <- max(d$Value)
    }

    
    ###########################################################################
    #
    #  Time to plot the data.
    #
    ###########################################################################

    # One last time, remove any factors that are not in the dataset.

    d$PID <- factor(d$PID)
    d$PID.Txt <- factor(d$PID.Txt)
    
    
    # Set the title of the plot.
    
    ylab <- ys
    t <- paste0("XY Plot of ",xs, " v. ", ylab)
    
    if (multiPlot) { 
        t <- paste0(t, " & ", ys2)
        ylab <- paste0(ylab, " & ", ys2)
    }
    

    # Put together the plot window.
    
    par(mfrow=c(1,1))
    
    plot(c(minx, maxx), c(miny,maxy), xlab=xs, ylab=ylab, type="n")
    title(t)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray98")
    axis(1, tck=1, lty=3, col="gray50", labels=FALSE)
    axis(2, tck=1, lty=3, col="gray50", labels=FALSE)

    
    # Based on the user's palette preferences, pick a real color.
    
    maxColors <- 2
    
    if (colorPalette == "Red") {
        plotColors <- c("orangered", "gray70")
    } else if (colorPalette == "Blue") {
        plotColors <- c("blue", "gray70")
    } else {
        plotColors <- c("green4", "gray70")
    }
    
    
    # Place the data on the plot.

    i <- 1    
    for (PID in levels(d$PID.Txt)) {
        x <- which(d$PID.Txt == PID)
        lines(d$Value[x], d$Y[x], col=plotColors[i], pch=20, type=ptype())
        i <- i + 1
        if (i > maxColors) { i <- 1 }
    }

  	})
})

