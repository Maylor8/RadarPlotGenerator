# 1. Description ----------------------------------------------------------

# Title:        Radar plot generator
# Version:      2.0 (Using Rstudio version 1.4.1106)
# Date:         09-07-2021
# Author:       Ben Maylor (bm259@leicester.ac.uk)
# Contributors: Alex Rowlands, Nathan Dawkins and Charlotte Edwardson
# URL:          https://github.com/Maylor8/RadarPlotGenerator

# Description
# A tool used for generating radar plots to assist with the interpretation of accelerometry data. 
# Please contact Ben Maylor (bm259@leicester.ac.uk) for any enquiries.


# Citing Radar plot Generator
# When using Radar plot generator, please cite: 
# Rowlands et al. (2019) - DOI: 10.1186/s40798-019-0225-9 
# The link to this github code so that others can access it and use it. 

# Update notes for version 2:
  # Added ability to include error ribbons to provide more context between group means.
  # Improved some automation. More to be done in the next update.


# 2. Library setup --------------------------------------------------------

# Packages required to generate Radar plots
library(ggplot2) # Verified using V3.3.5
library(scales)  # Verified using V1.1.1


# 3. Data setup -----------------------------------------------------------

Group.no    <- 3                                    # Number of groups to be compared  
Group.names <- c("Healthy",	"Overweight",	"Obese")  # Names of comparison groups
Metric.no   <- 5                                    # Number of different MX metrics used
Metriclist  <- c("M60", "M30", "M10", "M5", "M2")   # Quote each MX metric used in descending order. Report MX values in minutes to ensure they are ordered correctly.

# Make the data frame (Skip if you are importing an existing data frame)
Metric  <- c(rep(c(Metriclist), each = Group.no))
Group   <- c(rep(c(Group.names), times = Metric.no))

# Paste values where the first MX metric is listed for each Group in the order they appear above (e.g. in the provided example this would be M60-Healthy, Overweight, Obese, M30-Healthy etc..)                                           
Mg    <- c(105, 101, 120.2, 148.7, 140.4, 127, 196.4, 185, 168.2, 240.1, 225.6, 197.7, 359.3, 310.2, 254) # Data in Mg    

# Same ordering but for SD/SEM/95%CI. NOTE: These are the differences to the Mg, not the absolute values.
Error <- c(7.2, 7.5, 7.3, 10.3, 11.0, 10.7, 5.9, 6.2, 6.1, 9.4, 9.7, 9.9, 21, 17, 19)

# Make data frame
Data <- data.frame(Metric, Group, Mg, Error)
# Inspect data frame for accuracy
print(Data)

# Generate separate data frame for error ribbons
d <- Data
d$lowCI  <- Mg - Error
d$highCI <- Mg + Error

i   <- c(seq_along(d[[1]]), which(d$Metric==tail(Metriclist,n=1)))
dCI <- rbind(data.frame(d[i,1:2], CI=d$lowCI[i], type="low") ,
             data.frame(d[i,1:2], CI=d$highCI[i], type="high"))

dCI$Group <- factor(dCI$Group, levels = Group.names)

# Ordering of MX metrics for the plots
  temp <- vector(mode="numeric", length=length(Metriclist))

  for (i in 1:length(Metriclist)) {
    temp[i] <- as.numeric(substring(Metriclist[i],2,nchar(Metriclist[i])))}

  temp <- sort(temp, decreasing=T)
  temp <- paste("M", temp, sep="")
  
  Data$Metric <- factor(Data$Metric, levels = temp)  
  Data$Group <- factor(Data$Group, levels = Group.names)  


# 4. Generate plot --------------------------------------------------------

# The code first generates a polygon plot in which the data and main aesthetics are specified by the user  ##
# WARNING: When adding/removing lines of code here, ensure that symbols at the end of each line remain the ##
# same (e.g. + or ,). This is typically the most common cause for the code tripping up.                    ##
# The plot viewer in R will not reflect the true rendering quality applied to the exported image file      ## 

# Set Y axis parameters suitable for your data
  Ystart <- 0 # Y axis start value
  Yend   <- 400 # Y axis end value
  Yint   <- 50  # Y axis interval

# Make plot
  Plot <- ggplot(Data, aes(x = Metric, y = Mg, group = Group)) +
    geom_polygon(aes(group = Group, colour = Group), fill = NA, size = 1.1) +    
  
  #Deactivate this line below if you do not want shadings for the error
    geom_polygon(aes(x=Metric,y=CI, group = Group, fill=Group), data=dCI, alpha=.3) + 
  
  # OPTIONAL: Specify group colours/shade - Activate and edit one only at a time
  # A comprehensive list of colours can be found here: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
  # Examples
  # scale_colour_manual(values = c("grey10", "grey30", "grey50", "grey70", "grey90"),name = "Group") +
  # scale_colour_manual(values = c("black", "green", "blue","red","yellow"),name = "Group") +
  
  # Set Y axis gridlines
    geom_hline(yintercept = seq(Ystart,Yend, by = Yint), alpha = 0.8, colour = "grey70", size = 0.2) + # Automatically uses the Y axis interval value specified previously

  # Add vertical X lines for metrics
    geom_segment(aes(x="M60", y=Ystart, xend="M60", yend=Yend), colour = "grey70", size = 0.2) +  
    geom_segment(aes(x="M30", y=Ystart, xend="M30", yend=Yend), colour = "grey70", size = 0.2) +  
    geom_segment(aes(x="M10", y=Ystart, xend="M10", yend=Yend), colour = "grey70", size = 0.2) +
    geom_segment(aes(x="M5", y=Ystart, xend="M5", yend=Yend), colour = "grey70", size = 0.2) +
    geom_segment(aes(x="M2", y=Ystart, xend="M2", yend=Yend), colour = "grey70", size = 0.2) +

  # Add Acceleration cut-points
    geom_hline(yintercept = 100, colour = alpha("red", 0.7), size = 0.9, linetype = 5) + # add as many of these cutpoint as you want by pasting this line.
    geom_hline(yintercept = 200, colour = alpha("red", 0.7), size = 0.9, linetype = 4) + # For different line types: http://www.cookbook-r.com/Graphs/Shapes_and_line_types/
    geom_hline(yintercept = 400, colour = alpha("red", 0.7), size = 0.9, linetype = 6)   # Ensure The last line does not end in "+" or the plot will not be finalised.

# OPTIONAL: Quality check when trying to identify aesthetic and layout problems.
  Plot

# Make radar plot (wrap ggplot around a coordinate)
  Plot + coord_polar(start = -((180/Metric.no)*(pi/180))) + # Start argument ensures that the first MX metric begins directly vertical of the centre of the radar plot.
    theme_classic(base_size = 16) +
    theme(axis.ticks = element_blank(), # Theme edits. For examples of additional theme edits, see here: https://ggplot2.tidyverse.org/reference/theme.html
          axis.title = element_blank(),
          axis.line = element_blank(),
         #axis.text.x = element_blank(), Remove MX labels if you are adding MX symbols instead. Recommended that you do not activate this for the first run to ensure metrics are in the right order and the plot makes sense.
          axis.text.y = element_blank(),
          panel.grid = element_blank()) + # Remove various elements from plot
  
    scale_y_continuous(limits = c(Ystart, Yend),breaks = seq(Ystart,Yend,Yint)) + #set y axis limits
  
  # Add Y axis labels. currently takes a bit of manual adjustment. One of the main aims for future releases is to make this less cumbersome. 
  # geom_text(x = 0.7, y = (Yend/2)+10, label = expression(paste("MX (m",italic("g"),")")), angle = 93, colour = "grey20", hjust = "left", size = 5) + # Y Axis title 
    geom_text(x = 1, y = 100, label = "100", angle = 0, colour = "grey20", hjust = "right") +  
    geom_text(x = 1, y = 200, label = "200", angle = 0, colour = "grey20", hjust = "right") +
    geom_text(x = 1, y = 300, label = "300", angle = 0, colour = "grey20", hjust = "right") +
    geom_text(x = 1, y = 400, label = "400", angle = 0, colour = "grey20", hjust = "right")


# 5. Export Radar plot ----------------------------------------------------
  
  # Where do you want to save the plot?
    #setwd()

# Export plot at high rendering quality to you current working directory
#   ggsave("Radar Plot.jpg", width = 25, height = 20, units = "cm", dpi = 1500) # Input fill format here
