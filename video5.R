## Commented by ChatGPT

plot_nmds <- function (axes_file){
    # Load the table from the file. 
    # Assumes the first column contains row names (e.g., sample identifiers),
    # and the rest of the columns contain numerical data (e.g., ordination axes).
    axes <- read.table(file=axes_file, header=TRUE, row.names=1)

    # Extract the day number from the row names.
    # Example row name: "Mouse_D150"
    # Regular expression explanation:
    # - ".*D(\\d*)$":
    #     .*      : match any characters up to the last "D"
    #     D       : match the literal character "D"
    #     (\\d*)  : capture group for any number of digits (\\d means digit, * means zero or more)
    #     $       : end of the string
    # So this pattern captures the number at the end after a "D", e.g., from "Sample_D7" it extracts "7".
    day <- as.numeric(gsub(".*D(\\d*)$", "\\1", rownames(axes)))

    # Create a logical vector identifying "early" samples (days 0 to 10, inclusive)
    early <- day <= 10

    # Create a logical vector identifying "late" samples (days 140 to 150, inclusive)
    late <- day >= 140 & day <= 150

    # Combine early and late samples for plotting
    # Only keep rows (samples) from the original data that are either early or late
    plot.axes <- axes[early | late, ]

    # Keep the corresponding day numbers for the filtered samples
    plot.day <- day[early | late]

    # Filter the logical vectors to match only the subset used for plotting
    # This is needed to match up with the reduced data in plot.axes
    plot.early <- early[early | late]
    plot.late <- late[early | late]

    # Set up plotting characters (pch) for each sample
    # pch is a vector that will be used to assign point types in the plot
    pch <- vector()

    # Assign pch 21 (a circle with fill) for early samples
    pch[plot.early] <- 21

    # Assign pch 19 (a solid circle) for late samples
    pch[plot.late] <- 19

    # Extract a file name identifier from the input path to use for naming the output
    # strsplit splits the path by "/" and extracts the third component
    # (Assumes something like: "path/to/input/filename.txt", gets "filename.txt")
    output_file_stub <- strsplit(axes_file, split="\\/")[[1]][3]

    # Create the full path for the output PNG file
    # Example output: "results/figures/filename.txt.figure4.png"
    output_file <- paste0("results/figures/", output_file_stub, ".figure4.png")

    # Open PNG device for saving the plot
    png(file=output_file)

    # Create the scatter plot
    # Plot Axis 2 (Y-axis) vs Axis 1 (X-axis), using pch to mark early/late samples
    plot(plot.axes$axis2 ~ plot.axes$axis1, 
         pch=pch, 
         xlab="PCoA Axis 1", 
         ylab="PCoA Axis 2")

    # Add a legend identifying which symbol corresponds to "Early" and "Late"
    # Positions it near the bottom right of the plot
    legend(x=max(plot.axes$axis1) - 0.125, 
           y=min(plot.axes$axis2) + 0.125, 
           legend=c("Early", "Late"), 
           pch=c(21, 19))

    # Close the PNG device and save the plot to file
    dev.off()
}
