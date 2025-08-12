circularDNACircos - This is a R package to plot the circos plots for the circular DNA.

==== Use the library ================
library(circularDNACircos)
dual_circos_plot("/path_to_file1","/path_to_file2")

--optional arguments
- outer_track_height can be in between(0 to 1 ideally 0.25)
- inner_track_height can be in between( 0 to 1 ideally < 0.2)
- max_pairs_per_outer_bin number of pairs you want to show in the bin ( ideally 20 for track height of 0.25)
- max_pairs_per_inner_bin number of pairs you want to show in the bin ( ideally 20 for track height of 0.25)
- outer_track_arrow_head_width arrow head width (ideally in between 0.05-0.2)
- outer_track_arrow_head_length arrow head length( ideally in between 0.05-0.4)
- inner_track_arrow_head_width arrow head width (ideally in between 0.05-0.2)
- inner_track_arrow_head_length arrow head length( ideally in between 0.05-0.4)
- png1 name of the first circos plot
- png2 name of the second circos plot

=======================================

Both the files can be either in text or csv format.

Case-1: If the files are in the text format 
File -1( columns should be "TAB" seperated and no need to add headers but add values for the following columns "chromosome", "start1", "end1", "start2", "end2", "orientation")  
Example format
chr1  1000  1150  1350  1500  FR\n
chr1  1200  1350  1550  1700  FR\n
chr1  1000  1050  1900  2000  FF\n

File -2( columns should be "TAB" seperated and no need to add headers but add values for the following columns "chromosome", "start1", "end1", "start2", "end2", "start3", "end3", "orientation")
Example format 
chr1  1000  1050  1900  2000  1300  1450 FFR
chr1  1020  1070  1880  1980  1700  1850 RRF

Case-2: If the files are in the csv format 
Add the following column headers for file 1  "chromosome", "start1", "end1", "start2", "end2", "orientation"
Add the following column headers for file 2  "chromosome", "start1", "end1", "start2", "end2", "start3", "end3", "orientation"


After running the function two png files will be create one for each plot in the same directory(if output directory is not passed in the optional argments png1 and png2).

