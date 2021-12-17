# asv_raw_processing
processing scripts for raw ASV data that uses metadata template

 - you will need write permission in the DartFS folders to execute the entirety of the code.

# files and descriptions
*0.compile_metadata.R*: This script compiles metadata from the template format into a single csv in the parent folder for EpscorBlooms.
 - before running this, make sure all date, time, and file name formatting is correct in the metadata template in DartFS

*1.check_for_CV.R*: This script checks for new column names and adds them to the .csv translate table of column names to CV.

*2.compile_asv.R*: This script compiles the asv data into a raw file for further processing.

*3.process_asv.R*: this script processes each of the ASV deployments and truncates before/after launch, flags waypoint info, and flags loiter times

# script needs
need a script to pull in new column names into colnames_cv.csv files for each file type
