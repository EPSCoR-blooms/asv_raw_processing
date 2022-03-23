# asv_raw_processing
processing scripts for ASV data that uses metadata template

 - you will need write permission in the DartFS folders to execute the entirety of the code.


# files and descriptions
*0.compile_metadata.R*: This script compiles metadata from the template format into a single csv in the parent folder for EpscorBlooms.
 - before running this, make sure all date, time, and file name formatting is correct in the metadata template in DartFS

*1.check_for_CV.R*: This script checks for new column names and adds them to the .csv translate table of column names to CV.

*2.additional_compile_asv.R*: This script compiles the asv data into a raw file for further processing. You have the option to process only new files since last processing OR reprocess all files.

*3.process_asv.R*: this script processes each of the ASV deployments and truncates before/after launch, flags waypoint info, and flags loiter times

*x.manual_process.R*: this script processes ASV deplooyments that needed special attention and could not be run through the processing script workflow above.
    as of Feb 2022, this consists of 2021-07-22 at Sunapee, that combined two pathfiles

*y.additional_flags.R*: this script adds flags where the robot did not complete the prescribed path. this file must be run after 3.process_asv.R.

