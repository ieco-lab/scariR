# slfSpread 
# folder: data-raw and all sub-folders
# descriptions and changelog

# data-raw (raw downloads or data received from collaborators)

----------slf_lyde_raw_coords_DATE----------

the raw SLF coordinates pulled from the lydemapR package



----------SPECIES_gbif_raw_coords_DATE----------

original species occurrence data retrieval from gbif

*slf_gbif_raw_coords_2023-08-24 does not include any points from N America



## SPECIES_gbif_DATE.csv

the raw query data for the original gbif pull (raw_coords). 
This contains all of the raw information describing the gbif pull and all of the data that were pulled from gbif using the spocc package.

*slf_gbif_2023-08-24 does not include any points from N America

----------SPECIES_gbif_cleaned_----------

final version of lat/long coordinates for slf occurrences, retrieved from gbif

### changelog

v0- initial version
v1- (for SLF_gbif_cleaned) removed 22 points where data did not exist for global_bio_1



----------US_FIPS_Codes----------

This contains FIPS codes lists (per county), downloaded from the US census bureau




# CHELSA

----------_bookmark.duck----------

The data folder bookmarks from the CHELSA data server.
This can be used to download the files in bulk from a particular directory.

----------_URLs.txt----------

These files contain the URLs that are given if you choose to download the directory by file. 
Each URL immediately downloads the associated CHELSA raster file when put in a browser.





# data-old

----------slfSpread_jumps_locations_----------

see metadata for variable info

### changelog

v3- reran script when it was improved by Nadege, some data points were cut



----------tinyslf_presences_cleaned----------

records pulled from a pre-publication version of the lydemapR package dataset.
these data were cleaned and rarefied (spatial thinning).



----------tinyslf_absences----------

the corresponding SLF absence data from tinyslf_presences_cleaned


