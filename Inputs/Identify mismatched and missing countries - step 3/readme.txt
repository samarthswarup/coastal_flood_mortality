This folder contains the files used to compare country codes across
SLIIDERS and other sources (e.g., World Bank).

The file "SLIIDERS country codes with names and territorial mapping" contains
a column "Group with (for World Bank)" that is used to declare which country/
territory a country/territory with missing data should be grouped with.
These groupings are done for missing World Bank data, missing population scale
data (from SLIIDERS), etc., as identified in Table 1 in the manuscript.

The R script "Map mismatched countries.R" is used to locate on a map
certain SLIIDERS-identified countries with unknown ISO country codes
to ensure that they are mapped properly. 
