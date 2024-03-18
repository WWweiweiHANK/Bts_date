* Encoding: UTF-8.
CORRELATIONS
  /VARIABLES=NatPride_Mean Close_country_Mean
  /PRINT=TWOTAIL NOSIG FULL
  /MISSING=PAIRWISE.

COMPUTE National_ident_index=MEAN(NatPride_Mean,Close_country_Mean).
EXECUTE.


COMPUTE Mobility_index=MEAN(residential_percent_change_from_baseline,retail_and_recreation_percent_change_from_baseline,
    grocery_and_pharmacy_percent_change_from_baseline,parks_percent_change_from_baseline,
    transit_stations_percent_change_from_baseline,workplaces_percent_change_from_baseline).
EXECUTE.


DESCRIPTIVES VARIABLES=NatPride_Mean Close_country_Mean National_ident_index Mobility_index
  /STATISTICS=MEAN STDDEV MIN MAX.

RELIABILITY
  /VARIABLES=residential_percent_change_from_baseline retail_and_recreation_percent_change_from_baseline 
    grocery_and_pharmacy_percent_change_from_baseline parks_percent_change_from_baseline 
    transit_stations_percent_change_from_baseline workplaces_percent_change_from_baseline
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA.

CORRELATIONS
  /VARIABLES=National_ident_index Mobility_index NatPride_Mean Close_country_Mean 
    retail_and_recreation_percent_change_from_baseline 
    grocery_and_pharmacy_percent_change_from_baseline parks_percent_change_from_baseline 
    transit_stations_percent_change_from_baseline workplaces_percent_change_from_baseline 
    residential_percent_change_from_baseline
  /PRINT=TWOTAIL NOSIG FULL
  /MISSING=PAIRWISE.
