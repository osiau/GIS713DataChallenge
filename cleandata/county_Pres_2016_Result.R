
# The final clean data is called "county_pres_Result2016.csv"


dir_data <- 

  # Read data
county_pres_Result <- fread(file.path(data_dir, "2016_US_County_Level_Presidential_Results.csv"))


# Change FIPS name to countyFIPS
setnames(county_pres_Result, "combined_fips", "countyFIPS")

# Export to csv
write.csv(county_pres_Result, "county_pres_Result2016.csv")