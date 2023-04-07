#To filter a dataset without downloading it, you can use the readRDS() function in R to read the RDS file directly from the web. Here's an example code to filter the dataset in the provided URL:

# Load required packages
librarian::shelf(utils, 
                 dplyr, 
                 zen4R)

# Local import and export paths

raw_data <- "../1-swf-knowledge.base/assets/data/raw" 

processed_data <- "../1-swf-knowledge.base/assets/data/processed"


# Create a temporary directory to store the heavy data from Zenodo
temp_dir <- tempdir()

# Specify the target file name
target_files <- c("nhd_CR_stream_annual_resp_inputs_outputs.rda",
                  "nhd_CR_stream_spring_resp_inputs_outputs.rda",
                  "nhd_CR_stream_summer_resp_inputs_outputs.rda")

# # Generate a unique file name for the temporary file
# temp_files <- file.path(temp_dir, paste0(target_files))


# Download the RDA file from Zenodo to the temporary file
download_zenodo(path = temp_dir,
                doi = "10.5281/zenodo.6954107",
                files = target_files)

# Load the data from the RDA file
# Read in the files and assign them to variables
annual_dat <- readRDS(file.path(temp_dir, paste0(target_files[1])))
spring_dat <- readRDS(file.path(temp_dir, paste0(target_files[2])))
summer_dat <- readRDS(file.path(temp_dir, paste0(target_files[3])))

# Flagging each data set according to their temporal window
annual_dat$time_type <- "annual"
spring_dat$time_type <- "spring"
summer_dat$time_type <- "summer"

# Unifying column names:
un_colnames <- c("comid","pred_doc","pred_do",
                 "no3_conc_mg_l","logRT_total_hz_s","logq_hz_total_m_s",
                 "logtotco2_o2g_m2_day","logtotco2_ang_m2_day","logtotco2g_m2_day",
                 "time_type" )

colnames(annual_dat) <- un_colnames
colnames(spring_dat) <- un_colnames
colnames(summer_dat) <- un_colnames

resp_dat <- rbind(annual_dat,spring_dat,summer_dat)

write.csv(resp_dat,file=paste(raw_data,"230406_son_etal_22_results_zen.csv",sep = '/'),
          row.names = FALSE)

# Let's now merge this data with the land use data:

lnd_dat <- read_csv(file=paste(processed_data,"230324_inf_cont_lnd.csv",sep = "/"),
                    show_col_types = FALSE)

main_hbgc_dat <- merge(resp_dat,lnd_dat,by="comid")


write.csv(main_hbgc_dat,"../1-swf-knowledge.base/assets/data/processed/230406_hbgc_pnw_land.csv",row.names = FALSE)


