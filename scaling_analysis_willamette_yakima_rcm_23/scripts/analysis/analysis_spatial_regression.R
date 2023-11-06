################################################################################
# Generalized linear model to estimate scaling exponents
################################################################################

#Author: Francisco J. Guerrero
gc()
# Loading required packages: 

librarian::shelf(tidyverse,
                 dplyr,
                 utils,
                 leaflet,
                 sp,
                 sf,
                 nhdplusTools,
                 GGally,
                 htmltools,
                 foreign,
                 data.table,
                 nlme,
                 gstat,
                 spam,
                 fields,
                 boot)

# Local Import-Export
shapes_data <- "../../1-swf-knowledge.base/datasets/raw_data/nsi_ssn_network/data"
nhd21_shapes <- "../../1-swf-knowledge.base/datasets/raw_data/nhdplus_21_provisional/"
local_data <- "./data"
source("./source/design_scaling_graphic_prep.R")

results <- "./results"

pnw_rivers_dat <- st_transform(st_read(paste(shapes_data,
                                             "nsi_network_ywrb.shp",sep = "/")),4326)

# Convert LINESTRING geometries to POINT geometries
pnw_rivers_dat_point <- as.data.table(st_cast(pnw_rivers_dat, "POINT", warn = TRUE))

# Convert to data.table
pnw_rivers_dat_dt <- as.data.table(pnw_rivers_dat_point)

# Aggregate data for duplicated COMIDs
comid_coordinates <- pnw_rivers_dat_dt[, .(x = mean(st_coordinates(geometry)[, "X"]),
                                           y = mean(st_coordinates(geometry)[, "Y"])), 
                                       by = .(COMID)]

comid_coordinates <- comid_coordinates %>% 
  rename(comid = COMID)


scaling_analysis_dat <- scaling_analysis_dat %>% 
  merge(.,
        comid_coordinates,
        by = "comid",
        all.x = TRUE) %>% 
  rename(latitude = y,
         longitude = x)

p <- ggplot(data = scaling_analysis_dat,
            aes(x = latitude,
                y = log(accm_water_exchng_kg_d),
                color = log(accm_water_exchng_kg_d)))+
  geom_point()+
  scale_y_reverse()+
  scale_color_viridis_c()+
  facet_wrap(~basin_cat, ncol = 2, scales = "free")
p


basin_coords <- scaling_analysis_dat %>% 
  select(basin,
         comid,
         latitude,
         longitude)

model_dat <- scaling_analysis_dat %>% 
  select(basin,
         stream_order,
         wshd_area_km2,
         accm_totco2_o2g_day,
         accm_water_exchng_kg_d,
         wshd_max_elevation_m,
         mean_ann_pcpt_mm,
         mean_ann_runf_mm,
         mean_ann_flow_m3s,
         forest_3scp,
         latitude,
         longitude)
str(model_dat)

# Sample 10% of data maintaining proportion for each basin and stream order
set.seed(123)  # for reproducibility
sampled_df <- model_dat %>%
  group_by(basin, stream_order) %>%
  sample_frac(0.025, replace = FALSE)

# Calculate distance matrix using latitude and longitude
dist.mat <- rdist(sampled_df[, c("latitude", "longitude")])

# Create a spatial weights matrix (e.g., binary adjacency matrix)
threshold <- 0.01  # Adjust threshold as needed
spatial.weights <- ifelse(dist.mat < threshold, 1, 0)

# Fit a GLS model accounting for spatial autocorrelation
# Replace 'accm_totco2_o2g_day ~ 1' with your model formula
gls.model <- gls(log(accm_totco2_o2g_day) ~ log(wshd_area_km2) + 
                   log(accm_water_exchng_kg_d/wshd_area_km2) + 
                   log(mean_ann_pcpt_mm) + log(wshd_max_elevation_m) + 
                   log(mean_ann_flow_m3s) + basin, data=sampled_df, 
                   correlation=corExp(form=~latitude+longitude, nugget=TRUE))

# Summary of the model
summary(gls.model)

# 1. Residual Plots
residuals <- residuals(gls.model)

# Residuals vs Fitted Values
ggplot() +
  geom_point(aes(x=fitted(gls.model), y=residuals), color='blue') +
  ggtitle("Residuals vs Fitted Values") +
  xlab("Fitted Values") + ylab("Residuals")

# Histogram of Residuals
ggplot() +
  geom_histogram(aes(x=residuals), fill='blue', binwidth=0.1) +
  ggtitle("Histogram of Residuals") +
  xlab("Residuals") + ylab("Frequency")

# Bootstrapped approach:

# Define a function to fit the GLS model and return the coefficients and residuals
fit_gls <- function(data, indices) {
  sampled_data <- data[indices, ]
  
  # Add a small random jitter to latitude and longitude
  jitter_amount <- 1e-8  # adjust as needed
  sampled_data$latitude <- jitter(sampled_data$latitude, amount=jitter_amount)
  sampled_data$longitude <- jitter(sampled_data$longitude, amount=jitter_amount)
  
  # Try fitting the model and catch any errors
  result <- tryCatch({
    # Adjust control parameters to increase max iterations and change optimizer
    ctrl <- glsControl(opt="optim", maxIter=1000, msMaxIter=1000)
    
    # Fit the GLS model
    gls.model <- gls(log(accm_totco2_o2g_day) ~ log(wshd_area_km2) + 
                       log(accm_water_exchng_kg_d/wshd_area_km2) + 
                       log(mean_ann_pcpt_mm) + log(wshd_max_elevation_m) + 
                       log(mean_ann_flow_m3s) + basin, data=sampled_data, 
                     correlation=corExp(form=~latitude+longitude, nugget=TRUE),
                     control=ctrl)
    
    # Combine coefficients and residuals
    c(coef(gls.model), residuals(gls.model))
  }, error = function(e) {
    # Return NA if there's an error
    warning("Error fitting model on this bootstrap sample:", conditionMessage(e))
    return(NA)
  })
  
  return(result)
}

# Set seed for reproducibility
set.seed(123)

# Sample without replacement
sampled_df <- model_dat %>%
  group_by(basin, stream_order) %>%
  sample_frac(0.025, replace = FALSE)

# Perform bootstrap

# Define R

R = 500

results <- boot(data=sampled_df, statistic=fit_gls, R)

# Extract the coefficients from the bootstrap results
# Assuming you have 7 coefficients (including intercept), adjust if different
coef_results <- results$t[, 1:7]

# Convert coef_results to a data frame
coef_df <- as.data.frame(coef_results)

# Assign column names
colnames(coef_df) <- c("Intercept", "wshd_area_km2", "accm_water_exchng_kg_d", 
                       "mean_ann_pcpt_mm", "wshd_max_elevation_m", 
                       "mean_ann_flow_m3s", "basin")

# Display the data frame
head(coef_df) 

# Calculate the mean and 95% confidence intervals for each variable
summary_stats <- apply(coef_df, 2, function(x) {
  c(
    Mean = mean(x),
    `Lower CI` = quantile(x, probs = 0.025),
    `Upper CI` = quantile(x, probs = 0.975)
  )
})

# Convert the result to a data frame
summary_df <- as.data.frame(t(summary_stats))

# Display the summary data frame
print(summary_df)

# Extract residuals from bootstrap results
bootstrap_residuals <- results$t[, 8:ncol(results$t)]

# Diagnostic Plot 1: Histogram of residuals
hist(bootstrap_residuals, main="Histogram of Residuals", 
     xlab="Residuals", ylab="Frequency", col="skyblue", border="black")

# Diagnostic Plot 2: QQ plot of residuals
qqnorm(bootstrap_residuals, main="QQ Plot of Residuals")
qqline(bootstrap_residuals, col="red")

# Saving results
# Raw_coefficients dataset
write.csv(coef_df,paste(results,"guerrero_etal_23_scaling_exponents_boot_gls_model_outputs.csv",
                        sep = '/'), row.names = FALSE)

# Coefficient summaries and ci
write.csv(summary_df,paste(results,"guerrero_etal_23_scaling_exponents_boot_gls_summary_confint.csv",
                        sep = '/'), row.names = TRUE)


################################################################################
# Model predictions across the whole dataset

# 1. Importing model coefficients

model_coeff <- read_csv(paste(results,"guerrero_etal_23_scaling_exponents_boot_gls_summary_confint.csv",
                              sep = '/'), show_col_types = FALSE) %>% 
  rename(term = ...1)

# Extract mean coefficients from spc_tbl_
# Note: Make sure that the column names match those in spc_tbl_
mean_coefs <-model_coeff$Mean
names(mean_coefs) <- model_coeff$term

# 2. Subsetting models dat

model_dat <- scaling_analysis_dat %>% 
  select(basin,
         accm_totco2_o2g_day,
         wshd_area_km2,
         accm_water_exchng_kg_d,
         mean_ann_pcpt_mm,
         wshd_max_elevation_m,
         mean_ann_flow_m3s,
         longitude,
         latitude) %>% 
  mutate(basin_num = ifelse(basin=="yakima",1,0))


predictions <- model_dat %>%
  mutate(
    log_predicted_accm_totco2_o2g_day = mean_coefs['Intercept'] +
      mean_coefs['wshd_area_km2'] * log(wshd_area_km2) +
      mean_coefs['accm_water_exchng_kg_d'] * log(accm_water_exchng_kg_d/wshd_area_km2) +
      mean_coefs['mean_ann_pcpt_mm'] * log(mean_ann_pcpt_mm) +
      mean_coefs['wshd_max_elevation_m'] * log(wshd_max_elevation_m) +
      mean_coefs['mean_ann_flow_m3s'] * log(mean_ann_flow_m3s) +
      mean_coefs['basin'] *basin_num,
    # Transform back from log scale if original model was logged
    predicted_accm_totco2_o2g_day = exp(log_predicted_accm_totco2_o2g_day),
    residuals = accm_totco2_o2g_day - predicted_accm_totco2_o2g_day
  )

# View the first few rows of the predictions
head(predictions)


# Estimated vs. Predicted:

p <- ggplot(data = predictions,
            aes(x = accm_totco2_o2g_day,
                y = predicted_accm_totco2_o2g_day,
                color = basin))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(linetype = "dashed")+
  theme(legend.position = c(0.85, 0.15))
p

pm <- ggMarginal(p,type = "density", groupColour = TRUE, groupFill = TRUE)
pm

# residuals vs. location

r <- ggplot(data = predictions, 
            aes(x = longitude,
                y = latitude,
                color = log(abs(residuals))))+
  scale_color_viridis_c()+
  geom_point()+
  facet_wrap(~basin, ncol = 2, scales = "free")
r
  

