# Make a transect spatial object
# Note that in this case FID corresponds to rows, but they are distinct transects
make_transect_sf <- function(df, 
                             lat_start_col = "start_latitude", lon_start_col = "start_longitude",
                             lat_stop_col  = "stop_latitude",  lon_stop_col  = "stop_longitude",
                             id_col = "FID", crs = 4326) {
  library(data.table)
  library(sf)
  
  df <- as.data.table(df)
  
  # Create list of LINESTRING coordinate matrices with safe fallback
  lines_list <- lapply(seq_len(nrow(df)), function(i) {
    start_lon <- df[[lon_start_col]][i]
    start_lat <- df[[lat_start_col]][i]
    stop_lon  <- df[[lon_stop_col]][i]
    stop_lat  <- df[[lat_stop_col]][i]
    
    if (start_lon == stop_lon && start_lat == stop_lat) {
      # Handle identical start/stop case: add small jitter to stop point
      matrix(c(
        start_lon, start_lat,
        stop_lon + 1e-6, stop_lat + 1e-6  # small offset to create a valid line
      ), ncol = 2, byrow = TRUE)
    } else {
      matrix(c(
        start_lon, start_lat,
        stop_lon,  stop_lat
      ), ncol = 2, byrow = TRUE)
    }
  })
  
  # Convert to sf LINESTRING geometry
  line_geoms <- st_sfc(lapply(lines_list, st_linestring), crs = crs)
  
  # Return sf object
  st_sf(data.frame(df), geometry = line_geoms)
}


#-------------------------------------------------------------------------------
# Spatial difference matrix
#-------------------------------------------------------------------------------

compute_spatial_dist_matrix <- function(tar_coords, knwn_coords) {
  # Ensure coordinates are matrices
  tar_coords <- as.matrix(tar_coords)
  knwn_coords <- as.matrix(knwn_coords)
  
  # Combine for pairwise distance computation
  combined_coords <- rbind(tar_coords, knwn_coords)
  
  # Compute full distance matrix
  full_dist <- as.matrix(dist(combined_coords))
  
  # Extract distances from tar_coords to knwn_coords
  n_tar <- nrow(tar_coords)
  n_knwn <- nrow(knwn_coords)
  
  full_dist[1:n_tar, (n_tar + 1):(n_tar + n_knwn)]
}

#-------------------------------------------------------------------------------
# Temporal difference matrix
#-------------------------------------------------------------------------------

## Note that time_vec(s) are POSIXct (or Date) timestamps
## units can be "secs", "mins", "hours", "days", or "weeks" — same as in difftime.
compute_time_diff_matrix <- function(time_vec1, time_vec2, units = "secs"){
  outer(time_vec1, time_vec2, 
        FUN = function(t1, t2) abs(as.numeric(difftime(t1, t2, units = units))))
}