
set.seed(123)

require(raster)
# Generate 10 random variables
random_distributions <- replicate(10, rnorm(10000))

# Print the random variables
print(random_variables)

# Create an empty raster with the desired extent and resolution
raster_template <- raster(nrows = 100, ncols = 100, xmn = 100, xmx = 150, ymn = 20, ymx = 60)
projection(raster_template) <- "+proj=longlat +datum=WGS84"

# Assign the random variables to the raster cells
values(raster_template) <- random_distributions[,1]

# Plot the raster
plot(raster_template)

dim(raster_template)
100*100

require(rgeoda)

skater()