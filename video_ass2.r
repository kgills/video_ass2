library(png)
source("video_ass1_functions.r")

# Read in the original image
orig=readPNG("original.png")

# Convert original image to 8-bit
orig = float_8bit(orig)

# # Create the bright image
orig50 = add_bright(orig, 50)


# Save the images
png("orig_working.png")
print_bit(orig)
dev.off()

png("orig50.png")
print_bit(orig50)
dev.off()

