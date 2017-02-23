library(png)
source("functions.r")

# # Read in the original image
orig=readPNG("original_full.png")

# # Convert original image to 8-bit
orig = float_bit(orig)

# # Create the image with salt and pepper noise
# n2=snp_noise(orig, 0.02)

# # Use a median filter to remove the snp noise
# h2=median_filter(n2, 5)

# # Create the bright image
# orig50 = add_bright(orig, 50)

# # Add the gausian noise to the image
# n1 = gaus_noise(orig, 128)

# # Apply median filter to remove outliers
# h1 = median_filter(n1, 7)

# # Apply mean filter to smooth the pixels
# h1 = mean_filter(h1, 7)

# Create the binary image
print("Creating bin")
orig_bin=binary_filter2(orig, 130, 1.3)

# Erode then dialate
print("eroding")
orig_ed=erode_filter(orig_bin, 7)
print("dialating")
orig_ed=dialate_filter(orig_ed, 7)

# print_bit(orig_ed)

# print_bit(orig_ed)
print("Counting")
count_apples(orig_ed)

# Dialate then erode (lots of little shit)
# orig_de=dialate_filter(orig_bin, 7)
# orig_de=erode_filter(orig_de, 7)


# print_bit(orig_de)



# Save the images
png("orig_working.png")
print_bit(orig)
dev.off()

# png("orig50.png")
# print_bit(orig50)
# dev.off()

# png("h1.png")
# print_bit(h1)
# dev.off()

# png("h2.png")
# print_bit(h2)
# dev.off()

# png("n1.png")
# print_bit(n1)
# dev.off()

# png("n2.png")
# print_bit(n2)
# dev.off()

png("orig_bin.png")
print_bit(orig_bin)
dev.off()

png("orig_ed.png")
print_bit(orig_ed)
dev.off()

