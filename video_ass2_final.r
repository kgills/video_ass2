library(png)
source("functions.r")

# # Read in the original image
orig=readPNG("original_full.png")

# Convert original image to 8-bit
orig = float_bit(orig)

# Create the image with salt and pepper noise
n2=snp_noise(orig, 0.02)

# Use a median filter to remove the snp noise
h2=median_filter(n2, 5)

# Create the bright image
orig50 = add_bright(orig, 50)

# Add the gausian noise to the image
n1 = gaus_noise(orig, 128)

# Apply median filter to remove outliers
h1 = median_filter(n1, 7)

# Apply mean filter to smooth the pixels
h1 = mean_filter(h1, 7)

# Create the binary images
orig_bin=binary_filter2(orig, 130, 1.3)
n1_bin=binary_filter2(n1, 130, 1.3)
n2_bin=binary_filter2(n2, 130, 1.3)
h1_bin=binary_filter2(h1, 130, 1.3)
h2_bin=binary_filter2(h2, 130, 1.3)
orig50_bin=binary_filter2(orig50, 170, 1.3)

# Erode then dialate
orig_ed=erode_filter(orig_bin, 7)
orig_ed=dialate_filter(orig_ed, 7)

n1_ed=erode_filter(n1_bin, 7)
n1_ed=dialate_filter(n1_ed, 7)

n2_ed=erode_filter(n2_bin, 7)
n2_ed=dialate_filter(n2_ed, 7)

h1_ed=erode_filter(h1_bin, 7)
h1_ed=dialate_filter(h1_ed, 7)

h2_ed=erode_filter(h2_bin, 7)
h2_ed=dialate_filter(h2_ed, 7)

orig50_ed=erode_filter(orig50_bin, 7)
orig50_ed=dialate_filter(orig50_ed, 7)

# Count the number of apples
count_apples(orig_ed)
count_apples(n1_ed)
count_apples(n2_ed)
count_apples(h1_ed)
count_apples(h2_ed)
count_apples(orig50_ed)

# Save the images
png("orig_working.png")
print_bit(orig)
dev.off()

png("orig50.png")
print_bit(orig50)
dev.off()

png("h1.png")
print_bit(h1)
dev.off()

png("h2.png")
print_bit(h2)
dev.off()

png("n1.png")
print_bit(n1)
dev.off()

png("n2.png")
print_bit(n2)
dev.off()

png("orig_bin.png")
print_bit(orig_bin)
dev.off()

png("n1_bin.png")
print_bit(n1_bin)
dev.off()

png("n2_bin.png")
print_bit(n2_bin)
dev.off()

png("h1_bin.png")
print_bit(h1_bin)
dev.off()

png("h2_bin.png")
print_bit(h2_bin)
dev.off()

png("orig50_bin.png")
print_bit(orig50_bin)
dev.off()

png("orig_ed.png")
print_bit(orig_ed)
dev.off()

png("n1_ed.png")
print_bit(n1_ed)
dev.off()

png("n2_ed.png")
print_bit(n2_ed)
dev.off()

png("h1_ed.png")
print_bit(h1_ed)
dev.off()

png("h2_ed.png")
print_bit(h2_ed)
dev.off()

png("orig50_ed.png")
print_bit(orig50_ed)
dev.off()
