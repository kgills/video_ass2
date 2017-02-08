rgb_hsv <- function(rgb) {

    hsv = rgb
    for (i in seq_len(dim(rgb)[1])) {
        for (j in seq_len(dim(rgb)[2])) {

            rgb_max = max(rgb[i,j,])
            rgb_min = min(rgb[i,j,])
            rgb_delta = rgb_max - rgb_min

            # Calculate V
            if(rgb_max == 0) {
                hsv[i,j,1] = 0
                hsv[i,j,2] = 0
                hsv[i,j,3] = 0
                continue
            }
            hsv[i,j,3] = rgb_max

            # Calculate S
            hsv[i,j,2] = as.integer((rgb_delta/rgb_max) * 255)

            # Calculate H
            if(rgb_delta == 0) {
                hsv[i,j,1] = 0
            } else if(rgb[i,j,1] == rgb_max) {
                hsv[i,j,1] = as.integer(0 + 43 * (rgb[i,j,2] - rgb[i,j,3])/(rgb_delta))
            } else if(rgb[i,j,2] == rgb_max)  {
                hsv[i,j,1] = as.integer(85 + 43 * (rgb[i,j,3] - rgb[i,j,1])/(rgb_delta))
            } else {
                hsv[i,j,1] = as.integer(171 + 43 * (rgb[i,j,1] - rgb[i,j,2])/(rgb_delta))
            }

            # Wrap the values
            hsv[i,j,1] = hsv[i,j,1] %% 256 
            hsv[i,j,2] = hsv[i,j,2] %% 256 
            hsv[i,j,3] = hsv[i,j,3] %% 256 
        }
    }

    return(hsv)
}

float_bit <- function(float) {
    for (i in seq_len(dim(float)[1])) {
        for (j in seq_len(dim(float)[2])) {
            for (k in seq_len(dim(float)[3])) {

                # Convert orig image to 8-bit color
                float[i,j,k] = as.integer(float[i,j,k] * 255)
            }
        }
    }

    return(float)
}

bit_float <- function(bit) {
    for (i in seq_len(dim(bit)[1])) {
        for (j in seq_len(dim(bit)[2])) {
            for (k in seq_len(dim(bit)[3])) {

                # Convert orig image to floating
                bit[i,j,k] = as.double(bit[i,j,k] / 255)
            }
        }
    }

    return(bit)
}

add_bright <- function(img, val) {
    for (i in seq_len(dim(img)[1])) {
        for (j in seq_len(dim(img)[2])) {
            for (k in seq_len(dim(img)[3])) {

                # Increase the brightness
                img[i,j,k] = img[i,j,k]+val
                if(img[i,j,k] > 255) {
                    img[i,j,k] = 255
                } 
            }
        }
    }

    return(img)
}

print_bit <- function(img) {
    for (i in seq_len(dim(img)[1])) {
        for (j in seq_len(dim(img)[2])) {
            for (k in seq_len(dim(img)[3])) {

                # Convert orig image to floating
                img[i,j,k] = as.double(img[i,j,k] / 255)
            }
        }
    }

    grid::grid.raster(img)
}

# Add gausian noise to the given image
# Operates on 8-bit color image. Each color operated independently
# Does not operate on border pixels
gaus_filter <- function(img, sigma, dim) {
    
    # Operate on each color independently
    # h(u,v) = (1/(2*pi*sigma^2))*e^(-((u^2 + v^2)/sigma^2))
    # Calculate h(u,v) for the dimension

    for (i in seq_len(dim(img)[1])) {
        for (j in seq_len(dim(img)[2])) {
            # Continue if i or j is less than dim, or greater than 
            # dim(img) - dim

            for (k in seq_len(dim(img)[3])) {
                # Apply the calculated h(u,v) filter to the pixels

                # Sum the results
            }
        }
    }
}
