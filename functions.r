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
gaus_noise <- function(img, sigma) {
    out=img
    for (i in seq_len(dim(img)[1])) {
        for (j in seq_len(dim(img)[2])) {
            for (k in seq_len(dim(img)[3])) {
                out[i,j,k] = img[i,j,k] + as.integer(rnorm(1, mean=0, sd=sigma))

                if(out[i,j,k] < 0) {
                    out[i,j,k] = 0
                }

                if(out[i,j,k] > 255) {
                    out[i,j,k] = 255
                }
            }
        }
    }

    return(out)
}

snp_noise <- function(img, prob) {
    out=img
    # Add salt and pepper noise with probability prob
    for (i in seq_len(dim(img)[1])) {
        for (j in seq_len(dim(img)[2])) {
            if(runif(1) < prob) {
                if(runif(1) <= 0.50) {
                    out[i,j,1] = 255
                    out[i,j,2] = 255
                    out[i,j,3] = 255
                } else {
                    out[i,j,1] = 0
                    out[i,j,2] = 0
                    out[i,j,3] = 0
                }
            }
        }
    }

    return(out)
}

gaus_filter <- function(img, sigma, dim) {

    # h(u,v) = (1/(2*pi*sigma^2))*e^(-((u^2 + v^2)/sigma^2))
    h=matrix(nrow=dim, ncol=dim)
    offset=(dim+1)/2
    sum = 0

    # Calculate the gaussian distribution centered around the center pixel
    for(i in seq(1,dim)) {
        for(j in seq(1,dim)) {
            h[i,j] = (1/(2*pi*sigma^2))*exp(-(((i-offset)^2)+(j-offset)^2)/(sigma^2))
            sum = sum+h[i,j]
        }
    }

    # Normalize the filter
    for(i in seq(1,dim)) {
        for(j in seq(1,dim)) {
            h[i,j] = h[i,j] / sum
        }
    }

    # Apply the filter to the output
    out=img
    offset=(dim-1)/2

    for (i in seq(offset+1, dim(img)[1]-offset-1)) {
        for (j in seq(offset+1, dim(img)[2]-offset-1)) {
            for (k in seq_len(dim(img)[3])) {

                out[i,j,k] = 0

                for(l in seq(1,dim)) {
                    for(m in seq(1,dim)) {
                        out[i,j,k] = out[i,j,k] + img[(i+l-offset),(j+m-offset),k]*h[l,m]
                    }
                }

                if(out[i,j,k] < 0) {
                    out[i,j,k] = 0
                }

                if(out[i,j,k] > 255) {
                    out[i,j,k] = 255
                }
            }
        }
    }

    return(out)
}

median_filter <- function(img, dim) {

    offset=(dim-1)/2
    out=img

    for (i in seq(offset+1, dim(img)[1]-offset-1)) {
        for (j in seq(offset+1, dim(img)[2]-offset-1)) {
            for (k in seq_len(dim(img)[3])) {

                median_array = numeric()

                for(l in seq(1,dim)) {
                    for(m in seq(1,dim)) {
                        median_array =  c(median_array,img[i+l-offset,j+m-offset,k])
                    }
                }

                out[i,j,k] = median(median_array)
            }
        }
    }

    return(out)
}

mean_filter <- function(img, dim) {

    offset=(dim-1)/2
    out=img

    for (i in seq(offset+1, dim(img)[1]-offset-1)) {
        for (j in seq(offset+1, dim(img)[2]-offset-1)) {
            for (k in seq_len(dim(img)[3])) {

                avg_array = numeric()

                for(l in seq(1,dim)) {
                    for(m in seq(1,dim)) {
                        avg_array =  c(avg_array,img[i+l-offset,j+m-offset,k])
                    }
                }

                out[i,j,k] = mean(avg_array)
            }
        }
    }

    return(out)
}

binary_filter0 <- function(img, factor) {

    out = img

    for (i in seq(1, dim(img)[1])) {
        for (j in seq(1, dim(img)[2])) {

            # Convert the green to red

            # If one color dominates, set it to 1
            if((img[i,j,1] > (img[i,j,2]*factor)) || (img[i,j,1] > (img[i,j,3]*factor))) {
                out[i,j,1] = 255
                out[i,j,2] = 255
                out[i,j,3] = 255
            } else if((img[i,j,2] > (img[i,j,1]*factor)) || (img[i,j,2] > (img[i,j,3]*factor))) {
                out[i,j,1] = 255
                out[i,j,2] = 255
                out[i,j,3] = 255
            } else if((img[i,j,3] > (img[i,j,2]*factor)) || (img[i,j,3] > (img[i,j,1]*factor))) {
                out[i,j,1] = 255
                out[i,j,2] = 255
                out[i,j,3] = 255
            } else {
                out[i,j,1] = 0
                out[i,j,2] = 0
                out[i,j,3] = 0
            }
        }   
    } 

    return(out)
}

binary_filter2 <- function(img, thresh, factor) {

    out = img

    for (i in seq(1, dim(img)[1])) {
        for (j in seq(1, dim(img)[2])) {

            # Convert the green to red

            # If one color dominates, set it to 1
            if(mean(img[i,j,]) > thresh) {
                # If one color dominates, set it to 1
                if((img[i,j,1] > (img[i,j,2]*factor)) || (img[i,j,1] > (img[i,j,3]*factor))) {
                    out[i,j,1] = 255
                    out[i,j,2] = 255
                    out[i,j,3] = 255
                } else if((img[i,j,2] > (img[i,j,1]*factor)) || (img[i,j,2] > (img[i,j,3]*factor))) {
                    out[i,j,1] = 255
                    out[i,j,2] = 255
                    out[i,j,3] = 255
                } else if((img[i,j,3] > (img[i,j,2]*factor)) || (img[i,j,3] > (img[i,j,1]*factor))) {
                    out[i,j,1] = 255
                    out[i,j,2] = 255
                    out[i,j,3] = 255
                } else {
                    out[i,j,1] = 0
                    out[i,j,2] = 0
                    out[i,j,3] = 0
                }
            } else {
                out[i,j,1] = 0
                out[i,j,2] = 0
                out[i,j,3] = 0
            }
        }   
    } 

    return(out)
}

binary_filter1 <- function(img, thresh, factor) {

    out = img

    for (i in seq(1, dim(img)[1])) {
        for (j in seq(1, dim(img)[2])) {

            # Convert the green to red

            # If one color dominates, set it to 1
            if(mean(img[i,j,]) > thresh) {
                out[i,j,1] = 255
                out[i,j,2] = 255
                out[i,j,3] = 255
            } else {
                out[i,j,1] = 0
                out[i,j,2] = 0
                out[i,j,3] = 0
            }
        }   
    } 

    return(out)
}


erode_filter <- function(img, dim) {
    out = img
    offset=(dim-1)/2

    for (i in seq(offset+1, dim(img)[1]-offset-1)) {
        for (j in seq(offset+1, dim(img)[2]-offset-1)) {

            if(sum(img[seq(i-offset,i+offset), seq(j-offset, j+offset), 1]) == 255*dim^2) {
                out[i,j,1] = 255
                out[i,j,2] = 255
                out[i,j,3] = 255
            } else {
                out[i,j,1] = 0
                out[i,j,2] = 0
                out[i,j,3] = 0
            }
        }
    } 

    return(out)
}

dialate_filter <- function(img, dim) {
    out = img
    offset=(dim-1)/2

    for (i in seq(offset+1, dim(img)[1]-offset-1)) {
        for (j in seq(offset+1, dim(img)[2]-offset-1)) {

            if(sum(img[seq(i-offset,i+offset), seq(j-offset, j+offset), 1]) > 0) {
                out[i,j,1] = 255
                out[i,j,2] = 255
                out[i,j,3] = 255
            } else {
                out[i,j,1] = 0
                out[i,j,2] = 0
                out[i,j,3] = 0
            }
        }
    }

    return(out)
}

count_apples <- function(img) {
    out = img
    out[,,]=0
    label=1

    directory=character()
    entries=0

    for (i in seq(2, dim(img)[1])) {
        for (j in seq(2, dim(img)[2])) {

            if(img[i,j,1] > 0) {
               # If only left pixel is labeled
                if((out[i-1,j,1] > 0) && (out[i,j-1,1] == 0)) {
                    out[i,j,1] = out[i-1,j,1]
                } 

                # If only top pixel is labeled
                else if((out[i-1,j,1] == 0) && (out[i,j-1,1] > 0)) {
                    out[i,j,1] = out[i,j-1,1]
                }

                # If both labeled same thing
                else if((out[i-1,j,1] > 0) && (out[i,j-1,1] > 0) && (out[i,j-1,1] == out[i-1,j,1])) {
                    out[i,j,1] = out[i,j-1,1]
                }

                # If both labeled different
                else if((out[i-1,j,1] > 0) && (out[i,j-1,1] > 0) && (out[i,j-1,1] != out[i-1,j,1])) {

                    # Set label to left pixel value
                    out[i,j,1] = out[i-1,j,1]

                    # Find the left and right entries
                    entry_left=0
                    entry_top=0
                    for(k in seq(1, entries)) {
                        if(grepl(toString(out[i-1,j,1]), directory[k])) {
                            entry_left = k
                        }
                        if(grepl(toString(out[i,j-1,1]), directory[k])) {
                            entry_top = k
                        }
                    }

                    if(entry_left==0 || entry_top==0) {
                        print("Error searching directory")
                        return
                    }

                    if(entry_left != entry_top) {
                        # Merge the two entries
                        directory[entry_left]=paste(directory[entry_left], directory[entry_top])

                        # Delete directory top
                        directory=directory[-entry_top]
                        entries=entries-1
                    }
                }

                # Add new entry
                else {
                    out[i,j,1] = label
                    label = label+1
                    entries=entries+1
                    directory[entries]=toString(label-1)
                }
            }
        }
    }

    print(length(directory))
}

