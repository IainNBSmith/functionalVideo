library(av)
library(fda)
library(pixmap)

#av_video_images()
#***** Encode the images into pngs
filename <- "Example-BlackForwardBack.mp4"
filedir <- "Example-BlackForwardBack Images"
# can pass fractions to fps ex 0.2 is one image every 5 seconds
#av::av_video_images(filename, destdir = filedir,
#                    format = "pgm", fps = 30)

#***** Open selected images and read pixels
read_pixels <- function(filedir, pixel, image_header="image", n_images=314,
                        filter = c()) {
  a <- NULL
  try(a <- pixmap::read.pnm(paste0(filedir, "/", image_header, "_",
                                   sprintf("%06d",n_images), ".pgm")))
  if(is.null(a)) stop("Images do not exist or too many images! Check indexing!")

  if(is.vector(pixel) && !is.character(pixel)) retval <- c()
  else if(is.character(pixel) && pixel == "all") retval <- matrix(0, nrow = prod(a@size), ncol = n_images)
  else stop("Unsupported pixel argument!")

  for(i in 1:n_images) {
    a <- pixmap::read.pnm(paste0(filedir, "/", image_header, "_",
                                 sprintf("%06d",i), ".pgm"))
    #* add a pixel blur mode (eg. 4x4 box and average pixels within) so it
    #* would just move by the 4x4 box until the edge of the images
    if(is.vector(pixel) && !is.character(pixel)) {
      retval <- append(retval, a@grey[pixel[1],pixel[2]])
    }else if(is.character(pixel) && pixel == "all") {
      retval[, i] <- as.vector(a@grey)
      }
  }
  # non-conformable arguments try to fix tomorrow
  if(!is.null(filter)) {
  ind_vec <- 1:(a@size[1]*a@size[2])
  ind_vec <- ind_vec[ind_vec > a@size[1] & ind_vec %% a@size[1] != 0 & ind_vec < (a@size[1])*(a@size[2]-1) & ind_vec%%a@size[2] != 1]
  apply(as.array(ind_vec), 1, function(ind_val) {
    #pixel_values <- as.vector(a@grey[(j-1):(j+1), (k-1):(k+1)])
    retval[ind_val,] <- crossprod(t(filter), t(retval[c((ind_val-1):(ind_val+1) + a@size[1], 
                                                   (ind_val-1):(ind_val+1), 
                                                   (ind_val-1):(ind_val+1) - a@size[1]),]))
    #retval[j + a@size[1]*(k-1), i] <- temp_coef
    
  })
  }
  return(list(vd = retval,
              dim = a@size))
}

# use pixel [600, 600] for interesting data
full_img <- read_pixels(filedir, "all", filter = c(), n_images = 309)
#boring <- read_pixels(filedir, c(500,500))
#raw_data <- rbind(interesting, boring)
#basis <- create.bspline.basis(rangeval = seq(1,314, length.out=238),
#                              nbasis = 240) # play around with nbasis
#test_fd <- smooth.basis(argvals = seq(1,314, length.out=314),
#                        y = t( raw_data ),
#                        fdParobj = basis)$fd
#plot(test_fd)
#points(interesting, col = "black")
#points(boring, col = "red")


#***** Open the full data
#full_img <- read_pixels(filedir, "all")
# make it so I can only change the modified pixels if that's what I want
use_pixels <- c(1234*780 + 600, 1234*781+600, 1234*782 + 600,
                1234*780 + 601, 1234*781+601, 1234*782 + 601,
                1234*780 + 602, 1234*781+602, 1234*782 + 602)
expand_pixels <- function(use_pixels, img_dim = c(1234, 1150)) {
  new_pixels <- c()
  for( i in 1:length(use_pixels)) {
    # consider the given pixel as the upper corner of the pixels
    pixel <- use_pixels[i]
    for(j in 0:2) for(k in 0:2) new_pixels <- append(new_pixels,
                                                     pixel + k + j*(img_dim[1]))
  }
  return(new_pixels)
}


# doing blocks of pixels
# new_pix <- expand_pixels(use_pixels)
#
# use_data <- full_img[new_pix,]
# test_fd <- smooth.basis(argvals = seq(1,314, length.out=314),
#                         y = t(use_data),
#                         fdParobj = basis)$fd
# plot(test_fd)
#for(i in 1:nrow(use_data)) points(use_data[i,], col = i)

fd_to_pixels <- function(image_fd, image_dim = NULL, image_ref = NULL,
                         save_dir="", pixels_modified = NULL, scale = T) {
  if(!is.null(pixels_modified)) {
    frames <- seq(1,309, length.out=309)
      pixel_wrapper <- function(i) {
      a <- pixmap::read.pnm(paste0(image_ref, "_", sprintf("%06d",floor(i)), ".pgm"))
      fd_eval <- fda::eval.fd(i, image_fd)
      a@grey[a@grey > 0.7] = 0.5
      a@grey[pixels_modified] = fd_eval
      if(scale) a@grey[pixels_modified] <- abs(a@grey[pixels_modified]/max(abs(a@grey[pixels_modified])))
      a@grey[a@grey > 1] = 1
      a@grey[a@grey < 0] = 0
      pixmap::write.pnm(a, file = paste0(save_dir, "/frame_",
                                         sprintf("%06d",which(frames == i)), ".pgm"))
      }
      apply(as.array(frames), 1, FUN = pixel_wrapper)
  } else if(!is.null(image_ref)) {
    a <- NULL
    try(a <- pixmap::read.pnm(image_ref))
    if(is.null(a)) stop("Images do not exist!")

    if(prod(a@size) != ncol(image_fd$coefs)) stop("Non matching dimensions to fit image!")

    for(i in 1:length(image_fd$fdnames$time)) {
      fd_eval <- fda::eval.fd(i, image_fd)
      for(j in 1:a@size[1]) for(k in 1:a@size[2]) {
        a@grey[j, k] = fd_eval[j + a@size[1]*(k-1)]
      }
      a@grey[a@grey > 1] = 1
      a@grey[a@grey < 0] = 0
      pixmap::write.pnm(a, file = paste0(save_dir, "/frame_",
                                         sprintf("%06d",n_images), ".pgm"))
    }
  } else {
    stop("Unimplemented mode!")
  }


}
# av_encode_video
#***** Make the new images into a video
#use_pixels <- c(1:1234*599, 1:1234*600, 1:1234*601)
basis <- create.bspline.basis(rangeval = c(1,309),
                              nbasis = 241) # play around with nbasis
use_pixels <- c()
# use_pixels <- c(1234*780 + 600, 1234*781+600, 1234*782 + 600,
#                 1234*780 + 601, 1234*781+601, 1234*782 + 601,
#                 1234*780 + 602, 1234*781+602, 1234*782 + 602)
# small section
# for(j in 599:621) for(i in 779:801)  use_pixels <- append(use_pixels, 1234*i + j)
# for(j in 100:1000) use_pixels <- append(use_pixels, 1234*(100:1000) + j)
use_pixels <- as.vector(apply(as.array(50:900), 1, FUN=function(x) full_img$dim[1]*(50:900) + x))
#use_pixels <- c()
#for(i in c(1, 2, 9, 10, 11, 19, 20, 21, 29, 30)) use_pixels <- append(use_pixels, (600:800)*1234+(585+i))
#use_pixels <- c()
#use_pixels <- (600:800)*1234+(585+20)
use_data <- full_img$vd[use_pixels,]
test_fd <- smooth.basis(argvals = seq(1,309, length.out=309),
                        y = t(use_data),
                        fdParobj = basis)$fd
# grab pixels in sections of nine around a particular one
new_fd <- test_fd
bb <- c(101, 999, 101, 999)
for(i in bb[1]:bb[2]) for(j in bb[3]:bb[4]) {
  select_coefs <- c()
  for(y in (j-1):(j+1)) {for(x in (i-1):(i+1)){ select_coefs <- append(select_coefs, which((1234*y + x) == use_pixels))}}
  temp_coef <- c(-1,-1,-1,0,0,0,1,1,1) %*% t(test_fd$coefs[,select_coefs])
  new_fd$coefs[,which((1234*j + i) == use_pixels)] <- temp_coef

}

bb <- c(101, 999, 101, 999)
for(i in bb[1]:bb[2]) for(j in bb[3]:bb[4]) {
  select_coefs <- c()
  for(y in (j-1):(j+1)) {for(x in (i-1):(i+1)){ select_coefs <- append(select_coefs, which((1234*y + x) == use_pixels))}}
  temp_coef <- c(-1,-1,-1,0,0,0,1,1,1) %*% t(test_fd$coefs[,select_coefs])
  new_fd$coefs[,which((1234*j + i) == use_pixels)] <- temp_coef
  
}
# pixels_filter <- expand.grid(x=2:899, y=2:899)
# pixels_univariate <- apply(pixels_filter, 1, paste, collapse= "_")
len_upix <- length(use_pixels)
pixel_wrapper <- function(upix) {
  #a <- (upix-1):(upix+1)
  #select_coefs <-  c(a + 900, a, a-900)
  #if(!(any(select_coefs < 1 | select_coefs > len_upix))) {
    #print(select_coefs)
    #print(use_pixels[select_coefs])
    try(tcrossprod(c(-1,-1,-1,0,0,0,1,1,1), 
                   test_fd$coefs[,c((upix-1):(upix+1) + 900, 
                                    (upix-1):(upix+1), 
                                    (upix-1):(upix+1)-900)]),
        silent = T)
    #print(temp_coef)
    #print(test_fd$coefs[use_pixels[select_coefs]])
    #stop()
    #new_fd$coefs[,upix] <- temp_coef
  #}
}
upix_vec <- 1:length(use_pixels)
upix_vec <- upix_vec[upix_vec > 851 & upix_vec %% 851 != 0 & upix_vec < 851*850 & upix_vec%%851 != 1]
# remove the invalid indeces before apply
new_coef <- apply(as.array(upix_vec), 1, FUN = function(x) tcrossprod(c(-1,-1,-1,0,0,0,1,1,1), 
                                                              test_fd$coefs[,c(((x-1):(x+1))-851, 
                                                                               (x-1):(x+1), 
                                                                               ((x-1):(x+1))+851)]))
new_fd$coefs[,upix_vec] <- new_coef
#new_coef_format <- matrix(unlist(new_coef[sapply(new_coef, function(x) !inherits(x, "try-error"))]), nrow = 241)
#edge_coef <- c(-1,-1,-1,0,0,0,1,1,1) %*% t(test_fd$coefs)
#new_fd <- test_fd
#new_fd$coefs[,5] <- edge_coef
#plot(new_fd, col = c(1,1,1,1,2,1,1,1,1))

### NEW STUFF
# Look back up a change of basis and what we can do with it
# t(test_fd$coefs) %*% t(eval_b) = estimation of original values of X
# chol(cross_b) %*% t(eval_b) %*% eval_b ~ I (but pretty far off)
eval_b <- eval.basis(seq(1,314, length.out=314), basis)
cross_b <- crossprod(eval_b) # t(theta) %*% theta
# cross_b %*% test_fd$coefs = t(eval_b) %*% t(use_data) how to go from this to insert filters
# inv_b <- chol(eval_b) # (maybe change the basis of the filter?)
###
##
# also try the edge finding


fd_to_pixels(new_fd, save_dir = "Example_Large_FB_deriv1_edge", image_ref = "Example-BlackForwardBack Images/image", pixels_modified = use_pixels, scale = F)

av::av_encode_video(paste0("Example_Large_FB_deriv1_edge/", list.files("Example_Large_FB_deriv1_edge/")),
                    framerate = 30,
                    output = 'example_large_FB_deriv1_edge.mp4')

######
# next do the secong derivative

basis <- create.bspline.basis(rangeval = c(1,314),
                              nbasis = 241) # play around with nbasis
use_pixels <- c()
# use_pixels <- c(1234*780 + 600, 1234*781+600, 1234*782 + 600,
#                 1234*780 + 601, 1234*781+601, 1234*782 + 601,
#                 1234*780 + 602, 1234*781+602, 1234*782 + 602)
# small section
for(j in 599:621) for(i in 779:801)  use_pixels <- append(use_pixels, 1234*i + j)
# for(j in 100:1000) use_pixels <- append(use_pixels, 1234*(100:1000) + j)
use_pixels <- as.vector(apply(as.array(100:1000), 1, FUN=function(x) 1234*(100:1000) + x))
#use_pixels <- c()
#for(i in c(1, 2, 9, 10, 11, 19, 20, 21, 29, 30)) use_pixels <- append(use_pixels, (600:800)*1234+(585+i))
#use_pixels <- c()
#use_pixels <- (600:800)*1234+(585+20)
use_data <- data_filtered[use_pixels,]
test_fd_pre <- smooth.basis(argvals = seq(1,314, length.out=314),
                        y = t(use_data),
                        fdParobj = basis)$fd
# grab pixels in sections of nine around a particular one
new_fd_pre <- test_fd_pre

fd_to_pixels(new_fd_pre, save_dir = "Example_Large_Pre_Edge", image_ref = "Example-AdjustedVideo Images/image", pixels_modified = use_pixels, scale = F)

av::av_encode_video(paste0("Example-Wind Fitted/", list.files("Example-Wind Fitted/")),
                    framerate = 30,
                    output = 'example_wind_fitted.mp4')


##### FOR SETTING UP GCV COMPARISON
results <- list()


#ratios <- c(0.7, 0.75, 0.8, 0.85, 0.9, 0.95) # depending on how it goes (smaller?)
ratios <- c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35) # depending on how it goes (smaller?)
filename <- "Example-BlackForwardBack.mp4"
filedir <- c("Example-BlackForwardBack Images fps15", 
             "Example-BlackForwardBack Images", 
             "Example-BlackForwardBack Images fps60",
             "Example-AdjustedVideo Images fps15", 
             "Example-AdjustedVideo Images", 
             "Example-AdjustedVideo Images fps60")
n_images <- c(155, 309, 619, 157, 314, 628) # for FB to adjusted
for(i in 4:length(filedir)) {
  nimg <- n_images[i]
  fdir <- filedir[i]
  print(fdir)
  full_img <- read_pixels(fdir, "all", filter = c(), n_images = nimg)
  # results[[fdir]] <- list()
  use_pixels <- as.vector(apply(as.array(600:650), 1, FUN=function(x) full_img$dim[1]*(550:600) + x))
  for(rval in ratios) {
    print(rval)
    basis <- create.bspline.basis(rangeval = c(1, nimg),
                                  nbasis = ceiling(nimg*rval))
    test_fd <- smooth.basis(argvals = seq(1,nimg, length.out=nimg),
                            y = t(full_img$vd[use_pixels,]),
                            fdParobj = basis)
    results[[fdir]][[paste0(rval)]] <- colSums(as.matrix(test_fd$gcv))
  }
}




# frame_rate <- c(15, 30, 60)

# set up the fps, we want 15, 30, 60
av::av_video_images("Example-Wind.mp4", destdir = "Example-Wind Images",
                    format = "pgm", fps = 30) # returns a list of paths to all images


# nimages will come from av_video_images
# nbasis should be a ratio to the number of frames relative to framerate
# try 70%, 75%, 80%, 85%, 90%, 95% 
#  (eg. for 314 frames 220, 236, 251, 267, 283, 299 then narrow if need be)
#  we need to test these ratios for a few videos, start with side-side and FB
#  then move onto maybe more applied stuff and even maybe a static (high change)
#  for the hardest scenario possible (maybe 0-1 every frame?)

# i don't know, find or save it

use_pixels <- as.vector(apply(as.array(200:800), 1, FUN=function(x) 1032*(200:800) + x))

full_wind <- read_pixels("Example-Wind Images", "all", filter = c(), n_images = 309)
basis <- create.bspline.basis(rangeval = c(1, 309),
                              nbasis = ceiling(309*0.3)) # play around with nbasis
# we should be able to get gcv after fitting to check fit in all cases
# this value will be used to evaluate the best ratio in general
wind_fd <- smooth.basis(argvals = seq(1,309, length.out=309),
                        y = t(full_wind$vd),
                        fdParobj = basis)$fd

wind_deriv <- deriv.fd(wind_fd)
fd_to_pixels(wind_deriv, save_dir = "Example-Wind 30p Deriv", image_ref = "Example-Wind Images/image", 
             pixels_modified = 1:prod(full_wind$dim), scale = F)
av::av_encode_video(paste0("Example-Wind 30p Deriv/", list.files("Example-Wind 30p Deriv/")),
                    framerate = 30,
                    output = 'example_wind_30p_deriv.mp4')
# check and store gcv for the given ratio, framerate, image group
# maybe try to do in parrallel with parlapply or at least print where you are
#  and the gcv value
