#loading images

library(png)


land <- readPNG(source = "start.png",native = T,info =F)
land_mask <- readPNG(source = "start_mask.png",native = T,info =F)
char <- readPNG(source = "char.png",native = T,info =F)

xxxx <- land


#Set up the plot area
plot(1:58, type='n', main="", xlab="", ylab="", axes=F)

#Get the plot information so the image will fill the plot box, and draw it
lim <- par()
rasterImage(land, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4],interpolate = F)
xxxx[27:31,27:36] <- char
rasterImage(xxxx, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4],interpolate = F)
xxxx <- land

grid(nx = 58,ny = 58)


str(char)

if(land_mask[23:27,25:34]!=1) paste("You cannot go there!")

