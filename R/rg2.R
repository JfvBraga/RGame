setwd("~/Dropbox/Private/R game/The R Game/images")

rg2 <- function(){
  # Library
  library(png)
  
  land <- readPNG(source = "start.png",native = T,info =F)
  land_mask <- readPNG(source = "start_mask.png",native = T,info =F)
  char <- readPNG(source = "char.png",native = T,info =F)
  main <- readPNG(source= "main.png",native=T, info=F)
    
  
  #custom images
  #1 Knigth
  knight <- c(0,0,0,0,0,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,3,3,3,3,3,0,0,0,0,0,0,0,1,1,1,1,1,3,3,3,3,3,3,1,1,1,1,1,1
              ,1,3,1,1,1,3,3,3,3,3,1,1,1,0,0,0,0,1,3,1,1,1,3,3,3,3,1,1,1,1,0,0,0,0,1,3,1,1,1,3,3,3,1,1,1,1,1,0,0,0,0,1,3,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0
              ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,3,1,0,0,0,0,0,0,0,0,0,0,3,3,3,3,3,3,1,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,1,0,0,0,0,0,0,0,0,0,0)
  knight <- matrix(knight,nrow = 17,ncol = 12)
  
  #Intro
  readline("Welcome to Riel! (press enter to continue)")
  land_init <-matrix(sample(c(1,2,3),size = 30*30,replace = T),30,30)
  #image(land_init, axes = F, col=c("green","grey", "blue"))
  # image(rotate(knight),col=c("white","red","yellow","black"),axes=F)
  plot(1:58, type='n', main="", xlab="", ylab="", axes=F)
  lim <- par()
  rasterImage(main, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4],interpolate = F)
  name <- readline("Name yourself! : ")

  #starting position
  x1 <- 27
  y1 <- 27
  x2 <- x1+4
  y2 <- y1+9
  
  xxxx <- land
  
  xxxx[x1:x2,y1:y2] <- char

  #Set up the plot area
  plot(1:58, type='n', main="", xlab="", ylab="", axes=F)
  
  #Get the plot information so the image will fill the plot box, and draw it
  lim <- par()
  rasterImage(xxxx, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4],interpolate = F)

  print(paste(name,",you just woke up and you do not have any idea of how you got here.",name,",you better keep moving, there are monsters on these lands!"))
  run <-1
  while(run== 1){
    xxxx <- land 
    
    xx <- readline("Do you want to move left (-1), rigth (1) or none (0) ?")
    yy <- readline("Do you want to move UP (1), DOWN (-1) or none (0) ?")

    xx <- as.numeric(unlist(strsplit(xx, ",")))
    yy <- as.numeric(unlist(strsplit(yy, ",")))
    if(length(xx)==0) xx <- 0
    if(length(yy)==0) yy <- 0
    
    yy <- yy*-1
    
    
        
    x1 <- x1+xx
    y1 <- y1+yy
    x2 <- x1+4
    y2 <- y1+9
    
    xxxx[x1:x2,y1:y2] <- char
    rasterImage(xxxx, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4],interpolate = F)
  }
}


# ♥
# ♡