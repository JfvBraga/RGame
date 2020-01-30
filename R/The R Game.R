# Joao Braga (2015)
rg <- function(){
  #Dependencies
  rotate <- function(x) t(apply(x, 2, rev))
  library(beepr)
  
  #custom images
  #1 Knigth
  knight <- c(0,0,0,0,0,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,3,3,3,3,3,0,0,0,0,0,0,0,1,1,1,1,1,3,3,3,3,3,3,1,1,1,1,1,1
              ,1,3,1,1,1,3,3,3,3,3,1,1,1,0,0,0,0,1,3,1,1,1,3,3,3,3,1,1,1,1,0,0,0,0,1,3,1,1,1,3,3,3,1,1,1,1,1,0,0,0,0,1,3,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0
              ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,3,1,0,0,0,0,0,0,0,0,0,0,3,3,3,3,3,3,1,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,1,0,0,0,0,0,0,0,0,0,0)
  knight <- matrix(knight,nrow = 17,ncol = 12)
  
  #IntroS
  readline("Welcome to Riel! (press enter to continue)")
  land_init <-matrix(sample(c(1,2,3),size = 30*30,replace = T),30,30)
  #image(land_init, axes = F, col=c("green","grey", "blue"))
  beep(sound = 8)
  image(rotate(knight),col=c("white","red","yellow","black"),axes=F)
  name <- readline("Name yourself! : ")
 
  #starting position
  x <- 15
  y <- 15
  
  land_cur <- land_init
  
  land_cur[x,y] <- 4
  image(land_cur[11:20,11:20], axes = F, col=c("green","grey","blue","black"))
  #grid(nx=10, ny=10, lty=1, col="black")
  enemies <- matrix(sample(x = c(0,1),size = 30*30,replace = T),30,30)
  life <- 3
  run <- 1
  
  print(paste(name,",you just woke up and you do not have any idea of how you got here.",name,",you better keep moving, there are monsters on these lands!"))
  
  while(run== 1){
    
    if(land_init[x,y]==1) print(paste(name,"you are in a forest"))
    if(land_init[x,y]==2) print(paste(name,"you are standing in the middle of a road"))
    if(land_init[x,y]==3) print(paste(name,"you are in a pond"))
    
    xx <- readline("Do you want to move left (-1), rigth (1) or none (0) ?")
    yy <- readline("Do you want to move down (-1), up (1) or none (0) ?")
    
    xx <- as.numeric(unlist(strsplit(xx, ",")))
    yy <- as.numeric(unlist(strsplit(yy, ",")))
    
    land <- land_init
    
    x <- x+xx
    y <- y+yy
    
    land[x,y] <- 4
    
    # par(mfrow = c(2,1))
    image(land[11:20,11:20], axes = F, col=c("green","grey","blue","black"))
    # plot(y=rep(1,times = life),x=seq(1,life,1),pch="♥",cex=5, axes=F,main=name,ylab="HP")
    
    
    # grid(nx=10, ny=10, lty=1, col="black")
    
    while(enemies[x,y]!=0) {
      print("An enemy spotted you! You must FIGHT!!!")
      # par(mfrow = c(2,1))
      # image(land[11:20,11:20], axes = F, col=c("green","grey","blue","black"))
      # plot(y=rep(1,times = life),x=seq(1,life,1),pch="♥",cex=5,col="red", axes=F,main=name,ylab="HP",xlab="")
      image(rotate(knight),col=c("white","red","yellow","black"),axes=F)
      
      if(enemies[x,y]!=0) {
        
        dice <- readline("Press enter to roll the dice!")
        dice <- sample(x = c(1:6),size = 1,replace = FALSE)
        print(paste("You rolled:",dice,"."))
        
        enemyroll <- sample(x = c(1:6),size = 1,replace = FALSE)
        
        print(paste("The enemy rolled", enemyroll,"."))
        
    
        if(dice>=enemyroll) {
          enemies[x,y] <- 0
          beep(sound = 3)
          readline(paste(name,", you won! Continue your travels accross Riel (Press enter to continue)", sep=""))
          enemies[x,y] <- 0  
          # par(mfrow = c(2,1))
          image(land[11:20,11:20], axes = F, col=c("green","grey","blue","black"))
        }
        
        if(dice< enemyroll) {
          beep(sound = 9)
          readline(paste(name,", you missed! Try again! (Press enter to continue)", sep=""))
          life <- life -1
          
        }
        if(life == 0) {
          readline(paste(name,"you are dead (Press enter to exit)", sep=", "))
          stop()
        }
      }
    }	 
    
  } 
}

# ♥
# ♡