library(ggplot2)
library(magick)

dir_out <- "C:/Users/phf254/Desktop/images"

set.seed(4578)
n <- 50
mu1 <- c(1,1)
mu2 <- c(1,4)
mu3 <- c(3,7)
mu4 <- c(5,2)
Sigma <- diag(c(0.5,0.5))

true <- rbind(mu1,mu2,mu3,mu4)

g1 <- MASS::mvrnorm(n, mu=mu1, Sigma)
g2 <- MASS::mvrnorm(n, mu=mu2, Sigma)
g3 <- MASS::mvrnorm(n, mu=mu3, Sigma)
g4 <- MASS::mvrnorm(n, mu=mu4, Sigma)

data <- rbind(g1,g2,g3,g4)
data <- data.frame(x = data[,1], y = data[,2])


kmeans_pedro <- function(data, K){
  centers <- list()
  clusters <- list()
  
  for(k in 2:10000){
    if(k == 2){
      s <- sample(1:nrow(data), size = K)
      centers[[1]] <- data[s,]
    }
    
    cluster <- numeric(0)
    for(i in 1:nrow(data)){
      aux <- numeric(0)
      for(j in 1:nrow(centers[[(k-1)]])){
        aux[j] <- norm((data[i,]-(centers[[(k-1)]])[j,]), type = "2")
      }
      cluster[i] <- which.min(aux)
    }
    clusters[[k]] <- cluster
    
    center <- NULL
    for(j in 1:nrow(centers[[(k-1)]])){
      aux <- cluster==j
      data_aux <- data[aux,]
      
      centers_aux <- apply(data_aux, 2, mean)
      
      center <- rbind(center, centers_aux)
    }
    centers[[(k)]] <- center
    
    if(isTRUE(all.equal(centers[[(k)]],centers[[(k-1)]]))){
      return(list(center = centers, cluster = clusters))
    }
    
  }
  return(list(center = centers, cluster = clusters))
}

model <- kmeans_pedro(data, K = 4)

setwd(dir_out)
plots <- list()
for(i in 2:(length(model$cluster))){
data$cluster <- model$cluster[[i]]
centers <- model$center[[i]]

plots[[i]] <- ggplot(aes(x = x, y = y), data = data) +
  theme_classic() +
  geom_point(aes(color = as.factor(cluster))) +
  geom_point(aes(x = x, y = y),data = as.data.frame(centers),
             size = 3, color = "black") +
  geom_point(aes(x = x, y = y),data = data.frame(x=true[,1], y=true[,2]),
             size = 5, color = "blue", shape = 4) +
  theme(legend.position="none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave(plot = plots[[i]], 
       filename = i, 
       device = "png")

}

imgs <- list.files(dir_out, full.names = TRUE)
img_list <- lapply(imgs, image_read)
img_joined <- image_join(img_list)
img_animated <- image_animate(img_joined, fps = 2)

img_animated

image_write(image = img_animated,
            path = "kmeans.gif")
