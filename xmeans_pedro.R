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
plot(data)

#Create new children from a centroid
child <- function(center, data){
  n <- nrow(data)
  center_1 <- data[sample(1:n, size = 1),]
  center_2 <- (center_1-center)*(-1) + center
  return(rbind(center_1,center_2))
}

#Improve-Params
kmeans_pedro <- function(data, K, centers_provided = NULL, only_last = T){
  centers <- list()
  clusters <- list()
  
  for(k in 2:10000){
    
    if(k == 2){
      if(is.null(centers_provided)){
        s <- sample(1:nrow(data), size = K)
        centers[[1]] <- data[s,]
      }else{
        centers[[1]] <- centers_provided
        K <- nrow(centers_provided)
      }
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
      if(only_last==T){
        return(list(center = centers[[k]], cluster = clusters[[k]], data = data))
      }else{
        return(list(center = centers, cluster = clusters, data = data))
      }
      
    }
    
  }
  if(only_last==T){
    return(list(center = centers[[k]], cluster = clusters[[k]], data = data))
  }else{
    return(list(center = centers, cluster = clusters, data = data))
  }
}

#Calculating the BIC
bic <- function(center = model$center, cluster = model$cluster, data = data){
  K <- length(unique(cluster))
  M <- length(as.vector(center[1,]))
  p <- K-1 + M*K +1
  R <- nrow(data)
  
  ri <- as.vector(table(cluster))
  
  s2 <- sum((data-center[cluster,])^2)/(R-K)
  
  ll <-  R*log(1/(sqrt(2*pi*s2^M)))-((R-K)/(2)) + sum(log(ri/R))
  
  return(ll-p/2*log(R))
}



#The actual X-Means model
xmeans_pedro <- function(kmin,kmax, data){ #1)Select kmin and kmax
  #2)K=Kmin
  k <- kmin
  
  bic_k <- NULL
  model_k <- list()
  h=1 #counter
  
  #3) Improve-params
  model <- kmeans_pedro(data, K = k, only_last = T)
  bic_k <- rbind(bic_k, c(bic(center = model$center, cluster = model$cluster, data = data),k))
  model_k[[h]] <- model
  
  while(k<kmax){
    #4)Improve-structure
    center <- model$center
    cluster <- model$cluster
    
    center_new <- NULL
    for(j in 1:nrow(center)){
      data_aux <- data[(cluster==j),]
      center_aux <- center[j,, drop = F]
      bic_k1 <- bic(center = center_aux, cluster = rep(1, times = nrow(data_aux)), data = data_aux)
      
      centers_child <- child(center_aux, data_aux)
      model_aux2 <- kmeans_pedro(data_aux, centers_provided = centers_child, only_last = T)
      bic_k2 <- bic(center = model_aux2$center, cluster = model_aux2$cluster, data = data_aux)
      
      if(bic_k1>bic_k2){
        center_new <- rbind(center_new,center_aux)
      }else{
        center_new <- rbind(center_new,centers_child)
      }
    }
    k_old <- k
    k <- nrow(center_new)
    h = h+1 #Updating counter
    
    #3) Improve-params
    model <- kmeans_pedro(data, centers_provided = center_new, only_last = T)
    
    bic_k <- rbind(bic_k, c(bic(center = model$center, cluster = model$cluster, data = data),k))
    model_k[[h]] <- model
    
    if(k_old==k){ #If K is not changing anymore
      break
    }
    
    
  }

  return(model_k[[which.max(bic_k[,1])]])
  
}

plot <- list()
set.seed(124365)
test <- xmeans_pedro(kmin = 1, kmax = 1, data = data)

plot[[1]] <- ggplot(aes(x = x, y = y), data = data) +
  theme_classic() +
  geom_point(aes(color = as.factor(test$cluster))) +
  geom_point(aes(x = x, y = y),data = as.data.frame(test$center),
             size = 3, color = "black") +
  geom_point(aes(x = x, y = y),data = data.frame(x=true[,1], y=true[,2]),
             size = 5, color = "blue", shape = 4) +
  theme(legend.position="none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


set.seed(124365)
test <- xmeans_pedro(kmin = 1, kmax = 2, data = data)

plot[[2]] <- ggplot(aes(x = x, y = y), data = data) +
  theme_classic() +
  geom_point(aes(color = as.factor(test$cluster))) +
  geom_point(aes(x = x, y = y),data = as.data.frame(test$center),
             size = 3, color = "black") +
  geom_point(aes(x = x, y = y),data = data.frame(x=true[,1], y=true[,2]),
             size = 5, color = "blue", shape = 4) +
  theme(legend.position="none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


set.seed(124365)
test <- xmeans_pedro(kmin = 1, kmax = 3, data = data)

plot[[3]] <- ggplot(aes(x = x, y = y), data = data) +
  theme_classic() +
  geom_point(aes(color = as.factor(test$cluster))) +
  geom_point(aes(x = x, y = y),data = as.data.frame(test$center),
             size = 3, color = "black") +
  geom_point(aes(x = x, y = y),data = data.frame(x=true[,1], y=true[,2]),
             size = 5, color = "blue", shape = 4) +
  theme(legend.position="none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
