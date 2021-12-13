##1

N_size <- 30
N_bit <- 8
Gen <- 30
Population <- matrix(sample(c(0,1), size = N_size*N_bit, replace = T), N_size, N_bit)
mut_freq <- 0.05

Po_value <- 8*Population[,1] + 4*Population[,2] + 2*Population[,3] + 1*
    Population[,4] + 1/2*Population[,5] + 1/4*Population[,6] +1/8*Population[,7] + 1/16*Population[,8]
Po_value <- Po_value - 8

fit <- function(x){
    2*x^3 - 25*x^2 + 18*x + 3000 - x*sin(x)
}
fit_value <- fit(Po_value)

idx <- 1:N_size
max_Gen_value <- rep(0, Gen)
max_Gen_value[1] <- max(fit_value)
max_Gen_ind <- matrix(0, Gen, N_bit)
max_Gen_ind[1,] <- Population[which(fit_value == max(fit_value))[1],]
max_Gen_Po_value <- rep(0, Gen)
max_Gen_Po_value[1] <- Po_value[which(fit_value == max(fit_value))[1]]

for(now_Gen in 2:Gen){
    child <- matrix(0, N_size, N_bit)
    child[1,] <- Population[which(fit_value == max(fit_value))[1],]
    switch_bit <- sample(N_bit, 1)
    child[2,] <- child[1,]
    child[2, switch_bit:N_bit] <- !child[2, switch_bit:N_bit]
    child_size <- 2
    total_wheel <- sum(fit_value)
    select_frequency <- fit_value/total_wheel
    
    while(child_size < N_size){
        P_idx <- sample(idx, size = 2, replace = F, prob = select_frequency)
        P1 <- Population[P_idx[1],]
        P2 <- Population[P_idx[2],]
        
        switch_bit <- sample(N_bit, 2)
        switch_bit <- sort(switch_bit)
        P1_new <- P1
        P1_new[switch_bit[1]:switch_bit[2]] <- P2[switch_bit[1]:switch_bit[2]]
        P2_new <- P2
        P2_new[switch_bit[1]:switch_bit[2]] <- P1[switch_bit[1]:switch_bit[2]]
        if(runif(1, 0, 1) < mut_freq){
            tar_bit <- sample(N_bit,1)
            P1_new[tar_bit] <- !P1_new[tar_bit]
        }
        if(runif(1, 0, 1) < mut_freq){
            tar_bit <- sample(N_bit,1)
            P2_new[tar_bit] <- !P2_new[tar_bit]
        }
        child[child_size + 1,] <- P1_new
        child[child_size + 2,] <- P2_new
        child_size <- child_size + 2
    }
    
    Population <- child
    Po_value <- 8*Population[,1] + 4*Population[,2] + 2*Population[,3] + 1*Population[,4] + 1/2*Population[,5] + 1/4*Population[,6] +1/8*Population[,7] + 1/16*Population[,8]
    Po_value <- Po_value - 8
    fit_value <- fit(Po_value)
    max_Gen_value[now_Gen] <- max(fit_value)
    max_Gen_Po_value[now_Gen] <- Po_value[which(fit_value == max(fit_value))[1]]
    max_Gen_ind[now_Gen,] <- Population[which(fit_value == max(fit_value))[1],]
}

max(max_Gen_value)

##2
ans1000 <- c()

for(i in 1:1000){
    N_size <- 30
    N_bit <- 8
    Gen <- 30
    Population <- matrix(sample(c(0,1), size = N_size*N_bit, replace = T), N_size, N_bit)
    mut_freq <- 0.05
    
    Po_value <- 8*Population[,1] + 4*Population[,2] + 2*Population[,3] + 1*
        Population[,4] + 1/2*Population[,5] + 1/4*Population[,6] +1/8*Population[,7] + 1/16*Population[,8]
    Po_value <- Po_value - 8
    
    fit <- function(x){
        2*x^3 - 25*x^2 + 18*x + 3000 - x*sin(x)
    }
    fit_value <- fit(Po_value)
    
    idx <- 1:N_size
    max_Gen_value <- rep(0, Gen)
    max_Gen_value[1] <- max(fit_value)
    max_Gen_ind <- matrix(0, Gen, N_bit)
    max_Gen_ind[1,] <- Population[which(fit_value == max(fit_value))[1],]
    max_Gen_Po_value <- rep(0, Gen)
    max_Gen_Po_value[1] <- Po_value[which(fit_value == max(fit_value))[1]]
    
    for(now_Gen in 2:Gen){
        child <- matrix(0, N_size, N_bit)
        child[1,] <- Population[which(fit_value == max(fit_value))[1],]
        switch_bit <- sample(N_bit, 1)
        child[2,] <- child[1,]
        child[2, switch_bit:N_bit] <- !child[2, switch_bit:N_bit]
        child_size <- 2
        total_wheel <- sum(fit_value)
        select_frequency <- fit_value/total_wheel
        
        while(child_size < N_size){
            P_idx <- sample(idx, size = 2, replace = F, prob = select_frequency)
            P1 <- Population[P_idx[1],]
            P2 <- Population[P_idx[2],]
            
            switch_bit <- sample(N_bit, 2)
            switch_bit <- sort(switch_bit)
            P1_new <- P1
            P1_new[switch_bit[1]:switch_bit[2]] <- P2[switch_bit[1]:switch_bit[2]]
            P2_new <- P2
            P2_new[switch_bit[1]:switch_bit[2]] <- P1[switch_bit[1]:switch_bit[2]]
            if(runif(1, 0, 1) < mut_freq){
                tar_bit <- sample(N_bit,1)
                P1_new[tar_bit] <- !P1_new[tar_bit]
            }
            if(runif(1, 0, 1) < mut_freq){
                tar_bit <- sample(N_bit,1)
                P2_new[tar_bit] <- !P2_new[tar_bit]
            }
            child[child_size + 1,] <- P1_new
            child[child_size + 2,] <- P2_new
            child_size <- child_size + 2
        }
        
        Population <- child
        Po_value <- 8*Population[,1] + 4*Population[,2] + 2*Population[,3] + 1*Population[,4] + 1/2*Population[,5] + 1/4*Population[,6] +1/8*Population[,7] + 1/16*Population[,8]
        Po_value <- Po_value - 8
        fit_value <- fit(Po_value)
        max_Gen_value[now_Gen] <- max(fit_value)
        max_Gen_Po_value[now_Gen] <- Po_value[which(fit_value == max(fit_value))[1]]
        max_Gen_ind[now_Gen,] <- Population[which(fit_value == max(fit_value))[1],]
    }
    
    ans1000[i] <- max(max_Gen_value)
}

plot(ans1000, type="l", xlab = "index", ylab = "max value")

##3
size <- c(10, 30, 50)
repeat_max <- list()

for(i in 1:3){
    N_size <- size[i]
    N_bit <- 8
    Gen <- 30
    Population <- matrix(sample(c(0,1), size = N_size*N_bit, replace = T), N_size, N_bit)
    mut_freq <- 0.05
    
    Po_value <- 8*Population[,1] + 4*Population[,2] + 2*Population[,3] + 1*
        Population[,4] + 1/2*Population[,5] + 1/4*Population[,6] +1/8*Population[,7] + 1/16*Population[,8]
    Po_value <- Po_value - 8
    
    fit <- function(x){
        2*x^3 - 25*x^2 + 18*x + 3000 - x*sin(x)
    }
    fit_value <- fit(Po_value)
    
    idx <- 1:N_size
    max_Gen_value <- rep(0, Gen)
    max_Gen_value[1] <- max(fit_value)
    max_Gen_ind <- matrix(0, Gen, N_bit)
    max_Gen_ind[1,] <- Population[which(fit_value == max(fit_value))[1],]
    max_Gen_Po_value <- rep(0, Gen)
    max_Gen_Po_value[1] <- Po_value[which(fit_value == max(fit_value))[1]]
    
    for(now_Gen in 2:Gen){
        child <- matrix(0, N_size, N_bit)
        child[1,] <- Population[which(fit_value == max(fit_value))[1],]
        switch_bit <- sample(N_bit, 1)
        child[2,] <- child[1,]
        child[2, switch_bit:N_bit] <- !child[2, switch_bit:N_bit]
        child_size <- 2
        total_wheel <- sum(fit_value)
        select_frequency <- fit_value/total_wheel
        
        while(child_size < N_size){
            P_idx <- sample(idx, size = 2, replace = F, prob = select_frequency)
            P1 <- Population[P_idx[1],]
            P2 <- Population[P_idx[2],]
            
            switch_bit <- sample(N_bit, 2)
            switch_bit <- sort(switch_bit)
            P1_new <- P1
            P1_new[switch_bit[1]:switch_bit[2]] <- P2[switch_bit[1]:switch_bit[2]]
            P2_new <- P2
            P2_new[switch_bit[1]:switch_bit[2]] <- P1[switch_bit[1]:switch_bit[2]]
            if(runif(1, 0, 1) < mut_freq){
                tar_bit <- sample(N_bit,1)
                P1_new[tar_bit] <- !P1_new[tar_bit]
            }
            if(runif(1, 0, 1) < mut_freq){
                tar_bit <- sample(N_bit,1)
                P2_new[tar_bit] <- !P2_new[tar_bit]
            }
            child[child_size + 1,] <- P1_new
            child[child_size + 2,] <- P2_new
            child_size <- child_size + 2
        }
        
        Population <- child
        Po_value <- 8*Population[,1] + 4*Population[,2] + 2*Population[,3] + 1*Population[,4] + 1/2*Population[,5] + 1/4*Population[,6] +1/8*Population[,7] + 1/16*Population[,8]
        Po_value <- Po_value - 8
        fit_value <- fit(Po_value)
        max_Gen_value[now_Gen] <- max(fit_value)
        max_Gen_Po_value[now_Gen] <- Po_value[which(fit_value == max(fit_value))[1]]
        max_Gen_ind[now_Gen,] <- Population[which(fit_value == max(fit_value))[1],]
    }
    repeat_max[[i]] <- max_Gen_value
}

plot(repeat_max[[1]], type = "l", col = 1, ylab = "max value")
lines(repeat_max[[2]], type = "l", col = 2)
lines(repeat_max[[3]], type = "l", col = 3)
legend("bottomright",lty = 1, col = c(1,2,3), legend = c("size = 10", "size = 30", "size = 50"))


##4
Gen_set <- c(10, 30, 50)
N_size <- 30
N_bit <- 8
repeat_max <- list()
for(k in 1:3){
    Gen <- Gen_set[k]
    Population <- matrix(sample(c(0,1), size = N_size*N_bit, replace = T),N_size, N_bit)
    mut_freq <- 0.05

    Po_value <- 8*Population[,1] + 4*Population[,2] + 2*Population[,3] +1*Population[,4] + 1/2*Population[,5] + 1/4*Population[,6] +1/8*Population[,7] + 1/16*Population[,8]
    Po_value <- Po_value - 8

    fit <- function(x){
        2*x^3 - 25*x^2 + 18*x + 3000 - x*sin(x)
    }
    fit_value <- fit(Po_value)
    
    idx <- 1:N_size
    max_Gen_value <- rep(0, Gen)
    max_Gen_value[1] <- max(fit_value)
    max_Gen_ind <- matrix(0, Gen, N_bit)
    max_Gen_ind[1,] <- Population[which(fit_value == max(fit_value))[1],]
    max_Gen_Po_value <- rep(0, Gen)
    max_Gen_Po_value[1] <- Po_value[which(fit_value == max(fit_value))[1]
                                    ]
    
    for(now_Gen in 2:Gen){
        child <- matrix(0, N_size, N_bit)
        child[1,] <- Population[which(fit_value == max(fit_value))[1],]
        switch_bit <- sample(N_bit, 1)
        child[2,] <- child[1,]
        child[2, switch_bit:N_bit] <- !child[2, switch_bit:N_bit]
        child_size <- 2
        total_wheel <- sum(fit_value)
        select_frequency <- fit_value/total_wheel
        
        while(child_size < N_size){
            P_idx <- sample(idx, size = 2, replace = F, prob = select_frequency)
            P1 <- Population[P_idx[1],]
            P2 <- Population[P_idx[2],]
            
            switch_bit <- sample(N_bit, 2)
            switch_bit <- sort(switch_bit)
            P1_new <- P1
            P1_new[switch_bit[1]:switch_bit[2]] <- P2[switch_bit[1]:switch_bit[2]]
            P2_new <- P2
            P2_new[switch_bit[1]:switch_bit[2]] <- P1[switch_bit[1]:switch_bit[2]]
            if(runif(1, 0, 1) < mut_freq){
                tar_bit <- sample(N_bit,1)
                P1_new[tar_bit] <- !P1_new[tar_bit]
            }
            if(runif(1, 0, 1) < mut_freq){
                tar_bit <- sample(N_bit,1)
                P2_new[tar_bit] <- !P2_new[tar_bit]
            }
            child[child_size + 1,] <- P1_new
            child[child_size + 2,] <- P2_new
            child_size <- child_size + 2
        }

        Population <- child
        Po_value <- 8*Population[,1] + 4*Population[,2] + 2*Population[,3] + 1*Population[,4] + 1/2*Population[,5] + 1/4*Population[,6] +1/8*Population[,7] + 1/16*Population[,8]
        Po_value <- Po_value - 8
        fit_value <- fit(Po_value)
        max_Gen_value[now_Gen] <- max(fit_value)
        max_Gen_Po_value[now_Gen] <- Po_value[which(fit_value == max(fit_value))[1]]
        max_Gen_ind[now_Gen,] <- Population[which(fit_value == max(fit_value))[1],]
    }
    repeat_max[[k]] <- max_Gen_value
}
par(mfcol=c(1,3))
plot(repeat_max[[1]], type = "l", col = 1, main = "Generation = 10",
     xlab = "generation", ylab = "max value")
plot(repeat_max[[2]], type = "l", col = 2, main = "Generation = 30",
     xlab = "generation", ylab = "max value")
plot(repeat_max[[3]], type = "l", col = 3, main = "Generation = 50",
     xlab = "generation", ylab = "max value")


##5
N_size <- 30
N_bit <- 8
Gen <- 30
mut_freq_set <- c(0.05, 0.25, 0.5)
repeat_max <- list()
for(k in 1:3){
    mut_freq <- mut_freq_set[i]
    Population <- matrix(sample(c(0,1), size = N_size*N_bit, replace = T), N_size, N_bit)

    Po_value <- 8*Population[,1] + 4*Population[,2] + 2*Population[,3] + 1*Population[,4] + 1/2*Population[,5] + 1/4*Population[,6] +1/8*Population[,7] + 1/16*Population[,8]
    Po_value <- Po_value - 8

    fit <- function(x){
        2*x^3 - 25*x^2 + 18*x + 3000 - x*sin(x)
    }
    fit_value <- fit(Po_value)
    
    idx <- 1:N_size
    max_Gen_value <- rep(0, Gen)
    max_Gen_value[1] <- max(fit_value)
    max_Gen_ind <- matrix(0, Gen, N_bit)
    max_Gen_ind[1,] <- Population[which(fit_value == max(fit_value))[1],]
    max_Gen_Po_value <- rep(0, Gen)
    max_Gen_Po_value[1] <- Po_value[which(fit_value == max(fit_value))[1]
                                    ]
    
    for(now_Gen in 2:Gen){
        child <- matrix(0, N_size, N_bit)
        child[1,] <- Population[which(fit_value == max(fit_value))[1],]
        switch_bit <- sample(N_bit, 1)
        child[2,] <- child[1,]
        child[2, switch_bit:N_bit] <- !child[2, switch_bit:N_bit]
        child_size <- 2
        total_wheel <- sum(fit_value)
        select_frequency <- fit_value/total_wheel
        
        while(child_size < N_size){
            P_idx <- sample(idx, size = 2, replace = F, prob = select_frequency)
            P1 <- Population[P_idx[1],]
            P2 <- Population[P_idx[2],]

            switch_bit <- sample(N_bit, 2)
            switch_bit <- sort(switch_bit)
            P1_new <- P1
            P1_new[switch_bit[1]:switch_bit[2]] <- P2[switch_bit[1]:switch_bit[2]]
            P2_new <- P2
            P2_new[switch_bit[1]:switch_bit[2]] <- P1[switch_bit[1]:switch_bit[2]]
            if(runif(1, 0, 1) < mut_freq){
                tar_bit <- sample(N_bit,1)
                P1_new[tar_bit] <- !P1_new[tar_bit]
            }
            if(runif(1, 0, 1) < mut_freq){
                tar_bit <- sample(N_bit,1)
                P2_new[tar_bit] <- !P2_new[tar_bit]
            }
            child[child_size + 1,] <- P1_new
            child[child_size + 2,] <- P2_new
            child_size <- child_size + 2
        }

        Population <- child
        Po_value <- 8*Population[,1] + 4*Population[,2] + 2*Population[,3] + 1*Population[,4] + 1/2*Population[,5] + 1/4*Population[,6] +1/8*Population[,7] + 1/16*Population[,8]
        Po_value <- Po_value - 8
        fit_value <- fit(Po_value)
        max_Gen_value[now_Gen] <- max(fit_value)
        max_Gen_Po_value[now_Gen] <- Po_value[which(fit_value == max(fit_value))[1]]
        max_Gen_ind[now_Gen,] <- Population[which(fit_value == max(fit_value))[1],]
    }
    repeat_max[[k]] <- max_Gen_value
}
plot(repeat_max[[1]], type = "l", col = 1, xlab = "generation", ylab = "max value", main = "different mutation")
lines(repeat_max[[2]], type = "l", col = 2)
lines(repeat_max[[3]], type = "l", col = 3)
legend("bottomright",lty = 1, col = c(1,2,3), legend = c("mutation = 5%", "mutation = 25%", "mutation = 50%"))
