### 1

## 建立迴圈函數
Fibonacci_loop <- function(input){
    if(input <= 2){
        return(1)
    }else{
        sum1 <- 1
        sum2 <- 1
        for(ii in 3:input){
            out <- sum1 + sum2
            if(ii %% 2 == 0){
                sum1 <- out
            }else{
                sum2 <- out
            }
        }
        return(out)
    }
}

## 建立遞迴函數
Fibonacci <- function(input){
    if(input <= 2){
        return(1)
    }else{
        return(Fibonacci(input - 1) + Fibonacci(input - 2))
    }
}

## 紀錄遞迴時間
exe_time <- c()

for(ii in c(10,20,30)){
    Start_time <- Sys.time()
    Fibonacci(ii)
    exe_time[ii/10] <- as.numeric(Sys.time() - Start_time)
}

exe_time <- matrix(exe_time,3,1)
colnames(exe_time) <- "execute_time_遞迴"
row.names(exe_time) <- c("F(10)","F(20)","F(30)")
exe_time

## 記錄迴圈時間
exe_time <- c()

for(ii in c(10000,20000,30000)){
    Start_time <- Sys.time()
    Fibonacci_loop(ii)
    (Fibonacci_loop(ii))
    exe_time[ii/10000] <- as.numeric(Sys.time() - Start_time)
}
exe_time<-matrix(exe_time,3,1)
colnames(exe_time)<-"execute_time_迴圈"
row.names(exe_time)<-c("F(10000)","F(20000)","F(30000)")
exe_time

### 2
#selection sort

s<-c()

selectionsort2 <- function(input)
{
    for(i in 1:length(input))
    {
        aa<-which.min(input)
        min<-input[aa]
        input <- input[-aa]
        s<-c(s,min)
    }
    return(s)
}

#bubble sort.
bubblesort <- function(input)
{
    n<-length(input) #設定出序列的長度
    for(i in 1:(n-1)) #設定跑幾次1~n-1的迴圈(最後一個不會再有右邊的可以交換)
    {
        for(k in 1:(n-i)) #設定每跑1~n-1次迴圈時內部要跑的次數
        {
            if(input[k]>input[k+1]) #設定交換條件(如果左大於右則交換)
            {
                bigger<-input[k]#將要交換的較大一方(左邊)的值設定到bigger這個變數
                smaller<-input[k+1]#將要交換的較小一方(右邊)的值設定到smaller這個變數
                input[k+1]<-bigger  #將原先較大的值輸入到右邊
                input[k]<-smaller   #將原先較小的值輸入到左邊
                #因此已完成左>右的話，的左右交換。
            }
        }
    }
    return(input)
}

#創建一個隨機數列測試
c <- runif(20,10,100)
selectionsort2(c)
bubblesort(c)


### 3
test<-read.csv(file.choose())

#selection sort
selectiondatatime<-c(rep(0,5))
for (i in 1:5)
{
    Start_time<-Sys.time()
    selectionsort2(test[,i])
    selectiondatatime[i]<-as.numeric(Sys.time()-Start_time)
}

#bubble sort
bubblesorttime<-c(rep(0,5))
for (i in 3:5)
{
    Start_time<-Sys.time()
    bubblesort(test[,i])
    bubblesorttime[i]<-as.numeric(Sys.time()-Start_time)
}

#因為bubble sort會詳細比較兩個位置的大小，不像sort是找每次的最小值，因此有NA時無法跑。

kk<-test[1:50,1]
kkk<-test[1:500,2]

Start_time<-Sys.time()
bubblesort(kk)
a1<-as.numeric(Sys.time()-Start_time)

bubblesorttime[1]<-a1


Start_time<-Sys.time()
bubblesort(kkk)
a2<-as.numeric(Sys.time()-Start_time)

bubblesorttime[2]<-a2

selectiondatatime
bubblesorttime
