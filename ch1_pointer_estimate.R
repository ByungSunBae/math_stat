set.seed(1234)
# 비편향 추정량(unbiased estimate) 관련 R code

# 1)
## 앞면/뒷면이 나올 확률이 각각 1/2인 동전 던지기를 16번 시행한 걸과를 통해 
## 난수를 10개 생성후, 평균을 내어보자.
(x <- rbinom(10, 16, 0.5))
mean(x) # 7.9

## 얼마나 적합한지 몬테카를로 기법을 통해 1000번 시뮬레이션을 사용하여 확인해보도록하자.
meanx <- replicate(1000, mean(rbinom(10, 16, 0.5)))
mean(meanx) # 8.0027

meanx <- replicate(1000, mean(rbinom(10, 16, 0.5)))
mean(meanx) # 8.0327
## => 결과를 보면 위에서 추정한 값과 비슷한 것을 볼수 있다.
## 추정값을 과대/과소추정하지 않는다는 것을 알 수 있다.
## 실제로 Binomial(16, 0.5)를 사용하여 실제 평균값을 구해보면 8이다.
## 따라서, 표본평균은 모평균의 불편추정량으로 사용한다.

# 2)
## 모분산의 Bias 계산
## 동전던지기 16번 시행한 결과를 통해 난수 10개 생성 후 모분산인 4와 값을 비교
## 이때, 사용할 추정량은 2가지임.

num_sim <- 1000
ssx <-
  sapply(seq_len(num_sim), function(x) {
    rnd_x <- rbinom(10, 16, 0.5)
    return(sum((rnd_x - mean(rnd_x)) ^ 2))
  })
mean(ssx)

## 1) 편차의 제급합을 10으로 나눈 경우
mean(ssx)/10

## 2) 편차의 제급합을 9으로 나눈 경우
mean(ssx)/9

## => 실제 분산은 4이므로 9로 나눈 경우가 비교적 적합한 추정값임을 알 수 있다.


# 예제 1.2 (2)
library("ggplot2")
f <- function(x){
  sqrt(2/(x-1)) * gamma(x/2) / gamma((x-1)/2)
}

p <- ggplot(data.frame(x=c(2, 50)), aes(x=x))
p + 
  stat_function(fun = f) +
  labs(x = "n", 
       y = expression(
         frac(
           sqrt(2)*phantom(0)*Gamma*bgroup("(", frac(n, 2), ")"), 
           sqrt(n-1)*phantom(0)*Gamma*bgroup("(", frac(n-1, 2), ")")
         ))) +
  geom_hline(yintercept = 1, lty='dashed') + 
  theme_bw()

# 예제 1.3
theta <- seq(-10, 10, 0.2)
n <- 100 # 임의지정

Rdelta1 <- 1/n
Rdelta2 <- (16/25)*(1/n) + (theta^2)/25
Rdelta3 <- (1 - theta)^2
Rdelta4 <- 1

plot(theta, Rdelta2, "l", col="blue", xlab='theta', ylab="R(L(theta, delta))")
lines(theta, Rdelta3, "l", col="black")
abline(1/n, 0, col='green')
abline(1, 0, col='red')
legend(x = c(-8, -1), 
       y = c(3.2, 4), 
       c("R(L(theta, delta1))", "R(L(theta, delta2))", "R(L(theta, delta3))", "R(L(theta, delta4))"),
       lty=1, col=c('green', 'blue', 'black', 'red'))

# 예제 1.4
# 작성중




