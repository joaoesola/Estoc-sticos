#Nome: João Eduardo Sola
#RA: 170456
#Nome: Guilherme Michel Lima de Carvalho
#RA: 140652

#Variáveis que devemos fixar para geração de filas M/M/1
Tempo.da.simulacao <- 60 #min 
Tempo.inicial <- 0
n0 <- 0
n <- 142
m <- 142
Tb <- 60 #min

#Inferindo rho, mu e lambda:
##Inferindo rho:
###Função quadrática
funcao.rho <- function(a,b,c){
  if(delta(a,b,c) > 0){ # D>0
    x_1 = (-b+sqrt(delta(a,b,c)))/(2*a)
    x_2 = (-b-sqrt(delta(a,b,c)))/(2*a)
    result = c(x_1,x_2)
  }
  else if(delta(a,b,c) == 0){ # D=0
    x = -b/(2*a)
  }
  else {"There are no real roots."} # D<0
}

###Delta
delta<-function(a,b,c){
  b^2-4*a*c
}

###Rho.chapeu
Rho.chapeu <- funcao.rho((m - n0 - 1)*Tempo.da.simulacao, -((m-n0)*Tempo.da.simulacao+(n+n0+1)*Tb), (n+n0)*Tb)
Rho.chapeu

###Rho.chapeu.verdadeiro
if(Rho.chapeu[1]<1){
  Rho.chapeu.verdadeiro <- Rho.chapeu[1]
} else{
  Rho.chapeu.verdadeiro <- Rho.chapeu[2]
}
round(Rho.chapeu.verdadeiro, 3)

##Inferindo mu:
funcao.mu <- function(n,m,Tempo.da.simulacao,Tb,Rho.chapeu.verdadeiro){
  mu.chapeu = (n+m)/(Rho.chapeu.verdadeiro*Tempo.da.simulacao+Tb)
}
mu.chapeu.verdadeiro <- funcao.mu(n,m,Tempo.da.simulacao,Tb,Rho.chapeu.verdadeiro)
round(mu.chapeu.verdadeiro,3)

##Inferindo lambda
funcao.lambda <- function(n,m,Tempo.da.simulacao,Tb,Rho.chapeu.verdadeiro){
  lambda.chapeu = ((n+m)/(Rho.chapeu.verdadeiro*Tempo.da.simulacao+Tb))*Rho.chapeu.verdadeiro
}
lambda.chapeu.verdadeiro <- funcao.lambda(n,m,Tempo.da.simulacao,Tb,Rho.chapeu.verdadeiro)
round(lambda.chapeu.verdadeiro,3)

##MM1
Ns <- 1 #numero de servidores
#calculando p0
p0 <- 1 - Rho.chapeu.verdadeiro
p0
pn <- list()
for (i in 0:n){
  vetor <- (Rho.chapeu.verdadeiro^i)*(1-Rho.chapeu.verdadeiro)
  pn <- append(pn, vetor)
}
pn <- unlist(pn)
head(pn)
tail(pn)

pontosn <- c(0:30)
pn. <- pn[c(0:31)]
dadinhos <- data.frame(pontosn,pn.)
library(ggplot2)
ggplot(dadinhos, aes(x = pontosn, y = pn.)) + geom_bar(stat="identity",colour = "black") + xlab("Número de Clientes") + ylab("Probabilidade") + labs(title = "Probabilidade de P(n)")
#Calculando o numero esperado de clientes no sistema
L <- (lambda.chapeu.verdadeiro/(mu.chapeu.verdadeiro - lambda.chapeu.verdadeiro))
L
#Calculando o numero esperado de clientes na fila
Lq <- ((lambda.chapeu.verdadeiro^2)/(mu.chapeu.verdadeiro*(mu.chapeu.verdadeiro - lambda.chapeu.verdadeiro)))
Lq
#L-Lq precisa ser 1
L-Lq
#Calculando o numero esperado do tempo total (T) no sistema
W =  L/lambda.chapeu.verdadeiro
W <- round(W,2) #em minutos
W
#Calculando o numero esperado do tempo de serviço (Tq)
Wp = Lq/lambda.chapeu.verdadeiro
Wp <- round(Wp,2) #em minutos
Wp

#Gerando os dados da nossa fila M/M/1
t.end   <- 60 # duration of sim
t.clock <- 0    # sim time
Ta <- 2.503    # interarrival period
Ts <- 2.23    # service period
t1 <- 0         # time for next arrival
t2 <- t.end     # time for next departure
tn <- t.clock   # tmp var for last event time
tb <- 0         # tmp var for last busy-time start
n <- 0          # number in system
s <- 0          # cumulative number-time product
b <- 0          # total busy time
c <- 0          # total completions
qc <- 0         # plot instantaneous q size
tc <- 0         # plot time delta
plotSamples <- 60
set.seed(1)
vet.t1 <- list()
vet.t2 <- list()
vet.tn <- list()
vet.tb <- list()
vet.n <- list()
vet.s <- list()
vet.b <- list()
vet.c <- list()
vet.qc <- list()
vet.tc <- list()
while (t.clock < t.end) {
  if (t1 < t2) {      # arrival event
    t.clock <- t1
    s <- s + n * (t.clock - tn)  # delta time-weighted number in queue
    n <- n + 1
    if (t.clock < plotSamples) { 
      qc <- append(qc,n)
      tc <- append(tc,t.clock) 
    }
    tn <- t.clock
    t1 <- t.clock + rexp(1, 1/Ta)
    if(n == 1) { 
      tb <- t.clock
      t2 <- t.clock + rexp(1, 1/Ts)  # exponential  interarrival period
    }
  } else {            # departure event
    t.clock <- t2
    s <- s + n * (t.clock - tn)  # delta time-weighted number in queue
    n <- n - 1
    if (t.clock < plotSamples) { 
      qc <- append(qc,n)
      tc <- append(tc,t.clock)
    }
    tn <- t.clock
    c <- c + 1
    if (n > 0) { 
      t2 <- t.clock + rexp(1, 1/Ts)  # exponential  service period
    }
    else { 
      t2 <- t.end
      b <- b + t.clock - tb
    }
  }
  vet.t1 <- append(vet.t1,t1)
  vet.t2 <- append(vet.t2,t2)
  vet.tn <- append(vet.tn,tn)
  vet.tb <- append(vet.tb,tb)
  vet.n <- append(vet.n,n)
  vet.s <- append(vet.s,s)
  vet.b <- append(vet.b,b)
  vet.c <- append(vet.c,c)
  vet.qc <- append(vet.qc,qc)
  vet.tc <- append(vet.tc,tc)
}

dados <- data.frame(unlist(vet.t1),unlist(vet.t2),unlist(vet.tn),unlist(vet.tb),unlist(vet.n),unlist(vet.s),unlist(vet.b),unlist(vet.c))
plot(tc,qc, type = 's', xlab="Tempo em minutos",ylab="Contagem da fila",main="M/M/1 Simulação")


Contagem <- unlist(vet.n)
Temp.de.chegada <- unlist(vet.t1)
Temp.de.atend <- unlist(vet.tn)
dados.compilado <- data.frame(Contagem,Temp.de.atend,Temp.de.chegada)

#Comparação entre as filas M/M/1 e M/M/K

#Caixa 1
Tempo.da.simulacao <- 60 #min 
Tempo.inicial <- 0
n0 <- 0
n1 <- 142
m1 <- 142
Tb <- 60 #min

#Caixa 2
Tempo.da.simulacao <- 60 #min 
Tempo.inicial <- 0
n0 <- 0
n2 <- 138
m2 <- 138
Tb <- 60 #min

###Função quadrática
funcao.rho <- function(a,b,c){
  if(delta(a,b,c) > 0){ # D>0
    x_1 = (-b+sqrt(delta(a,b,c)))/(2*a)
    x_2 = (-b-sqrt(delta(a,b,c)))/(2*a)
    result = c(x_1,x_2)
  }
  else if(delta(a,b,c) == 0){ # D=0
    x = -b/(2*a)
  }
  else {"There are no real roots."} # D<0
}

###Delta
delta<-function(a,b,c){
  b^2-4*a*c
}


###Rho.chapeu1
Rho.chapeu1 <- funcao.rho((m1 - n0 - 1)*Tempo.da.simulacao, -((m1-n0)*Tempo.da.simulacao+(n1+n0+1)*Tb), (n1+n0)*Tb)
Rho.chapeu1
Rho.chapeu.verdadeiro1 <- Rho.chapeu1[2]
Rho.chapeu.verdadeiro1

###Rho.chapeu2
Rho.chapeu2 <- funcao.rho((m2 - n0 - 1)*Tempo.da.simulacao, -((m2-n0)*Tempo.da.simulacao+(n2+n0+1)*Tb), (n2+n0)*Tb)
Rho.chapeu2
Rho.chapeu.verdadeiro2 <- Rho.chapeu2[2]
Rho.chapeu.verdadeiro2

##Inferindo mu:
funcao.mu <- function(n,m,Tempo.da.simulacao,Tb,Rho.chapeu.verdadeiro){
  mu.chapeu = (n+m)/(Rho.chapeu.verdadeiro*Tempo.da.simulacao+Tb)
}
mu.chapeu.verdadeiro1 <- funcao.mu(n1,m1,Tempo.da.simulacao,Tb,Rho.chapeu.verdadeiro1)
round(mu.chapeu.verdadeiro1,3)

mu.chapeu.verdadeiro2 <- funcao.mu(n2,m2,Tempo.da.simulacao,Tb,Rho.chapeu.verdadeiro2)
round(mu.chapeu.verdadeiro2,3)

##Inferindo lambda
funcao.lambda <- function(n,m,Tempo.da.simulacao,Tb,Rho.chapeu.verdadeiro){
  lambda.chapeu = ((n+m)/(Rho.chapeu.verdadeiro*Tempo.da.simulacao+Tb))*Rho.chapeu.verdadeiro
}
lambda.chapeu.verdadeiro1<- funcao.lambda(n1,m1,Tempo.da.simulacao,Tb,Rho.chapeu.verdadeiro1)
round(lambda.chapeu.verdadeiro1,3)

lambda.chapeu.verdadeiro2 <- funcao.lambda(n2,m2,Tempo.da.simulacao,Tb,Rho.chapeu.verdadeiro2)
round(lambda.chapeu.verdadeiro2,3)

p0.1 <- 1 - Rho.chapeu.verdadeiro1
p0.1
p0.2 <- 1 - Rho.chapeu.verdadeiro2
p0.2

pn.1 <- list()
pn.2 <- list()
for (i in 0:n1){
  vetor.1 <- ((Rho.chapeu.verdadeiro1)^i)*(1-Rho.chapeu.verdadeiro1)
  pn.1 <- append(pn.1, vetor.1)
}
for (i in 0:n2){
  vetor.2 <- ((Rho.chapeu.verdadeiro2)^i)*(1-Rho.chapeu.verdadeiro2)
  pn.2 <- append(pn.2, vetor.2)
}
pn.1 <- unlist(pn.1)
pn.2 <- unlist(pn.2)
head(pn.1)
tail(pn.1)
head(pn.2)
tail(pn.2)
L1 <- (lambda.chapeu.verdadeiro1/(mu.chapeu.verdadeiro1 - lambda.chapeu.verdadeiro1))
L1
L2 <- (lambda.chapeu.verdadeiro2/(mu.chapeu.verdadeiro2 - lambda.chapeu.verdadeiro2))
L2
Lq1 <- ((lambda.chapeu.verdadeiro1^2)/(mu.chapeu.verdadeiro1*(mu.chapeu.verdadeiro1 - lambda.chapeu.verdadeiro1)))
Lq1
Lq2 <- ((lambda.chapeu.verdadeiro2^2)/(mu.chapeu.verdadeiro2*(mu.chapeu.verdadeiro2 - lambda.chapeu.verdadeiro2)))
Lq2
W1 =  L1/lambda.chapeu.verdadeiro1
W1 <- round(W1,2) #em minutos
W1
W2 =  L2/lambda.chapeu.verdadeiro2
W2 <- round(W2,2) #em minutos
W2
Wp1 = Lq1/lambda.chapeu.verdadeiro1
Wp1 <- round(Wp1,2) #em minutos
Wp1
Wp2 = Lq2/lambda.chapeu.verdadeiro2
Wp2 <- round(Wp2,2) #em minutos
Wp2

k=2
Tempo.da.simulacao <- 60 #min 
Tempo.inicial <- 0
n0 <- 0
n.tot <- n1+n2
m.tot <- m1+m2
Tb <- 60 #min

Rho.chapeu.mmk <- funcao.rho((m.tot - n0 - 1)*Tempo.da.simulacao, -((m.tot-n0)*Tempo.da.simulacao+(n.tot+n0+1)*Tb), (n.tot+n0)*Tb)
Rho.chapeu.mmk <- Rho.chapeu.mmk[2]
mu.chapeu.mmk <- funcao.mu(n.tot,m.tot,Tempo.da.simulacao,Tb,Rho.chapeu.mmk)
round(mu.chapeu.mmk,3)
lambda.chapeu.mmk<- funcao.lambda(n.tot,m.tot,Tempo.da.simulacao,Tb,Rho.chapeu.mmk)
round(lambda.chapeu.verdadeiro1,3)

px <- function(mu.chapeu.verdadeiro,lambda.chapeu.verdadeiro,h){
  (((lambda.chapeu.verdadeiro/mu.chapeu.verdadeiro)^h)/factorial(h))
}
py = 0
for (h in 0:(k-1)){
  py <- py + px(mu.chapeu.mmk,lambda.chapeu.mmk,h)
  pw <- py + (((lambda.chapeu.mmk/mu.chapeu.mmk)^k)/(factorial(k)*(1-Rho.chapeu.mmk)))
  p0 <- pw^(-1)
}
p0

Lq.mmk <- (Rho.chapeu.mmk*((lambda.chapeu.mmk/mu.chapeu.mmk)^k)*p0)/(factorial(k)*(1-Rho.chapeu.mmk)^2)
Lq.mmk

L.mmk <- (lambda.chapeu.mmk/mu.chapeu.mmk)+((1/factorial(k-1))*(lambda.chapeu.mmk/mu.chapeu.mmk)^k)*p0*((lambda.chapeu.mmk*mu.chapeu.mmk)/((k*mu.chapeu.mmk-lambda.chapeu.mmk)^2))
L.mmk

Wq.mmk <- (((lambda.chapeu.mmk/mu.chapeu.mmk)^k)*p0)/(factorial(k)*mu.chapeu.mmk*k*((1-Rho.chapeu.mmk)^2))
Wq.mmk

W.mmk <- Wq.mmk + 1/mu.chapeu.mmk
W.mmk

dados. <- data.frame(Rho.chapeu.verdadeiro1,lambda.chapeu.verdadeiro1,mu.chapeu.verdadeiro1,L1,Lq1,W1,Wp1)
Dados. <-t(dados.)
dados.. <- data.frame(Rho.chapeu.verdadeiro2,lambda.chapeu.verdadeiro2,mu.chapeu.verdadeiro2,L2,Lq2,W2,Wp2)
Dados.. <-t(dados..)
dados... <- data.frame(Rho.chapeu.mmk,lambda.chapeu.mmk,mu.chapeu.mmk,L.mmk,Lq.mmk,W.mmk,Wq.mmk)
Dados... <-t(dados...)
Dados <- cbind(Dados.,Dados..,Dados...)
