library(ggplot2)
library(gridExtra)

#Parameter
N = 125190000
E = 10
I = 50
S = N - E -I
R = 0
beta_1 = 0.0027
mu = 0.1
v = 0.3
d = 0.2
rho = 0.3
lambda = beta_1*(I/N)

#Empty_list
S_vec = c(S)
E_vec = c(E)
I_vec = c(I)
R_vec = c(R)

#Calc
for(i in 1:100) {
  Sa = S_vec[i] - S_vec[i]*(lambda+mu)
  S_vec = append(S_vec, Sa)
  Ea = E_vec[i] + lambda*S_vec[i] - E_vec[i]*(mu+v)
  E_vec = append(E_vec, Ea)
  Ia = I_vec[i] + v*E_vec[i] - I_vec[i]*(mu+d+rho)
  I_vec = append(I_vec, Ia)
  Ra = R_vec[i] + rho*I_vec[i] - mu*R_vec[i]
  R_vec = append(R_vec, Ra)
}

df = data.frame(S_g=S_vec, E_g=E_vec, I_g=I_vec, R_g=R_vec)


g = ggplot(df)

g1 = g + geom_line(aes(x=1:length(S_g), y=S_g), size=2, colour="red") +
  labs(y="S", x="100days")
g2 = g +geom_line(aes(x=1:length(E_g), y=E_g), size=2, colour="blue") +
  labs(y="E", x="100days")
g3 = g +geom_line(aes(x=1:length(I_g), y=I_g), size=2, colour="green") +
  labs(y="I", x="100days")
g4 = g + geom_line(aes(x=1:length(R_g), y=R_g), size=2, colour="yellow") +
  labs(y="R", x="100days")

options(scipen=2)


grid.arrange(g1, g2, g3, g4)