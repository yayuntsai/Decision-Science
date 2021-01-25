library(pracma)
library(EnvStats)
library(MASS)

S = 10000

#four place in Taipei
place_supply = c(15000, 5000, 2000, 1200)
place_tax = c(0.15, 0.08, 0.09, 0.09)

#price
price_max = c(6000, 3300, 3000, 1500)
price_min = c(2500, 1800, 1200, 600)
price_mode = c(3200, 2400, 2000, 1000)

sim_price = matrix(nrow = 4, ncol = S)
for(p in 1:S){
  sim_price[1, p] = round(rtri(1, price_min[1], price_max[1], price_mode[1]), 0)
  sim_price[2, p] = round(rtri(1, price_min[2], price_max[2], price_mode[2]), 0)
  sim_price[3, p] = round(rtri(1, price_min[3], price_max[3], price_mode[3]), 0)
  sim_price[4, p] = round(rtri(1, price_min[4], price_max[4], price_mode[4]), 0)
}

#cost
cost = rep(0, 4)
cost_min = c(0.6, 0.48, 0.37, 0.3)
cost_max = c(0.7, 0.58, 0.47, 0.4)
for(c in 1:4){
  cost[c] = runif(n = 1, min = price_mode[c] * cost_min[c], 
                  max = price_mode[c] * cost_max[c]) * place_supply[c]
}

#demand
ticket_demand = function(level){
  artist_mean = c(15000, 8000, 5000, 2000, 900)
  artist_sd = c(1000, 800, 500, 200, 150)
  
  demand = round(rnorm(S, mean = artist_mean[level], sd = artist_sd[level]))
}

Hooke_Jeeves_model = function(par, level, demand){
  price = round(par[1], 0)
  #print(price)
  
  place = round(par[2], 0)
  #print(place)
  
  #假設對價錢的接受度為預期的1.2倍，找出可以接受這個價格的需求比例
  accept_high_price_propotion = sum(sim_price[place, ] * 1.2 > price)/S
  #print(accept_high_price)
  
  revenue = rep(0, S)
  for(s in 1:S){
    revenue[s] = (1 - place_tax[place]) * (price * min(demand[s] * accept_high_price_propotion, 
                                                       place_supply[place])) - cost[place]
  }
  return(mean(revenue))
}

#####################
#run
artist_level = 5 #[1, 2, 3, 4, 5]

#用最一開始的需求判斷足夠空間的場地作為搜尋起始點
demand = ticket_demand(artist_level)
if(mean(demand) > 15000){
  suf_place = 1
} else if(mean(demand) < 15000 && mean(demand) > 5000){
  suf_place = 2
} else if(mean(demand) < 5000 && mean(demand) > 2000){
  suf_place = 3
} else if(mean(demand) < 2000){
  suf_place = 4
}

Hooke_Jeeves = fminsearch(Hooke_Jeeves_model, 
                          c(price_mean[suf_place], suf_place), level = artist_level, demand = demand,
                          lower = c(price_min[suf_place], 1), 
                          upper = c(price_max[suf_place], 4),
                          method = c("Hooke-Jeeves"),
                          minimize = FALSE)

#result
cat('\n', 'Best venue :', round(Hooke_Jeeves$xmin[2]), '\n',
    'Best price :', round(Hooke_Jeeves$xmin[1]), '\n',
    'Expected Demand :', round(mean(demand), 0), '\n',
    'Expected Cost :', round(mean(cost[round(Hooke_Jeeves$xmin[2])]), 0), '\n',
    'Expected Revenue :', round(Hooke_Jeeves$fmin))
