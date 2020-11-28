savings = 1000000
life.mean = 20
life.stdev = 10

#Return
remain.cap = rnorm(S,0.08, 0.02)
scale.est = life.stdev^2 / life.mean
shape.est = life.mean / scale.est
life.long = rgamma(S, shape.est, scale.est)
