#1
S = 1000
pbad = 0.17
pgood = 1-pbad
pbad.irreg = 0.1
pgood.irreg = 0.05
pbad.reg = 1-pbad.irreg
pgood.reg = 1-pgood.irreg

inspector = 50
chips = 20
workhours = 8
inspect.day = inspector*chips*workhours

dailychips = function(n){
  #defective_true
  defects = rbinom(n,inspect.day,pbad)
  #defective_false
  defects.false = rbinom(n,inspect.day,pbad.reg)
  #regular_true
  regular.true = rbinom(n,inspect.day,pgood.reg)
  #regular_false
  regular.false = rbinom(n,inspect.day,pbad.reg)
}