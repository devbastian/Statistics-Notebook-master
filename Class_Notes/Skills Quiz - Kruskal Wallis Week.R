library(mosaic)
?SaratogaHouses
view(SaratogaHouses)
table(SaratogaHouses$fuel)

kruskal.test(price ~ fuel, data = SaratogaHouses)

pander(favstats(price ~ fuel, data = SaratogaHouses))
