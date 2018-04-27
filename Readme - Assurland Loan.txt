Cumulated evolution

Cumulated evolution is a metric to compare the evolution of price for one insurer from period 1 to period 2. 
We use the 'Oneperiodlog' function, which compute log(p2/p1) (p2=pricefor period2), and then we take the exponential minus 1. 
So if the cumulated evolution is positive, than the price has augmented, and inversilly if the cumulated evolution is negative, the price has fallen.





Average premium

Average premium is just the mean of the price, for a given insurer/coverage/period, computed in the 'Oneperiodstat' function.









Display rate

Display rate is the proportion of display, computed in the 'Oneperiodstat' function.
We use the metrics nb_profils into the function : values of displayed profils for one insurer/coverage/period divided by the total value for coverage/period.

	
	
	
	
	
	
	
	
	
Global ranking

The global ranking give for each insurer the proportion it makes in the top 1 or top 3 prices for a given period/coverage. 
For that we use the function 'top_propor_Generic', which compute thanks to the rank function a rank for every line by profilID/coverage/period, and then we compute the 
proportion thanks to 2 metrics, cumsum (total nb of rank 1(to 3) for one insurer during one period for selected coverage) and cumsum2 (same but for all insurers), 
so proportion=cumsum/cumsum2.












Ranking by player

The ranking by player, for one given insurer, give its proportion to be 1st/2nd/3rd/4th/5th/6th+ (in term of price, for one period and one coverage).
We use the function 'genrankovermonths', which itself uses the functions rank and cut (which divide the rank vector into intervals, and then we divide by the total length).
	

	
	
	
	
	
	
	
	

Price gap

Price gap give the density of the difference between AXA and its closest classique competitor in term of price.
We use New_Table, that we filter into classique players.
Then we separate Group AXA prices from others, we keep only the minimum price for each profilID/coverage for other classique competitors, and we compute the 
delta = (priceAXA/min.priceother)-1.
So delta > 0 if price AXA > min.priceother. Finally we compute the density for the graph.













Market intensity

Market intensity gives the dispersion between the top 3 (in terms of global ranking) classique players, to see if they are close from each other in term of price.
We use the top3_classique player table. Then we compute the coefficient of variation = sd/mean, which is a standardized measure of dispersion.
The higher the coefficient of variation is, the higher is the dispersion. 
Finally we compute the density for the graph.