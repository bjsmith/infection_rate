#figure for COVID-positive patients is 3/65
#figure for COVID-negative patients is 12/9000
#could/should probably break this down to the same month that the 12/9000 comes from

#it can be shown that these proportions are unlikely to come from the same population:
prop.test(c(3,12),c(65,9000))


#can we calculate what a confidence interval from the 3/65 should look like?

#for a large sample size >15 and p<0.1, we should use a poisson approximation; see
#https://www.statisticshowto.com/binomial-confidence-interval/#:~:text=The%20binomial%20confidence%20interval%20is,p%20for%20a%20binomial%20distribution.
#(Montgomery, 2001).

#poisson confidence interval can be estimated using epitools::pois.daly
#https://rdrr.io/cran/epitools/man/pois.conf.int.html
#test its performance here:
pois.daly(x=10,100)
pois.daly(x=100,1000)

#what does it say about the CI for 3/65?
pois.daly(x=3,65)

#anything from 1% to 13%

#and what about 12/9000?

pois.daly(x=12,9000)

#BUT hang on, for July, a better figure is 1/30
#The 3/65 refers to a larger period across all of the lockdown period
pois.daly(x=1,30)
#lowest possible value it could plausibly be is 0.08%

#this is credibly similar to 12/9000
#what does that look like in a prop test?

prop.test(c(1,12),c(30,9000))

#so - still unlikely they've come from the same population
#a compromise figure might be:
1/30
12/9000
exp(mean(log(c(1/30,12/9000))))