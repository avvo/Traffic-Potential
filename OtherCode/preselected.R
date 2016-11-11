#Immigration,,15
cit.not_citizen,hh.median_income,(eth.white+eth.black)/eth.base_count

#Divorce,,15
mar.yes,hh.married_couple,hh.median_income,fer.birth_count,fer.unmarried_birth_pml,emp.no

#Criminal,Defense
age_male,emp.no/pop.base_count,hh.median_income,pov.below/pop.base_count,hh.male_only,eth.white+eth.asian

#DUI
(pop.15_19+pop.20_24+pop.25_29),emp.no/pop.base_count,hh.median_income,pov.below/pop.base_count,hh.male_only,eth.white

#Real,Estate
hh.median_income,emp.military,eth.white,hh.families,(pop.25_29+pop.20_24+pop.15_19+pop.10_14+pop.5_9)

