#######################################################################################################################################################
#Here I list the different scripts in order to explain what was conducted on each them 
#######################################################################################################################################################
0) We have look with the help of the package taxsise for the valid species names for both plants and pollinators.

1) We merge the species names searched previously with the networks coverted to long format.

2) We impute the missing data, all the columns are under 50% of missing data except the quantitative selfing that is around this percentage

3) We calculate the functional groups with HCLUST 

4) We calculate Z-scores in order to have not just visitation data but also an standardize measurement of it

5) We merge the outputs of scripts 2,3 and 4 and now we have the data ready for analysis.

6) Data analysis with response variable Z-scores model 1 and the visitation data (integers of visits) model 2
      -6.1) Plot models output for publication
#######################################################################################################################################################
# Additionaly we explore on the folder of Analysis_Bee_Visitation the same models but just for bee species because they are the most dominant
#######################################################################################################################################################

