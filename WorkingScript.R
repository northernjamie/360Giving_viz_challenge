# Need to absolutely sort this horrorshow out, but time waits for no man, and I had to get it submitted

library(dplyr)
library(ggplot2)
library(data.table)

# Import the data frm GrantNav
df_360Giving <- read.csv("~/Projects/Propolis_Stuff/360GivingVizChallenge/grantnav-20180726072751.csv")

# get rid of unneeded columns
df_360_stripped <- df_360Giving[c(1,2,3,5,6,8,16,17,20,21,22,31,36,38,39,73,74,75,76)]

# count how many postcodes there are
sum(df_360_stripped$Recipient.Org.Postal.Code != "")

# filter to only show unempty postcodes
df_360_stripped_pcd <- df_360_stripped[which(df_360_stripped$Recipient.Org.Postal.Code != ""), ]

# remove any punctuation from the postcodes
df_360_stripped_pcd$Recipient.Org.Postal.Code <- gsub("[[:punct:]]", " ", df_360_stripped_pcd$Recipient.Org.Postal.Code)
head(df_360_stripped_pcd)


# match to get only valid postcodes
# This function runs imdr against the supplied dataframe with postcodes
# This is absolutely not the best way to do this - merging against a postcode to LSOA file, then LSOA to IMD would be much quicker
get_imd <- function(df,df_length) {
  start <- 1001
  end <- 2000
  
  repeat{
    imd_360G_new <- imd_lookup(df[start:end,],9)
    imd_360G_all <- rbind(imd_360G_all,imd_360G_new)
    start <- start + 1000
    end <-  end + 1000
    print(end)
    if(end > df_length){
      return(imd_360G_all)
      break
    }
  }
}

# create a dataframe with the first 1000 rows matched
imd_360G_all <- imd_lookup(head(df_360_stripped_pcd,1000),9)

# run the function to get IMD data for each grant
imd_360G <- get_imd(df_360_stripped_pcd,61000)

# Get rid of anything with NA in the IMD rank field, as that's grants that haven't matched to an LSOA
imd_360G_imd <- imd_360G[complete.cases(imd_360G[,22]),]

# Backup the matched imd file to csv in case of error (only because generating it took so long)
write.csv(imd_360G_imd, file="imd_360G_imd_match.csv")

# insert the vigintile from the overall rank
imd_360G_vigintile <- imd_360G_imd %>%
  mutate(vigintile = ntile(`a. Index of Multiple Deprivation (IMD)`, 20))

# get rid of extra columns

imd_360G_vigintile <- imd_360G_vigintile[ c(2,3,4,6,7,9,13,22,32)]

# aggregate the dataframe to show average grant and total grants by organisation
DT <- data.table(imd_360G_vigintile)
grant_maker_avge <- DT[, mean(Amount.Awarded), by=Funding.Org.Name]
grant_maker_total <- DT[, sum(Amount.Awarded), by=Funding.Org.Name]
merged_agg <- merge(x = grant_maker_avge, y = grant_maker_total,by="Funding.Org.Name",all=T)
imd_360G_vigintile <- merge(x=imd_360G_vigintile, y = merged_agg,by = "Funding.Org.Name", all = T)

#reorder the factor levels of the org name so they display in order when ordering the facet by total given
imd_360G_vigintile$funder_ord_total_grant <- reorder(imd_360G_vigintile$Funding.Org.Name,imd_360G_vigintile$V1.x)
imd_360G_vigintile$funder_ord_mean_grant <- reorder(imd_360G_vigintile$Funding.Org.Name,imd_360G_vigintile$V1.y)

# draw the faceted violin plot
e <- ggplot(imd_360G_vigintile, aes(Funding.Org.Name, vigintile, fill = Funding.Org.Name))
e <- e + facet_wrap(~ Funding.Org.Name, strip.position = 'bottom', scales = 'free_x',ncol = 8)
e <- e + geom_violin(color="white") + ggtitle("England") +
  theme_classic() + theme(plot.subtitle = element_text(color = 'white', size = 18, face='italic'),plot.title = element_text(color = "white",size=26),strip.background = element_rect(fill = 'black'),strip.text = element_text(color='white',size=8),axis.text.y = element_blank(),axis.ticks.y = element_blank(), axis.title.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.x=element_blank(), legend.position="none",axis.title = element_text(color="white",size=12),plot.background=element_rect(fill="black"),panel.background = element_rect(fill="black"))
e
ggsave("e_360G_imd.svg",e,width=24,height=36,units="in")

# Issues / Actions: 
## * this just counts the number of grants, and pays no consideration to the amount awarded
##  * To address this, need an observation for each £1 (or lowest common denominator across all grants)
##  * This will massively increase the number of rows in the dataframe. Maybe try rounding each grant to the nearest £100
##  * Then calculating how many £100s are given to each Deprivation vigintile by each funder, and creating the appropriate dataframe

## * Add a sort - and colour - maybe by average or total grants awarded

## * Need something to do with the themes - otherwise it doesn't meet the criteria
##   * But can't see theme in the data



# ideas

## amount funded vs amount awarded by region, etc

## 3d map

## Keyword something

## Network diag

## Cross filter

## Lava lamp plot for funders
### Don't think I can do this - no LSOAs



