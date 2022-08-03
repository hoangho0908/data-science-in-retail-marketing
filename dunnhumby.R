#Import packages
install.packages(c("caret","dplyr","lubridate","stringr","readr","ggplot","ggcharts","plyr","data.table","Hmisc","tidyverse","synthpop","hrbrthemes","viridis","forcats","ggridges","sqldf","fpc","smotefamily","mltools","randomForest","cluster","olsrr","glmnet","klaR","gbm","mlbench","e1071","fastDummies","caTools"))
pacman::p_load(caret,dplyr,lubridate,stringr,readr,ggplot2,ggcharts,plyr,data.table,Hmisc,tidyverse,synthpop,hrbrthemes,viridis,forcats,ggridges,sqldf,smotefamily,mltools,fpc,randomForest,cluster,olsrr,glmnet,klaR,gbm,mlbench,e1071,fastDummies,caTools)
memory.limit(size=160000)
mycols <- c("aquamarine", "bisque2", "cadetblue1", "blueviolet","blue","burlywood2")

#Load datasets
demo<- read.csv("hh_demographic.csv")
campaign<-read.csv("campaign_table.csv")
camptime<-read.csv("campaign_desc.csv")
coupon<- read.csv("coupon.csv")
prod <- read.csv("product.csv")
redem<- read.csv("coupon_redempt.csv") 
trans<- read.csv("transaction_data.csv")
causal<- read.csv("causal_data.csv")

#######################################################################################
##Demographic
#Transformation
#age to factor
demo$AGE_DESC <- factor(demo$AGE_DESC)
str(demo$AGE_DESC)
#Transform income to factor (with levels)
demo$INCOME_DESC <- factor(demo$INCOME_DESC, levels = c("Under 15K", "15-24K", "25-34K", "35-49K","50-74K", "75-99K" , "100-124K", "125-149K" ,"150-174K","175-199K", "200-249K" ,"250K+"))
#Kid number: none/unknown to 0, +3 to 3
demo$KID_CATEGORY_DESC  <- str_replace(demo$KID_CATEGORY_DESC , "None/Unknown", "0")
demo$KID_CATEGORY_DESC  <- str_replace(demo$KID_CATEGORY_DESC , "3\\+", "3")
#household size: replace 5+ to 5
demo$HOUSEHOLD_SIZE_DESC <- str_replace(demo$HOUSEHOLD_SIZE_DESC, "5\\+", "5")
#Add extra col: Number of adults
demo$KID_CATEGORY_DESC <- as.numeric(demo$KID_CATEGORY_DESC)
demo$HOUSEHOLD_SIZE_DESC <- as.numeric(demo$HOUSEHOLD_SIZE_DESC)
demo<- mutate(demo, ADULT_NUM = HOUSEHOLD_SIZE_DESC  - KID_CATEGORY_DESC )
head(demo)
#Change KID_CATEGORY and ADULT_NUM to factor
demo$KID_CATEGORY_DESC <- factor(demo$KID_CATEGORY_DESC,levels=c("0","1","2","3"))
demo$ADULT_NUM <- factor(demo$ADULT_NUM,levels=c("1","2"))

#Exploration
#Income
inc<- count(demo, 'INCOME_DESC')
inc$percent<- round(prop.table(inc$freq)*100,digits=2)
ggplot(inc, aes(x = INCOME_DESC, y = freq))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = percent), vjust = -0.3) 

#Age
age<- count(demo, 'AGE_DESC')
age$percent<- round(prop.table(age$freq)*100,digits=2)
ggplot(age, aes(x = AGE_DESC, y = freq))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = percent), vjust = -0.3) 

#Marital status
mar<- count(demo, 'MARITAL_STATUS_CODE')
mar$percent<- round(prop.table(mar$freq)*100,digits=2)
ggplot(mar, aes(x = MARITAL_STATUS_CODE, y = freq))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = percent), vjust = -0.3) 

#Homeowning status
homeownredem<- count(demo, 'HOMEOWNER_DESC')
homeownredem$percent<- round(prop.table(homeownredem$freq)*100,digits=2)
ggplot(homeownredem, aes(x = HOMEOWNER_DESC, y = freq))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = percent), vjust = -0.3) 

#Household composition
hhcompredem<- count(demo, 'HH_COMP_DESC ')
hhcompredem$percent<- round(prop.table(hhcompredem$freq)*100,digits=2)
ggplot(hhcompredem, aes(x = HH_COMP_DESC , y = freq))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = percent), vjust = -0.3)

#Household size
hhsize<- count(demo, 'HOUSEHOLD_SIZE_DESC  ')
hhsize$percent<- round(prop.table(hhsize$freq)*100,digits=2)
p1<- ggplot(hhsize, aes(x = HOUSEHOLD_SIZE_DESC  , y = freq))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = percent), vjust = -0.3)

#Number of kids
kidno<- count(demo, 'KID_CATEGORY_DESC ')
kidno$percent<- round(prop.table(kidno$freq)*100,digits=2)
p2<- ggplot(kidno, aes(x = KID_CATEGORY_DESC   , y = freq))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = percent), vjust = -0.3)


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
multiplot(p1, p2, cols=2)


############################################################################################
##Campaign_table+campaign_desc

dim(campaign)
head(campaign)
dim(camptime)
head(camptime)

#check missing values
table(is.na(campaign))#No missing values
table(is.na(camptime))#No missing values

#Merge time with campaign
campfull<-inner_join(campaign,camptime,by="CAMPAIGN")
campfull<- subset(campfull, select=-c(DESCRIPTION.y))
campfull<- campfull %>% dplyr::rename(DESCRIPTION = DESCRIPTION.x)


#Unique values of data
sapply(campfull, function(x) length(unique(x)))

#Types of campaigns
campdessum<- count(campfull, 'DESCRIPTION')
campdessum$percent<- round(prop.table(campdessum$freq)*100,digits=2)
#Amount of campaigns mailed by each type
ggplot(campdessum, aes(x = "", y = freq, fill = DESCRIPTION)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = freq, label = percent), color = "black", position = position_stack(vjust = 0.5))+
  scale_fill_manual(name="DESCRIPTION",values = mycols) + 
  theme_void()


#Duration summarized and visualized
summary(campfull$DURATION)
ggplot (data = campfull) + geom_boxplot( mapping = aes(x =DURATION)) +
  labs(x = "DURATION",
       title = "Average duration of campaigns")
campfull %>% dplyr::filter(DURATION %in% c("161")) 
#Duration's outlier
freq(campfull$DURATION)

#campaign: 30
campsum<- count(campfull, 'CAMPAIGN')
campsum$percent<- round(prop.table(campsum$freq)*100,digits=2)
campsum<- inner_join(campsum,camptime, by="CAMPAIGN")

#common campaigns deployed visualized
treemap(campsum, index=c("CAMPAIGN"),     vSize="percent", type="index",
        
        fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
        fontcolor.labels=c("white","orange"),    # Color of labels
        fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
        bg.labels=c("transparent"),              # Background color of labels
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")
        ),                                   # Where to place labels in the rectangle?
        overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        inflate.labels=F,  
        title="Porpotion of campaigns sent"# If true, labels are bigger when rectangle is bigger.
        
)

#Type of campaign with longest duration (description x duration)
camptypedur<- aggregate(DURATION ~ DESCRIPTION, data=camptime, FUN=sum)
camptypedur<- as.data.frame(camptypedur)
ggplot(camptypedur, aes(x=DESCRIPTION, y=DURATION)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none")


#Types of campaign with the most amount of campaigns
camptype<- ddply(camptime,~DESCRIPTION,summarise,no_campaign=length(unique(CAMPAIGN)))
camptype$fraction <- camptype$no_campaign / sum(camptype$no_campaign )
camptype$ymax <- cumsum(camptype$fraction)
camptype$ymin <- c(0, head(camptype$ymax, n=-1))
camptype$labelPosition <- (camptype$ymax + camptype$ymin) / 2
camptype$label <- paste0(camptype$DESCRIPTION, "\n Quantity: ", camptype$no_campaign)
ggplot(camptype, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=DESCRIPTION)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")

###############################################################################################################
##Coupon/product data

#Check missing data
table(is.na(coupon))#No missing values
#Remove prod size and blank values in products
prod<- prod[,-c(7)]
prod[prod==" "] <- NA
table(is.na(prod))
prod<- na.omit(prod)
#Unique values of data:
sapply(coupon, function(x) length(unique(x)))

#Unique coupons: 1135
#Number of coupons deplopyed;
nrow(coupon)#124548 coupons deployed

#Number of campaigns with coupons: 30
#Campaign x Coupon_UPC: Campaigns with most coupons sent
coupcampsum<- count(coupon, 'CAMPAIGN')
coupcampsum<- inner_join(coupcampsum,camptime, by="CAMPAIGN")
coupcamptype<- aggregate(freq ~ DESCRIPTION, data=coupcampsum, FUN=sum)
coupcamptype$percent<- round(prop.table(coupcamptype$freq)*100,digits=2)
#campaign with most unique coupons:
uniqcoupcamp<- ddply(coupon,~CAMPAIGN,summarise,unique_coupons=length(unique(COUPON_UPC)))
uniqcouptype<- inner_join(uniqcoupcamp,camptime, by="CAMPAIGN")
uniqcouptype<- aggregate(unique_coupons ~ DESCRIPTION, data=uniqcouptype, FUN=sum)
uniqcouptype$percent<- round(prop.table(uniqcouptype$unique_coupons)*100,digits=2)

#Products eligible for coupons: 44133
#Products elligible with most unique coupons:
uniqcoupprod<- ddply(coupon,~PRODUCT_ID,summarise,unique_coupons=length(unique(COUPON_UPC)))
#Types of products that are most elligible for coupons:
coupprod<-inner_join(coupon,prod,by="PRODUCT_ID")
uniqcoupprodtype<- setDT(coupprod)[, .(UnCOUP=uniqueN(COUPON_UPC)), .(PRODUCT_ID,MANUFACTURER,DEPARTMENT,BRAND,COMMODITY_DESC,SUB_COMMODITY_DESC)]
uniqcoupprodtype<- subset(uniqcoupprodtype,uniqcoupprodtype$UnCOUP>9)#only 70 products are eligible for 10 coupons. 

sapply(uniqcoupprodtype, function(x) length(unique(x)))
coupinfo<-describe(uniqcoupprodtype)#Types of product that are likely to be eligible for multiple coupons

###############################################################################
##Coupon redemption
#Remove households that has no household data
hhmiss <- anti_join(redem, demo, by = "household_key")[,1]
hhmiss1 <- as.vector(hhmiss)
redem2 <- redem[!redem$household_key %in% hhmiss1, ]
dim(redem2)

#unique values of data
sapply(redem2, function(x) length(unique(x)))
#no of redemption
nrow(redem2)#1856 redemptions

#campaign
#link with campaign dataset
redemcamp<-inner_join(redem2,camptime,by="CAMPAIGN")
#Campaigns with most redemptions
redemcampsum<- count(redemcamp, 'CAMPAIGN')
redemcampsum$percent<- round(prop.table(redemcampsum$freq)*100,digits=2)
#Types of campaigns with most redemption
redemtypesum<- count(redemcamp, 'DESCRIPTION')
redemtypesum$percent<- round(prop.table(redemtypesum$freq)*100,digits=2)
#Campaign with most coupons redeemed (campaign x couponUPC)
redemcamno<- ddply(redemcamp,~CAMPAIGN,summarise,unique_coupons=length(unique(COUPON_UPC)))
redemtypeno<- inner_join(redemcamno,camptime,by="CAMPAIGN")
redemtypeno<- aggregate(unique_coupons ~ DESCRIPTION, data=redemtypeno, FUN=sum)


#Demographic of household that redeems coupons
#Link with demographic 
demoredem<-inner_join(redem2,demo,by="household_key")
describe(demoredem)

#Income
incredem<- count(demoredem, 'INCOME_DESC')
incredem$percent<- round(prop.table(incredem$freq)*100,digits=2)
ggplot(incredem, aes(x = INCOME_DESC, y = freq))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = percent), vjust = -0.3) 

#Age
ageredem<- count(demoredem, 'AGE_DESC')
ageredem$percent<- round(prop.table(ageredem$freq)*100,digits=2)
ggplot(ageredem, aes(x = AGE_DESC, y = freq))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = percent), vjust = -0.3) 

#Marital status
marredem<- count(demoredem, 'MARITAL_STATUS_CODE')
marredem$percent<- round(prop.table(marredem$freq)*100,digits=2)
ggplot(marredem, aes(x = MARITAL_STATUS_CODE, y = freq))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = percent), vjust = -0.3) 

#Homeowning status
homeownredem<- count(demoredem, 'HOMEOWNER_DESC')
homeownredem$percent<- round(prop.table(homeownredem$freq)*100,digits=2)
ggplot(homeownredem, aes(x = HOMEOWNER_DESC, y = freq))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = percent), vjust = -0.3) 

#Household composition
hhcompredem<- count(demoredem, 'HH_COMP_DESC ')
hhcompredem$percent<- round(prop.table(hhcompredem$freq)*100,digits=2)
ggplot(hhcompredem, aes(x = HH_COMP_DESC , y = freq))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = percent), vjust = -0.3)

#Household size
hhsizeredem<- count(demoredem, 'HOUSEHOLD_SIZE_DESC  ')
hhsizeredem$percent<- round(prop.table(hhsizeredem$freq)*100,digits=2)
ggplot(hhsizeredem, aes(x = HOUSEHOLD_SIZE_DESC  , y = freq))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = percent), vjust = -0.3)

#Number of kids
nokidredem<- count(demoredem, 'KID_CATEGORY_DESC ')
nokidredem$percent<- round(prop.table(nokidredem$freq)*100,digits=2)
ggplot(nokidredem, aes(x = KID_CATEGORY_DESC   , y = freq))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = percent), vjust = -0.3)

#remove HH_COMP_DESC and HOUSEHOLD_SIZE_DESC and re-order
demo<- demo[,-c(5,6)]
demo <- demo[c("household_key", "AGE_DESC", "MARITAL_STATUS_CODE","INCOME_DESC","HOMEOWNER_DESC","ADULT_NUM","KID_CATEGORY_DESC")]
##############################################################################
#product data
#general stats
describe(prod)

#Product_ID: No sold: 92353

#manufacturers: 6476
manucount<- count(prod, 'MANUFACTURER ')
manucount$percent<- round(prop.table(manucount$freq)*100,digits=2)

#Department: 44. Most common product department
departcount<- count(prod, 'DEPARTMENT ')
departcount$percent<- round(prop.table(departcount$freq)*100,digits=2)
departcount<- transform(departcount, DEPARTMENT = reorder(DEPARTMENT, freq))
ggplot(departcount, aes(DEPARTMENT, freq),ylim=c(0,30)) +
  geom_col(aes(y = 100), fill = "grey70") +
  geom_col(fill = "navyblue") +
  coord_flip()

#Brand
brandcount<- count(prod, 'BRAND ')
brandcount$percent<- round(prop.table(brandcount$freq)*100,digits=2)
brandcount<- transform(brandcount, BRAND = reorder(BRAND, freq))
ggplot(brandcount, aes(x = BRAND   , y = freq))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = percent), vjust = -0.3)

#Most common sub-desc of top departments:
groceryprod<- prod%>% dplyr::filter(DEPARTMENT %in% c("GROCERY")) 
grocercount<- count(groceryprod, 'COMMODITY_DESC ')
grocercount$percent<- round(prop.table(grocercount$freq)*100,digits=2)
grocercount<-arrange(grocercount,desc(freq))
grocercount<- head(grocercount,10)
grocercount<- transform(grocercount, COMMODITY_DESC = reorder(COMMODITY_DESC, freq))
ggplot(grocercount, aes(COMMODITY_DESC, freq),ylim=c(0,30)) +
  geom_col(aes(y = 100), fill = "grey70") +
  geom_col(fill = "navyblue") +
  coord_flip()

drugprod<- prod%>% dplyr::filter(DEPARTMENT %in% c("DRUG GM")) 
drugcount<- count(drugprod, 'COMMODITY_DESC ')
drugcount$percent<- round(prop.table(drugcount$freq)*100,digits=2)
drugcount<-arrange(drugcount,desc(freq))
drugcount<- head(drugcount,10)
drugcount<- transform(drugcount, COMMODITY_DESC = reorder(COMMODITY_DESC, freq))
ggplot(drugcount, aes(COMMODITY_DESC, freq),ylim=c(0,30)) +
  geom_col(aes(y = 100), fill = "grey70") +
  geom_col(fill = "navyblue") +
  coord_flip()

###################################################################################
#Transactional data

#Retail discount value: Transfer positive discounts to negative
trans[trans$RETAIL_DISC > 0,"RETAIL_DISC"] <- -(trans[trans$RETAIL_DISC > 0,"RETAIL_DISC"])

#Transtime: convert to hour form
trans$TRANS_TIME <- as.numeric(trans$TRANS_TIME)
h <- floor(trans$TRANS_TIME/100)
trans$TRANS_TIME <- h
trans$TRANS_TIME <- as.character(trans$TRANS_TIME)


#Products sold in terms of quantity
productsold<- aggregate(trans$QUANTITY, by=list(PRODUCT_ID=trans$PRODUCT_ID), FUN=sum)#92,353 unique products
quantile(productsold$x, probs = c(0.25, 0.5, 0.75, 0.95, 1))# Majority (Up to 95% / 87,000 products) are sold under 147 times. 
topsellers<- subset(productsold,productsold$x>1000)#Top selling products

#Link with product data 
topsellers<- inner_join(topsellers,prod,by="PRODUCT_ID")


#Linking transaction and causal
admail <- inner_join(trans,causal, by=c("PRODUCT_ID","STORE_ID","WEEK_NO"))#Linking transactions which products that are advertised or mailed
noadmail<- anti_join(trans,causal, by=c("PRODUCT_ID","STORE_ID","WEEK_NO"))##Linking transactions which products are not advertised or mailed



##############################################################################################
###Classification data creation

###Generate predictors
#Mean basekt value:  How much does a household spend per trip, on average? 
transprod<- inner_join(trans,prod,by="PRODUCT_ID")
transprod$SALES_ADJUSTED<- (transprod$SALES_VALUE+transprod$COUPON_DISC)
transvalue<- transprod%>% dplyr::select(c(household_key,BASKET_ID,SALES_ADJUSTED))
basketval<- aggregate(SALES_ADJUSTED ~ household_key + BASKET_ID, data=transvalue, FUN=sum)
householdval<-aggregate(basketval[,3], list(basketval$household_key), mean)
householdval<- householdval %>% dplyr::rename(household_key = "Group.1") %>% dplyr::rename(MEAN_BASKET_VALUE = "x") 
householdval<- as.data.frame(householdval)
householdval[,'MEAN_BASKET_VALUE']=round(householdval[,'MEAN_BASKET_VALUE'],2)
summary(householdval$MEAN_BASKET_VALUE)

#Spending per week: How much does each customer spend per week, on average? (HHID, WEEK_NO,SALES_ADJSUTED)
weeksales<- transprod %>% dplyr::select(household_key,WEEK_NO,SALES_ADJUSTED)
weekspend<- aggregate(SALES_ADJUSTED ~ household_key + WEEK_NO, data=weeksales, FUN=sum)#Total spending per week
weekspend2<-aggregate(weekspend[,3], list(weekspend$household_key), mean)
weekspend2<- weekspend2 %>% dplyr::rename(household_key = "Group.1") %>% dplyr::rename(WEEK_SPEND = "x") 
weekspend2<- as.data.frame(weekspend2)
weekspend2[,'WEEK_SPEND']=round(weekspend2[,'WEEK_SPEND'],2)#Average spending per week
summary(weekspend2$WEEK_SPEND)

#Total trips: how many times does each customer shop in the supermarket in total (HHID,BasketID)
trips<- transprod %>% dplyr::select(c(household_key,BASKET_ID))
tripsum<- ddply(trips,~household_key,summarise,TRIP_SUM=length(unique(BASKET_ID)))
summary(tripsum$TRIP_SUM)
histogram(~TRIP_SUM,data=tripsum)

mostfrequent<- subset(tripsum,tripsum$TRIP_SUM>300)
mostfrequent<- inner_join(mostfrequent, demo, by="household_key")

#Trips per week: How many times each customer shop per week on average (HHID,WEEK_NO,Baketid)
tripweek<- transprod %>% dplyr::select(household_key,WEEK_NO,BASKET_ID)
tripperweek<- tripweek %>%
  dplyr::group_by(household_key,WEEK_NO)%>%
  dplyr::summarise(UNIQUE_TRIPS = n_distinct(BASKET_ID))
tripperweek2<-aggregate(tripperweek[,3], list(tripperweek$household_key), mean)
tripperweek2<- tripperweek2 %>% dplyr::rename(household_key = "Group.1") %>% dplyr::rename(TRIPS_PER_WEEK = "UNIQUE_TRIPS") 
tripperweek2<- as.data.frame(tripperweek2)
tripperweek2[,'TRIPS_PER_WEEK']=round(tripperweek2[,'TRIPS_PER_WEEK'],0)#Average spending per week

#Combine sumtrip and tripperweek2 for trip information for each customer
custtrip<- inner_join(tripsum,tripperweek2,by="household_key")
#Determine frequent customer
freqcust<- subset(custtrip,custtrip$TRIP_SUM>100)#Customers with trips>100
freqcust<-freqcust[,-c(2,3)]
freqcust<-as.data.frame(freqcust)
freqcust$FREQUENT<- "YES"
freqcust<- freqcust%>% dplyr::rename(household_key = "freqcust")
custtrip<- left_join(custtrip,freqcust,by="household_key")
custtrip[is.na(custtrip)] <- "NO"
describe(custtrip)

#Average transaction time: What time do customers shop on average (HHID,BASKET,TIME)
transtime<-transprod %>% dplyr::select(household_key,BASKET_ID,TRANS_TIME)
transtime2<-distinct(transtime)
transtime2<- transtime2[,-c(2)]
transtime2$TRANS_TIME<- as.numeric(transtime2$TRANS_TIME)
transtime2$TRANS_TIME<-cut(transtime2$TRANS_TIME, breaks = c(-Inf,6,12,18,Inf),labels=c("Early Morning","Morning","Afternoon","Evening"))
freq(transtime2, "TRANS_TIME")

frequenttime<- transtime2 %>% 
  group_by(household_key, TRANS_TIME) %>%
  dplyr::mutate(n = n()) %>%
  group_by(household_key) %>%
  slice(which.max(n)) %>%
  dplyr::select(-n)%>%
  dplyr::rename(FREQUENT_TIME = "TRANS_TIME")
freq(frequenttime, "FREQUENT_TIME")

##Create dataset with all customer's buying behaviour and demographic data
#Customer behaviour: Which products does each customer buy frequently
purchasebehaviour<- transprod %>% 
  group_by(household_key,QUANTITY,DEPARTMENT, BRAND, COMMODITY_DESC,SUB_COMMODITY_DESC) %>%
  dplyr::mutate(n = n()) %>%
  group_by(household_key) %>%
  slice(which.max(n)) %>%
  dplyr::select(-n)


#Remove irrelevant columns and join with mean baket value
purchasebehaviour<- purchasebehaviour[,-c(2,3,4,6,7,8,9,10,11,12)]
householdval<- inner_join(householdval, weekspend2,by="household_key")#Merge weekspend and average spend
custtrip<- inner_join(custtrip, frequenttime,by="household_key")#Merge with customer trip info:
addin<- inner_join(householdval,custtrip,by="household_key")
purchasebehaviour<- inner_join(purchasebehaviour,addin, by="household_key")


##Generate extra data for demographic
#Find household keys with no demo data
nodemo<- anti_join(purchasebehaviour, demo, by = "household_key")#Extract household that has transaction data but no household data
nodemo<- nodemo %>% dplyr::select(household_key)#household with no demo data
nodemo<-as.data.frame(nodemo)
#synthetic prep and creation
na.list <- list(MARITAL_STATUS_CODE = c(NA,"U"), HOMEOWNER_DESC = c(NA, "Unknown"))
synthdat<- demo[,-c(1)]
synth1<- syn(synthdat,k=1699,cont.na = na.list,seed=1234)
nodemo1<- as.data.frame(synth1$syn)
#Check distribution with original data
mycols <- c("darkmagenta", "turquoise","blue")
compare(synth1, demo, nrow = 3, ncol = 3, cols = mycols)$plot
#demographic data of the remaining household
nodemo<-cbind(nodemo,nodemo1)

#Merge synthesized data with original data, giving the full demo data. Merge with purchase behaviour data
demofull<- rbind(demo,nodemo)#merge synthesized with og
anti_join(demofull, purchasebehaviour, by = "household_key")#Check to see if it is a match
fulldata<- inner_join(demofull,purchasebehaviour, by= "household_key")#Full customer infrormation dataset



#final data: 
camp18<- campaign %>% dplyr::filter(CAMPAIGN %in% c("18")) #People who are sent campaign 13
camp18<- camp18[,-c(3)]
redem18<- redem %>% dplyr::filter(CAMPAIGN %in% c("18"))
redem18<- redem18 %>% dplyr::select(household_key,CAMPAIGN)
redem18<- distinct(redem18)
redem18$participate<- "1"
redem18<- redem18[,-c(2)]#People who redeem campaign 13

final<- inner_join(camp18,fulldata, by="household_key")
final2<- left_join(final,redem18, by="household_key")
final2[is.na(final2)] <- 0
final2<- final2[,-c(1,15)]
final2<- final2[,-c(1)]
view(final2)

#final data (all cus call campaign)
redemall<- redem %>% dplyr::select(household_key)
redemall<- distinct(redemall)
redemall$participate<- redemall$participate<- "1"

final<- left_join(fulldata,redemall, by="household_key")
final[is.na(final)] <- 0
final<- final[,-c(1,14)]
view(final)

### Final data preprocessing
##Preprocessing
#Scale numeric variables
cols <- c("QUANTITY","MEAN_BASKET_VALUE","WEEK_SPEND","TRIP_SUM","TRIPS_PER_WEEK")
final2[cols] <- lapply(final2[cols], log)
final2[,c(7,13:16)]<- scale(final2[,c(7,13:16)], center = TRUE, scale = TRUE)
final2$participate<- as.factor(final2$participate)

################################################################################################
#Further exploration
#Type of product that is most liked by households
hhdep<- count(final2, 'DEPARTMENT ')
hhdep$percent<- round(prop.table(hhdep$freq)*100,digits=2)
hhdep<-arrange(hhdep,desc(freq))
hhdep<- head(hhdep,5)
hhdep<- transform(hhdep, DEPARTMENT = reorder(DEPARTMENT, freq))
ggplot(hhdep, aes(DEPARTMENT, freq),ylim=c(0,30)) +
  geom_col(aes(y = 100), fill = "grey70") +
  geom_col(fill = "navyblue") +
  coord_flip()

hhbrand<- count(final2, 'BRAND ')
hhbrand$percent<- round(prop.table(hhbrand$freq)*100,digits=2)
hhbrand<- transform(hhbrand, BRAND = reorder(BRAND, freq))
ggplot(hhbrand, aes(BRAND, freq),ylim=c(0,30)) +
  geom_col(aes(y = 100), fill = "grey70") +
  geom_col(fill = "#0073C2FF") 

#Numeric variables for each age group
hhage<- final2%>%dplyr::select(AGE_DESC,MEAN_BASKET_VALUE,WEEK_SPEND,TRIP_SUM,TRIPS_PER_WEEK)
#Age group with highest spending per trip on average
hhagemeanbasket<- aggregate( MEAN_BASKET_VALUE ~ AGE_DESC, hhage, mean )
hhagemeanbasket$MEAN_BASKET_VALUE = round(hhagemeanbasket[,'MEAN_BASKET_VALUE'],2)

p1<- ggplot(hhagemeanbasket, aes(x=AGE_DESC, y=MEAN_BASKET_VALUE)) +
  geom_segment( aes(x=AGE_DESC, xend=AGE_DESC, y=0, yend=MEAN_BASKET_VALUE)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) 
#Age group with highest spending per week
hhageweekspend<- aggregate( WEEK_SPEND ~ AGE_DESC, hhage, mean )
hhageweekspend$WEEK_SPEND = round(hhageweekspend[,'WEEK_SPEND'],2)

p2<-ggplot(hhageweekspend, aes(x=AGE_DESC, y=WEEK_SPEND)) +
  geom_segment( aes(x=AGE_DESC, xend=AGE_DESC, y=0, yend=WEEK_SPEND)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) 

#Age group with highest trips total
hhagetripsum<- aggregate( TRIP_SUM ~ AGE_DESC, hhage, mean )
hhagetripsum$TRIP_SUM = round(hhagetripsum[,'TRIP_SUM'],0)
ggplot(hhagetripsum, aes(x = AGE_DESC   , y = TRIP_SUM))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = TRIP_SUM), vjust = -0.3)
p3<- ggplot(hhagetripsum, aes(x=AGE_DESC, y=TRIP_SUM)) +
  geom_segment( aes(x=AGE_DESC, xend=AGE_DESC, y=0, yend=TRIP_SUM)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) 

#Age group with highest trips per week
hhagetripweek<- aggregate( TRIPS_PER_WEEK ~ AGE_DESC, hhage, mean )
hhagetripweek$TRIPS_PER_WEEK = round(hhagetripweek[,'TRIPS_PER_WEEK'],0)
p4<- ggplot(hhagetripweek, aes(x = AGE_DESC   , y = TRIPS_PER_WEEK))+
  geom_segment( aes(x=AGE_DESC, xend=AGE_DESC, y=0, yend=TRIPS_PER_WEEK)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) 


#Numeric variables for income groups
#Income group with highest spending per trip on average
hhincomemeanbasket<- aggregate( MEAN_BASKET_VALUE ~ INCOME_DESC, final2, mean )
hhincomemeanbasket$MEAN_BASKET_VALUE = round(hhincomemeanbasket[,'MEAN_BASKET_VALUE'],2)
p1<- ggplot(hhincomemeanbasket, aes(x = INCOME_DESC   , y = MEAN_BASKET_VALUE))+
  geom_segment( aes(x=INCOME_DESC, xend=INCOME_DESC, y=0, yend=MEAN_BASKET_VALUE)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) 
#Income group with highest spending per week
hhincomeweekspend<- aggregate( WEEK_SPEND ~ INCOME_DESC, final2, mean )
hhincomeweekspend$WEEK_SPEND = round(hhincomeweekspend[,'WEEK_SPEND'],2)
p2<- ggplot(hhincomeweekspend, aes(x = INCOME_DESC   , y = WEEK_SPEND))+
  geom_segment( aes(x=INCOME_DESC, xend=INCOME_DESC, y=0, yend=WEEK_SPEND)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) 
#Income group with highest trips total
hhincometripsum<- aggregate( TRIP_SUM ~ INCOME_DESC, final2, mean )
hhincometripsum$TRIP_SUM = round(hhincometripsum[,'TRIP_SUM'],0)
p3<- ggplot(hhincometripsum, aes(x = INCOME_DESC   , y = TRIP_SUM))+
  geom_segment( aes(x=INCOME_DESC, xend=INCOME_DESC, y=0, yend=TRIP_SUM)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) 
#Income group with highest trips per week
hhincometripweek<- aggregate( TRIPS_PER_WEEK ~ INCOME_DESC, final2, mean )
hhincometripweek$TRIPS_PER_WEEK = round(hhincometripweek[,'TRIPS_PER_WEEK'],0)
p4<- ggplot(hhincometripweek, aes(x = INCOME_DESC   , y = TRIPS_PER_WEEK))+
  geom_segment( aes(x=INCOME_DESC, xend=INCOME_DESC, y=0, yend=TRIPS_PER_WEEK)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) 

#Numeric variables for homeowner groups
#homeowners group with highest spending per trip on average
hhhomemeanbasket<- aggregate( MEAN_BASKET_VALUE ~ HOMEOWNER_DESC, final2, mean )
hhhomemeanbasket$MEAN_BASKET_VALUE = round(hhhomemeanbasket[,'MEAN_BASKET_VALUE'],2)
ggplot(hhhomemeanbasket, aes(x = HOMEOWNER_DESC   , y = MEAN_BASKET_VALUE))+
  geom_segment( aes(x=HOMEOWNER_DESC, xend=HOMEOWNER_DESC, y=0, yend=MEAN_BASKET_VALUE)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) 
#homeowner group with highest spending per week
hhhomeweekspend<- aggregate( WEEK_SPEND ~ HOMEOWNER_DESC, final2, mean )
hhhomeweekspend$WEEK_SPEND = round(hhhomeweekspend[,'WEEK_SPEND'],2)
ggplot(hhhomeweekspend, aes(x = HOMEOWNER_DESC   , y = WEEK_SPEND))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = WEEK_SPEND), vjust = -0.3)
#homeowner group with highest trips total
hhhometripsum<- aggregate( TRIP_SUM ~ HOMEOWNER_DESC, final2, mean )
hhhometripsum$TRIP_SUM = round(hhhometripsum[,'TRIP_SUM'],0)
ggplot(hhhometripsum, aes(x = HOMEOWNER_DESC   , y = TRIP_SUM))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = TRIP_SUM), vjust = -0.3)
#homeowner group with highest trips per week
hhhometripweek<- aggregate( TRIPS_PER_WEEK ~ HOMEOWNER_DESC, final2, mean )
hhhometripweek$TRIPS_PER_WEEK = round(hhhometripweek[,'TRIPS_PER_WEEK'],0)
ggplot(hhhometripweek, aes(x = HOMEOWNER_DESC   , y = TRIPS_PER_WEEK))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = TRIPS_PER_WEEK), vjust = -0.3)

#Numeric variables for adult num groups
#adult num group with highest spending per trip on average
hhadultmeanbasket<- aggregate( MEAN_BASKET_VALUE ~ ADULT_NUM, final2, mean )
hhadultmeanbasket$MEAN_BASKET_VALUE = round(hhadultmeanbasket[,'MEAN_BASKET_VALUE'],2)
ggplot(hhadultmeanbasket, aes(x = ADULT_NUM   , y = MEAN_BASKET_VALUE))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = MEAN_BASKET_VALUE), vjust = -0.3)
#adult num group with highest spending per week
hhadultweekspend<- aggregate( WEEK_SPEND ~ ADULT_NUM, final2, mean )
hhadultweekspend$WEEK_SPEND = round(hhadultweekspend[,'WEEK_SPEND'],2)
ggplot(hhadultweekspend, aes(x = ADULT_NUM   , y = WEEK_SPEND))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = WEEK_SPEND), vjust = -0.3)
#adult num group with highest trips total
hhadulttripsum<- aggregate( TRIP_SUM ~ ADULT_NUM, final2, mean )
hhadulttripsum$TRIP_SUM = round(hhadulttripsum[,'TRIP_SUM'],0)
ggplot(hhadulttripsum, aes(x = ADULT_NUM   , y = TRIP_SUM))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = TRIP_SUM), vjust = -0.3)
#adult num group with highest trips per week
hhadulttripweek<- aggregate( TRIPS_PER_WEEK ~ ADULT_NUM, final2, mean )
hhadulttripweek$TRIPS_PER_WEEK = round(hhadulttripweek[,'TRIPS_PER_WEEK'],0)
ggplot(hhadulttripweek, aes(x = ADULT_NUM   , y = TRIPS_PER_WEEK))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = TRIPS_PER_WEEK), vjust = -0.3)

#Numeric variables for income groups
#Income group with highest spending per trip on average
hhchildmeanbasket<- aggregate( MEAN_BASKET_VALUE ~ KID_CATEGORY_DESC, final2, mean )
hhchildmeanbasket$MEAN_BASKET_VALUE = round(hhchildmeanbasket[,'MEAN_BASKET_VALUE'],2)
ggplot(hhchildmeanbasket, aes(x = KID_CATEGORY_DESC   , y = MEAN_BASKET_VALUE))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = MEAN_BASKET_VALUE), vjust = -0.3)
#Income group with highest spending per week
hhchildweekspend<- aggregate( WEEK_SPEND ~ KID_CATEGORY_DESC, final2, mean )
hhchildweekspend$WEEK_SPEND = round(hhchildweekspend[,'WEEK_SPEND'],2)
ggplot(hhchildweekspend, aes(x = KID_CATEGORY_DESC   , y = WEEK_SPEND))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = WEEK_SPEND), vjust = -0.3)
#Income group with highest trips total
hhchildtripsum<- aggregate( TRIP_SUM ~ KID_CATEGORY_DESC, final2, mean )
hhchildtripsum$TRIP_SUM = round(hhchildtripsum[,'TRIP_SUM'],0)
ggplot(hhchildtripsum, aes(x = KID_CATEGORY_DESC   , y = TRIP_SUM))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = TRIP_SUM), vjust = -0.3)
#Income group with highest trips per week
hhchildtripweek<- aggregate( TRIPS_PER_WEEK ~ KID_CATEGORY_DESC, final2, mean )
hhchildtripweek$TRIPS_PER_WEEK = round(hhchildtripweek[,'TRIPS_PER_WEEK'],0)
ggplot(hhchildtripweek, aes(x = KID_CATEGORY_DESC   , y = TRIPS_PER_WEEK))+
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = TRIPS_PER_WEEK), vjust = -0.3)

################################################################################################
##Marketing effectiveness: Campaigns 18,13 and 8
weekrange<-read.csv("week_range.csv")
#Find start and end week of each campaign
camptimextra <- sqldf("SELECT l.*, r.`Week.no`
       FROM camptime as l
       LEFT JOIN weekrange as r
       ON  l.START_DAY BETWEEN r.`Start` AND r.`End`")
camptimextra<- camptimextra %>% dplyr::rename(START_WEEK = "Week.no")
camptimextra <- sqldf("SELECT l.*, r.`Week.no`
       FROM camptimextra as l
       LEFT JOIN weekrange as r
       ON  l.END_DAY BETWEEN r.`Start` AND r.`End`")
camptimextra<- camptimextra %>% dplyr::rename(END_WEEK = "Week.no")

#Campaign 18:
#Spend per week change
camp18<- campaign %>% dplyr::filter(DESCRIPTION %in% c("TypeC"))
otherthan18<- campaign[!(campaign$DESCRIPTION=="TypeC"),]
camp18<- anti_join(camp18,otherthan18,by="household_key")#households that receives only campaign 18 and nothing else. 

transcamp18<- inner_join(camp18,trans, by="household_key")#Transactions of household that are mailed campaign 18
transcamp18<- transcamp18%>% dplyr::select(household_key,WEEK_NO,SALES_VALUE)
weekspendcamp18<- aggregate(SALES_VALUE ~ household_key + WEEK_NO, data=transcamp18, FUN=sum)
weekspendcamp18<-aggregate(weekspendcamp18[,3], list(weekspendcamp18$WEEK_NO), mean)
weekspendcamp18<- weekspendcamp18 %>% dplyr::rename(WEEK_NO = "Group.1") %>% dplyr::rename(WEEKLY_SPENDING = "x") 
weekspendcamp18<- subset(weekspendcamp18,weekspendcamp18$WEEK_NO>27 & weekspendcamp18$WEEK_NO<102 )#Top selling products


rect18 <- data.frame(xmin=84, xmax=92, ymin=-Inf, ymax=Inf)
rect13 <- data.frame(xmin=72, xmax=79, ymin=-Inf, ymax=Inf)
rect8 <- data.frame(xmin=59, xmax=66, ymin=-Inf, ymax=Inf)
rect26 <- data.frame(xmin=32, xmax=38, ymin=-Inf, ymax=Inf)
rect30 <- data.frame(xmin=47, xmax=53, ymin=-Inf, ymax=Inf)



camp18plot<- ggplot(weekspendcamp18, aes(x=WEEK_NO, y=WEEKLY_SPENDING)) +
  geom_line() + geom_point()+
  geom_rect(data=rect3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,fill="campaign 3"),
              alpha=0.1,
              inherit.aes = FALSE) +
  geom_rect(data=rect6, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,fill="campaign 6"),
            alpha=0.1,
            inherit.aes = FALSE)+
  geom_rect(data=rect14, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,fill="campaign 14"),
            alpha=0.1,
            inherit.aes = FALSE)+
  geom_rect(data=rect15, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,fill="campaign 15"),
            alpha=0.1,
            inherit.aes = FALSE)+
  geom_rect(data=rect20, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,fill="campaign 20"),
            alpha=0.1,
            inherit.aes = FALSE)+
  geom_rect(data=rect27, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,fill="campaign 27"),
            alpha=0.1,
            inherit.aes = FALSE)

camp18plot+ scale_fill_manual("Campaign",values=c("red","green","blue","orange","purple","cyan"))





#TRIPS PER WEEK CHANGE: 
tripcamp18<- inner_join(camp18,tripperweek, by="household_key")#trips per week of household that are mailed campaign 18
tripcamp18<- tripcamp18%>% dplyr::select(household_key,WEEK_NO,UNIQUE_TRIPS)
weektripcamp18<-aggregate(tripcamp18[,3], list(tripcamp18$WEEK_NO), mean)
weektripcamp18<- weektripcamp18 %>% dplyr::rename(WEEK_NO = "Group.1") %>% dplyr::rename(WEEKLY_TRIPS = "x") 

camp18tripplot<- ggplot(weektripcamp18, aes(x=WEEK_NO, y=WEEKLY_TRIPS)) +
  geom_line()

camp18tripplot + geom_rect(data=rect3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                           color="red",
                           fill="red",
                           alpha=0.1,
                           inherit.aes = FALSE) +
  geom_rect(data=rect6, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color="green",
            fill="green",
            alpha=0.1,
            inherit.aes = FALSE)+
  geom_rect(data=rect14, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color="blue",
            fill="blue",
            alpha=0.1,
            inherit.aes = FALSE)+
  geom_rect(data=rect15, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color="orange",
            fill="orange",
            alpha=0.1,
            inherit.aes = FALSE)+
  geom_rect(data=rect20, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color="purple",
            fill="purple",
            alpha=0.1,
            inherit.aes = FALSE)

#Campaign 13:
#Spend per week change
camp13<- campaign %>% dplyr::filter(CAMPAIGN %in% c("13"))
transcamp13<- inner_join(camp13,trans, by="household_key")#Transactions of household that are mailed campaign 13
transcamp13<- transcamp13%>% dplyr::select(household_key,WEEK_NO,SALES_VALUE)
weekspendcamp13<- aggregate(SALES_VALUE ~ household_key + WEEK_NO, data=transcamp13, FUN=sum)
weekspendcamp13<-aggregate(weekspendcamp13[,3], list(weekspendcamp13$WEEK_NO), mean)
weekspendcamp13<- weekspendcamp13 %>% dplyr::rename(WEEK_NO = "Group.1") %>% dplyr::rename(WEEKLY_SPENDING = "x") 

camp13plot<- ggplot(weekspendcamp13, aes(x=WEEK_NO, y=WEEKLY_SPENDING)) +
  geom_line()
rect3 <- data.frame(xmin=72, xmax=79, ymin=-Inf, ymax=Inf)
camp13plot + geom_rect(data=rect3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                       color="grey20",
                       alpha=0.5,
                       inherit.aes = FALSE)
#Trips per week change
tripcamp13<- inner_join(camp13,tripperweek, by="household_key")#trips per week of household that are mailed campaign 13
tripcamp13<- tripcamp13%>% dplyr::select(household_key,WEEK_NO,UNIQUE_TRIPS)
weektripcamp13<-aggregate(tripcamp13[,3], list(tripcamp13$WEEK_NO), mean)
weektripcamp13<- weektripcamp13 %>% dplyr::rename(WEEK_NO = "Group.1") %>% dplyr::rename(WEEKLY_TRIPS = "x") 

camp13tripplot<- ggplot(weektripcamp13, aes(x=WEEK_NO, y=WEEKLY_TRIPS)) +
  geom_line()
rect4 <- data.frame(xmin=72, xmax=79, ymin=-Inf, ymax=Inf)
camp13tripplot + geom_rect(data=rect4, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                           color="grey20",
                           alpha=0.5,
                           inherit.aes = FALSE)

#Campaign 8
#Spend per week change
camp8<- campaign %>% dplyr::filter(CAMPAIGN %in% c("8"))
transcamp8<- inner_join(camp8,trans, by="household_key")#Transactions of household that are mailed campaign 8
transcamp8<- transcamp8%>% dplyr::select(household_key,WEEK_NO,SALES_VALUE)
weekspendcamp8<- aggregate(SALES_VALUE ~ household_key + WEEK_NO, data=transcamp8, FUN=sum)
weekspendcamp8<-aggregate(weekspendcamp8[,3], list(weekspendcamp8$WEEK_NO), mean)
weekspendcamp8<- weekspendcamp8 %>% dplyr::rename(WEEK_NO = "Group.1") %>% dplyr::rename(WEEKLY_SPENDING = "x") 

camp8plot<- ggplot(weekspendcamp8, aes(x=WEEK_NO, y=WEEKLY_SPENDING)) +
  geom_line()
rect5 <- data.frame(xmin=59, xmax=66, ymin=-Inf, ymax=Inf)
camp8plot + geom_rect(data=rect5, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                       color="grey20",
                       alpha=0.5,
                       inherit.aes = FALSE)
#Trip difference
tripcamp8<- inner_join(camp8,tripperweek, by="household_key")#trips per week of household that are mailed campaign 8
tripcamp8<- tripcamp8%>% dplyr::select(household_key,WEEK_NO,UNIQUE_TRIPS)
weektripcamp8<-aggregate(tripcamp8[,3], list(tripcamp8$WEEK_NO), mean)
weektripcamp8<- weektripcamp8 %>% dplyr::rename(WEEK_NO = "Group.1") %>% dplyr::rename(WEEKLY_TRIPS = "x") 

camp8tripplot<- ggplot(weektripcamp8, aes(x=WEEK_NO, y=WEEKLY_TRIPS)) +
  geom_line()
rect6 <- data.frame(xmin=59, xmax=66, ymin=-Inf, ymax=Inf)
camp8tripplot + geom_rect(data=rect4, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                           color="grey20",
                           alpha=0.5,
                           inherit.aes = FALSE)

####################################################################################################
### Final data preprocessing
##Preprocessing
#Scale numeric variables
final2$ADULT_NUM<-as.numeric(final2$ADULT_NUM)
final2$KID_CATEGORY_DESC<-as.numeric(final2$KID_CATEGORY_DESC)
cols <- c("ADULT_NUM","KID_CATEGORY_DESC","QUANTITY","MEAN_BASKET_VALUE","WEEK_SPEND","TRIP_SUM","TRIPS_PER_WEEK")
final2[cols] <- lapply(final2[cols], log)
final2[,c(6,7,8,14:17)]<- scale(final2[,c(6,7,8,14:17)], center = TRUE, scale = TRUE)
#Remove household ID for less interference
final2<- final2[,-c(1)]
#Check balance
partifreq<- count(final2, 'participate')
partifreq$percent<- round(prop.table(partifreq$freq)*100,digits=2)
view(partifreq)

#Reduce categories for Department and Commodity_desc
finaldepcount<- count(final2, 'DEPARTMENT')
finaldepcount$percent<- round(prop.table(finaldepcount$freq)*100,digits=2)#Top 3 has most counts
final2<- final2 %>%
  mutate(DEPARTMENT = fct_lump(DEPARTMENT %>% as.factor, n=3))
finalcomcount<- count(final2, 'COMMODITY_DESC')
finalcomcount$percent<- round(prop.table(finalcomcount$freq)*100,digits=2)
final2<- final2 %>%
  mutate(COMMODITY_DESC = fct_lump(COMMODITY_DESC %>% as.factor, n=5))
#Convert frequent to factor
final2$FREQUENT<- as.factor(final2$FREQUENT)
describe(final2)
str(final2)

################################################################################
##Spot testing
#Set parameters
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"

final2$MANUFACTURER<- as.factor(final2$MANUFACTURER)

colnames(final2)<- make.names(colnames(final2))
final2$participate<- as.factor(final2$participate)
final2$FREQUENT<- as.factor(final2$FREQUENT)

#Spot test algorithms
#LDA
set.seed(seed)
fit.lda <- train(participate~., data=final2, method="lda", metric=metric, trControl=control)
#GLM
set.seed(seed)
fit.glm <- train(participate~., data=final2, method="glm", metric=metric, trControl=control)
# NNET
set.seed(seed)
fit.nnet <- train(participate~., data=final2, method="nnet",algorithm = 'backprop', learningrate = 0.1,metric=metric, trControl=control,linout = FALSE)
# SVM Radial
set.seed(seed)
fit.svmRadial <- train(participate~., data=final2, method="svmRadial", metric=metric, trControl=control, fit=FALSE)
# kNN
set.seed(seed)
fit.knn <- train(participate~., data=final2, method="knn", metric=metric, trControl=control)
# CART
set.seed(seed)
fit.cart <- train(participate~., data=final2, method="rpart", metric=metric, trControl=control)
# C5.0
set.seed(seed)
fit.c50 <- train(participate~., data=final2, method="C5.0", metric=metric, trControl=control)
# Bagged CART
set.seed(seed)
fit.treebag <- train(participate~., data=final2, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(participate~., data=final2, method="rf", metric=metric, trControl=control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(participate~., data=final2, method="gbm", metric=metric, trControl=control, verbose=FALSE)

#Results
check <- resamples(list(c50=fit.c50,nnet=fit.nnet))
summary(check)
results <- resamples(list(lda=fit.lda, logistic=fit.glm, nnet=fit.nnet,
                          svm=fit.svmRadial, knn=fit.knn, cart=fit.cart, c50=fit.c50,
                          bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))

summary(results)#Use Random Forest, Bagging and C50. 
################################################################################################################
###Classification: Random Forest
#Seperate into train and test set
## 70% of the sample size
set.seed(7) 
sample = sample.split(findum,SplitRatio = 0.7)
train =subset(findum,sample ==TRUE)
test=subset(findum, sample==FALSE)

#Seperate input and output 
x<- train[,1:42]
y<- train[,43]
xtest<- test[,1:42]
ytest<- test[,43]

#Clarify paratmeters again
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"

#Test algorithm: Default parameters
mtry <- sqrt(ncol(x))#Default mtry is 6
tunegrid <- expand.grid(.mtry=mtry)
set.seed(7)
rf_default <- train(participate~., 
                    data=train, 
                    method='rf', 
                    metric=metric, 
                    tuneGrid=tunegrid, 
                    trControl=control)
print(rf_default)

#Find optimal mtry: random search
control <- trainControl(method="repeatedcv", number=10, repeats=3,search = 'random')
set.seed(7)
rf_random <- train(participate ~ .,
                   data = train,
                   method = 'rf',
                   metric = metric,
                   tuneLength=15,
                   trControl=control)
print(rf_random)
plot(rf_random)#mtry=5 is the best value

#Find optimal ntree: Manual search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(x))))
set.seed(7)
modellist <- list()
for (ntree in c(1000, 1500, 2000, 2500)) {
  set.seed(seed)
  fit <- train(participate~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)#Optimal number of trees: 2000

#Final machine learning model
set.seed(7)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
rforest <- train(participate ~ ., data = train,
                 method = "rf",
                 ntree = 2000,
                 trControl = control,
                 tuneGrid = data.frame(mtry = 5))
print(rforest)
#Predict test data
set.seed(7)
rfmodel <- predict(rforest, xtest)

#Evaluate model
rfmodel<- as.factor(rfmodel)
test$participate<- as.factor(test$participate)
rfperf<- confusionMatrix(rfmodel, ytest, positive = "1")# class value of 1 indicates the classification of participation as "yes"
rfperf #Confusion matrix of model on testing set

#Extra calculations
rfm<- table(rfmodel, test$participate)
rfTP = rfm[2,2] #Correctly predict customer participation
rfFP = rfm[2,1] #Not-interested customer wrongly classified as interested
rfFN = rfm[1,2] #Interested customers wrongly perdicted as not-interested
rfTN = rfm [1,1]#Correctly predict not-interested customers

rfTPR= (rfTP/(rfTP+rfFN))*100 #Sensitivity aka true positive rate
rfTPR
rfFPR= (rfFP/(rfFP+rfTN))*100 #Fall-out aka False Positive Rate
rfFPR
rfFNR=(rfFN/(rfFN+rfTP))*100#False Negative rate
rfFNR
rfTNR=(rfTN/(rfTN+rfFP))*100#True Negative Rate
rfTNR

final2<- final2 %>% 
  mutate(participate = factor(participate, 
                              labels = make.names(levels(participate))))
