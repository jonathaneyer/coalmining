


# ---- start ---------------------------------------------------------------

library(mfx)
library(tidyverse)
library(broom)
library(dplyr)

opts_mine <- readRDS("0-data/opts_mine.rds") %>% 
  filter(distances < 335, distances > 10) %>%
  mutate(bin = factor(floor(distances/10)), Var1 = factor(Var1),
         dist2 = distances^2, dist3 = distances^3, dist4 = distances^4,
         plantmine = paste(Var2, Var3, sep = "_"))
opts_minecounty <- readRDS("0-data/opts_minecounty.rds")%>% 
  filter(distances < 335, distances > 10)%>%
  mutate(bin = factor(floor(distances/10)), Var1 = factor(Var1),
         dist2 = distances^2, dist3 = distances^3, dist4 = distances^4,
         plantmine = paste(Var2, Var3, sep = "_"))

r_mine_1 <- logitmfx(anytrade ~ Var1 +distances  +samestate  + samecongress + mineprod + plantpurch, data = opts_mine,clustervar1 = "plantmine")
r_mine_2 <- logitmfx(anytrade ~ Var1 +distances + dist2  +samestate  + samecongress + mineprod + plantpurch, data = opts_mine,clustervar1 = "plantmine")
r_mine_3 <- logitmfx(anytrade ~ Var1 +distances + dist2 + dist3 +samestate  + samecongress + mineprod + plantpurch, data = opts_mine,clustervar1 = "plantmine")
r_mine_4 <- logitmfx(anytrade ~ Var1 +distances + + dist2 + dist3 + dist4  +samestate  + samecongress + mineprod + plantpurch, data = opts_mine,clustervar1 = "plantmine")
r_mine_bin <- logitmfx(anytrade ~ Var1 +bin  +samestate  + samecongress + mineprod + plantpurch, data = opts_mine,clustervar1 = "plantmine")
r_county_1<- logitmfx(anytrade ~ Var1 +distances +samestate  + samecongress + countyprod + plantpurch_county, data = opts_minecounty,clustervar1 = "plantmine")
r_county_2<- logitmfx(anytrade ~ Var1 +distances + dist2 +samestate  + samecongress + countyprod + plantpurch_county, data = opts_minecounty,clustervar1 = "plantmine")
r_county_3<- logitmfx(anytrade ~ Var1 +distances + dist2 + dist3  +samestate  + samecongress + countyprod + plantpurch_county, data = opts_minecounty,clustervar1 = "plantmine")
r_county_4<- logitmfx(anytrade ~ Var1 +distances + dist2 + dist3 + dist4 +samestate  + samecongress + countyprod + plantpurch_county, data = opts_minecounty,clustervar1 = "plantmine")
r_county_bin<- logitmfx(anytrade ~ Var1 +bin +samestate  + samecongress + countyprod + plantpurch_county, data = opts_minecounty,clustervar1 = "plantmine")

varname_mine_1 = names(r_mine_1$fit$coefficients)[2:length(r_mine_1$fit$coefficients)]
varname_mine_2 = names(r_mine_2$fit$coefficients)[2:length(r_mine_2$fit$coefficients)]
varname_mine_3 = names(r_mine_3$fit$coefficients)[2:length(r_mine_3$fit$coefficients)]
varname_mine_4 = names(r_mine_4$fit$coefficients)[2:length(r_mine_4$fit$coefficients)]
varname_mine_bin = names(r_mine_bin$fit$coefficients)[2:length(r_mine_bin$fit$coefficients)]

varname_county_1 = names(r_county_1$fit$coefficients)[2:length(r_county_1$fit$coefficients)]
varname_county_2 = names(r_county_2$fit$coefficients)[2:length(r_county_2$fit$coefficients)]
varname_county_3 = names(r_county_3$fit$coefficients)[2:length(r_county_3$fit$coefficients)]
varname_county_4 = names(r_county_4$fit$coefficients)[2:length(r_county_4$fit$coefficients)]
varname_county_bin = names(r_county_bin$fit$coefficients)[2:length(r_county_bin$fit$coefficients)]

# ---- reg1 ---------------------------------------------------------------

star_pval <- function(p.value) {
  unclass(symnum(p.value, corr = FALSE, na = FALSE,
                 cutpoints = c(0, 0.01, 0.05, 0.1, 1),
                 symbols = c("***", "**", "*", " ")))
}

r_mine_1 <- data.frame(r_mine_1$mfxest) %>% 
  mutate(model = "Linear", est = paste0(round(dF.dx, 5),
                                        star_pval(P..z.)),
         se = paste0("(", round(Std..Err., 5), ")"),
         varname = varname_mine_1)
r_mine_2 <- data.frame(r_mine_2$mfxest)%>% 
  mutate(model = "Squared", est = paste0(round(dF.dx, 5),
                                         star_pval(P..z.)),
         se = paste0("(", round(Std..Err., 5), ")"),
         varname = varname_mine_2)
r_mine_3 <- data.frame(r_mine_3$mfxest)%>% 
  mutate(model = "Cubic", est = paste0(round(dF.dx, 5),
                                       star_pval(P..z.)),
         se = paste0("(", round(Std..Err., 5), ")"),
         varname = varname_mine_3)
r_mine_4 <- data.frame(r_mine_4$mfxest)%>% 
  mutate(model = "Quartic", est = paste0(round(dF.dx, 5),
                                         star_pval(P..z.)),
         se = paste0("(", round(Std..Err., 5), ")"),
         varname = varname_mine_4)
r_mine_bin <- data.frame(r_mine_bin$mfxest)%>% 
  mutate(model = "Bin", est = paste0(round(dF.dx, 5),
                                     star_pval(P..z.)),
         se = paste0("(", round(Std..Err., 5), ")"),
         varname = varname_mine_bin)

allmodels <- bind_rows(r_mine_3,r_mine_4,r_mine_bin) 

regtable <- allmodels %>% select(model, varname, est,se ) %>%
  filter(varname %in% c("samestateTRUE","samecongressTRUE","mineprod","plantpurch")) %>%
  select(model, varname, est,se) %>% 
  gather(key, value, est:se) %>% 
  spread(model, value) %>% 
  mutate(varname = c("Mine Production", "Mine Production","Plant Purchases","Plant Purchases","Same Congressional District","Same Congressional District","Same State","Same State"))

# Remove every other term
regtable$varname[seq(2, nrow(regtable), 2)] <- ""

names(regtable)[1] <- " "

regtable %>% select(-key) %>%
  kable(caption = "Effect of State and Congressional Boundries on Trades between Plants and Mines")

# ---- reg2 ---------------------------------------------------------------

star_pval <- function(p.value) {
  unclass(symnum(p.value, corr = FALSE, na = FALSE,
                 cutpoints = c(0, 0.01, 0.05, 0.1, 1),
                 symbols = c("***", "**", "*", " ")))
}

r_county_1 <- data.frame(r_county_1$mfxest) %>% 
  mutate(model = "Linear", est = paste0(round(dF.dx, 5),
                                        star_pval(P..z.)),
         se = paste0("(", round(Std..Err., 5), ")"),
         varname = varname_county_1)
r_county_2 <- data.frame(r_county_2$mfxest)%>% 
  mutate(model = "Squared", est = paste0(round(dF.dx, 5),
                                         star_pval(P..z.)),
         se = paste0("(", round(Std..Err., 5), ")"),
         varname = varname_county_2)
r_county_3 <- data.frame(r_county_3$mfxest)%>% 
  mutate(model = "Cubic", est = paste0(round(dF.dx, 5),
                                       star_pval(P..z.)),
         se = paste0("(", round(Std..Err., 5), ")"),
         varname = varname_county_3)
r_county_4 <- data.frame(r_county_4$mfxest)%>% 
  mutate(model = "Quartic", est = paste0(round(dF.dx, 5),
                                         star_pval(P..z.)),
         se = paste0("(", round(Std..Err., 5), ")"),
         varname = varname_county_4)
r_county_bin <- data.frame(r_county_bin$mfxest)%>% 
  mutate(model = "Bin", est = paste0(round(dF.dx, 5),
                                     star_pval(P..z.)),
         se = paste0("(", round(Std..Err., 5), ")"),
         varname = varname_county_bin)

allmodels <- bind_rows(r_county_3,r_county_4,r_county_bin) 

regtable <- allmodels %>% select(model, varname, est,se ) %>%
  filter(varname %in% c("samestateTRUE","samecongressTRUE","countyprod","plantpurch_county")) %>%
  select(model, varname, est,se) %>% 
  gather(key, value, est:se) %>% 
  spread(model, value) %>% 
  mutate(varname = c("county Production", "county Production","Plant Purchases","Plant Purchases","Same Congressional District","Same Congressional District","Same State","Same State"))

# Remove every other term
regtable$varname[seq(2, nrow(regtable), 2)] <- ""

names(regtable)[1] <- " "

regtable %>% select(-key) %>%
  kable(caption = "Effect of State and Congressional Boundries on Trades between Plants and Counties")

# ---- reg3 ---------------------------------------------------------------

require('readstata13')
require('Hmisc')
library(tidyverse)


eyer3 <- read.dta13("C:/Users/Jonathan/Downloads/eyer3.dta") %>% 
  mutate(statefips = floor(fips/1000))


countypairs <- unique(eyer3$pair[eyer3$statefips %in% floor(opts_mine$coalminefips/1000)])

opts_mine <-apply(as.matrix(countypairs),1, function(x)
{opts_mine[opts_mine$fips %in% eyer3$fips[eyer3$pair == x] & 
             floor(opts_mine$coalminefips/1000) %in% floor(eyer3$fips[eyer3$pair==x]/1000),]}) %>%
  ldply(data.frame)

for(i in 1:length(countypairs)){
  uniqmines <- unique(opts_mine$coalminefips[floor(opts_mine$coalminefips/1000) %in% floor(eyer3$fips[eyer3$pair==countypairs[i]]/1000)])
  for(j in 1:length(uniqmines)){
    opts_mine %<>% cbind(opts_mine$fips %in% eyer3$fips[eyer3$pair==countypairs[i]] & opts_mine$coalminefips ==uniqmines[j])
    colnames(opts_mine)[ncol(opts_mine)] <- paste("var",i,"_",j, sep = "" )
  }
}

r <- lm(anytrade ~ samestate + samecongress + factor(coalminefips)*var1+factor(coalminefips)*var2+factor(coalminefips)*var3+factor(coalminefips)*var4+factor(coalminefips)*var5+factor(coalminefips)*var6+factor(coalminefips)*var7+factor(coalminefips)*var8+factor(coalminefips)*var9+  
          factor(coalminefips)*var10+factor(coalminefips)*var11+factor(coalminefips)*var12+factor(coalminefips)*var13+factor(coalminefips)*var14+factor(coalminefips)*var15+factor(coalminefips)*var16+factor(coalminefips)*var17+factor(coalminefips)*var18 +
          factor(coalminefips)*var19+factor(coalminefips)*var20+factor(coalminefips)*var21+factor(coalminefips)*var22+factor(coalminefips)*var23+factor(coalminefips)*var24+factor(coalminefips)*var25+factor(coalminefips)*var26+factor(coalminefips)*var27 +
          factor(coalminefips)*var28+factor(coalminefips)*var29+factor(coalminefips)*var30+factor(coalminefips)*var31+factor(coalminefips)*var32+factor(coalminefips)*var33+factor(coalminefips)*var34+factor(coalminefips)*var35+factor(coalminefips)*var36 +
          factor(coalminefips)*var37+factor(coalminefips)*var38+factor(coalminefips)*var39+factor(coalminefips)*var40+factor(coalminefips)*var41+factor(coalminefips)*var42+factor(coalminefips)*var43+factor(coalminefips)*var44+factor(coalminefips)*var45 +
          factor(coalminefips)*var46+factor(coalminefips)*var47+factor(coalminefips)*var48+factor(coalminefips)*var49+factor(coalminefips)*var50+factor(coalminefips)*var51+factor(coalminefips)*var52+factor(coalminefips)*var53+factor(coalminefips)*var54 +
          factor(coalminefips)*var55+factor(coalminefips)*var56+factor(coalminefips)*var57+factor(coalminefips)*var58+factor(coalminefips)*var59+factor(coalminefips)*var60+factor(coalminefips)*var61+factor(coalminefips)*var62+factor(coalminefips)*var63 +
          factor(coalminefips)*var64+factor(coalminefips)*var65+factor(coalminefips)*var66+factor(coalminefips)*var67+factor(coalminefips)*var68+factor(coalminefips)*var69+factor(coalminefips)*var70+factor(coalminefips)*var71+factor(coalminefips)*var72 +
          factor(coalminefips)*var73+factor(coalminefips)*var74+factor(coalminefips)*var75+factor(coalminefips)*var76+factor(coalminefips)*var77+factor(coalminefips)*var78+factor(coalminefips)*var79+factor(coalminefips)*var80+factor(coalminefips)*var81 +
          factor(coalminefips)*var82+factor(coalminefips)*var83+factor(coalminefips)*var84+factor(coalminefips)*var85+factor(coalminefips)*var86+factor(coalminefips)*var87+factor(coalminefips)*var88+factor(coalminefips)*var89+factor(coalminefips)*var90 +
          factor(coalminefips)*var91+factor(coalminefips)*var92+factor(coalminefips)*var93+factor(coalminefips)*var94+factor(coalminefips)*var95+factor(coalminefips)*var96+factor(coalminefips)*var97+factor(coalminefips)*var98+factor(coalminefips)*var99 +
          factor(coalminefips)*var100+factor(coalminefips)*var101+factor(coalminefips)*var102+factor(coalminefips)*var103+factor(coalminefips)*var104+factor(coalminefips)*var105+factor(coalminefips)*var106+factor(coalminefips)*var107+factor(coalminefips)*var108+
          factor(coalminefips)*var109+factor(coalminefips)*var110+factor(coalminefips)*var111+factor(coalminefips)*var112+factor(coalminefips)*var113+factor(coalminefips)*var114+factor(coalminefips)*var115+factor(coalminefips)*var116+factor(coalminefips)*var117+
          factor(coalminefips)*var118+factor(coalminefips)*var119+factor(coalminefips)*var120+factor(coalminefips)*var121+factor(coalminefips)*var122+factor(coalminefips)*var123+factor(coalminefips)*var124, data = opts_mine)

