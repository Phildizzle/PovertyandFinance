##
##
##
##
# Final code summary (do-file) for the paper "Financial development and poverty", ordered after eesults for each paper sections
# Read datafile as data frame "df" 
df <-read.delim2("C:\\Users\\user1\\Desktop\\Final Data.txt", header = TRUE, blank.lines.skip = FALSE, dec = ",")
# Read correlation-prepared data
cf <-read.delim2("C:\\Users\\user1\\Desktop\\Corr Data.txt", header = TRUE, blank.lines.skip = FALSE, dec = ",")

# Open necessary packages if not installed please do so in advance

library("plm")
library("stargazer")
library("SemiPar")

# 4. Data section
# Table 1. for Descriptive Statistics
stargazer(cf, type = "text", title="Descriptive statistics", style= "qje", digits=2, iqr=TRUE, notes = "Note: This table contains descriptive statistics calculated for all variables in the analysis."
          ,covariate.labels=c("Poverty (% of population)","Domestic private credit (% of GDP)",
                              "Bank liquid to assets ratio","Market cap. of dom. companies (% of GDP)",
                              "GDP per capita growth (annual %)","Government expenditures (% of GDP)",
                              "Inflation, GDP deflator (annual %)","Foreign direct investment (% of GDP)","Prevalence of HIV (% of population)", "R&D expenditure (% of GDP)", "Natural resources rent (% of GDP)",
                              "Population"))

# Table 2. for correlation matrix

corr_matrix <- cor(cf, use= "pairwise.complete.obs", method="pearson")
colnames(corr_matrix) <- c("Poverty", "Domcredit", "Bankreserves", "Marketcap", "Gdpcap", "Gov", "Infl", "FDI", "HIV", "R&D", "Natres", "Pop")
rownames(corr_matrix) <- c("Poverty", "Domcredit", "Bankreserves", "Marketcap", "Gdpcap", "Gov", "Infl", "FDI", "HIV", "R&D", "Natres", "Pop")
corr_matrix[lower.tri(corr_matrix)] <- NA

stargazer(corr_matrix, type="latex", digits=2, title="Correlation matrix", notes = "Note: This table contains Pearson-correlations calculated for all variables in the analysis.")

#Alternative  Figure 1. Correlation Heatmap
library("corrplot")
corrplot(corr_matrix, method="square", tl.cex=.9, tl.col ="black", number.cex = 0.5)

# 5. Results section
# 5.1 Main results
# Table 3. Results for linear dynamic panel 
plmlin1 <- plm(formula = pov3 ~ domcredit, data = df, index=c("country", "year"), model="within")
plmlin2 <- plm(formula = pov3 ~ domcredit + gov, data = df, index=c("country", "year"), model="within")
plmlin3 <- plm(formula = pov3 ~ domcredit + gov + infl , data = df, index=c("country", "year"), model="within")
plmlin4 <- plm(formula = pov3 ~ domcredit + gov + infl + fdi + hiv + gdpcap, data = df, index=c("country", "year"), model="within")
plmlin5 <- plm(formula = pov3 ~ domcredit + gov + infl + fdi + hiv + gdpcap + natres + log(pop), data = df, index=c("country", "year"), model="within")

stargazer(plmlin1,plmlin2,plmlin3,plmlin4,plmlin5, type="text", style ="qje", dep.var.labels  = "Poverty", covariate.labels = c("Domcredit", "Gov", "Infl", "FDI", "HIV", "GDPCAP", "Natres", "ln(Pop)"))

# Table 4. Results for dynamic panel
plmquad1 <- plm(formula = pov3 ~ domcredit + I(domcredit^2), data = df, index=c("country", "year"), model="within")
plmquad2 <- plm(formula = pov3 ~ domcredit + I(domcredit^2) + gov, data = df, index=c("country", "year"), model="within")
plmquad3 <- plm(formula = pov3 ~ domcredit + I(domcredit^2) + gov + infl, data = df, index=c("country", "year"), model="within")
plmquad4 <- plm(formula = pov3 ~ domcredit + I(domcredit^2) + gov + infl + fdi + hiv + gdpcap, data = df, index=c("country", "year"), model="within")
plmquad5 <- plm(formula = pov3 ~ domcredit + I(domcredit^2) + gov + infl + fdi + hiv + gdpcap  + natres + log(pop), data = df, index=c("country", "year"), model="within")

stargazer(plmquad1,plmquad2,plmquad3,plmquad4,plmquad5, type="text", digits = 5, style ="qje", dep.var.labels  = "Poverty", covariate.labels = c("Domcredit", "Domcredit2", "Gov", "Infl", "FDI", "HIV", "GDPCAP", "Natres", "ln(Pop)"))

# Table 5. IV Results for dynamic panel

plmiv <- plm(formula = pov3 ~ domcredit + I(domcredit^2) + gov + infl + fdi + hiv + gdpcap  + natres + log(pop) | lag(domcredit,1) + lag(I(domcredit^2),1:5) +gov + infl + fdi + hiv + gdpcap  + natres + log(pop) , data = df,  index=c("country", "year"), model="within")
plmstand <- plm(formula = pov3 ~ domcredit + I(domcredit^2) + gov + infl + fdi + hiv + gdpcap  + natres + log(pop), data = df, index=c("country", "year"), model="within")

stargazer(plmiv,plmstand, type="latex", style ="qje", dep.var.labels  = "Poverty", covariate.labels = c("Domcredit", "Domcredit2", "Gov", "Infl", "FDI", "HIV", "GDPCAP", "Natres", "ln(Pop)"), column.labels = c("IV", "Panel"))

# Semi-parametric regressions
# Figure 2. Semi-parametric regression
# Open package and load dataset into currrent working memory
attach(df)

# Run regression, output results, and output plot
spm1 <- spm(form = pov3 ~ f(domcredit) + gov + infl + hiv + fdi + natres + log(pop), omit.missing ="TRUE", family="poisson", spar.method = "ML")
summary(spm1)

#give correct dimensions for the semi-parametric output plot
# Plot the semi-parametric regression
par(mar=c(1,1,1,1))
plot(spm1, xlab = "Financial development", ylab = "Poverty")

# 5.2 Robustness check:
# Different measures for financial development (marketcap & bankasset)
# Table 6. Results for quadratic panel with market capitalization
plmquadm1 <- plm(formula = pov3 ~ marketcap + I(marketcap^2), data = df, index=c("country", "year"), model="within")
plmquadm2 <- plm(formula = pov3 ~ marketcap + I(marketcap^2) + gov, data = df, index=c("country", "year"), model="within")
plmquadm3 <- plm(formula = pov3 ~ marketcap + I(marketcap^2) + gov + infl, data = df, index=c("country", "year"), model="within")
plmquadm4 <- plm(formula = pov3 ~ marketcap + I(marketcap^2) + gov + infl + fdi + hiv + gdpcap, data = df, index=c("country", "year"), model="within")
plmquadm5 <- plm(formula = pov3 ~ marketcap + I(marketcap^2) + gov + infl + fdi + hiv + gdpcap + natres + log(pop), data = df, index=c("country", "year"), model="within")

stargazer(plmquadm1,plmquadm2,plmquadm3,plmquadm4,plmquadm5, type="latex", style ="qje", dep.var.labels  = "Poverty", covariate.labels = c("Marketcap", "Marketcap2", "Gov", "Infl", "FDI", "HIV", "GDPCAP", "Natres", "ln(Pop)"))

# Table 7. Results for dynamic panel with bank reserves
plmquadb1 <- plm(formula = pov3 ~ bankreserves + I(bankreserves^2), data = df, index=c("country", "year"), model="within")
plmquadb2 <- plm(formula = pov3 ~ bankreserves + I(bankreserves^2) + gov, data = df, index=c("country", "year"), model="within")
plmquadb3 <- plm(formula = pov3 ~ bankreserves + I(bankreserves^2) + gov + infl, data = df, index=c("country", "year"), model="within")
plmquadb4 <- plm(formula = pov3 ~ bankreserves + I(bankreserves^2) + gov + infl + fdi + hiv + gdpcap, data = df, index=c("country", "year"), model="within")
plmquadb5 <- plm(formula = pov3 ~ bankreserves + I(bankreserves^2) + gov + infl + fdi + hiv + gdpcap + natres + log(pop), data = df, index=c("country", "year"), model="within")

stargazer(plmquadb1,plmquadb2,plmquadb3,plmquadb4,plmquadb5, type="latex", style ="qje", dep.var.labels  = "Poverty", covariate.labels = c("Bankreserves", "Bank reserves 2", "Gov", "Infl", "FDI", "HIV", "GDPCAP", "Natres", "ln(Pop)"))

# Regressions over different time periods
# Table 8. Regressions over different time periods

plmot1 <- plm(formula = pov3 ~ domcredit + I(domcredit^2) + gov + infl + fdi + hiv + gdpcap + natres + log(pop), data = df, index=c("country", "year"), model="within", subset= df$year < 2001)
plmot2 <- plm(formula = pov3 ~ domcredit + I(domcredit^2) + gov + infl + fdi + hiv + gdpcap + natres + log(pop), data = df, index=c("country", "year"), model="within", subset= df$year < 2006 & 1999 < df$year)
plmot3 <- plm(formula = pov3 ~ domcredit + I(domcredit^2) + gov + infl + fdi + hiv + gdpcap + natres + log(pop), data = df, index=c("country", "year"), model="within", subset= df$year < 2011 & 2004 < df$year)
plmot4 <- plm(formula = pov3 ~ domcredit + I(domcredit^2) + gov + infl + fdi + hiv + gdpcap + natres + log(pop), data = df, index=c("country", "year"), model="within")
colnam <- c("1980-2000", "2000-2005", "2005-2010", "1980-2014")

stargazer(plmot1,plmot2,plmot3,plmot4,type="latex", column.labels=colnam, style ="qje", dep.var.labels  = "Poverty", covariate.labels = c("Domcredit", "Domcredit2", "Gov", "Infl", "FDI", "HIV", "GDPCAP", "Natres", "ln(Pop)"))

# Figure 3. for financial development over time in India, China, Nigeria, and South Africa
attach(df)
#For some computers the "Plots" window has to be resized

india <- subset(df, country == "India")
china <- subset(df, country == "China")
ghana <- subset(df, country == "Ghana")
southafrica <- subset(df, country == "South Africa")

par(mfrow=c(2,2))

plot(india$year, india$domcredit, type="l", main="India", xlab="Year", ylab="Financial development")
plot(china$year, china$domcredit, type="l", main="China", xlab="Year", ylab="Financial development")
plot(ghana$year, ghana$domcredit, type="l", main="Ghana", xlab="Year", ylab="Financial development")
plot(southafrica$year, southafrica$domcredit, type="l", main="South Africa", xlab="Year", ylab="Financial development")


# Arellano-Bond model
#subset data set to filter out NAs
sub <- subset(df, sub$year > 1999)
sub1 <- sub[complete.cases(sub$pov3),]
sub11 <- sub1[complete.cases(sub1$domcredit),]

# Estimate full model
z1 <- pgmm(pov3 ~ lag(pov3,1) + lag(domcredit,1) + lag(I(domcredit^2),1) + lag(gov,1) + lag(infl,1) + lag(hiv,1) + lag(gdpcap,1) + lag(fdi,1) + lag(natres,1) + lag(log(pop),1) | lag(pov3,2), data = df, effect = "twoways", model = "twosteps")
summary(z1)

# Model with shortened sample period
z2 <- pgmm(pov3 ~ lag(pov3,1) + lag(domcredit,1:2) + lag(I(domcredit^2),1) + lag(gov,1) + lag(infl,1) + lag(hiv,1) + lag(gdpcap,1) + lag(fdi,1) + lag(natres,1) + lag(log(pop),1) | lag(pov3,2:4), data = sub11, effect = "individual", model = "twosteps")
summary(z1,z2)

#output the output
stargazer(z1, type="latex", dep.var.labels  = "Poverty", column.labels = c("Arellano-Bond"))
covariate.labels = c("Domcredit", "Domcredit2", "Gov", "Infl", "HIV", "GDPCAP", "FDI", "Natres", "ln(Pop)")

#
#
# End of this do-file