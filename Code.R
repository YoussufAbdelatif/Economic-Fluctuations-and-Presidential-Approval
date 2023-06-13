setwd("C:/Users/Yussuf Schwarz/OneDrive/Desktop/UniVWL/411G-Economic Forecasting Methods/Assignments/Termpaper")

############################Termpaper###########################################

library(quantmod) 
library(fBasics)
library(ggplot2)
library(zoo)
library(readxl)
library(kableExtra)
library(stargazer)
library(moments)
library(lubridate)
library(lmtest)
library(forecast)
library(gridExtra)
library(tseries)
library(vars)
library(bruceR)
library(tseries)
library(fGarch)
library(dplyr)
library (glmnet)
library (plotmo)
library(tsDyn)
library(readxl)

############################Data################################################

approval=as.data.frame(read_excel("approval.xlsx"))

Date=c()
for (i in 1:length(approval$`End Date`)) {
  if (nchar(approval$`End Date`[i]) == 5) {
    Date[i] <- format(as.Date(as.numeric(approval$`End Date`[i]), origin = "1899-12-30"))
  } else {
    Date[i] <- format(as.Date(approval$`End Date`[i], format = "%m/%d/%Y"))
  }
}
approval=arrange(approval, (approval$`End Date`))
approval$`End Date`=as.Date(approval$`End Date`,format="%m/%d/%Y")
approval$quarter <-quarters(approval$`End Date`)
approval$year <-year(approval$`End Date`)

app=data.frame(cbind(aggregate(approval$Approving,by = list(approval$quarter,approval$year),FUN=mean),aggregate(approval$Disapproving,by = list(approval$quarter,approval$year),FUN=mean)))
app=app[,c(1,2,3,6)]
colnames(app)=c("quarter","year","approval","disapproval")

pres=aggregate(approval$President,by = list(approval$quarter,approval$year),FUN=max)
app$president=pres$x
app$president[113]="Biden"
app$president[35]="Bush"
app$president[33]="Bush"
pres=pres[1:120,]
app=app[1:120,]
app=data.frame(cbind(app,pres))

getSymbols("USEPUFEARINDX",src="FRED")
migration = ts(USEPUFEARINDX, start = c(1990, 1),end = c(2022,10), frequency = 4)
migration=window(migration,start=c(1993,1),end=c(2022,4))
migration=as.ts(migration)

getSymbols("GFDEGDQ188S",src="FRED")
debt = ts(GFDEGDQ188S, start = c(1966, 1),end = c(2022,10), frequency = 4)
debt=window(debt,start=c(1992,4),end=c(2022,4))
debt=as.ts(debt)
debt=diff(log(debt))

getSymbols("PRS85006091",src="FRED")
productivity = ts(PRS85006091, start = c(1948, 1),end = c(2023,1), frequency = 4)
productivity=window(productivity,start=c(1993,1),end=c(2022,4))
productivity=as.ts(productivity)

getSymbols("GDPC1",src="FRED") 
GDPC = ts(GDPC1, start = c(1947, 1),end = c(2023,1), frequency = 4)
GDPC=diff(log((GDPC)))*100*4
GDPC=window(GDPC,start=c(1993,1),end=c(2022,4))
GDPC=as.ts(GDPC)

getSymbols("UNRATE",src="FRED")
UNRATE = ts(UNRATE, start = c(1948, 1),end = c(2023,4), frequency = 12)
UNRATE=window(UNRATE,start=c(1993,1),end=c(2022,12))
UNRATE=as.ts(UNRATE)
UNRATE = aggregate.ts(UNRATE, nfrequency = 4,FUN = mean)

getSymbols("MEDCPIM158SFRBCLE",src="FRED")
CPI = ts(MEDCPIM158SFRBCLE, start = c(1983, 1),end = c(2023,3), frequency = 12)
CPI=window(CPI,start=c(1993,1),end=c(2022,12))
CPI=as.ts(CPI)
CPI= aggregate.ts(CPI, nfrequency = 4,FUN = mean)

getSymbols("CUSR0000SEHA",src="FRED")
CPIrent = ts(CUSR0000SEHA, start = c(1981, 1),end = c(2023,3), frequency = 12)
CPIrent=diff(log((CPIrent)))*100
CPIrent=window(CPIrent,start=c(1993,1),end=c(2022,12))
CPIrent=as.ts(CPIrent)
CPIrent= aggregate.ts(CPIrent, nfrequency = 4,FUN = mean)

getSymbols("BOPGSTB",src="FRED")
trade = ts(BOPGSTB, start = c(1992, 1),end = c(2023,3), frequency = 12)
trade=window(trade,start=c(1993,1),end=c(2022,12))
trade=as.ts(trade)
trade= aggregate.ts(trade, nfrequency = 4,FUN = mean)

gasprice=read_excel("gas.xls",skip = 10)
gasprice=gasprice[125:1689,]
gasprice$quarter <-quarters(gasprice$observation_date)
gasprice$year <-year(gasprice$observation_date)
gasprice=aggregate(gasprice$GASREGW,by = list(gasprice$quarter,gasprice$year),FUN=mean)
gasprice=gasprice$x

oil=read.csv("oil.csv")
oil=oil$DCOILBRENTEU

recession=read.csv("recession.csv")
recession=recession$USRECD

dollar=read.csv("dollar.csv")
dollar=dollar$DEXCAUS

getSymbols("WUIGLOBALWEIGHTAVG",src="FRED")
WUI = ts(WUIGLOBALWEIGHTAVG, start = c(1990, 1),end = c(2023,1), frequency = 4)
WUI=window(WUI,start=c(1993,1),end=c(2022,4))
WUI=as.ts(WUI)

############################Graphs: Data########################################

library(ggcorrplot)
cordata = cbind(app$approval,GDPC,gasprice,CPI,oil,debt)
colnames(cordata)=c("Approval","Real GDP","Gasprice","Inflation","Oilprice","US Debt Level")
cor1 = cor(cordata)
ggcorrplot(cor1[,5:1], hc.order = FALSE,lab = TRUE,lab_col = "black",outline.col = "white",colors = c("#6D9EC1", "white", "#E46726"))+
  ggtitle("Correlation Matrix between Economic Variables and Presidential Approval")


plot1=ggplot() +
  geom_line(aes(x = as.Date(seq(as.Date("1993-01-01"),as.Date("2022-12-01"),by="quarter"))
, y =app$approval)) +
  geom_rect(aes(xmin=as.Date("1993-01-01"),xmax=as.Date("2001-01-19")
            ,fill="green"),ymin=0,ymax=100, size=0.5, alpha=0.08) +
  geom_rect(aes(xmin=as.Date("2001-01-20"),xmax=as.Date("2009-01-19")
            ,fill="red"),ymin=0,ymax=100, size=0.5, alpha=0.08) +
  geom_rect(aes(xmin=as.Date("2009-01-20"),xmax=as.Date("2017-01-19")
            ,fill="orange"),ymin=0,ymax=100, size=0.5, alpha=0.08) +
  geom_rect(aes(xmin=as.Date("2017-01-20"),xmax=as.Date("2021-01-19")
            ,fill="blue"),ymin=0,ymax=100, size=0.5, alpha=0.08) +
  geom_rect(aes(xmin=as.Date("2021-01-20"),xmax=as.Date("2022-12-31")
            ,fill="black"),ymin=0,ymax=100, size=0.5, alpha=0.08) +
  theme_classic() +
  labs(title="Gallup Presidential Approval Rating",subtitle="1993-2022")+xlab("Time")+ylab("Percent") +
  scale_fill_identity(labels = c(green="Clinton", red="Bush", orange="Obama", blue="Trump", black="Biden"),guide="legend",name=NULL)
plot1
ggsave(plot1,dpi = 500,width = 12,height = 8,filename="fig2.png")

par(mfrow=c(1,2))
acf(app$approval,main="Autocorrelation Function (ACF) of Presidential Approval")
pacf(app$approval,main="Partial Autocorrelation Function (PACF) of Presidential Approval")

############################Stationarity########################################


kpss.test(migration) #no
kpss.test(debt) #no
kpss.test(productivity) #yes
kpss.test(UNRATE) #yes
kpss.test(trade) #no
kpss.test(dollar) #no
kpss.test(GDPC) #yes
kpss.test(CPI) #yes
kpss.test(CPIrent) #yes
kpss.test(gasprice) #no
kpss.test(oil) #no
kpss.test(recession) #yes
approval=app$approval
kpss.test(approval) #no
kpss.test(WUI) #no


p_values <- c(
  migration = kpss.test(migration)$p.value,
  debt = kpss.test(debt)$p.value,
  productivity = kpss.test(productivity)$p.value,
  UNRATE = kpss.test(UNRATE)$p.value,
  trade = kpss.test(trade)$p.value,
  dollar = kpss.test(dollar)$p.value,
  GDPC = kpss.test(GDPC)$p.value,
  CPI = kpss.test(CPI)$p.value,
  CPIrent = kpss.test(CPIrent)$p.value,
  gasprice = kpss.test(gasprice)$p.value,
  oil = kpss.test(oil)$p.value,
  recession = kpss.test(recession)$p.value,
  approval = kpss.test(approval)$p.value,
  WUI = kpss.test(WUI)$p.value
)

p_values_df <- data.frame(Variables = names(p_values), P_Values = p_values)

table <- stargazer(p_values_df, out="kpss.html", title = "P-values of KPSS Test", rownames = FALSE,summary=FALSE)

model <- lm(app$approval ~ migration + debt + productivity + UNRATE +
              trade + dollar + GDPC + CPI + CPIrent + gasprice +
              oil + recession)
kpss_result=kpss.test(model$residuals)

stargazer(model, 
          title = "Cointegration Regression following the Engle-Granger method", 
          omit.stat = "f",
          omit.table.layout = "n",add.lines = paste("P-Value of KPSS test of residuals=",kpss_result$p.value),
          single.row = TRUE,out="appenix2.html")

############################LASSO###############################################
dev.off()
set.seed(10)
x=matrix(cbind(scale(migration),scale(debt),scale(productivity),scale(GDPC),scale(UNRATE),scale(CPI),scale(CPIrent),scale(trade),scale(gasprice),scale(oil),scale(recession),scale(dollar),scale(WUI)),nrow=120,ncol=13)
colnames(x)=c("migration","debt","productivity","GDP","UNRATE","CPI","CPIrent","trade","gasprice","oil","recession","dollar","uncertainty")
y=as.vector(scale(app$approval))
lasso.mod.sp =glmnet (x = x,y = y)
plot_glmnet(lasso.mod.sp,xvar="lambda", label=13)  
cv.lasso=cv.glmnet(x,y,nfolds = 5)
plot(cv.lasso)
lam.best=cv.lasso$lambda.min
lam.best
coef(lasso.mod.sp,s=lam.best)

lasso.mod.sp =glmnet (x = x,y = y)
plot_glmnet(lasso.mod.sp,xvar="lambda", label=13)
abline(v=log(lam.best),col="green")
############################VAR#################################################


var=matrix(cbind(migration,productivity,UNRATE,trade,dollar,app$approval),ncol=6)
colnames(var)=c("Migration","Productivity","UNRATE","Trade","Dollar","Approval")
varselect=VARselect(var)
stargazer(varselect,out = "varselect.html",title = "Optimal Lag Length")

VECM1 = VECM(var,lag=0, r=3, estim = c("ML"),include = "const") 

summ = summary(rank.test(VECM1, type = c("eigen", "trace"), cval = 0.05))
rang= c(summ$r)
trace = c(summ$trace_pval)
eigen = c(summ$eigen_pval)
dt = data.frame(cbind(rang,trace,eigen))
dt %>%
  kbl(caption = "Test for Cointegration VAR1: RGDP",col.names = c("Rank","P-Value Trace","P-Value Eigenvalue")) %>%
  kable_classic_2(full_width =T , html_font = "Cambria",row_label_position = "c") %>% kable_styling(row_label_position = "c")%>% 
  row_spec(0:6, align = "c") 


Var1=VAR(var,p = 1,type = "const")

sink("VARsummary.txt")
summary(Var1)
sink()

############################IRF#################################################

plot_irf = function(var_model, cause_var, effect_var) {
  irf = irf(var_model, response = effect_var, impulse = cause_var, boot = T,n.ahead=30,ortho=T)
  data=data.frame(cbind(data.frame(irf$irf)[effect_var],data.frame(irf$Lower)[effect_var],data.frame(irf$Upper)[effect_var]))
  irf_plot=ggplot(data = data, aes(x = 0:30, y = data[,1])) +
    geom_line() +
    geom_line(aes(y = data[,2], colour = 'red')) +
    geom_line(aes(y = data[,3]), colour = 'red')+
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = paste("Reaction of",
                       effect_var, "to a shock in",cause_var), x = "Time", y = "Impulse Response") +
    theme_classic()+
    guides(color = "none")
  return(irf_plot)
}

set.seed(1000)
arrangedirf=grid.arrange(plot_irf(Var1,"Migration","Approval"),
plot_irf(Var1,"Productivity","Approval"),
plot_irf(Var1,"UNRATE","Approval"),
plot_irf(Var1,"Trade","Approval"),
plot_irf(Var1,"Dollar","Approval"), nrow=2,ncol=3)
ggsave("IRF.png",plot = arrangedirf,scale = 1,width = 14,height = 8,device='png', dpi=500)

############################Granger#############################################

granger_causality(Var1,var.x = c("Migration","Productivity","UNRATE","Trade","Dollar"),var.y = c("Approval"))


############################OUT OF SAMPLE#######################################

approval_train=app$approval[1:(0.8*floor(length(app$approval)))]
approval_test=app$approval[(0.8*floor(length(app$approval))+1):120]

arforecast = function(datatrain,datadifftest,mde) {
  n_steps = length(datadifftest)
  forecasts = numeric(n_steps)
  data=c(datatrain,datadifftest)
  for (i in 1:n_steps) {
    train_set = data[1:97+i-1]
    test_set = datadifftest[i]
    arima_model = arima(train_set,c(1,0,0))
    forecast = predict(arima_model, n.ahead=1)$pred
    forecasts[i] = forecast
    
  }
  name1=paste0("f_", mde, "_forecasts")
  name2=paste0("fe_", mde, "_forecast_errors")
  forecast_errors = datadifftest - forecasts
  assign(name1, forecasts,envir = parent.frame())
  assign(name2, forecast_errors,envir = parent.frame())
  
}


arforecast(approval_train,approval_test,"Approval")

Datediff=as.Date(seq(as.Date("1993-01-01"),as.Date("2022-12-01"),by="quarter"))
plot_ar = function(data,datadifftrain,fore,name) {
  forecast_df = data.frame(cbind(data, c(datadifftrain ,fore)))
  colnames(forecast_df)=c("actual","forecast")
  ggplot(forecast_df, aes(x = Datediff[1:length(data)])) +
    geom_line(aes(y = forecast, color = "Forecast")) +
    geom_line(aes(y = actual, color = "Real")) +
    labs(title = "ARIMA Forecasts vs Real Values",
         x = "Time",
         y = "INVdiff") +
    scale_color_manual(values = c("Forecast" = "blue", "Real" = "red")) +
    xlim((Datediff)[97], (Datediff)[120])+
    ylim(35,60)+
    labs(title = paste("Forecast vs Real values of",
                       name), x = "Time", y = "Index") +
    theme_classic()
}

plot_ar(app$approval,approval_train,f_Approval_forecasts,"Approval")

vapproval_train=var[1:(0.8*floor(length(app$approval))),]
vapproval_test=var[(0.8*floor(length(app$approval))+1):120,]

varforecast = function(datatrain,datadifftest,mde,namefe) {
  n_steps = length(datadifftest[,1])
  forecasts_v = numeric(n_steps)
  data=data.frame(rbind(datatrain,datadifftest))
  for (i in 1:n_steps) {
    train_set = data[1:97+i-1,]
    test_set = datadifftest[i,]
    var_model = VAR(train_set,p = 1,type="none")
    forecast_v = predict(var_model,n.ahead=1,se.fit=FALSE)
    forecasts_v[i] = forecast_v$fcst[[mde]][1]
    
  }
  name1=paste0("f_", mde, "_forecasts_v",namefe)
  name2=paste0("fe_", mde, "_forecast_errors_v",namefe)
  forecast_errors_v = datadifftest[,1] - forecasts_v
  assign(name1, forecasts_v,envir = parent.frame())
  assign(name2, forecast_errors_v,envir = parent.frame())
}

varforecast(vapproval_train,vapproval_test,"Approval","1")

plot_var=function(dataactual,dataforecast,name){
  forgraph=data.frame(cbind(as.Date(Datediff[97:120]),dataactual[97:120],dataforecast))
  forgraph[,1]=as.Date(forgraph[,1])
  ggplot(forgraph,aes(x = forgraph[,1])) +
    geom_line(aes(y = forgraph[,3], color = "Forecast")) +
    geom_line(aes(y = forgraph[,2], color = "Real")) +
    labs(title = "VAR Forecasts vs Real Values",
         x = "Time",
         y = "INVdiff") +
    scale_color_manual(values = c("Forecast" = "blue", "Real" = "red")) +
    ylim(35,60)+
    labs(title = paste("Forecast vs Real values of Presidential",name), x = "Time", y = "Index") +
    theme_classic()
}
plot_var(var[,6],f_Approval_forecasts,"Approval")

Approvalactual=var[97:120,6]
Approvalforecasts=f_Approval_forecasts_v1
model1 = lm(Approvalactual~ Approvalforecasts)
stargazer(model1, 
          title = "Regression Results for Forecast Quality",
          align = TRUE, type = "text",out = "Forecastquality.html")


CW_Approval=fe_Approval_forecast_errors^2- fe_Approval_forecast_errors_v1^2+(fe_Approval_forecast_errors- fe_Approval_forecast_errors_v1)^2
reg1 = lm(CW_Approval ~ 1)
stargazer(reg1,type = "text",title = "Results of the Clark-West test", covariate.labels = NULL,header = FALSE, align = TRUE, digits = 3,out="CW.html" )

library(neuralnet)
var=matrix(cbind(migration,productivity,UNRATE,trade,dollar,app$approval),ncol=6)
nn=neuralnet(Approval~Migration+Productivity+UNRATE+Trade+Dollar+Approval,data=var[1:99,],hidden = c(3,2))
predict_5000 <- predict(nn, newdata=var[100:120,])
plot(predict_5000,type="l")
