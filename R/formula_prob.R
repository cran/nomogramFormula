#' @title Explore the Formula of Probabilities and Total Points
#' @description Explore the formula of probabilities and total points by the best power.
#'
#' @param nomogram results of nomogram() function in 'rms' package
#' @param power power can be automatically selected based on all R2 equal 1
#' @param digits default is 6
#' @importFrom utils sessionInfo
#' @importFrom stats as.formula lm predict t.test
#' @importFrom graphics legend par plot points
#' @return formula is the formula of probabilities and total points. test is the R2 and RMSE which are used to test the fitted probabilities. diff is difference between nomogram probabilities and fitted probabilities
#' @export
#'
#' @examples
#' library(rms)  # needed for nomogram
#' set.seed(2018)
#' n <-2019
#' age <- rnorm(n,60,20)
#' sex <- factor(sample(c('female','male'),n,TRUE))
#' sex <- as.numeric(sex)
#' weight <- sample(50:100,n,replace = TRUE)
#' time <- sample(50:800,n,replace = TRUE)
#' units(time)="day"
#' death <- sample(c(1,0,0),n,replace = TRUE)
#' df <- data.frame(time,death,age,sex,weight)
#' ddist <- datadist(df)
#' oldoption <- options(datadist='ddist')
#' f <- cph(formula(Surv(time,death)~sex+age+weight),data=df,
#'          x=TRUE,y=TRUE,surv=TRUE,time.inc=3)
#' surv <- Survival(f)
#' nomo <- nomogram(f,
#'                  lp=TRUE,
#'                  fun=list(function(x) surv(365,x),
#'                           function(x) surv(365*2,x)),
#'                  funlabel=c("1-Year Survival Prob",
#'                             "2-Year Survival Prob"))
#' options(oldoption)
#' formula_prob(nomogram = nomo)
#' formula_prob(nomogram = nomo,power = 2)
#' formula_prob(nomogram = nomo,power = 3)
formula_prob <- function(nomogram,power,digits=6){
    #get probabilities part
    if ("lp" %in% names(nomogram)){
        lp_location=("lp" %==% names(nomogram))
        prob_part=nomogram[(lp_location+1):length(nomogram)]
    }else if("total.points" %in% names(nomogram)){
        ttp_location=("total.points" %==% names(nomogram))
        prob_part=nomogram[(ttp_location+1):length(nomogram)]
    }
    if (missing(power)){
        #missing power : choose power automatically
        power = 0
        test=data.frame(R2=0.5)
        while (any(test$R2<1)) {
            power=power+1
            for (i in 1:length(prob_part)) {
                if (i==1){
                    nomo.reslut=data.frame()
                    test=data.frame()
                    real_fit_list=list()
                }
                ######get each 3 variables
                nomo.i=prob_part[i]
                var.i=names(nomo.i)
                #change name of nomo.i to a, to get list points
                names(nomo.i)="a"
                points.i=nomo.i$a$x
                names(points.i)=NULL
                value.i=nomo.i$a$x.real
                ######calculate
                formu=as.formula(paste0('value.i~',inner_Add_Symbol(paste0("I(points.i^",1:power,")"))))
                reg=lm(formu)
                #diff
                fit.i=predict(reg)
                value.i=value.i
                diff=value.i-fit.i
                real_fit=t(data.frame(nomogram=value.i,fit=fit.i,diff))
                colnames(real_fit)=round(points.i,digits)
                real_fit.i=list(real_fit)
                names(real_fit.i)=var.i
                real_fit_list=c(real_fit_list,real_fit.i)
                #test
                R2=suppressWarnings(summary(reg)$r.squared)
                RMSE=(mean(predict(reg)-value.i)^2)^(1/2)
                test.i=data.frame(R2,RMSE)
                test=rbind(test,test.i)
                #formula
                coef=reg$coefficients
                lm.result=data.frame(t(coef))
                rownames(lm.result)=var.i
                colnames(lm.result)=c("b0",paste0("x^",1:power))
                nomo.reslut=rbind(nomo.reslut,lm.result)
            }
        }
    }else{
        #exist power
        if (power<1) stop("power must not be less 1")
        for (i in 1:length(prob_part)) {
            if (i==1){
                nomo.reslut=data.frame()
                test=data.frame()
                real_fit_list=list()
            }
            ######get each 3 variables
            nomo.i=prob_part[i]
            var.i=names(nomo.i)
            #change name of nomo.i to a, to get list points
            names(nomo.i)="a"
            points.i=nomo.i$a$x
            names(points.i)=NULL
            value.i=nomo.i$a$x.real
            ######calculate
            formu=as.formula(paste0('value.i~',inner_Add_Symbol(paste0("I(points.i^",1:power,")"))))
            reg=lm(formu)
            #diff
            fit.i=predict(reg)
            value.i=value.i
            diff=value.i-fit.i
            real_fit=t(data.frame(nomogram=value.i,
                                  fit=fit.i,diff))
            colnames(real_fit)=round(points.i,digits)
            real_fit.i=list(real_fit)
            names(real_fit.i)=var.i
            real_fit_list=c(real_fit_list,real_fit.i)
            #test
            R2=suppressWarnings(summary(reg)$r.squared)
            RMSE=(mean(predict(reg)-value.i)^2)^(1/2)
            test.i=data.frame(R2,RMSE)
            test=rbind(test,test.i)
            #formula
            coef=reg$coefficients
            lm.result=data.frame(t(coef))
            rownames(lm.result)=var.i
            colnames(lm.result)=c("b0",paste0("x^",1:power))
            nomo.reslut=rbind(nomo.reslut,lm.result)
        }
    }
    rownames(test)=rownames(nomo.reslut)
    result=list(formula=round(nomo.reslut,digits),
                test=round(test,digits),
                diff=real_fit_list)
    return(result)
}

