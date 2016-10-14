#' @import ashr

#' @title Main Adaptive Shrinkage function
#'
#' @description Takes vectors of estimates (betahat) and their
#'     standard errors (sebetahat), together with degrees of freedom (df)
#'     and applies shrinkage to them, using Empirical Bayes methods, to compute shrunk estimates for
#'     beta.
#'
#' @details This function is actually just a simple wrapper that
#'     passes its parameters to \code{\link{ash.workhorse}} which
#'     provides more documented options for advanced use. See readme
#'     for more details.
#'
#' @param betahat a p vector of estimates
#' @param sebetahat a p vector of corresponding standard errors
#' @param mixcompdist distribution of components in mixture
#'     ("uniform","halfuniform" or "normal"; "+uniform" or
#'     "-uniform"), the default is "uniform". If you believe your
#'     effects may be asymmetric, use "halfuniform". If you want to
#'     allow only positive/negative effects use "+uniform"/"-uniform".
#'     The use of "normal" is permitted only if df=NULL.
#' @param df appropriate degrees of freedom for (t) distribution of
#'     betahat/sebetahat, default is NULL which is actually treated as
#'     infinity (Gaussian)
#' @param ... Further arguments to be passed to
#'     \code{\link{ash.workhorse}}.
#'
#' @return ash returns an object of \code{\link[base]{class}} "ash", a list with some or all of the following elements (determined by outputlevel) \cr
#' \item{fitted_g}{fitted mixture}
#' \item{loglik}{log P(D|fitted_g)}
#' \item{logLR}{log[P(D|fitted_g)/P(D|beta==0)]}
#' \item{result}{A dataframe whose columns are}
#' \describe{
#'  \item{NegativeProb}{A vector of posterior probability that beta is negative}
#'  \item{PositiveProb}{A vector of posterior probability that beta is positive}
#'  \item{lfsr}{A vector of estimated local false sign rate}
#'  \item{lfdr}{A vector of estimated local false discovery rate}
#'  \item{qvalue}{A vector of q values}
#'  \item{svalue}{A vector of s values}
#'  \item{PosteriorMean}{A vector consisting the posterior mean of beta from the mixture}
#'  \item{PosteriorSD}{A vector consisting the corresponding posterior standard deviation}
#'  }
#' \item{call}{a call in which all of the specified arguments are specified by their full names}
#' \item{data}{a list containing details of the data and models used (mostly for internal use)}
#' \item{fit_details}{a list containing results of mixture optimization, and matrix of component log-likelihoods used in this optimization}
#'
#' @seealso \code{\link{ash.workhorse}} for complete specification of ash function
#' @seealso \code{\link{ashci}} for computation of credible intervals after getting the ash object return by \code{ash()}
#'
#' @export
#' @examples
#' beta = c(rep(0,100),rnorm(100))
#' sebetahat = abs(rnorm(200,0,1))
#' betahat = rnorm(200,beta,sebetahat)
#' beta.ash = ash(betahat, sebetahat)
#' names(beta.ash)
#' head(beta.ash$result) # the main dataframe of results
#' graphics::plot(betahat,beta.ash$result$PosteriorMean,xlim=c(-4,4),ylim=c(-4,4))
#'
#' CIMatrix=ashci(beta.ash,level=0.95)
#' print(CIMatrix)
#'
#' #Illustrating the non-zero mode feature
#' betahat=betahat+5
#' beta.ash = ash(betahat, sebetahat)
#' graphics::plot(betahat,beta.ash$result$PosteriorMean)
#' betan.ash=ash(betahat, sebetahat,mode=5)
#' graphics::plot(betahat, betan.ash$result$PosteriorMean)
#' summary(betan.ash)
bad_ash <- function(betahat, sebetahat, mixcompdist = c("uniform", "halfuniform",
    "normal", "+uniform", "-uniform"), df = NULL, ...) {
    return(utils::modifyList(bad_ash.workhorse(betahat, sebetahat,
                                           mixcompdist = mixcompdist,
                                           df = df, ...), list(call = match.call())))
}


#' @title Detailed Adaptive Shrinkage function
#'
#' @description Takes vectors of estimates (betahat) and their
#'     standard errors (sebetahat), and applies shrinkage to them,
#'     using Empirical Bayes methods, to compute shrunk estimates for
#'     beta. This is the more detailed version of ash for "research"
#'     use.  Most users will be happy with the ash function, which
#'     provides the same usage, but documents only the main options
#'     for simplicity.
#'
#' @details See readme for more details.
#'
#' @param betahat a p vector of estimates
#' @param sebetahat a p vector of corresponding standard errors
#' @param method specifies how ash is to be run. Can be "shrinkage"
#'     (if main aim is shrinkage) or "fdr" (if main aim is to assess
#'     fdr or fsr) This is simply a convenient way to specify certain
#'     combinations of parameters: "shrinkage" sets pointmass=FALSE
#'     and prior="uniform"; "fdr" sets pointmass=TRUE and
#'     prior="nullbiased".
#' @param mixcompdist distribution of components in mixture (
#'     "uniform","halfuniform","normal" or "+uniform"), the default
#'     value is "uniform" use "halfuniform" to allow for assymetric g,
#'     and "+uniform"/"-uniform" to constrain g to be
#'     positive/negative.
#' @param optmethod specifies the function implementing an optimization method. Default is
#'     "mixIP", an interior point method, if REBayes is installed;
#'     otherwise an EM algorithm is used. The interior point method is
#'     faster for large problems (n>2000), particularly when method="shrink".
#' @param df appropriate degrees of freedom for (t) distribution of
#'     betahat/sebetahat, default is NULL(Gaussian)
#' @param nullweight scalar, the weight put on the prior under
#'     "nullbiased" specification, see \code{prior}
#' @param mode either numeric (indicating mode of g) or string "estimate",
#'      to indicate mode should be estimated.
#' @param pointmass logical, indicating whether to use a point mass at
#'     zero as one of components for a mixture distribution
#' @param prior string, or numeric vector indicating Dirichlet prior
#'     on mixture proportions (defaults to "uniform", or (1,1...,1);
#'     also can be "nullbiased" (nullweight,1,...,1) to put more
#'     weight on first component), or "unit" (1/K,...,1/K) [for
#'     optmethod=mixVBEM version only]
#' @param mixsd vector of sds for underlying mixture components
#' @param gridmult the multiplier by which the default grid values for
#'     mixsd differ by one another. (Smaller values produce finer
#'     grids)
#' @param outputlevel determines amount of output. There are several numeric options [0=just fitted g;
#'     1=also PosteriorMean and PosteriorSD; 2= everything usually
#'     needed; 3=also include results of mixture fitting procedure
#'     (includes matrix of log-likelihoods used to fit mixture); 4=
#'     output additional things required by flash (flash_data)]. Otherwise the user can also specify
#'     the output they require in detail (see Examples)
#' @param g the prior distribution for beta (usually estimated from
#'     the data; this is used primarily in simulated data to do
#'     computations with the "true" g)
#' @param fixg if TRUE, don't estimate g but use the specified g -
#'     useful for computations under the "true" g in simulations
#' @param alpha numeric value of alpha parameter in the model
#' @param control A list of control parameters passed to optmethod
#' @param lik contains details of the likelihood used; for general ash
#'
#' @return ash returns an object of \code{\link[base]{class}} "ash", a list with some or all of the following elements (determined by outputlevel) \cr
#' \item{fitted_g}{fitted mixture, either a normalmix or unimix}
#' \item{loglik}{log P(D|mle(pi))}
#' \item{logLR}{log[P(D|mle(pi))/P(D|beta==0)]}
#' \item{result}{A dataframe whose columns are}
#' \describe{
#'  \item{NegativeProb}{A vector of posterior probability that beta is negative}
#'  \item{PositiveProb}{A vector of posterior probability that beta is positive}
#'  \item{lfsr}{A vector of estimated local false sign rate}
#'  \item{lfdr}{A vector of estimated local false discovery rate}
#'  \item{qvalue}{A vector of q values}
#'  \item{svalue}{A vector of s values}
#'  \item{PosteriorMean}{A vector consisting the posterior mean of beta from the mixture}
#'  \item{PosteriorSD}{A vector consisting the corresponding posterior standard deviation}
#'  }
#' \item{call}{a call in which all of the specified arguments are specified by their full names}
#' \item{data}{a list containing details of the data and models used (mostly for internal use)}
#' \item{fit_details}{a list containing results of mixture optimization, and matrix of component log-likelihoods used in this optimization}
#'
#' @seealso \code{\link{ash}} for simplified specification of ash function
#' @seealso \code{\link{ashci}} for computation of credible intervals
#'     after getting the ash object return by \code{ash()}
#'
#' @export
#' @examples
#' beta = c(rep(0,100),rnorm(100))
#' sebetahat = abs(rnorm(200,0,1))
#' betahat = rnorm(200,beta,sebetahat)
#' beta.ash = ash(betahat, sebetahat)
#' names(beta.ash)
#' head(beta.ash$result) #dataframe of results
#' head(get_lfsr(beta.ash)) #get lfsr
#' head(get_pm(beta.ash)) #get posterior mean
#' graphics::plot(betahat,get_pm(beta.ash),xlim=c(-4,4),ylim=c(-4,4))
#'
#' CIMatrix=ashci(beta.ash,level=0.95) #note currently default is only compute CIs for lfsr<0.05
#' print(CIMatrix)
#'
#' #Running ash with different error models
#' beta.ash1 = ash(betahat, sebetahat, lik = normal_lik())
#' beta.ash2 = ash(betahat, sebetahat, lik = t_lik(df=4))
#'
#' e = rnorm(100)+log(rf(100,df1=10,df2=10)) # simulated data with log(F) error
#' e.ash = ash(e,1,lik=logF_lik(df1=10,df2=10))
#'
#'
#' # Specifying the output
#' beta.ash = ash(betahat, sebetahat, output = c("fitted_g","logLR","lfsr"))
#'
#' #Illustrating the non-zero mode feature
#' betahat=betahat+5
#' beta.ash = ash(betahat, sebetahat)
#' graphics::plot(betahat,beta.ash$result$PosteriorMean)
#' betan.ash=ash(betahat, sebetahat,mode=5)
#' graphics::plot(betahat, betan.ash$result$PosteriorMean)
#' summary(betan.ash)
#'
#'
#' #Running ash with a pre-specified g, rather than estimating it
#' beta = c(rep(0,100),rnorm(100))
#' sebetahat = abs(rnorm(200,0,1))
#' betahat = rnorm(200,beta,sebetahat)
#' true_g = normalmix(c(0.5,0.5),c(0,0),c(0,1)) # define true g
#' ## Passing this g into ash causes it to i) take the sd and the means
#' ## for each component from this g, and ii) initialize pi to the value
#' ## from this g.
#' beta.ash = ash(betahat, sebetahat,g=true_g,fixg=TRUE)
bad_ash.workhorse <- function(betahat, sebetahat,
                              method = c("fdr", "shrink"),
                              mixcompdist = c("uniform",
                                              "halfuniform", "normal", "+uniform", "-uniform"),
                              optmethod = c("mixIP",
                                            "cxxMixSquarem", "mixEM", "mixVBEM"),
                              df = NULL, nullweight = 10,
                              pointmass = TRUE,
                              prior = c("nullbiased", "uniform", "unit"),
                              mixsd = NULL, gridmult = sqrt(2),
                              outputlevel = 2, g = NULL, fixg = FALSE,
                              mode = 0, alpha = 0, control = list(),
                              lik = NULL) {

    mixcompdist <- match.arg(mixcompdist)
    method      <- match.arg(method)
    prior <- match.arg(prior)

    if (is.null(df)) {
        lik <- ashr::normal_lik()
    } else {
        lik <- ashr::t_lik(df = df)
    }

    data <- ashr::set_data(betahat = betahat, sebetahat = sebetahat, lik = lik)

    ## get grid ----------------------------------------------------------------------------
    if (!is.null(g)) {
        k <- ashr::ncomp(g)
        null.comp <- 1 #null.comp not actually used
        prior <- setprior.copy(prior, k, nullweight, null.comp)
    } else {
        if (is.null(mixsd)){
            mixsd <- autoselect.mixsd.copy(data = data, mult = gridmult, mode = mode)
        }
        if(pointmass){
            mixsd <- c(0, mixsd)
        }
        null.comp <- which.min(mixsd) #which component is the "null"

        k     <- length(mixsd)
        prior <- setprior.copy(prior, k, nullweight, null.comp)
        pi    <- stats::rgamma(k, 1, 1)
        pi    <- pi / sum(pi)

        if(!is.element(mixcompdist, c("normal", "uniform", "halfuniform", "+uniform", "-uniform")))
            stop("Error: invalid type of mixcompdist")
        if(mixcompdist == "normal")   g <- ashr::normalmix(pi, rep(mode, k), mixsd)
        if(mixcompdist == "uniform")  g <- ashr::unimix(pi, mode - mixsd, mode + mixsd)
        if(mixcompdist == "+uniform") g <- ashr::unimix(pi, rep(mode, k), mode + mixsd)
        if(mixcompdist == "-uniform") g <- ashr::unimix(pi, mode - mixsd, rep(mode, k))
        if(mixcompdist == "halfuniform"){
            if (min(mixsd) > 0) { #simply reflect the components
                g <- ashr::unimix(c(pi, pi) / 2, c(mode - mixsd, rep(mode, k)), c(rep(mode, k),
                                                                           mode + mixsd))
                prior <- rep(prior, 2)
                pi <- rep(pi, 2)
            } else { #define two sets of components, but don't duplicate null component
                null.comp <- which.min(mixsd)
                g <- ashr::unimix(c(pi, pi[-null.comp]) / 2, c(mode - mixsd, rep(mode, k - 1)),
                            c(rep(mode, k), mode + mixsd[-null.comp]))
                prior <- c(prior, prior[-null.comp])
                pi <- c(pi, pi[-null.comp])
            }
        }
    }

    tsamp <- stats::rnorm(n = length(betahat), mean = betahat / sebetahat,
                          sd = stats::mad(betahat / sebetahat))

    lfdr <- stats::runif(length(betahat))
    PositiveProb <- (1 - lfdr) * stats::pnorm(tsamp)
    NegativeProb <- (1 - lfdr) * stats::pnorm(-tsamp)
    lfsr <- pmin(PositiveProb + lfdr, NegativeProb + lfdr)
    svalue <- ashr::qval.from.lfdr(lfsr)
    qvalue <- ashr::qval.from.lfdr(lfdr)
    PosteriorMean <- stats::rnorm(betahat)
    PosteriorSD   <- abs(stats::rnorm(sebetahat))

    ashout <- list()
    ashout$fitted_g <- g
    ashout$loglik <- stats::runif(1, -500, 0)
    ashout$logLR  <- stats::runif(1 , 0, 1000)
    ashout$data   <- data
    ashout$result <- data.frame(betahat = betahat,
                                sebetahat = sebetahat,
                                NegativeProb = NegativeProb,
                                PositiveProb = PositiveProb,
                                lfsr = lfsr,
                                svalue = svalue,
                                lfdr = lfdr,
                                qvalue = qvalue,
                                PosteriorMean = PosteriorMean,
                                PosteriorSD = PosteriorSD)
    ashout$call <- "ashr::ash(betahat = betahat, sebetahat = sebetahat)"
    class(ashout) <- "ash"
    return(ashout)
}

autoselect.mixsd.copy <- function(data,mult,mode){
  betahat = data$x - mode
  sebetahat = data$s
  exclude = get_exclusions.copy(data)
  betahat = betahat[!exclude]
  sebetahat = sebetahat[!exclude]

  sigmaamin = min(sebetahat)/10 #so that the minimum is small compared with measurement precision
  if(all(betahat^2<=sebetahat^2)){
    sigmaamax = 8*sigmaamin #to deal with the occassional odd case where this could happen; 8 is arbitrary
  }else{
    sigmaamax = 2*sqrt(max(betahat^2-sebetahat^2)) #this computes a rough largest value you'd want to use, based on idea that sigmaamax^2 + sebetahat^2 should be at least betahat^2
  }
  if(mult==0){
    return(c(0,sigmaamax/2))
  }else{
    npoint = ceiling(log2(sigmaamax/sigmaamin)/log2(mult))
    return(mult^((-npoint):0) * sigmaamax)
  }
}

get_exclusions.copy <- function (data)
{
    return((data$s == 0 | data$s == Inf | is.na(data$x) | is.na(data$s)))
}


setprior.copy <- function(prior,k,nullweight,null.comp){
  if(!is.numeric(prior)){
    if(prior=="nullbiased"){ # set up prior to favour "null"
      prior = rep(1,k)
      prior[null.comp] = nullweight #prior 10-1 in favour of null by default
    }else if(prior=="uniform"){
      prior = rep(1,k)
    } else if(prior=="unit"){
      prior = rep(1/k,k)
    }
  }
  if(length(prior)!=k | !is.numeric(prior)){
    stop("invalid prior specification")
  }
  return(prior)
}


## The evil part
assignInNamespace("ash", value = bad_ash, ns = "ashr", envir = as.environment("package:ashr"))
assignInNamespace("ash.workhorse", value = bad_ash.workhorse, ns = "ashr",
                  envir = as.environment("package:ashr"))
