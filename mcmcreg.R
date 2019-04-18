#######################################
## author: Rob Williams              ##
## contact: jayrobwilliams@gmail.com ##
## project: misc R functions         ##
## created: November 4, 2017         ##
## updated: November 7, 2017         ##
#######################################

## this function is a wrapper to texreg() which produces publication quality
## regression table with interval measures of uncertainty for MCMC output. the
## user can specify the confidence level of the interval, as well as credible or
## highest posterior density intervals. many of the arguments are standard texreg
## arguments that are passed to texreg() at the end of the function to create
## the table.

## inspiration drawn from Johannes Karreth's mcmctab function

## depends on texreg and whichever MCMC package the model object(s) require

## arguments:
## mod: a single MCMC model object, or a list of model objects of the same class.
## pars: a scalar or vector of the parameters you wish to include in the table.
##       stanfit objects can use either the individual parameter names, or the
##       names of the indexed parameter to retrieve the entire parameter e.g.
##       pars = 'beta' will return beta[1], beta[2], and beta[3] for a stanfit
##       model with a three element beta parameter. parameter arguments for all
##       other model object types must contain the entire set of parameters you
##       wish to include in the table. when combining models with different
##       parameters in one table, this argument also accepts a list the length
##       of the number of models. this is necessary when combining JAGS models
##       with different parameters because coda and JAGS related packages do not
##       support parameter family extraction like Stan.
## point_est: a character indicating whether to use the mean or median for point
##            estimates in the table.
## ci: a scalar indicating the confidence level of the uncertainty intervals.
## hpdi: a logical indicating whether to use highest posterior density intervals.
## ci_test: value to compare for the interval test. set to NULL for no stars.
## model_names: an optional vector of models names.
## custom_coef: an optional vector or list of vectors containing parameter names
##              for each model. if there are multiple models, the list must have
##              the same number of elements as there are models, and the vector
##              of names in each list element must match the number of parameters.
##              if not supplied, the function will use the parameter names in the
##              model object(s).
## gof: a named list of goodness of fit statistics, or a list of such lists
## caption: an optional caption for the table.
## label: an optional label for the table.
## reorder_coef: an optional vector reordering the coefficients in the final
##               texreg table
## sideways: a logical indicating whether to rotate the table; this function sets
##           use.packages = F in texreg(), so you will have to add the rotating
##           package to your preamble.
## filename: an optional character giving the name of a file to save the table to

mcmcreg <- function(mod, pars, point_est = 'mean', ci = .95, hpdi = F, ci_test = 0,
                    model_names = NULL, custom_coef = NULL, gof = numeric(0),
                    gof_names = character(0), caption, label = NULL, groups = NULL,
                    reorder_coef = NULL, coef_map = NULL, sideways = F, float_pos = '',
                    filename, format = 'latex', scalebox = NULL) {
  
  ## if only one model object, coerce to a list
  if (class(mod) != 'list') mod <- list(mod)
  
  ## if only one parameter vector, coerce to a list
  if (class(pars) != 'list') pars <- list(pars)
  
  ## if only one gof statistic scalar or vector, coerce to a list
  if (class(gof) != 'list') gof <- list(rep(gof, times = length(mod)))
  
  ## if only one gof statistic name scalar or vector, coerce to a list
  if (class(gof_names) != 'list') gof_names <- list(gof_names)
  
  ## if no caption, assign default
  if (missing(caption) & length(mod) > 1) caption <- 'Statistical Models'
  if (missing(caption) & length(mod) == 1) caption <- 'Statistical Model'
  
  ## extract samples and variable names from stanfit object
  if (lapply(mod, class)[[1]] == 'stanfit') {
    
    ## extract coefficient names from list of model ojects
    coef_names <- mapply(function(x, y) rownames(rstan::summary(x, pars = y)$summary),
                         mod, pars, SIMPLIFY = F)
    
    ## extract posterior samples from list of model objects
    samps <- mapply(function(x, y) as.data.frame(rstan::extract(x, pars = y)),
                    mod, pars, SIMPLIFY = F)
    
  }
  
  ## extract samples and variable names from runjags object
  if (lapply(mod, class)[[1]] == 'runjags') {
    
    ## extract posterior samples from list of model objects
    samps <- mapply(function(x, y) runjags:::as.mcmc.list.runjags(x, vars = y),
                    mod, pars, SIMPLIFY = F)
    
    ## average over chains and convert to dataframe
    samps <- lapply(samps, function(x) as.data.frame(Reduce("+", x) / length(x)))
    
    ## extract coefficient names from dataframe
    coef_names <- lapply(samps, colnames)
    
  }
  
  ## extract samples and variable names from mcmc.list object
  if (lapply(mod, class)[[1]] == 'mcmc.list') {
    
    ## extract posterior samples from list of model objects
    samps <- lapply(mod, function(x) as.data.frame(Reduce("+", x) / length(x)))
    
    ## drop columns not in pars
    samps <- mapply(function(x, y) x[, colnames(x) %in% y], samps, pars,
                    SIMPLIFY = F)
    
    ## extract coefficient names from dataframe
    coef_names <- lapply(samps, colnames)
    
  }
  
  ## extract samples and variable names from mcmc object
  if (lapply(mod, class)[[1]] == 'mcmc') {
    
    ## extract posterior samples from list of model objects
    samps <- mapply(function(x) coda:::as.data.frame.mcmc(x, vars = y),
                    mod, pars, SIMPLIFY = F)
    
    ## extract coefficient names from dataframe
    coef_names <- lapply(samps, colnames)
    
  }
  
  ## calculate point estimate of posterior density
  if (point_est == 'mean') {
    
    samps_pe <- lapply(samps, function(x) apply(x, 2, mean))
    
  } else {
    
    samps_pe <- lapply(samps, function(x) apply(x, 2, median))
    
  }
  
  ## calculate uncertainty interval for ci argument
  if (hpdi == F) {
    
    samps_ci <- lapply(samps, function(x) apply(x, 2, quantile,
                                                probs = c(.5 - ci/2, .5 + ci/2)))
    
  } else {
    
    samps_ci <- lapply(samps, function(x) t(coda::HPDinterval(coda::as.mcmc(x),
                                                            prob = ci)))
    
  }
  
  ## if coefficent names supplied, replace names from model object(s)
  if (!is.null(custom_coef) & !is.list(custom_coef)) coef_names <- list(custom_coef)
  if (!is.null(custom_coef)) coef_names <- custom_coef
  
  ##
  if (length(mod) != length(coef_names)) {

    stop('number of models does not match number of custom coefficient vectors')
    
  }
  
  ## create list of texreg object(s) with point estimates and interval
  tr_list <- mapply(function(v, w, x, y, z) texreg::createTexreg(coef.names = v,
                                                           coef = w,
                                                           ci.low = x[1, ],
                                                           ci.up = x[2, ],
                                                           gof = y,
                                                           gof.names = z),
                    coef_names, samps_pe, samps_ci, gof, gof_names)
  
  ## create LaTeX output
  if (format == 'latex') {
    
    ## create LaTeX code
    tr <- texreg::texreg(l = tr_list, custom.model.names = model_names,
                         caption = caption, label = label, ci.test = ci_test,
                         sideways = sideways, reorder.coef = reorder_coef,
                         custom.coef.map = coef_map, groups = groups,
                         scalebox = scalebox, float.pos = float_pos,
                         use.packages = F)
    
    ## replace confidence w/ credible or highest posterior density in texreg output
    if (hpdi == F) {
      
      tr <- sub('outside the confidence interval',
                paste('outside ', ci * 100 ,'\\\\% credible interval', sep = ''),
                tr)
      
    } else {
      
      tr <- sub('outside the confidence interval',
                paste('outside ', ci * 100 ,'\\\\% highest posterior density interval',
                      sep = ''), tr)
      
    }
    
    ## return LaTeX code to console or write to file
    if (missing(filename)) {
      
      return(tr)
      
    } else {
      
      ## remove newline at start of LaTeX code
      tr <- sub('^\\n', '', tr)
      
      tex_file <- file(paste(filename, 'tex', sep = '.'))
      writeLines(tr, tex_file, sep = '')
      close(tex_file)
      
    }
    
  }
  
  ## create HTML output
  if (format == 'html') {
    
    hr <- texreg::htmlreg(l = tr_list, custom.model.names = model_names,
                          caption = caption, ci.test = ci_test,
                          reorder.coef = reorder_coef, custom.coef.map = coef_map,
                          groups = groups)
    
    ## replace confidence w/ credible or highest posterior density in texreg output
    if (hpdi == F) {
      
      hr <- sub('outside the confidence interval',
                paste('outside ', ci * 100, '% credible interval', sep = ''),
                hr)
      
    } else {
      
      tr <- sub('outside the confidence interval',
                paste('outside ', ci * 100, '% highest posterior density interval',
                      sep = ''), hr)
      
    }
    
    ## return html code to console or write to file
    if (missing(filename)) {
      
      return(hr)
      
    } else {
      
      hmtl_file <- file(paste(filename, 'html', sep = '.'))
      writeLines(hr, html_file, sep = '')
      close(html_file)
      
    }
    
  }
  
}

## need to figure out how to get log likelihood matrix from various objects for WAIC