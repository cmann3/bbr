packages <- c("lmtest", "ggplot2", "cowplot", "sandwich", "dplyr")
for (i in packages){
  if (!(i %in% installed.packages())) install.packages(i)
  library(i, character.only = TRUE)
}


diagnose <- function(x, ...) UseMethod("diagnose")
diagnose.lm <- function(model, thresh.outlier=3, digits=3,
                        rm.outliers=FALSE, rm.leverage=FALSE,
                        se = NULL, ...){
  p <- model$rank
  n <- model$df.residual + p

  ## DATA
  ## ----
  .data <- model$model
  .data$.obs    <- 1:nrow(.data)
  .data$.fitted <- model$fitted.values
  .data$.resids <- model$residuals / sd(model$residuals)
  .data$.cooks  <- cooks.distance(model)
  .data$.lever  <- hatvalues(model)

  ## P TEST FUNCTION
  ## ---------------
  p.test <- function(x){
    if (x < 0.001) return("***")
    if (x < 0.01)  return("**")
    if (x < 0.05)  return("*")
    if (x < 0.1)   return(".")
    return(" ")
  }
  p.test.all <- function(x) sapply(x, p.test)

  ## COEFFICIENTS
  ## ------------
  .summary <- summary(model)
  .coeffs <- as.data.frame(signif(.summary$coefficients, digits))
  if (!is.null(se) && se == "robust"){
    white <- as.table(coeftest(model, vcovHC(model, type="HC0")))
    for (i in 2:4) .coeffs[, i] <- signif(white[, i], digits)
  }
  .coeffs$`  ` <- p.test.all(.coeffs[,4])

  ## TESTS
  ## -----
  test_rainbow <- raintest(model)
  test_rainbow$H0 <- "Linearity"
  test_rainbow$method <- "Rainbow"

  test_reset  <- resettest(model)
  test_reset$H0 <- "Functional Form"
  test_reset$method <- "RESET"

  test_bp <- bptest(model)
  test_bp$H0 <- "Homoskedasticity"
  test_bp$method <- "Breusch-Pagan"

  test_shapiro <- shapiro.test(model$residuals)
  test_shapiro$H0 <- "Normality"
  test_shapiro$method <- "Shapiro-Wilk"

  test_dw <- dwtest(model)
  test_dw$H0 <- "Autocorrelation"
  test_dw$method <- "Durbin-Watson"

  test_bg <- bgtest(model)
  test_bg$H0 <- "Serial Correlation"
  test_bg$method <- "Breusch-Godfrey"

  test_list <- list(test_rainbow, test_reset, test_bp, test_shapiro, test_dw, test_bg)
  for (i in 1:length(test_list)){
    test_list[[i]]$star <- p.test(test_list[[i]]$p.value)
  }

  tests <- data.frame(
    H0         = sapply(test_list, function(i) i$H0),
    Estimate   = signif(sapply(test_list, function(i) i$statistic), digits)
  )
  row.names(tests)  <- sapply(test_list, function(i) i$method)
  tests$`Pr(>|t|)` <- signif(sapply(test_list, function(i) i$p.value), digits)
  tests$`  `       <- sapply(test_list, function(i) i$star)

  ## OUTLIERS
  ## --------
  thresh.leverage <- 2*p/n
  thresh.influent <-  4/(n - p - 2)

  high.leverage <- which(.data$.lever >= thresh.leverage)
  outliers      <- which(abs(.data$.resids) >= thresh.outlier)
  influential   <- which(.data$.cooks >= thresh.influent)

  if (rm.outliers | rm.leverage){
    rows <- numeric(0)
    if (rm.outliers) rows <- union(rows, outliers)
    if (rm.leverage) rows <- union(rows, high.leverage)
    .data = .data[-rows,]
    ncall <- which(names(model$call) == "data")
    if (length(ncall) == 0){ ncall <- 3 }
    model$call[[ncall]] <- .data
    new_model <- eval(model$call)
    return(diagnose(new_model))
  }

  ## OTHER STATS
  ## -----------
  LL   <- sum(log(exp(-.data$.resids/(2*.summary$sigma^2))/(.summary$sigma*sqrt(2*pi))))
  AIC  <- 2*(p+1) - 2*LL
  BIC  <- (p+1)*log(n) - 2*LL
  MSE  <- mean(.data$.resids^2)
  RMSE <- sqrt(MSE)


  ## RETURN STRUCTURE
  ## ----------------
  structure(
    list(
      method          = "lm",
      call            = model$call,
      data            = .data,
      coefficients    = .coeffs,
      r.squared       = signif(.summary$r.squared, digits),
      adj.r.squared   = signif(.summary$adj.r.squared, digits),
      ll              = signif(LL, digits),
      aic             = signif(AIC, digits),
      bic             = signif(BIC, digits),
      mse             = signif(MSE, digits),
      rmse            = signif(RMSE, digits),
      fstatistic      = signif(.summary$fstatistic, digits),
      df              = .summary$df,
      sigma           = .summary$sigma,
      cov.unscaled    = .summary$cov.unscaled,
      N               = n,
      K               = p - 1,
      tests           = tests,
      outliers        = outliers,
      high.leverage   = high.leverage,
      influential     = influential,
      thresh.outlier  = thresh.outlier,
      thresh.leverage = thresh.leverage,
      thresh.influent = thresh.influent,
      se_type         = se
    ),
    class = "Diagnosis"
  )
}

print.Diagnosis <- function(x, plot = TRUE, ...){
  mchar <- max(nchar(row.names(x$coefficients)))
  adjust_char <- function(x, m){
    n <- nchar(x)
    left <- m - n
    paste0(x, paste(rep(" ", left), collapse=""))
  }

  ## Plot
  if (plot) print(plot_diagnose(x))

  ## Tables
  if (x$method == "lm") cat("\nORDINARY LEAST SQUARES REGRESSION\n---------------------------------\n")
  cat("y:", colnames(x$data)[1], ",   N: ", x$N)
  if (!is.null(x$se_type)){
    if (x$se_type == "robust") cat(",   (White Standard Errors)")
  }
  cat("\n\n")
  print(x$coefficients)
  cat(paste(rep("-", mchar), collapse=""), "\n")
  tab <- data.frame(
    Estimate = c(x$r.squared, x$adj.r.squared, x$mse, x$rmse),
    Blank  = "     ",
    Series = c( "LL ", "AIC", "BIC", "F  " ),
    Estimate2 = c(x$ll, x$aic, x$bic, x$fstatistic[1])
  )
  row.names(tab) <- c(adjust_char("R2", mchar), "R2-adj", "MSE", "RMSE")
  colnames(tab) <- c("Estimate", "   ", "     ", "Estimate")
  print(tab)


  cat("\nTESTS\n-----\n")
  print(x$tests)

  cat("\nOUTLIERS\n--------\n")
  outliers <- x$outliers
  if (length(outliers) == 0){ cat("No outliers detected.\n")
  } else {
    print(x$data[outliers, 1:(ncol(x$data)-5)])
  }

  cat("\nHIGH LEVERAGE\n-------------\n")
  lever <- x$high.leverage
  if (length(lever) == 0){ cat("No observations with high leverage detected.\n")
  } else {
    lever_data <- x$data[lever, c(".fitted", ".resids", ".cooks", ".lever")]
    if (nrow(lever_data) > 5){
      print(arrange(lever_data, desc(.lever), desc(.cooks))[1:5, ])
    } else {
      print(lever_data)
    }
  }
}

plot_fit <- function(model, ...){
  ggplot(model$data, aes(.fitted, .resids)) +
    geom_point() +
    geom_hline(yintercept=0, lty=2) +
    geom_smooth(col="red", method="loess", formula=y~x) +
    ggtitle("Residuals vs. Fitted Values") +
    xlab("Fitted Values") +
    ylab("Standardized Residuals")
}

plot_scale <- function(model, ...){
  dat <- model$data
  dat$.resids2 <- sqrt(dat$.resids^2)
  ggplot(dat, aes(.fitted, .resids2)) +
    geom_point() +
    geom_smooth(col="red", method="loess", formula=y~x) +
    ggtitle("Scale-Location Plot") +
    xlab("Fitted Values") +
    ylab("Root Squared Residuals")
}

plot_qq <- function(model, ...){
  n     <- nrow(model$data)
  dat   <- data.frame(
    resids = sort(model$data$.resids),
    z      = qnorm((1:n - 0.5)/n)
  )
  min_val <- min(dat$resids, dat$z)
  max_val <- max(dat$resids, dat$z)
  ggplot(dat, aes(z, resids)) +
    geom_point(pch=1, size=4) +
    geom_abline(intercept = 0, slope = 1, lty=2, linewidth=1.25, col="red") +
    ggtitle("Normal Q-Q") +
    xlab("Theoretical Z-Score") +
    ylab("Standardized Residuals") +
    xlim(min_val, max_val) +
    ylim(min_val, max_val)
}

plot_leverage <- function(model, ...){
  ggplot(model$data, aes(.lever, .resids)) +
    geom_point() +
    geom_hline(yintercept=0, lty=2) +
    geom_vline(xintercept=model$thresh.leverage, lty=2, col="blue") +
    geom_smooth(method="loess", formula=y~x, col="red") +
    ggtitle("Residuals vs Leverage") +
    xlab("Leverage") +
    ylab("Standardized Residuals")
}

plot_cooks <- function(model,...){
  ggplot(model$data, aes(.obs, .cooks)) +
    geom_col() +
    geom_hline(yintercept=model$thresh.influent, lty=2, col="red") +
    ggtitle("Cook's Distance") +
    xlab("Observation Number") +
    ylab("Cook's Distance")
}

plot_diagnose <- function(model, ...){
  plot_grid(
    plot_fit(model),
    plot_scale(model),
    plot_qq(model),
    plot_leverage(model),
    ncol = 2
  )
}



