# Introduction

Welcome to [fHMM](https://loelschlaeger.de/fHMM/), an R package for
modeling financial time series data with hidden Markov models (HMMs).
This introduction [motivates the approach](#motivation), gives an
[overview](#package-and-vignettes-overview) of the package functionality
and the included vignettes, and [places the approach in the existing
literature](#placement-in-the-literature).

## Motivation

Earning money with stock trading is simple: one “only” needs to buy and
sell stocks at the right moment. In general, stock traders seek to
invest at the beginning of upward trends (hereon termed as bullish
markets) and repel their stocks just in time before the prices fall
again (hereon termed as bearish markets). As stock prices depend on a
variety of environmental factors ([Humpe and Macmillan
2009](#ref-hum09); [Cohen et al. 2013](#ref-coh13)), chance certainly
plays a fundamental role in hitting those exact moments. However,
investigating market behavior can lead to a better understanding of how
trends alternate and thereby increases the chance of making profitable
investment decisions.

The [fHMM](https://loelschlaeger.de/fHMM/) package aims at contributing
to those investigations by applying HMMs to detect bearish and bullish
markets in financial time series. It also implements the hierarchical
model extension presented in Oelschläger and Adam ([2021](#ref-oel21)),
which improves the model’s capability for distinguishing between short-
and long-term trends and allows to interpret market dynamics at multiple
time scales.

## Package and vignettes overview

The functionality of the [fHMM](https://loelschlaeger.de/fHMM/) package
can be classified into functions for data preparation, model estimation,
and model evaluation. The following flowchart visualizes their
dependencies:

![A flowchart of the {fHMM} package: Functions are boxed and classes
displayed as circles.](flowchart.png)

A flowchart of the {fHMM} package: Functions are boxed and classes
displayed as circles.

The tasks *data preparation*, *model estimation*, and *model evaluation*
as well as their corresponding functions and classes are explained in
detail in separate vignettes:

- The vignette [*Model
  definition*](https://loelschlaeger.de/fHMM/articles/v01_model_definition.html)
  defines the HMM and its hierarchical extension.

- The vignette
  [*Controls*](https://loelschlaeger.de/fHMM/articles/v02_controls.html)
  introduces the
  [`set_controls()`](https://loelschlaeger.de/fHMM/reference/set_controls.md)
  function which is used for model specifications.

- The vignette [*Data
  management*](https://loelschlaeger.de/fHMM/articles/v03_data_management.html)
  explains how to prepare or simulate data and introduces the
  [`download_data()`](https://loelschlaeger.de/fHMM/reference/download_data.md)
  function that can download financial data directly from
  <https://finance.yahoo.com/>.

- The vignette [*Model
  estimation*](https://loelschlaeger.de/fHMM/articles/v04_model_estimation.html)
  defines the likelihood function and explains the task of its numerical
  maximization via the
  [`fit_model()`](https://loelschlaeger.de/fHMM/reference/fit_model.md)
  function.

- The vignette [*State decoding and
  prediction*](https://loelschlaeger.de/fHMM/articles/v05_state_decoding_and_prediction.html)
  introduces the Viterbi algorithm that is used for decoding the most
  likely underlying state sequence and subsequently for forecasting.

- The vignette [*Model
  checking*](https://loelschlaeger.de/fHMM/articles/v06_model_checking.html)
  explains the task of checking a fitted model via computing (pseudo-)
  residuals, which is implemented in the
  [`compute_residuals()`](https://loelschlaeger.de/fHMM/reference/compute_residuals.md)
  function.

- The vignette [*Model
  selection*](https://loelschlaeger.de/fHMM/articles/v07_model_selection.html)
  discusses the task of selecting the (in some sense) best model among a
  set of competing models via the
  [`compare_models()`](https://loelschlaeger.de/fHMM/reference/compare_models.md)
  function.

## Placement in the literature

Over the last decades, various HMM-type models have emerged as popular
tools for modeling financial time series that are subject to
state-switching over time ([Schaller and Van Norden 1997](#ref-sch97);
[Dias et al. 2010](#ref-dia09); [Ang and Timmermann 2012](#ref-ang12);
[De Angelis and Viroli 2017](#ref-dea17)). Rydén et al.
([1998](#ref-ryd98)), Bulla and Bulla ([2006](#ref-bul06)), and Nystrup
et al. ([2015](#ref-nys15a)), e.g., used HMMs to derive stylized facts
of stock returns, while Hassan and Nath ([2005](#ref-has05)) and Nystrup
et al. ([2017](#ref-nys17)) demonstrated that HMMs can prove useful for
economic forecasting. More recently, Lihn ([2017](#ref-lih17)) applied
HMMs to the Standard and Poor’s 500, where HMMs were used to identify
different levels of market volatility, aiming at providing evidence for
the conjecture that returns exhibit negative correlation with
volatility. Another application to the S&P 500 can be found in Nguyen
([2018](#ref-ngu18)), where HMMs were used to predict monthly closing
prices to derive an optimal trading strategy, which was shown to
outperform the conventional buy-and-hold strategy. Further applications,
which involve HMM-type models for asset allocation and portfolio
optimization, can be found in Bekaert and Ang ([2002](#ref-ang02)),
Bulla et al. ([2011](#ref-bul11)), Nystrup et al. ([2015](#ref-nys15a))
and Nystrup et al. ([2018](#ref-nys18)), to name but a few examples. All
these applications demonstrate that HMMs constitute a versatile class of
time series models that naturally accounts for the dynamics typically
exhibited by financial time series.

## References

Ang, A., and A. Timmermann. 2012. “Regime Changes and Financial
Markets.” *Annual Review of Financial Economics* 4 (1): 313–37.
<https://doi.org/10.1146/annurev-financial-110311-101808>.

Bekaert, G., and A. Ang. 2002. “International Asset Allocation with
Regime Shifts.” *Review of Financial Studies* 15 (February): 1137–87.
<https://doi.org/10.1093/rfs/15.4.1137>.

Bulla, J., and I. Bulla. 2006. “Stylized Facts of Financial Time Series
and Hidden Semi-Markov Models.” *Computational Statistics and Data
Analysis* 51 (4): 2192–209.
<https://doi.org/10.1016/j.csda.2006.07.021>.

Bulla, J., S. Mergner, I. Bulla, A. Sesboüé, and C. Chesneau. 2011.
“Markov-Switching Asset Allocation: Do Profitable Strategies Exist?”
*Journal of Asset Management* 12 (July): 310–21.
<https://doi.org/10.1057/jam.2010.27>.

Cohen, L., K. Diether, and C. Malloy. 2013. “Legislating Stock Prices.”
*Journal of Financial Economics* 110 (3): 574–95.

De Angelis, L., and C. Viroli. 2017. “A Markov-Switching Regression
Model with Non-Gaussian Innovations: Estimation and Testing.” *Studies
in Nonlinear Dynamics & Econometrics* 21 (2).
<https://doi.org/doi:10.1515/snde-2015-0118>.

Dias, J., J. Vermunt, and S. Ramos. 2010. “Mixture Hidden Markov Models
in Finance Research.” *Psycho-Oncology - PSYCHO-ONCOL*, January, 451–59.
<https://doi.org/10.1007/978-3-642-01044-6_41>.

Hassan, Md, and Baikunth Nath. 2005. “Stock Market Forecasting Using
Hidden Markov Model: A New Approach.” *5th International Conference on
Intelligent Systems Design and Applications* 2005 (October): 192–96.
<https://doi.org/10.1109/ISDA.2005.85>.

Humpe, Andreas, and Peter Macmillan. 2009. “Can Macroeconomic Variables
Explain Long-Term Stock Market Movements? A Comparison of the US and
Japan.” *Applied Financial Economics* 19 (2): 111–19.
<https://doi.org/10.1080/09603100701748956>.

Lihn, S. H. 2017. “Hidden Markov Model for Financial Time Series and Its
Application to s&p 500 Index.” *Quantitative Finance (Forthcoming)*.

Nguyen, N. 2018. “Hidden Markov Model for Stock Trading.” *International
Journal of Financial Studies* 6 (2).

Nystrup, P., H. Madsen, and E. Lindström. 2015. “Stylised Facts of
Financial Time Series and Hidden Markov Models in Continuous Time.”
*Quantitative Finance* 15 (9): 1531–41.
<https://doi.org/10.1080/14697688.2015.1004801>.

Nystrup, P., H. Madsen, and E. Lindström. 2017. “Long Memory of
Financial Time Series and Hidden Markov Models with Time-Varying
Parameters.” *Journal of Forecasting* 36 (8): 989–1002.

Nystrup, P., H. Madsen, and E. Lindström. 2018. “Dynamic Portfolio
Optimization Across Hidden Market Regimes.” *Quantitative Finance* 18
(1): 83–95. <https://doi.org/10.1080/14697688.2017.1342857>.

Oelschläger, L., and T. Adam. 2021. “Detecting Bearish and Bullish
Markets in Financial Time Series Using Hierarchical Hidden Markov
Models.” *Statistical Modelling*, ahead of print.
<https://doi.org/10.1177/1471082X211034048>.

Rydén, T., T. Teräsvirta, and S. Åsbrink. 1998. “Stylized Facts of Daily
Return Series and the Hidden Markov Model.” *Journal of Applied
Econometrics* 13 (3): 217–44.

Schaller, H., and S. Van Norden. 1997. “Regime Switching in Stock Market
Returns.” *Applied Financial Economics* 7 (2): 177–91.
<https://doi.org/10.1080/096031097333745>.
