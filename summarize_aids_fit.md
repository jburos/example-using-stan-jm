Summarize drug comparison
================
Jacki Novik
5/4/2017

Data summary
------------

We are analyzing data from the `aids` dataset, provided by the [JM package](https://cran.r-project.org/web/packages/JM/index.html).

A brief description of the data are provided in the [package documentation](https://rdrr.io/cran/JM/man/aids.html), with more detail in [the original manuscript](https://www.ncbi.nlm.nih.gov/pubmed/8556398).

For our purposes, it's perhaps sufficient know that we are looking at data from a clinical trial published in 1996. This trial enrolled 467 patients with an HIV diagnosis who were either intolerant or resistant to AZT treatment. Patients were randomized to receive one of two drugs - we will call them `ddI` (didanosine) and `ddC` (zalcitabine).

Data are provided in two formats -

-   `aids` contains longitudinal measures (several obs per patient)
-   `aids.id` contains values at time of randomization (one obs per patient)

``` r
data(aids, package = "JM")
data(aids.id, package = "JM")
```

### Survival submodel

#### Data exploration

Overall, there is differential survival according to treatment.

``` r
aids.id %>%
  survfit(Surv(Time, death) ~ drug, data = .) %>%
  ggsurvplot(risk.table = FALSE,
             risk.table.y.text = FALSE,
             #ncensor.plot = TRUE,
             #surv.median.line='v',
             conf.int = TRUE,
             size = 0.5, 
             xlab = 'days',
             data = aids.id) +
  ggtitle('Survival among all patients') +
  scale_y_continuous(labels = percent)
```

    ## Scale for 'y' is already present. Adding another scale for 'y', which
    ## will replace the existing scale.

![](summarize_aids_fit_files/figure-markdown_github/plot-km-curve-1.png)

However, this trial enrolled patients according to previous opportunistic infection status (AIDS vs noAIDS) and according to AZT status (intolerance vs resistant). This prior status has pretty important prognostic value and may impact response to therapy.

Here, for example, are survival curves among patients without prior opportunistic infections (ie, no AIDs at time of enrollment).

``` r
plot_noaids <- aids.id %>%
  dplyr::filter(prevOI == 'noAIDS') %>%
  survfit(Surv(Time, death) ~ drug, data = .) %>%
  ggsurvplot(risk.table = FALSE,
             risk.table.y.text = FALSE,
             #ncensor.plot = TRUE,
             #surv.median.line='v',
             conf.int = TRUE,
             size = 0.5, 
             xlab = 'days',
             data = aids.id %>%
               dplyr::filter(prevOI == 'noAIDS')) +
  ggtitle('Patients without previous infections (no AIDs)') +
  scale_y_continuous(labels = percent)
```

    ## Scale for 'y' is already present. Adding another scale for 'y', which
    ## will replace the existing scale.

``` r
plot_noaids
```

![](summarize_aids_fit_files/figure-markdown_github/plot-KM-curve-by-prevOI-1.png)

By comparison, showing survival among patients with prior opportunitistic infections.

``` r
plot_aids <- aids.id %>%
  dplyr::filter(prevOI == 'AIDS') %>%
  survfit(Surv(Time, death) ~ drug, data = .) %>%
  ggsurvplot(risk.table = FALSE,
             risk.table.y.text = FALSE,
             #ncensor.plot = TRUE,
             surv.median.line='v',
             conf.int = TRUE,
             size = 0.5, 
             xlab = 'days',
             data = aids.id %>%
               dplyr::filter(prevOI == 'AIDS')) +
  ggtitle('Patients with previous opportunistic infections (AIDs)') +
  scale_y_continuous(labels = percent)
```

    ## Scale for 'y' is already present. Adding another scale for 'y', which
    ## will replace the existing scale.

``` r
plot_aids
```

![](summarize_aids_fit_files/figure-markdown_github/plot-KM-curve-among-aids-1.png)

Other important indicators include AZT status

``` r
plot_by_azt <- aids.id %>%
  survfit(Surv(Time, death) ~ AZT, data = .) %>%
  ggsurvplot(risk.table = FALSE,
             risk.table.y.text = FALSE,
             #ncensor.plot = TRUE,
             surv.median.line='v',
             conf.int = TRUE,
             size = 0.5, 
             xlab = 'days',
             data = aids.id
             ) +
  ggtitle('Among all patients') +
  scale_y_continuous(labels = percent)
```

    ## Scale for 'y' is already present. Adding another scale for 'y', which
    ## will replace the existing scale.

``` r
plot_by_azt
```

![](summarize_aids_fit_files/figure-markdown_github/plot-KM-by-azt-1.png)

And gender, although the majority of patients enrolled are male.

``` r
plot_by_gender <- aids.id %>%
  survfit(Surv(Time, death) ~ gender, data = .) %>%
  ggsurvplot(risk.table = FALSE,
             risk.table.y.text = FALSE,
             #ncensor.plot = TRUE,
             surv.median.line='v',
             conf.int = TRUE,
             size = 0.5, 
             xlab = 'days',
             data = aids.id
             ) +
  ggtitle('Among all patients') +
  scale_y_continuous(labels = percent)
```

    ## Scale for 'y' is already present. Adding another scale for 'y', which
    ## will replace the existing scale.

``` r
plot_by_gender
```

![](summarize_aids_fit_files/figure-markdown_github/plot-KM-by-gender-1.png)

#### Parameterizing the submodel

Taking these factors into account, we end up with a survival submodel as follows:

``` r
surv_form <- as.formula(Surv(Time, death) ~ drug*prevOI + gender + AZT)
```

which has the following MLE estimates from the standard cox-ph model:

``` r
cox.fit <- coxph(surv_form, data = aids.id)
summary(cox.fit)
```

    ## Call:
    ## coxph(formula = surv_form, data = aids.id)
    ## 
    ##   n= 467, number of events= 188 
    ## 
    ##                       coef exp(coef) se(coef)      z Pr(>|z|)    
    ## drugddI             1.0139    2.7565   0.4185  2.423   0.0154 *  
    ## prevOIAIDS          1.8593    6.4194   0.3841  4.840  1.3e-06 ***
    ## gendermale         -0.3266    0.7213   0.2453 -1.332   0.1830    
    ## AZTfailure          0.1538    1.1663   0.1635  0.941   0.3468    
    ## drugddI:prevOIAIDS -0.9269    0.3958   0.4476 -2.071   0.0384 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                    exp(coef) exp(-coef) lower .95 upper .95
    ## drugddI               2.7565     0.3628    1.2138    6.2598
    ## prevOIAIDS            6.4194     0.1558    3.0235   13.6295
    ## gendermale            0.7213     1.3863    0.4460    1.1667
    ## AZTfailure            1.1663     0.8574    0.8465    1.6067
    ## drugddI:prevOIAIDS    0.3958     2.5266    0.1646    0.9515
    ## 
    ## Concordance= 0.645  (se = 0.022 )
    ## Rsquare= 0.136   (max possible= 0.99 )
    ## Likelihood ratio test= 68.17  on 5 df,   p=2.467e-13
    ## Wald test            = 45.72  on 5 df,   p=1.034e-08
    ## Score (logrank) test = 56.92  on 5 df,   p=5.265e-11

#### Including baseline value of longitudinal covariate `CD4`

The longitudinal endpoint of interest here is `CD4` count. CD4+ cells are a type of T cell, and they play a critical part of the immune system function. Their levels are often depressed in aids patients, particularly as the disease progresses.

We thus expect higher levels of CD4 count to be associated with improved survival.

Before considering the longitudinal submodel, we should include the baseline value of `CD4` count in our survival submodel. We do this for two reasons - (1) to sanity check our expectation of improved survival with higher levels of CD4 count, and (2) to see how the inclusion of this covariate impacts our parameter estimates.

``` r
cox.fit2 <- coxph(update(surv_form, ~ . + CD4), data = aids.id)
summary(cox.fit2)
```

    ## Call:
    ## coxph(formula = update(surv_form, ~. + CD4), data = aids.id)
    ## 
    ##   n= 467, number of events= 188 
    ## 
    ##                       coef exp(coef) se(coef)      z Pr(>|z|)    
    ## drugddI             1.1513    3.1623   0.4194  2.745 0.006054 ** 
    ## prevOIAIDS          1.4074    4.0854   0.3867  3.639 0.000273 ***
    ## gendermale         -0.2173    0.8047   0.2440 -0.891 0.373110    
    ## AZTfailure          0.1339    1.1433   0.1622  0.825 0.409116    
    ## CD4                -0.1516    0.8593   0.0237 -6.396  1.6e-10 ***
    ## drugddI:prevOIAIDS -1.0503    0.3498   0.4484 -2.343 0.019153 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                    exp(coef) exp(-coef) lower .95 upper .95
    ## drugddI               3.1623     0.3162    1.3899    7.1949
    ## prevOIAIDS            4.0854     0.2448    1.9146    8.7179
    ## gendermale            0.8047     1.2427    0.4988    1.2981
    ## AZTfailure            1.1433     0.8747    0.8319    1.5711
    ## CD4                   0.8593     1.1637    0.8203    0.9002
    ## drugddI:prevOIAIDS    0.3498     2.8586    0.1453    0.8424
    ## 
    ## Concordance= 0.725  (se = 0.022 )
    ## Rsquare= 0.223   (max possible= 0.99 )
    ## Likelihood ratio test= 117.9  on 6 df,   p=0
    ## Wald test            = 81.02  on 6 df,   p=2.22e-15
    ## Score (logrank) test = 96.08  on 6 df,   p=0

As we suspected, higher `CD4` count is associated with better prognosis in this cohort. It is also notable that the inclusion of this covariate substantially increases the benefit of drug `ddI` (didanosine) over `ddC` (zalcitabine), and lessens the magnitude of association between prior AIDS status and survival, as well as its interaction with the drug effect.

This would suggest that the impact of prior AIDS on survival is explained at least in part by differences in CD4 levels.

Which is indeed the case.

``` r
ggplot(aids.id, aes(x = CD4, group = prevOI, colour = prevOI)) +
  geom_density() +
  theme_minimal() +
  ggtitle('CD4 count at baseline by previous OI (opportunistic infection) status')
```

![](summarize_aids_fit_files/figure-markdown_github/plot-cd4-baseline-by-prevOI-1.png)

Cool. At this point, we are ready to investigate the longitudinal submodel.

### Longitudinal submodel

We now begin to investigate the longitudinal (on-treatment) measurements of `CD4` count.

As noted above, we would expect higher levels of CD4 count to be associated with improved survival. We also now expect these measurements (at least at baseline) to be correlated with previous AIDS status.

Here we plot trajectories of CD4 count over the course of treatment for each patient, by previous infection status.

``` r
ggplot(aids, aes(x = obstime, y = CD4, group = patient, colour = prevOI)) +
  geom_line(alpha = 0.2) +
  scale_x_continuous('Days') +
  theme_minimal() +
  ggtitle('CD4 count over time per patient, according to previous infection status')
```

![](summarize_aids_fit_files/figure-markdown_github/plot-CD4-long-1.png)

You will notice that timepoints of collection here are highly regular, although we do have some missing values. At each timepoint, there are also waves of censoring in the longitudinal data.

It is customary to use `sqrt(CD4)` when modeling these data instead of CD4. Although it's not strictly necessary for our model, we will proceed with this convention.

``` r
ggplot(aids2 %>% 
         dplyr::mutate(`sqrt(CD4)` = sqrt_cd4) %>%
         tidyr::gather('variable', 'value', CD4, `sqrt(CD4)`),
       aes(x = value, group = variable, colour = variable)) + 
  geom_density() +
  facet_wrap(~variable, scale = 'free') +
  theme_minimal() +
  theme(legend.position = 'none')
```

![](summarize_aids_fit_files/figure-markdown_github/compare-cd4-sqrt-1.png)

*TODO fill in details about longitudinal model fit*

We have fit several parameterizations of the longitudinal submodel.

Using `LOO-PSIS` as a model-comparison criterion, we can sort models from the best fit (`long3`) to the worst (`long0`).

``` r
loo_comp_table %>% dplyr::select(model_name, RHS, looic, se_looic) %>% print(right=F)
```

    ##   model_name
    ## 1 long3     
    ## 2 long2     
    ## 3 long5     
    ## 4 long4     
    ## 5 long6     
    ## 6 long1     
    ## 7 long0     
    ##   RHS                                                                                                           
    ## 1 obstime + drug + obstime:drug + gender + prevOI + AZT + (1 + obstime | patient)                               
    ## 2 obstime * drug + (1 + obstime | patient)                                                                      
    ## 3 obstime + drug + obstime:drug + prevOI + obstime:prevOI + drug:prevOI + gender + AZT + (1 + obstime | patient)
    ## 4 obstime + drug + obstime:drug + prevOI + obstime:prevOI + gender + AZT + (1 + obstime | patient)              
    ## 5 obstime + drug + obstime:drug + prevOI + obstime:prevOI + gender + AZT + (1 + obstime | patient)              
    ## 6 obstime + (1 + obstime | patient)                                                                             
    ## 7 obstime + (1 | patient)                                                                                       
    ##   looic    se_looic
    ## 1 1926.718 91.69184
    ## 2 1930.031 90.83887
    ## 3 1930.792 92.13360
    ## 4 1932.482 92.60514
    ## 5 1933.828 91.95204
    ## 6 1952.837 93.82012
    ## 7 2044.633 94.16735

From the perspective of model comparison, there are a few things to note:

1.  The biggest improvements in fit came from:
    -   allowing slope of `CD4` over time to vary by patient (long0 -&gt; long1)
    -   including tx effect & allowing slope of `CD4` over time to vary by tx (long1 -&gt; long2)
    -   including covariate effects of for `prevOI` and `AZT` (long2 -&gt; long3)

2.  There was very little improvement in fit when we included an interaction between the slope of `CD4` (`obstime`) & `prevOI` (long4), whether this was included with or without interactions between drug & `prevOI` or other interaction effects.

I would also note the slight interaction effect between drug (`ddI`) and `obstime`, since any differential survival due to treatment might be mediated by effect of treatment on the rate of change in `CD4`.

Let's summarize the posterior parameter estimates for `long3`.

``` r
long3 <- readRDS(file = file.path(CACHE_DIR, 'long3.rds'))
print(long3)
```

    ## stan_glmer(formula = sqrt(CD4) ~ obstime + drug + obstime:drug + 
    ##     gender + prevOI + AZT + (1 + obstime | patient), data = aids)
    ## 
    ## Estimates:
    ##                 Median MAD_SD
    ## (Intercept)      3.1    0.1  
    ## obstime          0.0    0.0  
    ## drugddI          0.1    0.1  
    ## gendermale       0.0    0.1  
    ## prevOIAIDS      -0.9    0.1  
    ## AZTfailure      -0.1    0.1  
    ## obstime:drugddI  0.0    0.0  
    ## sigma            0.4    0.0  
    ## 
    ## Error terms:
    ##  Groups   Name        Std.Dev. Corr 
    ##  patient  (Intercept) 0.763         
    ##           obstime     0.037    -0.05
    ##  Residual             0.369         
    ## Num. levels: patient 467 
    ## 
    ## Sample avg. posterior predictive 
    ## distribution of y (X = xbar):
    ##          Median MAD_SD
    ## mean_PPD 2.5    0.0   
    ## 
    ## ------
    ## For info on the priors used see help('prior_summary.stanreg').

While it's important to note that inferences from this model aren't valid (due to data not missing at random), we can still investigate the population-level parameter estimates from the model. Mostly because it will be useful to compare these estimates to those obtained from the joint model.

``` r
bayesplot::mcmc_areas(as.array(long3), pars = colnames(coefficients(long3)$patient)[-1]) + 
  bayesplot::vline_0() +
  ggtitle('Posterior parameter estimates for model fit with stan_lmer', subtitle = 'Long3: longitudinal model for assoc with sqrt(CD4) over time (obstime)')
```

![](summarize_aids_fit_files/figure-markdown_github/plot-population-level-parameters-1.png)

As expected, the CD4 count is generally decreasing in this population -- this is a very sick population (remember this is in 1996!). Also, as expected, having had a prior opportunistic infection (`prevOIAIDS`) leads to lower initial values of `CD4`. Similarly, having had prior AZT failure may confer a worse prognosis.

In order to obtain valid inferences for these parameters, we would need to use the Joint Model which adjusts for the informative censoring due to clinical events.

### Joint model for longitudinal biomarker + survival event data

At this point we are ready to fit the joint model for longitudinal & time-to-event data.

We start with a fit incorporating our work on the individual submodels.

``` r
# TODO revert to f7 once that run has finished
#print(f7)
## for now, proceed with f3 as if it's f7
f7 <- readRDS(file.path(CACHE_DIR, 'f3.rds'))
print(f7)
```

    ## stan_jm(formulaLong = sqrt_cd4 ~ obstime + drug + obstime:drug + 
    ##     gender + prevOI + AZT + (1 + obstime | patient), dataLong = aids2, 
    ##     formulaEvent = Surv(Time, death) ~ gender + drug + prevOI + 
    ##         AZT, dataEvent = aids.id, time_var = "obstime", assoc = c("etavalue", 
    ##         "etaslope"), basehaz = "bs", chains = 4, adapt_delta = 0.999)
    ## 
    ## Longitudinal submodel: sqrt_cd4
    ##                 Median MAD_SD
    ## (Intercept)      3.087  0.128
    ## obstime         -0.042  0.005
    ## drugddI          0.060  0.074
    ## gendermale      -0.006  0.125
    ## prevOIAIDS      -0.859  0.093
    ## AZTfailure      -0.083  0.090
    ## obstime:drugddI  0.004  0.006
    ## sigma            0.371  0.011
    ## 
    ## Event submodel:
    ##                Median MAD_SD exp(Median)
    ## gendermale     -0.380  0.278  0.684     
    ## drugddI         0.372  0.166  1.451     
    ## prevOIAIDS      0.729  0.257  2.072     
    ## AZTfailure      0.146  0.172  1.157     
    ## Long1|etavalue -0.941  0.135  0.390     
    ## Long1|etaslope -5.198  5.558  0.006     
    ## basehaz-coef1  -4.070  0.800     NA     
    ## basehaz-coef2  -1.549  0.734     NA     
    ## basehaz-coef3  -3.264  0.702     NA     
    ## basehaz-coef4  -1.040  0.773     NA     
    ## basehaz-coef5  -3.112  0.974     NA     
    ## basehaz-coef6  -1.959  1.439     NA     
    ## 
    ## Group-level random effects:
    ##  Groups  Name              Std.Dev. Corr
    ##  patient Long1|(Intercept) 0.76071      
    ##          Long1|obstime     0.03661  0.03
    ## Num. levels: patient 467

Let's see how closely the estimated 'bs' baseline hazard matches our observed KM curves.

``` r
# TODO uncomment once run has finished
# f7.ps_check
```

#### Summarize parameter estimates graphically

``` r
bayesplot::mcmc_areas(as.array(f7), regex_pars = '^Long1\\|[^(Intercept)|sigma]') + 
  bayesplot::vline_0() +
  ggtitle('Posterior parameter estimates for model fit with stan_jm', subtitle = 'f7: longitudinal submodel for assoc with sqrt(CD4) over time (obstime)')
```

![](summarize_aids_fit_files/figure-markdown_github/show-coefs-long-1.png)

Here we see increased *hazard* (worse survival) with prior opportunistic infections, prior AZT failure, and treatment with `ddI` (didanosine).

There is better overall survival among men than women, but there isn't a lot of confidence in this signal.

These coefficient estimates are consistent with what we saw in the exploratory analysis & the MLE estimates obtained using `coxph`.

``` r
bayesplot::mcmc_areas(as.array(f7), regex_pars = '^Event\\|[^basehaz]') + 
  bayesplot::vline_0() +
  ggtitle('Posterior parameter estimates for model fit with stan_jm',
          subtitle = 'Event submodel for hazard of mortality')
```

![](summarize_aids_fit_files/figure-markdown_github/show-coefs-event-1.png)

Let's look at the coefficients for `Assoc`, which relates the longitudinal submodel to the event submodel.

``` r
bayesplot::mcmc_areas(as.array(f7), regex_pars = '^Assoc\\|') + 
  bayesplot::vline_0() +
  ggtitle('Posterior parameter estimates for model fit with stan_jm',
          subtitle = 'Association between longitudinal submodel & event submodel (hazard)')
```

![](summarize_aids_fit_files/figure-markdown_github/show-coefs-assoc-1.png)

We see a very narrow but somewhat certain association of `etavalue` (the current value at any time of `CD4` count) and subsequent mortality. This is an inverse association, so that higher `CD4` counts yield improved survival (as we expect).

There is also a weak but plausible trend towards improved survival with increasing slopes of `CD4` count.

#### Compare long-model coefficients

Next we compare coefficient values from the longitudinal model fit to their counterparts in the `stan_jm` fit.

``` r
p_joint <- bayesplot::mcmc_areas(as.array(f7), regex_pars = '^Long1\\|[^(Intercept)|sigma]') + 
  bayesplot::vline_0() +
  ggtitle('Fit within Joint Model')

p_long <- bayesplot::mcmc_areas(as.array(long3), pars = colnames(coefficients(long3)$patient)[-1]) + 
  bayesplot::vline_0() +
  ggtitle('Fit as standalone')

bayesplot::bayesplot_grid(p_joint, p_long,
                          grid_args = list(ncol = 2),
                          xlim = c(-1.2, 0.5))
```

    ## Scale for 'x' is already present. Adding another scale for 'x', which
    ## will replace the existing scale.
    ## Scale for 'x' is already present. Adding another scale for 'x', which
    ## will replace the existing scale.

![](summarize_aids_fit_files/figure-markdown_github/compare-coefs-long-1.png)

The decision problem
--------------------

Now we get to the crux of the issue - namely, informing the treatment plan for an individual patient.

Given the model, you might think that we could simply trat all patients with `ddC`, since `ddI` is associated with higher hazard for mortality.

However, the picture may not be that simple.

Consider:

1.  The survival benefit of `ddC` over `ddI` may not be the same over all patients.
    -   There may be a subset of patients for whom `ddI` is just as good as `ddC`
    -   There may also be a subset of patients for whom `ddI` is better than `ddC`
    -   The relative benefit of each drug may *change* over the course of treatment

2.  In addition, survival is not the only outcome to consider.
    -   The side-effects from the two drugs may not be comparable
    -   The risk of adverse events may vary by patient, and/or by drug
    -   The two drugs may not have the same cost

In practice, we want to consider all of these factors when making a treatment decision.

### Is the benefit of `ddC` over `ddI` the same for all patients?

### Example for one patient
