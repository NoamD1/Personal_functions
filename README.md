# Personal_functions
This folder contains personal functions that I've created along the way for personal work in the context of academic research in social science. All functions are work in progress and are updated every now and then.

Several functions are meant to facilitate the work with SHARE [(Survey of Health, Ageing and Retirement in Europe)](http://www.share-project.org/)  data:
* *add_Vars.R*: searches for SHARE data files on the computer and adds variable from SHARE data.
* *SHARE_recode_mis.R*: is designed to imitate sharetom5 do-file provided by SHARE. The file recodes values (-2, -1 etc.) to NA.

Other functions were written to speed up common procedures during research:
* *clean lm pois functions.R*: Clean lm/glm objects results using `broom` package.
* *crosstab_m.R*: preforms crosstab for many variables over one grouping variable, adding chi2 test and creating (based on `janitor::crostab`} command)
* *multiple_mean_sd_anova_function.R*: produces Summary statistic table for continues variables by group variable (with 2 or 3+ categories)
* *multiple_pwcorr_function.R*: produces summary statistic table for 2 continues variables using `Hmisc` and `broom` packages.
* *tab1.R*: preforms one way tabulation for more than one vriaables (based on STATA's `tab1` command)
