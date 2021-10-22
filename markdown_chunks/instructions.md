---
title: ""
output: html_document
---

The following are instructions on how to navigate the app:

-   **Load Data**

    -   Users can select whether they want to use an example data or upload their own data.

    -   The *example dataset* is from a meta-analysis conducted to examine interventions to decrease cyberbullying (Polanin et al., 2021). The data contains effect size, variance of the effect size, and three factors: outcome measure, type of treatment assignment, and school setting.

    -   If users want to upload their own data, they can select whether to upload an effect size level data or a summary level data. Users can upload csv, tsv, txt, or xlsx files.

        -   *Effect size level data* refers to raw meta-analytic dataset with each row containing an effect size and data on variables like outcome measure, methodology, comparison type etc. related to that effect size.

        -   *Summary level data* refers to data that contains number of studies and/or average effect size aggregated for combination of factors.

    -   Users will be prompted to select variables to be used in the creation of EGMs.

-   **Create Summary Data**

    -   For the example and effect size level data, users can select parameters to run meta-regression models. By default, the app will calculate the average effect sizes using correlated effects model for cells that have more than 2 studies. For cells that have less than or equal to two studies, the app will run a univariate random effects model. For cells that only have one effect size estimate, the app outputs the raw effect size estimate. Users can select the value for the within-study correlation between the effect sizes to be input in the correlated effects model. Users can then click "Create Summary Data" button, which will prompt the app to run meta-regression to calculate average effect sizes per combination of factors as well as the number of studies and the number of effect sizes per combination of factors.
    -   For summary level data, users do not have to select any parameters and can view the data they uploaded.

-   **Create Evidence Gap Map**

    -   The Create Plot tab contains options to overlay the plot with number of studies, average effect size, or nothing (with the default being nothing). Further, users can add the labels for the x-axis, y-axis, and/or the color mapping.

    -   The Download tab contains options to name of the image to be downloaded, and adjust height and the width of the figure.

-   **R Syntax**

    -   This tab outputs reactive syntax corresponding to the data and variables inputted by the users, as well as any other selections made. Users can copy the syntax, paste into R Studio, and edit the script as necessary to make any changes.
