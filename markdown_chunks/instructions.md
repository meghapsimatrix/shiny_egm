---
title: ""
output: html_document
---

The following are instructions on how to navigate the app:

-   **Load Data**

    -   Users can select whether they want to use an example data or upload their own data.

    -   The *example dataset* is from a meta-analysis conducted to examine interventions to decrease cyberbullying (Polanin et al., 2021). The data contains effect size, variance of the effect size, and three factors: outcome measure, type of treatment assignment, and school setting.

    -   If users want to upload their own data, they can select whether to upload an effect size level data or a summary level data. Users can upload csv, tsv, txt, or xlsx files.

        -   *Effect size level data* refers to raw meta-analytic dataset with each row containing an effect size and corresponding data on variables like outcome measure, methodology, comparison type etc.

        -   *Summary level data* refers to data that contains number of studies and/or average effect size aggregated for combination of factors.

    -   Users will be prompted to select variables to be used in the creation of EGMs.

-   **Create Summary Data**

    *We refer to the intersection of the levels of each of the factors mapped onto the x-axis, y-axis or color as cells.*

    -   For the example and effect size level data, the app will calculate the average effect size estimate per cell as follows:

        -   For cells that contain more than two studies, by default, the app will calculate the average effect sizes using the correlated effects model (Hedges, Tipton & Johnson, 2010). Users can select the value for the within-study correlation between the effect sizes to be input in the correlated effects model. The default value for the correlation is set 0.8 following the default set in the robumeta package (Fisher, Tipton & Zhipeng, 2017).

        -   For cells that have less than or equal to two studies, the app will run a univariate random effects model.

        -   For cells that only have one effect size estimate, the app will just use the raw effect size estimate.

    -   Users can then click "Create Summary Data" button, which will prompt the app to run meta-regression to calculate average effect sizes per combination of factors as well as the number of studies and the number of effect sizes per combination of factors.

    -   For summary level data, users do not have to select any parameters and click the "Create Summary Data" button to view the data they uploaded.

-   **Create Evidence Gap Map**

    -   The Create Plot tab contains options to map average effect size on color if applicable, and options to overlay the plot with number of studies, average effect size, or nothing (with the default being nothing). Further, users can add informative labels for the x-axis, y-axis, and/or the color and shape mapping. Users can click the "Create Plot" button to generate the plot.

    -   The Download tab contains options to name of the image to be downloaded, and adjust height and the width of the figure. Users can click the "Download" button to download the graph.

-   **R Syntax**

    -   This tab outputs reactive syntax corresponding to the data and variables inputted by the users, as well as any other selections made. Users can copy the syntax, paste it in R Studio, and edit the script as necessary to make any further changes.
