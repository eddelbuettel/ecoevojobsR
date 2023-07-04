
<!-- README.md is generated from README.Rmd. Please edit that file -->

## What is this respository for?

[ecoevojobs.net](http://ecoevojobs.net) is a widely-used community board
for academic jobs in ecology, evolutionary biology, and related
disciplines. However, the ecoevojobs spreadsheet has relatively little
information automatically included about the institution where the job
posting is.

This repository automatically pulls public information from [the
Carnegie Classification of Institutions of Higher
Education](https://carnegieclassifications.acenet.edu/) and merges it
into the ecoevojobs spreadsheet.

[You can find the most-recently updated faculty jobs listing file
here](https://github.com/mikeblazanin/ecoevojobsR/blob/master/data-raw/ecoevojobsR_faculty.csv)

[You can find the most-recently updated postdoc jobs listing file
here](https://github.com/mikeblazanin/ecoevojobsR/blob/master/data-raw/ecoevojobsR_postdoc.csv)

Note that I couldn’t figure out an easy way to automate the updating of
these spreadsheets. If you have an idea how that could be done, email
me! <mikeblazanin@gmail.com>. Otherwise, they’ll be updated when I can,
or when people ask me to.

If you want an up-to-date version for yourself and don’t want to wait on
me, you can simply follow these steps:

1.  Create a folder. Open R and set your working directory to this
    folder.
2.  Download and save the
    [ecoevo_jobs_analysis.R](https://github.com/mikeblazanin/ecoevojobsR/blob/master/R/ecoevo_jobs_analysis.R)
    file into that folder
3.  Create a `data-raw` subfolder
4.  Download and save the files in
    [data-raw](https://github.com/mikeblazanin/ecoevojobsR/tree/master/data-raw)
    into that subfolder
5.  Download the latest ecoevojobs spreadsheet as an Excel file, and
    overwrite the existing `ecoevojobs 22-23.xlsx` file
6.  Run ecoevo_jobs_analysis.R (with the working directory in the parent
    folder). The script should automatically update the
    `ecoevojobsR_faculty.csv` and `ecoevojobsR_postdocs.csv` files.

Note that some institution names may be unable to be matched
automatically. If this is the case, during execution
`ecoevo_jobs_analysis.R` will print statement with the number of
institutions for which this is true.

If you’d like to manually tell the script how to match those:

1.  Find the `aliases_new1.csv` and `aliases_new2.csv` files that were
    created by running script, they should be located in the `data-raw`
    folder
2.  Copy-paste the rows from those files where the `checked` column is
    `N` into the `aliases.csv` file
3.  For each institution, find the corresponding row in the `Data` tab
    of the `CCIHE2021-PublicData.xlsx` file.
4.  Copy-paste the `name` from that row in `CCIHE2021-PublicData.xlsx`
    into the `carnegie_name` column of the corresponding row in
    `aliases.csv`
