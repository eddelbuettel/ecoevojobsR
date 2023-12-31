
<!-- README.md is generated from README.Rmd. Please edit that file -->

## What is this respository for?

[ecoevojobs.net](http://ecoevojobs.net) is a widely-used community board
for academic jobs in ecology, evolutionary biology, and related
disciplines. However, the ecoevojobs spreadsheet has relatively little
information automatically included about the institution where the job
posting is.

Once per hour, this repository automatically pulls the latest ecoevojobs
spreadsheet and merges in public information from [the Carnegie
Classification of Institutions of Higher
Education](https://carnegieclassifications.acenet.edu/). It then writes
the merged output to two files:

- [You can find the most-recently updated faculty jobs listing file
  here](https://github.com/mikeblazanin/ecoevojobsR/blob/master/data-raw/ecoevojobsR_faculty.csv)
- [You can find the most-recently updated fixed-term faculty jobs
  listing file
  here](https://github.com/mikeblazanin/ecoevojobsR/blob/master/data-raw/ecoevojobsR_fixed_term.csv)
- [You can find the most-recently updated postdoc jobs listing file
  here](https://github.com/mikeblazanin/ecoevojobsR/blob/master/data-raw/ecoevojobsR_postdoc.csv)

If you have any suggestions or see any errors, email me!
<mikeblazanin@gmail.com>

Note that, when institution names in the ecoevojobs spreadsheet are not
entered in a way that automatically matches the names in the Carnegie
database, I have to use a manually-curated list of aliases to match
them. Because of this, there may be a delay for the newest entries,
where they don’t match until the next time I get around to updating the
aliases list.

If you’d like to see how this code works, or want to download the script
to play around with yourself, you can follow these steps:

1.  Create a folder. Open R and set your working directory to this
    folder.
2.  Download and save the
    [ecoevo_jobs_analysis.R](https://github.com/mikeblazanin/ecoevojobsR/blob/master/R/ecoevo_jobs_analysis.R)
    file into that folder
3.  Create a `data-raw` subfolder
4.  Download and save the files in
    [data-raw](https://github.com/mikeblazanin/ecoevojobsR/tree/master/data-raw)
    into that subfolder
5.  Run ecoevo_jobs_analysis.R (with the working directory in the parent
    folder). The script should automatically update the
    `ecoevojobsR_faculty.csv` and `ecoevojobsR_postdocs.csv` files.

When running this yourself, note that the the script checks the
`aliases.csv` for names it can’t automatically match. I’ve manually
filled in `aliases.csv` with previously listed institution names from
ecoevojobs and the corresponding name in the Carnegie Classifications.
However, if some new names are not found in `aliases.csv`, during
execution `ecoevo_jobs_analysis.R` will print a statement with the
number of institutions for which this is true.

If you’d like to manually tell the script how to match those
(i.e. update `aliases.csv` yourself):

1.  Find the `aliases_new1.csv` and `aliases_new2.csv` files that were
    created by running the script, they should be located in the
    `data-raw` folder
2.  Copy-paste the rows from those files where the `checked` column is
    `N` into the `aliases.csv` file
3.  For each institution, find the corresponding row in the `Data` tab
    of the `CCIHE2021-PublicData.xlsx` file.
4.  Copy-paste the `name` from that row in `CCIHE2021-PublicData.xlsx`
    into the `carnegie_name` column of the corresponding row in
    `aliases.csv`
