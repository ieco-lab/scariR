# Description

This project explores the complicated relationship between the great city of Pawnee, Indiana, and the population of raccoons inhabiting it. According to Ron Swanson, in Pawnee raccoons "are active 24/7" and if they are not removed from the Christmas village, "they'll hunt the children for sport".

More information on the issue can be found here: https://www.youtube.com/watch?v=cUcjn1CuRZI
And here: https://www.reddit.com/r/FanTheories/comments/8naibb/parks_and_rec_why_pawnee_is_overrun_with_raccoons/

On a less serious note, this part of a project is meant to provide a collaborator with all the information required to download and install the package, and reproduce the analyses contained in it. The following paragraphs help with that.

## How to clone this project locally

Once starting to use git and uploading the project to a repository such as Github or GitLab, here we can add instructions on how to clone the package (if one has access to it).

```
git clone git@gitlab.com:leslieknope/raccoons.git
```

## How to prepare your R for the analyses

It's good practice to assume not everyone has the same R enviroment and set of packages installed locally. Here we can provide a ready made list of packages our project/package depends on.

Before installing our package, please copy-paste the following in your R console.

```
install.packages(c('tidyverse', 'here', 'wesanderson', 'magrittr', 'rmarkdown', 'pkgdown', 'knitr'))
```


# How to reproduce this analysis

Here a nice description on how to reproduce the analyses (for example instructing one to install the package and then run `pkgdown::build_site()` locally to reproduce the site)

# Data

Here some info on the data, how it was collected, whom it belongs to, permission to share etc.

# References

For further information, contact Councilwoman Knope @voteknope (you can swap in your GitLab/Github user name instead of Leslie's once the repository is online).
