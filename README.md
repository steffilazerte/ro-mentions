This script attempts to find possible use-case tweets for rOpenSci packages within the last week. 

1. Search twitter for instances of
    - #rstats
    - ropensci
2. Filter to tweets containing mentions of rOpenSci package names
3. Remove some twitter accounts (i.e. rOpenSci, and ones that only tweet package updates)
4. Guess the author link for maintainers of rOpenSci packages
5. Create a mock draft use-case tweet (that will definitely have to be edited)
6. Keep link to original tweet and link to tweet author
7. Spin script to dated html file and manually scan for actually usecases
