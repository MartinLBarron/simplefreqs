# basic freq of vector

    Code
      freq(iris$Species, plot = F, markdown = F)
    Output
      
      Variable: Species
      Class: factor
      NA's: 0 (0.0%)
      ════════════════════════════════════════════════════════════
              Species     Freq        %     Cum. Freq     Cum. %
      ────────────────────────────────────────────────────────────
               setosa       50     33.3            50       33.3
           versicolor       50     33.3           100       66.7
            virginica       50     33.3           150      100.0
      ────────────────────────────────────────────────────────────
                Total      150     100%           150       100%
      ════════════════════════════════════════════════════════════

# basic freq of tidy vector

    Code
      freq(iris, Species, plot = F, markdown = F)
    Output
      
      Variable: Species
      Class: factor
      NA's: 0 (0.0%)
      ════════════════════════════════════════════════════════════
              Species     Freq        %     Cum. Freq     Cum. %
      ────────────────────────────────────────────────────────────
               setosa       50     33.3            50       33.3
           versicolor       50     33.3           100       66.7
            virginica       50     33.3           150      100.0
      ────────────────────────────────────────────────────────────
                Total      150     100%           150       100%
      ════════════════════════════════════════════════════════════

# basic freq of indexing

    Code
      freq(iris[["Species"]], plot = F, markdown = F)
    Output
      
      Variable: Species
      Class: factor
      NA's: 0 (0.0%)
      ════════════════════════════════════════════════════════════
              Species     Freq        %     Cum. Freq     Cum. %
      ────────────────────────────────────────────────────────────
               setosa       50     33.3            50       33.3
           versicolor       50     33.3           100       66.7
            virginica       50     33.3           150      100.0
      ────────────────────────────────────────────────────────────
                Total      150     100%           150       100%
      ════════════════════════════════════════════════════════════

