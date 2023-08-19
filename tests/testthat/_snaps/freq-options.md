# Works with inner_table_padding

    Code
      freq(iris, Species, plot = F, markdown = F)
    Output
      
      Variable: Species
      Class: factor
      NAs: 0 (0.0%)
      ═════════════════════════════════════════════
           Species  Freq     %  Cum. Freq  Cum. %
      ─────────────────────────────────────────────
            setosa    50  33.3         50    33.3
        versicolor    50  33.3        100    66.7
         virginica    50  33.3        150   100.0
      ─────────────────────────────────────────────
             Total   150  100%        150    100%
      ═════════════════════════════════════════════

# Works with table_symbol

    Code
      freq(iris, Species, plot = F, markdown = F)
    Output
      
      Variable: Species
      Class: factor
      NAs: 0 (0.0%)
      ############################################################
              Species     Freq        %     Cum. Freq     Cum. %
      ────────────────────────────────────────────────────────────
               setosa       50     33.3            50       33.3
           versicolor       50     33.3           100       66.7
            virginica       50     33.3           150      100.0
      ────────────────────────────────────────────────────────────
                Total      150     100%           150       100%
      ############################################################

# Works with row_divider_symbol

    Code
      freq(iris, Species, plot = F, markdown = F)
    Output
      
      Variable: Species
      Class: factor
      NAs: 0 (0.0%)
      ════════════════════════════════════════════════════════════
              Species     Freq        %     Cum. Freq     Cum. %
      ############################################################
               setosa       50     33.3            50       33.3
           versicolor       50     33.3           100       66.7
            virginica       50     33.3           150      100.0
      ############################################################
                Total      150     100%           150       100%
      ════════════════════════════════════════════════════════════

# Works with print_table_symbol

    Code
      freq(iris, Species, plot = F, markdown = F)
    Output
      
      Variable: Species
      Class: factor
      NAs: 0 (0.0%)
              Species     Freq        %     Cum. Freq     Cum. %
      ────────────────────────────────────────────────────────────
               setosa       50     33.3            50       33.3
           versicolor       50     33.3           100       66.7
            virginica       50     33.3           150      100.0
      ────────────────────────────────────────────────────────────
                Total      150     100%           150       100%

# Works with print_table_total_row

    Code
      freq(iris, Species, plot = F, markdown = F)
    Output
      
      Variable: Species
      Class: factor
      NAs: 0 (0.0%)
      ════════════════════════════════════════════════════════════
              Species     Freq        %     Cum. Freq     Cum. %
      ────────────────────────────────────────────────────────────
               setosa       50     33.3            50       33.3
           versicolor       50     33.3           100       66.7
            virginica       50     33.3           150      100.0
      ════════════════════════════════════════════════════════════

# Works with print_table_metadata

    Code
      freq(iris, Species, plot = F, markdown = F)
    Output
      ════════════════════════════════════════════════════════════
              Species     Freq        %     Cum. Freq     Cum. %
      ────────────────────────────────────────────────────────────
               setosa       50     33.3            50       33.3
           versicolor       50     33.3           100       66.7
            virginica       50     33.3           150      100.0
      ────────────────────────────────────────────────────────────
                Total      150     100%           150       100%
      ════════════════════════════════════════════════════════════

# Works with print_header_divider

    Code
      freq(iris, Species, plot = F, markdown = F)
    Output
      
      Variable: Species
      Class: factor
      NAs: 0 (0.0%)
      ════════════════════════════════════════════════════════════
              Species     Freq        %     Cum. Freq     Cum. %
               setosa       50     33.3            50       33.3
           versicolor       50     33.3           100       66.7
            virginica       50     33.3           150      100.0
      ────────────────────────────────────────────────────────────
                Total      150     100%           150       100%
      ════════════════════════════════════════════════════════════

