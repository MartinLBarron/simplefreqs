# order when fed a raw character is alphabetic

    Code
      freq(df, x, sort = F, plot = F, markdown = F)
    Output
      
      Variable: x
      Class: character
      NA's: 0 (0.0%)
      ═══════════════════════════════════════════════════════
               x     Freq        %     Cum. Freq     Cum. %
      ───────────────────────────────────────────────────────
               A        3     30.0             3       30.0
               B        4     40.0             7       70.0
               C        2     20.0             9       90.0
               D        1     10.0            10      100.0
      ───────────────────────────────────────────────────────
           Total       10     100%            10       100%
      ═══════════════════════════════════════════════════════

# order when fed a raw character is alphabetic, with NA at end

    Code
      freq(df, x, sort = F, plot = F, markdown = F)
    Output
      
      Variable: x
      Class: character
      NA's: 1 (9.1%)
      ═══════════════════════════════════════════════════════
               x     Freq        %     Cum. Freq     Cum. %
      ───────────────────────────────────────────────────────
               A        3     27.3             3       27.3
               B        4     36.4             7       63.6
               C        2     18.2             9       81.8
               D        1      9.1            10       90.9
            <NA>        1      9.1            11      100.0
      ───────────────────────────────────────────────────────
           Total       11     100%            11       100%
      ═══════════════════════════════════════════════════════

# order when fed a unspecified factor is alphabetic, with NA at end

    Code
      freq(df, x, sort = F, plot = F, markdown = F)
    Output
      
      Variable: x
      Class: factor
      NA's: 1 (9.1%)
      ═══════════════════════════════════════════════════════
               x     Freq        %     Cum. Freq     Cum. %
      ───────────────────────────────────────────────────────
               A        3     27.3             3       27.3
               B        4     36.4             7       63.6
               C        2     18.2             9       81.8
               D        1      9.1            10       90.9
            <NA>        1      9.1            11      100.0
      ───────────────────────────────────────────────────────
           Total       11     100%            11       100%
      ═══════════════════════════════════════════════════════

# if factor has other ordering freq will use that

    Code
      freq(df, x, sort = F, plot = F, markdown = F)
    Output
      
      Variable: x
      Class: factor
      NA's: 1 (9.1%)
      ═══════════════════════════════════════════════════════
               x     Freq        %     Cum. Freq     Cum. %
      ───────────────────────────────────────────────────────
               D        1      9.1             1        9.1
               B        4     36.4             5       45.5
               C        2     18.2             7       63.6
               A        3     27.3            10       90.9
            <NA>        1      9.1            11      100.0
      ───────────────────────────────────────────────────────
           Total       11     100%            11       100%
      ═══════════════════════════════════════════════════════

# an ordered variable is also retained

    Code
      freq(df, x, sort = F, plot = F, markdown = F)
    Output
      
      Variable: x
      Class: factor
      NA's: 1 (9.1%)
      ═══════════════════════════════════════════════════════
               x     Freq        %     Cum. Freq     Cum. %
      ───────────────────────────────────────────────────────
               D        1      9.1             1        9.1
               C        2     18.2             3       27.3
               B        4     36.4             7       63.6
               A        3     27.3            10       90.9
            <NA>        1      9.1            11      100.0
      ───────────────────────────────────────────────────────
           Total       11     100%            11       100%
      ═══════════════════════════════════════════════════════

# an ordered variable is overrode by sort

    Code
      freq(df, x, sort = T, plot = F, markdown = F)
    Output
      
      Variable: x
      Class: factor
      NA's: 1 (9.1%)
      ═══════════════════════════════════════════════════════
               x     Freq        %     Cum. Freq     Cum. %
      ───────────────────────────────────────────────────────
               B        4     36.4             4       36.4
               A        3     27.3             7       63.6
               C        2     18.2             9       81.8
               D        1      9.1            10       90.9
            <NA>        1      9.1            11      100.0
      ───────────────────────────────────────────────────────
           Total       11     100%            11       100%
      ═══════════════════════════════════════════════════════

# a numeric is also sorted

    Code
      freq(df, x, sort = F, plot = F, markdown = F)
    Output
      
      Variable: x
      Class: factor
      NA's: 1 (11.1%)
      ═══════════════════════════════════════════════════════
               x     Freq        %     Cum. Freq     Cum. %
      ───────────────────────────────────────────────────────
               1        3     33.3             3       33.3
               2        4     44.4             7       77.8
              10        1     11.1             8       88.9
            <NA>        1     11.1             9      100.0
      ───────────────────────────────────────────────────────
           Total        9     100%             9       100%
      ═══════════════════════════════════════════════════════
