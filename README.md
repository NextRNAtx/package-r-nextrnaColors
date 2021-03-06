# package-r-nextrnaColors

Limited package to enable easy plotting of data using ggplot2 and NextRNA corporate colors. Code from [here.](https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2)

### Colors:
- ![#662F8E](https://via.placeholder.com/15/662F8E/000000?text=+)   purple  #662F8E
- ![#F69126](https://via.placeholder.com/15/F69126/000000?text=+)   orange  #F69126
- ![#0d8273](https://via.placeholder.com/15/0d8273/000000?text=+)   teal    #0d8273
- ![#0656c2](https://via.placeholder.com/15/0656c2/000000?text=+)   blue    #0656c2
- ![#b3040c](https://via.placeholder.com/15/b3040c/000000?text=+)   red     #b3040c
- ![#ffc425](https://via.placeholder.com/15/ffc425/000000?text=+)   yellow  #ffc425
- ![#d60d89](https://via.placeholder.com/15/d60d89/000000?text=+)   pink    #d60d89

### Main functions: 
`scale_color_nextrna()` and `scale_fill_nextrna()`

### Usage examples: 
```
ggplot(data = lin28_df, aes(x = exp, y = estimate, label = primary_diagnosis)) + 
  geom_point(aes(color = version), alpha = .8) + scale_color_nextrna()
          
ggplot(data = lin28reg, aes(label = primary_diagnosis)) +
 geom_col(aes(x = as.character(mirna_id), y = estimate, fill = csd_status)) +
 scale_fill_nextrna()

# need discrete = F to create continuous color scale for heatmap
ggplot(cor_df, aes(x = let_version, y = primary_diagnosis, label = p.value)) + 
  geom_raster(aes(fill = estimate)) + scale_fill_nextrna(discrete = F)
```
