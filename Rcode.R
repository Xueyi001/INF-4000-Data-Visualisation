> data_first_1000000 <- head(data,1000000)
> write.table(data_first_1000000,"/Users/chenxueyi/Desktop/title_basics_first_1000000.tsv",sep="\t",row.names=FALSE,quote=FALSE)
> data <- read_delim("/Users/chenxueyi/Desktop/title_basics_first_1000000.tsv",delim="\t")
Rows: 1000000 Columns: 9                                                                                
── Column specification ────────────────────────────────────────────────────────────────────────────────
Delimiter: "\t"
chr (8): tconst, titleType, primaryTitle, originalTitle, startYear, endYear, runtimeMinutes, genres
dbl (1): isAdult

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
> library(tidyverse)
── Attaching core tidyverse packages ──────────────────────────────────────────────── tidyverse 2.0.0 ──
✔ dplyr     1.1.4     ✔ purrr     1.0.2
✔ forcats   1.0.0     ✔ stringr   1.5.1
✔ ggplot2   3.5.1     ✔ tibble    3.2.1
✔ lubridate 1.9.4     ✔ tidyr     1.3.1
── Conflicts ────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
✖ dplyr::filter() masks stats::filter()
✖ dplyr::lag()    masks stats::lag()
ℹ Use the conflicted package to force all conflicts to become errors
> library(cluster)
> library(factoextra)
Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa
> library(corrplot)
corrplot 0.95 loaded
> library(ggplot2)
> library(caret)
Loading required package: lattice

Attaching package: ‘caret’

The following object is masked from ‘package:purrr’:
  
  lift

> library(randomForest)
randomForest 4.7-1.2
Type rfNews() to see new features/changes/bug fixes.

Attaching package: ‘randomForest’

The following object is masked from ‘package:dplyr’:
  
  combine

The following object is masked from ‘package:ggplot2’:
  
  margin

> library(glmnet)
Loading required package: Matrix

Attaching package: ‘Matrix’

The following objects are masked from ‘package:tidyr’:
  
  expand, pack, unpack

Loaded glmnet 4.1-8
> cleaned_data <- data[, !(names(data) %in% c("tconst", "originalTitle"))]
> cleaned_data[cleaned_data == "\\N"] <- NA
> cleaned_data$runtimeMinutes <- ifelse(is.na(cleaned_data$runtimeMinutes), 0, as.numeric(cleaned_data$runtimeMinutes))
> cleaned_data$runtimeMinutes <- scale(cleaned_data$runtimeMinutes)
> library(tidyr)
> cleaned_data <- cleaned_data %>% separate_rows(genres, sep = ",") %>% mutate(genres = trimws(genres))
> unique_genres <- unique(cleaned_data$genres)
> unique_genres <- unique_genres[!is.na(unique_genres)]
> for (genre in unique_genres) {
  +     cleaned_data[[genre]] <- as.integer(cleaned_data$genres == genre)
  +  }
> library(dplyr)
> cleaned_data <- cleaned_data %>% group_by(primaryTitle, isAdult, startYear, endYear, runtimeMinutes, titleType) %>% summarise(across(all_of(unique_genres), ~ max(.x, na.rm = TRUE, default = 0)), .groups = "drop")
> write.csv(cleaned_data, file = "/Users/chenxueyi/Desktop/cleaned_data.csv", row.names = FALSE)
> cleaned_data$titleType <- as.factor(cleaned_data$titleType)
> titleType_dummies <- model.matrix(~ titleType - 1, data = cleaned_data)
> cleaned_data <- cbind(cleaned_data, titleType_dummies)
> cleaned_data$titleType <- NULL
> cleaned_data <- cleaned_data[, !(names(cleaned_data) %in% c("endYear"))]
> output_dir <- "/Users/chenxueyi/Desktop/output"
> if (!dir.exists(output_dir)) {
  +     dir.create(output_dir, recursive = TRUE)
  + }
> data_cleaned <- cleaned_data %>% na.omit()
> data_cleaned$runtimeMinutes <- as.numeric(data_cleaned$runtimeMinutes)
> data_cleaned$startYear <- as.factor(data_cleaned$startYear)
> p_start_year_bar <- ggplot(data_cleaned, aes(x = startYear)) + geom_bar(fill = "purple", color = "black", alpha = 0.7) + labs(title = "Bar Plot: Distribution of Start Year", x = "Start Year", y = "Count") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
> ggsave(filename = paste0(output_dir, "/start_year_bar_distribution.png"), plot = p_start_year_bar)
Saving 7 x 7 in image
> ggplot(data_cleaned, aes(x = runtimeMinutes)) +
  +     geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  +     labs(
  +         title = "Distribution of Movie Runtime Minutes",
  +         x = "Runtime Minutes (scaled)",
  +         y = "Frequency"
  +     ) +
  +     theme_minimal()
> ggplot(data_cleaned, aes(x = startYear, y = runtimeMinutes)) +
  +     geom_boxplot(aes(group = startYear), fill = "coral", alpha = 0.6) +
  +     labs(
  +         title = "Runtime Minutes by Start Year",
  +         x = "Start Year",
  +         y = "Runtime Minutes (scaled)"
  +     ) +
  +     theme_minimal() +
  +     theme(axis.text.x = element_text(angle = 45, hjust = 1))
> genre_counts <- data_cleaned %>% select(Comedy, Drama, Action) %>% summarise(across(everything(), sum)) %>% pivot_longer(cols = everything(), names_to = "Genre", values_to = "Count")
> ggplot(genre_counts, aes(x = Genre, y = Count)) +
  +     geom_bar(stat = "identity", fill = "purple", alpha = 0.7) +
  +     labs(
  +         title = "Distribution of Movie Genres",
  +         x = "Genre",
  +         y = "Count"
  +     ) +
  +     theme_minimal()
> melted_data <- data_cleaned %>% pivot_longer(cols = c(Comedy, Drama, Action), names_to = "Genre", values_to = "Presence") %>% filter(Presence == 1)
> ggplot(melted_data, aes(x = Genre, y = runtimeMinutes)) +
  +     geom_boxplot(fill = "lightblue", alpha = 0.7) +
  +     labs(
  +         title = "Runtime Distribution Across Genres",
  +         x = "Genre",
  +         y = "Runtime Minutes (scaled)"
  +     ) +
  +     theme_minimal()
> genre_year_data <- data_cleaned %>% group_by(startYear) %>% summarise(across(c(Comedy, Drama, Action), sum)) %>% pivot_longer(cols = -startYear, names_to = "Genre", values_to = "Count")
> ggplot(genre_year_data, aes(x = startYear, y = Count, color = Genre, group = Genre)) +
  +     geom_line(size = 1) +
  +     geom_point(size = 2) +
  +     labs(
  +         title = "Trends in Movie Genres Over Years",
  +         x = "Start Year",
  +         y = "Number of Movies"
  +     ) +
  +     theme_minimal()
Warning message:
  Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
ℹ Please use `linewidth` instead.
This warning is displayed once every 8 hours.
Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated. 
> is_adult_genre <- data_cleaned %>% pivot_longer(cols = c(Comedy, Drama, Action), names_to = "Genre", values_to = "Presence") %>% filter(Presence == 1)
> print(head(is_adult_genre))
# A tibble: 6 × 41
primaryTitle  isAdult startYear runtimeMinutes Documentary Short Animation Romance Sport  News Fantasy
<chr>           <dbl> <fct>              <dbl>       <dbl> <dbl>     <dbl>   <dbl> <dbl> <dbl>   <dbl>
1 #1                  0 2005             -0.0731           0     1         0       0     0     0       0
2 #1 Fan: A Da…       0 2005             -0.438            0     1         0       0     0     0       0
3 #1 with a Bu…       0 1984              0.313            0     0         0       0     0     0       0
4 #1 with a Bu…       0 1984              0.313            0     0         0       0     0     0       0
5 #14 with a B…       0 1985             -0.717            0     0         0       1     0     0       0
6 #703 D.L. Hu…       0 2003             -0.717            0     0         0       0     0     0       0
# ℹ 30 more variables: Horror <dbl>, Biography <dbl>, Music <dbl>, War <dbl>, Crime <dbl>,
#   Western <dbl>, Family <dbl>, Adventure <dbl>, History <dbl>, Mystery <dbl>, `Sci-Fi` <dbl>,
#   Musical <dbl>, Thriller <dbl>, `Film-Noir` <dbl>, `Talk-Show` <dbl>, `Game-Show` <dbl>,
#   `Reality-TV` <dbl>, Adult <dbl>, titleTypemovie <dbl>, titleTypeshort <dbl>,
#   titleTypetvEpisode <dbl>, titleTypetvMiniSeries <dbl>, titleTypetvMovie <dbl>,
#   titleTypetvSeries <dbl>, titleTypetvShort <dbl>, titleTypetvSpecial <dbl>, titleTypevideo <dbl>,
#   titleTypevideoGame <dbl>, Genre <chr>, Presence <dbl>
> str(is_adult_genre)
tibble [630,227 × 41] (S3: tbl_df/tbl/data.frame)
$ primaryTitle         : chr [1:630227] "#1" "#1 Fan: A Darkomentary" "#1 with a Bullet" "#1 with a Bullet" ...
$ isAdult              : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ startYear            : Factor w/ 139 levels "1888","1889",..: 118 118 97 97 98 116 76 132 132 130 ...
$ runtimeMinutes       : num [1:630227] -0.0731 -0.438 0.3132 0.3132 -0.717 ...
$ Documentary          : num [1:630227] 0 0 0 0 0 0 1 0 0 0 ...
$ Short                : num [1:630227] 1 1 0 0 0 0 1 0 0 0 ...
$ Animation            : num [1:630227] 0 0 0 0 0 0 0 1 0 0 ...
$ Romance              : num [1:630227] 0 0 0 0 1 0 0 0 0 0 ...
$ Sport                : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ News                 : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ Fantasy              : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ Horror               : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ Biography            : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ Music                : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ War                  : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ Crime                : num [1:630227] 0 0 1 1 0 0 0 0 0 0 ...
$ Western              : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ Family               : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ Adventure            : num [1:630227] 0 0 0 0 0 0 0 1 0 0 ...
$ History              : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ Mystery              : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ Sci-Fi               : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ Musical              : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ Thriller             : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ Film-Noir            : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ Talk-Show            : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ Game-Show            : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ Reality-TV           : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ Adult                : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ titleTypemovie       : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ titleTypeshort       : num [1:630227] 1 0 0 0 0 0 1 0 0 0 ...
$ titleTypetvEpisode   : num [1:630227] 0 0 1 1 1 1 0 1 1 1 ...
$ titleTypetvMiniSeries: num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ titleTypetvMovie     : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ titleTypetvSeries    : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ titleTypetvShort     : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ titleTypetvSpecial   : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ titleTypevideo       : num [1:630227] 0 1 0 0 0 0 0 0 0 0 ...
$ titleTypevideoGame   : num [1:630227] 0 0 0 0 0 0 0 0 0 0 ...
$ Genre                : chr [1:630227] "Drama" "Comedy" "Drama" "Action" ...
$ Presence             : num [1:630227] 1 1 1 1 1 1 1 1 1 1 ...
> data_cleaned$isAdult <- factor(data_cleaned$isAdult, levels = c(0, 1), labels = c("No", "Yes"))
> p_start_year_bar <- ggplot(data_cleaned, aes(x = isAdult)) + geom_bar(fill = "blue", color = "black", alpha = 0.7) + labs(title = "Bar Plot: Distribution of isAdult", x = "isAdult", y = "Count") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
> ggsave(filename = paste0(output_dir, "/isAdult_bar_distribution.png"), plot = p_start_year_bar)
Saving 5.14 x 4.93 in image
> p_runtime <- ggplot(data_cleaned, aes(x = runtimeMinutes)) + geom_histogram(bins = 30, fill = "orange", color = "black", alpha = 0.7) + geom_density(aes(y = after_stat(density) * max(after_stat(count))), color = "red", linewidth = 1) + labs(title = "Distribution of Runtime Minutes", x = "Runtime Minutes", y = "Frequency") + theme_minimal()
> ggsave(filename = paste0(output_dir, "/runtime_minutes_distribution.png"), plot = p_runtime)
Saving 5.14 x 4.93 in image
> genre_data <- cleaned_data %>% select(all_of(unique_genres)) %>% summarise(across(everything(), sum, na.rm = TRUE)) %>% pivot_longer(cols = everything(), names_to = "Genre", values_to = "Count") %>% filter(Count > 0)
Warning message:
  There was 1 warning in `summarise()`.
ℹ In argument: `across(everything(), sum, na.rm = TRUE)`.
Caused by warning:
  ! The `...` argument of `across()` is deprecated as of dplyr 1.1.0.
Supply arguments directly to `.fns` through an anonymous function instead.

# Previously
across(a:b, mean, na.rm = TRUE)

# Now
across(a:b, \(x) mean(x, na.rm = TRUE))
This warning is displayed once every 8 hours.
Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated. 
> p_genre <- ggplot(genre_data, aes(x = reorder(Genre, -Count), y = Count)) + geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) + labs(title = "Distribution of Movie Genres", x = "Genre", y = "Count") + theme(axis.text.x = element_text(angle = 45, hjust = 1), minimal = TRUE)
> ggsave(filename = paste0(output_dir, "/movie_genres_distribution.png"), plot = p_genre)
Saving 5.14 x 4.93 in image
Warning messages:
  1: In plot_theme(plot) :
  The `minimal` theme element is not defined in the element hierarchy.
2: In plot_theme(plot) :
  The `minimal` theme element is not defined in the element hierarchy.

> clustering_data <- data_cleaned %>% select(runtimeMinutes, all_of(unique_genres), starts_with("titleType")) %>% as.data.frame()
> clustering_data <- na.omit(clustering_data)
> set.seed(123)
> wss <- sapply(1:10, function(k) {
  +     kmeans(clustering_data, centers = k, nstart = 25, iter.max = 300)$tot.withinss
  + })
> p_elbow <- ggplot(data.frame(k = 1:10, wss = wss), aes(x = k, y = wss)) + geom_line(color = "blue") + geom_point(color = "red") + labs(title = "Elbow Method for Optimal Clusters", x = "Number of Clusters", y = "Total Within-Cluster Sum of Squares") + theme_minimal()
> ggsave(filename = paste0(output_dir, "/elbow_method.png"), plot = p_elbow)
Saving 5.14 x 4.93 in image
> optimal_k <- 5
> kmeans_result <- kmeans(clustering_data, centers = optimal_k, nstart = 25, iter.max = 300)
> data_cleaned$cluster <- as.factor(kmeans_result$cluster)
> pca_result <- prcomp(clustering_data, scale. = TRUE)
> pca_data <- as.data.frame(pca_result$x[, 1:2])
> pca_data$cluster <- data_cleaned$cluster
> p_clusters <- ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) + geom_point(alpha = 0.7, size = 2) + labs(title = "Clustering Results (PCA)", x = "Principal Component 1", y = "Principal Component 2") + theme_minimal()
> ggsave(filename = paste0(output_dir, "/clustering_results.png"), plot = p_clusters)
Saving 5.14 x 4.93 in image
> cluster_summary <- data_cleaned %>% group_by(cluster) %>% summarise(across(c(runtimeMinutes, all_of(unique_genres)), mean, na.rm = TRUE))
> write.csv(cluster_summary, file = paste0(output_dir, "/cluster_summary.csv"), row.names = FALSE)
> p_cluster_runtime <- ggplot(data_cleaned, aes(x = runtimeMinutes, fill = cluster)) + geom_histogram(bins = 30, position = "dodge", alpha = 0.7) + labs(title = "Runtime Minutes Distribution by Cluster", x = "Runtime Minutes", y = "Count") + theme_minimal()
> ggsave(filename = paste0(output_dir, "/runtime_minutes_by_cluster.png"), plot = p_cluster_runtime)
Saving 5.14 x 4.93 in image
> write.csv(cleaned_data, file = paste0(output_dir, "/cleaned_data_full.csv"), row.names = FALSE)
> write.csv(data_cleaned, file = paste0(output_dir, "/data_cleaned_full.csv"), row.names = FALSE)
> print("K-Means Clustering Results:")
[1] "K-Means Clustering Results:"
> print(kmeans_result)
K-means clustering with 5 clusters of sizes 239411, 864, 147024, 375381, 153693

Cluster means:
  runtimeMinutes Documentary        Short   Animation    Comedy    Romance       Sport        News
1      1.3079747  0.07758207 0.0000710076 0.009185042 0.2350769 0.10516225 0.012292668 0.001474452
2     12.2181835  0.18055556 0.0000000000 0.016203704 0.1250000 0.05092593 0.023148148 0.010416667
3     -0.5045273  0.19702906 0.9635909783 0.114783981 0.2503809 0.02022799 0.007121286 0.003271575
4     -0.3254613  0.06992629 0.0034125329 0.076628812 0.3645070 0.10851109 0.016900163 0.029980207
5     -0.4540791  0.16935709 0.0031361220 0.031315675 0.1366881 0.03859642 0.016070999 0.008406369
Drama    Fantasy      Horror   Biography      Music         War      Crime     Western     Family
1 0.4403891 0.02656937 0.040833546 0.018390968 0.03447210 0.021807686 0.10141556 0.022793439 0.03126423
2 0.4502315 0.01388889 0.010416667 0.047453704 0.04282407 0.052083333 0.08680556 0.003472222 0.05092593
3 0.1717747 0.01501796 0.013378768 0.003332789 0.01403172 0.007012461 0.01152873 0.021309446 0.03533437
4 0.4110464 0.03849689 0.016263476 0.012613851 0.07143143 0.008103767 0.14852110 0.027444117 0.13364022
5 0.1682640 0.02020261 0.009356314 0.009486444 0.04152434 0.007898863 0.02595434 0.009714170 0.05680155
Adventure      Action     History     Mystery      Sci-Fi     Musical   Thriller    Film-Noir
1 0.055035065 0.084766364 0.020429304 0.038373341 0.018157060 0.025249466 0.05337265 3.621387e-03
2 0.118055556 0.065972222 0.116898148 0.028935185 0.004629630 0.002314815 0.03819444 0.000000e+00
3 0.008304767 0.007821852 0.004550278 0.007005659 0.008379584 0.009188976 0.01105942 6.801611e-06
4 0.113298222 0.099016732 0.018732967 0.046915001 0.015680069 0.006055181 0.01702270 0.000000e+00
5 0.050997768 0.060393121 0.012036983 0.011236686 0.012342787 0.013084526 0.01181576 0.000000e+00
Talk-Show    Game-Show   Reality-TV        Adult titleTypemovie titleTypeshort titleTypetvEpisode
1 0.0067791371 0.0019172051 0.0013157290 0.1078271257     0.69231572   4.176918e-05         0.04153109
2 0.0023148148 0.0023148148 0.0115740741 0.0127314815     0.09953704   0.000000e+00         0.01273148
3 0.0001632387 0.0001156274 0.0003876918 0.0026050169     0.00000000   9.428869e-01         0.00000000
4 0.0858061543 0.0492273184 0.0280887951 0.0007592286     0.00000000   0.000000e+00         0.99988279
5 0.0151340660 0.0152576890 0.0165264521 0.0790341785     0.39155329   1.301295e-05         0.00000000
titleTypetvMiniSeries titleTypetvMovie titleTypetvSeries titleTypetvShort titleTypetvSpecial
1          0.0090555572       0.09756026      0.0180860529     8.353835e-06       0.0115074078
2          0.4675925926       0.03703704      0.2627314815     0.000000e+00       0.0347222222
3          0.0001700403       0.00000000      0.0002244532     1.207286e-02       0.0009658287
4          0.0001038944       0.00000000      0.0000000000     1.331980e-05       0.0000000000
5          0.0221350354       0.14508143      0.2577280683     8.848809e-04       0.0256160007
titleTypevideo titleTypevideoGame
1     0.12976012       1.336614e-04
2     0.08101852       4.629630e-03
3     0.04365954       2.040483e-05
4     0.00000000       0.000000e+00
5     0.12252347       3.446481e-02

Clustering vector:
  1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20 
4    3    3    5    4    4    3    3    4    3    4    4    4    5    3    4    4    4    1    5 
22   23   24   25   26   27   28   29   30   31   32   33   34   35   36   37   38   39   40   41 
1    3    3    5    5    1    4    4    1    1    3    4    5    5    3    5    5    4    1    1 
42   43   44   45   46   47   48   49   50   51   52   53   54   55   56   57   58   59   60   61 
5    5    1    4    4    3    5    3    3    1    5    3    4    3    4    4    5    1    3    5 
62   63   64   65   66   67   68   69   70   71   72   73   74   75   76   77   78   79   80   81 
4    5    4    3    5    1    3    3    4    3    4    4    5    1    1    1    1    1    4    3 
82   83   84   85   86   87   88   89   90   91   92   93   94   95   96   97   98   99  100  101 
5    3    1    3    1    5    3    3    3    4    4    4    4    4    4    4    4    4    4    1 
102  103  104  105  106  107  108  109  110  111  112  113  114  115  116  117  118  119  120  121 
4    4    4    4    5    1    4    4    4    4    4    4    4    4    4    1    1    4    4    4 
122  123  124  125  126  127  128  130  132  133  134  135  136  137  138  139  140  141  142  143 
4    4    4    4    4    4    1    1    5    4    4    1    1    4    4    1    4    1    4    5 
144  145  146  147  148  149  150  151  152  153  154  155  156  157  158  159  160  161  162  163 
1    3    5    4    4    4    4    4    4    4    4    4    4    4    4    4    4    4    3    1 
164  165  166  167  168  169  170  171  172  173  174  175  176  177  178  179  180  181  182  183 
4    5    4    4    3    4    4    4    4    5    5    3    5    1    3    3    3    4    3    4 
184  185  186  187  188  189  190  191  192  193  194  195  196  197  198  199  200  201  202  203 
1    5    4    3    1    5    3    4    1    4    1    3    3    3    3    3    3    3    4    1 
204  205  206  207  208  209  210  211  212  213  214  215  216  217  218  219  220  221  222  223 
3    3    5    4    4    3    3    3    3    3    3    1    1    3    3    3    3    3    3    3 
224  225  226  227  228  229  230  231  232  233  234  235  236  237  238  239  240  241  242  243 
3    1    3    3    3    3    3    3    3    3    1    4    4    3    3    4    3    4    5    5 
244  245  246  247  248  249  250  251  252  253  254  255  256  257  258  259  260  261  262  263 
5    1    4    4    4    3    4    4    3    1    1    5    1    4    5    5    4    3    3    3 
264  265  266  267  268  269  270  271  272  273  274  275  276  277  278  279  280  281  282  283 
3    5    4    3    3    3    3    3    3    4    1    1    4    1    3    3    3    1    5    1 
284  285  286  287  288  289  290  291  292  293  294  295  296  297  298  299  300  301  302  303 
1    3    3    4    3    3    4    3    1    4    4    1    3    1    1    1    1    3    3    5 
304  305  306  307  308  309  310  311  312  313  314  315  316  317  318  319  320  321  322  323 
1    5    3    5    5    3    1    4    3    1    1    4    4    5    5    1    3    1    5    3 
324  325  326  327  328  329  330  331  332  333  334  335  337  338  339  340  341  342  343  344 
3    3    3    1    4    3    4    4    3    4    4    4    5    3    3    1    3    4    4    4 
345  346  347  348  349  350  351  352  353  354  355  356  357  358  359  360  361  362  363  364 
4    3    4    4    4    4    1    4    4    4    3    1    5    3    1    1    5    4    5    3 
365  366  367  368  369  370  371  372  373  374  375  376  377  378  379  380  381  382  383  384 
1    4    4    1    3    3    4    4    4    5    4    4    3    3    3    4    4    4    1    1 
385  386  387  388  389  390  391  392  393  394  395  396  397  398  399  400  401  402  403  404 
3    1    5    1    3    3    4    1    3    1    5    3    1    5    3    3    3    3    5    3 
405  406  407  408  409  410  411  412  413  414  415  416  417  418  419  420  421  422  423  424 
3    1    1    4    4    1    3    4    3    4    3    5    5    5    1    1    1    1    1    3 
425  426  427  428  429  430  431  432  433  434  435  436  437  438  439  440  441  442  443  444 
3    1    1    3    1    3    5    1    3    3    1    1    3    1    4    1    3    3    3    3 
445  446  447  448  449  450  451  452  453  454  455  456  457  458  459  460  461  462  463  464 
3    3    4    1    3    4    4    4    1    3    5    5    1    5    1    3    3    3    3    3 
465  466  467  468  469  470  471  472  473  474  475  476  477  478  479  480  481  482  483  484 
3    3    4    3    3    4    3    3    4    1    1    3    3    3    1    4    3    1    3    1 
485  486  487  488  489  490  491  492  493  494  495  496  497  498  499  500  501  502  503  504 
3    5    3    3    3    3    3    5    1    5    1    1    4    3    4    5    3    4    1    4 
505  506  507  508  509  510  511  512  513  514  515  516  517  518  519  520  521  522  523  524 
4    1    4    1    3    3    5    3    5    5    3    5    5    1    3    3    3    3    3    3 
525  526  527  528  529  530  531  532  533  534  535  536  537  538  539  540  541  542  543  544 
3    4    4    3    3    3    3    3    3    3    3    3    3    3    4    1    4    3    1    1 
545  546  547  548  549  550  551  552  553  554  555  556  557  558  559  560  561  562  563  564 
3    3    3    3    3    4    5    3    4    1    3    5    4    4    3    3    4    1    1    4 
565  566  567  568  569  570  571  572  573  574  575  576  577  578  579  580  581  582  583  584 
5    5    5    5    5    3    3    3    3    3    3    3    3    3    3    3    3    3    4    4 
585  586  587  588  589  590  591  592  593  594  595  596  597  598  599  600  601  602  603  604 
4    4    4    4    3    4    4    4    5    5    4    4    4    3    4    1    4    1    5    4 
605  606  607  608  609  610  611  612  613  614  615  616  617  618  619  620  621  622  623  624 
3    4    4    4    1    4    1    4    3    4    4    4    1    4    4    4    4    4    4    4 
625  626  627  628  629  630  632  633  634  635  636  637  638  639  640  641  642  643  644  645 
1    4    1    4    1    4    4    4    3    3    1    4    3    4    1    1    4    4    3    4 
646  647  648  649  650  651  652  653  654  655  656  657  658  659  660  661  662  663  664  665 
3    3    3    3    3    4    4    4    4    4    4    5    3    4    4    4    3    3    4    5 
666  667  668  669  670  671  672  673  674  675  676  677  678  679  680  681  682  683  684  685 
3    3    3    4    4    4    4    4    4    4    4    1    5    4    4    4    3    4    5    1 
686  687  688  689  690  691  692  693  694  695  696  697  698  699  700  701  702  703  704  705 
4    4    4    4    4    4    4    4    4    4    4    4    3    3    3    4    4    3    5    4 
706  707  708  709  710  711  712  713  714  715  716  717  718  719  720  721  722  723  724  725 
4    4    4    1    4    3    1    5    3    1    3    3    3    3    3    3    3    3    5    3 
726  727  728  729  730  731  732  733  734  735  736  737  738  739  740  741  742  743  744  745 
3    3    3    3    3    3    4    5    1    1    5    3    5    4    4    3    3    4    5    4 
746  747  748  749  750  751  752  753  754  755  756  757  758  759  760  761  762  763  764  765 
4    5    4    3    5    4    3    5    1    3    5    4    4    1    5    5    1    1    1    1 
766  767  768  769  770  771  772  773  774  775  776  777  778  779  780  781  782  783  784  785 
4    1    4    4    5    5    4    1    4    5    5    4    4    5    1    5    5    4    5    5 
786  787  788  789  790  791  792  793  794  795  796  797  798  799  800  801  802  804  805  806 
1    3    3    3    4    4    4    4    4    4    4    4    4    4    4    4    4    4    4    4 
807  808  809  810  811  812  813  814  815  816  817  818  819  820  821  822  823  824  825  826 
4    4    4    4    4    4    4    4    4    4    4    4    4    4    4    3    1    3    3    3 
827  828  829  830  831  832  833  834  835  836  837  838  839  840  841  842  843  844  845  846 
4    1    1    4    1    4    4    4    4    4    4    4    4    4    4    4    3    4    1    4 
847  848  849  850  851  852  853  854  855  856  857  858  859  860  861  863  864  865  866  867 
1    4    1    4    1    4    4    4    3    3    1    1    5    4    3    3    5    4    3    3 
868  869  870  871  872  873  874  875  876  877  878  879  880  881  882  883  884  885  886  887 
1    1    3    1    3    1    3    3    1    4    3    3    3    3    2    3    5    1    4    4 
888  889  890  891  892  893  894  895  896  897  898  899  900  901  902  903  904  905  906  907 
1    5    5    3    3    4    1    1    1    3    4    1    4    4    4    4    5    4    4    4 
908  909  910  911  912  913  914  915  916  917  918  919  920  921  922  923  924  925  926  927 
4    4    4    4    4    4    4    4    4    4    4    1    4    4    4    4    4    4    4    4 
928  929  930  931  932  933  934  935  936  937  938  939  940  941  942  943  944  945  946  947 
4    4    4    4    4    4    4    4    4    4    4    4    4    4    4    4    4    4    4    4 
948  949  950  951  952  953  954  955  956  957  958  959  960  961  962  963  964  965  966  967 
5    1    4    4    4    4    4    4    3    1    3    4    1    1    5    4    4    4    4    4 
968  969  970  971  972  973  974  975  976  977  978  979  980  981  982  983  984  985  986  987 
4    1    4    4    1    1    4    1    5    1    4    1    4    5    5    4    4    4    4    3 
988  989  990  991  992  993  994  995  996  997  998  999 1000 1001 1002 1003 1004 1005 1006 1007 
4    4    1    4    4    4    4    4    4    4    4    4    4    4    1    1    4    4    4    4 
[ reached getOption("max.print") -- omitted 915373 entries ]

Within cluster sum of squares by cluster:
  [1] 516766.2 108084.3 145832.9 680367.2 288758.8
(between_SS / total_SS =  41.1 %)

Available components:
  
[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"   
[7] "size"         "iter"         "ifault"      
> write.csv(data_cleaned, file = paste0(output_dir, "/data_with_clusters.csv"), row.names = FALSE)
> cluster_centers <- kmeans_result$centers
> write.csv(cluster_centers, file = paste0(output_dir, "/cluster_centers.csv"), row.names = TRUE)
> print("Cluster Centers:")
[1] "Cluster Centers:"
> print(cluster_centers)
runtimeMinutes Documentary        Short   Animation    Comedy    Romance       Sport        News
1      1.3079747  0.07758207 0.0000710076 0.009185042 0.2350769 0.10516225 0.012292668 0.001474452
2     12.2181835  0.18055556 0.0000000000 0.016203704 0.1250000 0.05092593 0.023148148 0.010416667
3     -0.5045273  0.19702906 0.9635909783 0.114783981 0.2503809 0.02022799 0.007121286 0.003271575
4     -0.3254613  0.06992629 0.0034125329 0.076628812 0.3645070 0.10851109 0.016900163 0.029980207
5     -0.4540791  0.16935709 0.0031361220 0.031315675 0.1366881 0.03859642 0.016070999 0.008406369
Drama    Fantasy      Horror   Biography      Music         War      Crime     Western     Family
1 0.4403891 0.02656937 0.040833546 0.018390968 0.03447210 0.021807686 0.10141556 0.022793439 0.03126423
2 0.4502315 0.01388889 0.010416667 0.047453704 0.04282407 0.052083333 0.08680556 0.003472222 0.05092593
3 0.1717747 0.01501796 0.013378768 0.003332789 0.01403172 0.007012461 0.01152873 0.021309446 0.03533437
4 0.4110464 0.03849689 0.016263476 0.012613851 0.07143143 0.008103767 0.14852110 0.027444117 0.13364022
5 0.1682640 0.02020261 0.009356314 0.009486444 0.04152434 0.007898863 0.02595434 0.009714170 0.05680155
Adventure      Action     History     Mystery      Sci-Fi     Musical   Thriller    Film-Noir
1 0.055035065 0.084766364 0.020429304 0.038373341 0.018157060 0.025249466 0.05337265 3.621387e-03
2 0.118055556 0.065972222 0.116898148 0.028935185 0.004629630 0.002314815 0.03819444 0.000000e+00
3 0.008304767 0.007821852 0.004550278 0.007005659 0.008379584 0.009188976 0.01105942 6.801611e-06
4 0.113298222 0.099016732 0.018732967 0.046915001 0.015680069 0.006055181 0.01702270 0.000000e+00
5 0.050997768 0.060393121 0.012036983 0.011236686 0.012342787 0.013084526 0.01181576 0.000000e+00
Talk-Show    Game-Show   Reality-TV        Adult titleTypemovie titleTypeshort titleTypetvEpisode
1 0.0067791371 0.0019172051 0.0013157290 0.1078271257     0.69231572   4.176918e-05         0.04153109
2 0.0023148148 0.0023148148 0.0115740741 0.0127314815     0.09953704   0.000000e+00         0.01273148
3 0.0001632387 0.0001156274 0.0003876918 0.0026050169     0.00000000   9.428869e-01         0.00000000
4 0.0858061543 0.0492273184 0.0280887951 0.0007592286     0.00000000   0.000000e+00         0.99988279
5 0.0151340660 0.0152576890 0.0165264521 0.0790341785     0.39155329   1.301295e-05         0.00000000
titleTypetvMiniSeries titleTypetvMovie titleTypetvSeries titleTypetvShort titleTypetvSpecial
1          0.0090555572       0.09756026      0.0180860529     8.353835e-06       0.0115074078
2          0.4675925926       0.03703704      0.2627314815     0.000000e+00       0.0347222222
3          0.0001700403       0.00000000      0.0002244532     1.207286e-02       0.0009658287
4          0.0001038944       0.00000000      0.0000000000     1.331980e-05       0.0000000000
5          0.0221350354       0.14508143      0.2577280683     8.848809e-04       0.0256160007
titleTypevideo titleTypevideoGame
1     0.12976012       1.336614e-04
2     0.08101852       4.629630e-03
3     0.04365954       2.040483e-05
4     0.00000000       0.000000e+00
5     0.12252347       3.446481e-02
> print("Within-Cluster Sum of Squares (WSS):")
[1] "Within-Cluster Sum of Squares (WSS):"
> print(kmeans_result$tot.withinss)
[1] 1739809
> wss_data <- data.frame(Cluster = 1:optimal_k, WSS = kmeans_result$withinss)
> write.csv(wss_data, file = paste0(output_dir, "/cluster_wss.csv"), row.names = FALSE)
> write.csv(pca_data, file = paste0(output_dir, "/pca_data_with_clusters.csv"), row.names = FALSE)
> list.files(output_dir)
[1] "cleaned_data_full.csv"            "cluster_centers.csv"             
[3] "cluster_summary.csv"              "cluster_wss.csv"                 
[5] "clustering_results.png"           "data_cleaned_full.csv"           
[7] "data_with_clusters.csv"           "elbow_method.png"                
[9] "isAdult_bar_distribution.png"     "movie_genres_distribution.png"   
[11] "pca_data_with_clusters.csv"       "Rplot.png"                       
[13] "Rplot01.png"                      "Rplot02.png"                     
[15] "Rplot03.png"                      "Rplot04.png"                     
[17] "runtime_minutes_by_cluster.png"   "runtime_minutes_distribution.png"
[19] "start_year_bar_distribution.png" 
> results <- list(
  +     cleaned_data = cleaned_data,
  +     data_cleaned = data_cleaned,
  +     kmeans_result = kmeans_result,
  +     cluster_centers = cluster_centers,
  +     cluster_summary = cluster_summary,
  +     pca_data = pca_data
  + )
> save(results, file = paste0(output_dir, "/analysis_results.RData"))

> library(randomForest)
> data_rf <- data_cleaned
> data_rf$cluster <- as.factor(data_rf$cluster)
> X <- data_rf[, !(names(data_rf) %in% c("cluster"))]
> y <- data_rf$cluster
> set.seed(123)
> train_indices <- createDataPartition(y, p = 0.8, list = FALSE)
> X_train <- X[train_indices, ]
> y_train <- y[train_indices]
> X_test <- X[-train_indices, ]
> y_test <- y[-train_indices]
> str(X_train)
    'data.frame':	733101 obs. of  42 variables:
    $ primaryTitle         : chr  "# 1997 Honda Accord: Gauges Upgrade -" "#1" "#1 Fan: A Darkomentary" "#1 Single" ...
    $ isAdult              : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
    $ startYear            : Factor w/ 139 levels "1888","1889",..: 118 118 118 119 97 98 115 116 76 132 ...
    $ runtimeMinutes       : num  -0.717 -0.0731 -0.438 -0.0731 0.3132 ...
    $ Documentary          : num  0 0 0 0 0 0 0 0 1 0 ...
    $ Short                : num  0 1 1 0 0 0 1 0 1 0 ...
    $ Animation            : num  0 0 0 0 0 0 0 0 0 1 ...
    $ Comedy               : num  0 0 1 0 0 0 0 1 1 0 ...
    $ Romance              : num  0 0 0 0 0 1 0 0 0 0 ...
    $ Sport                : num  0 0 0 0 0 0 0 0 0 0 ...
    $ News                 : num  0 0 0 0 0 0 0 0 0 0 ...
    $ Drama                : num  0 1 0 0 1 1 0 0 0 0 ...
    $ Fantasy              : num  0 0 0 0 0 0 0 0 0 0 ...
    $ Horror               : num  0 0 0 0 0 0 0 0 0 0 ...
    $ Biography            : num  0 0 0 0 0 0 0 0 0 0 ...
    $ Music                : num  0 0 0 0 0 0 0 0 0 0 ...
    $ War                  : num  0 0 0 0 0 0 0 0 0 0 ...
    $ Crime                : num  0 0 0 0 1 0 0 0 0 0 ...
    $ Western              : num  0 0 0 0 0 0 0 0 0 0 ...
    $ Family               : num  0 0 0 0 0 0 0 0 0 0 ...
    $ Adventure            : num  0 0 0 0 0 0 0 0 0 1 ...
    $ Action               : num  0 0 0 0 1 0 0 0 0 1 ...
    $ History              : num  0 0 0 0 0 0 0 0 0 0 ...
    $ Mystery              : num  0 0 0 0 0 0 0 0 0 0 ...
    $ Sci-Fi               : num  0 0 0 0 0 0 0 0 0 0 ...
    $ Musical              : num  0 0 0 0 0 0 0 0 0 0 ...
    $ Thriller             : num  0 0 0 0 0 0 0 0 0 0 ...
    $ Film-Noir            : num  0 0 0 0 0 0 0 0 0 0 ...
    $ Talk-Show            : num  1 0 0 0 0 0 0 0 0 0 ...
    $ Game-Show            : num  0 0 0 0 0 0 0 0 0 0 ...
    $ Reality-TV           : num  0 0 0 1 0 0 0 0 0 0 ...
    $ Adult                : num  0 0 0 0 0 0 0 0 0 0 ...
    $ titleTypemovie       : num  0 0 0 0 0 0 0 0 0 0 ...
    $ titleTypeshort       : num  0 1 0 0 0 0 1 0 1 0 ...
    $ titleTypetvEpisode   : num  1 0 0 0 1 1 0 1 0 1 ...
    $ titleTypetvMiniSeries: num  0 0 0 0 0 0 0 0 0 0 ...
    $ titleTypetvMovie     : num  0 0 0 0 0 0 0 0 0 0 ...
    $ titleTypetvSeries    : num  0 0 0 1 0 0 0 0 0 0 ...
    $ titleTypetvShort     : num  0 0 0 0 0 0 0 0 0 0 ...
    $ titleTypetvSpecial   : num  0 0 0 0 0 0 0 0 0 0 ...
    $ titleTypevideo       : num  0 0 1 0 0 0 0 0 0 0 ...
    $ titleTypevideoGame   : num  0 0 0 0 0 0 0 0 0 0 ...
> sapply(X_train, function(col) if (is.factor(col)) length(levels(col)) else NA)
                              primaryTitle               isAdult             startYear        runtimeMinutes 
                              NA                     2                   139                    NA 
                              Documentary                 Short             Animation                Comedy 
                              NA                    NA                    NA                    NA 
                              Romance                 Sport                  News                 Drama 
                              NA                    NA                    NA                    NA 
                              Fantasy                Horror             Biography                 Music 
                              NA                    NA                    NA                    NA 
                              War                 Crime               Western                Family 
                              NA                    NA                    NA                    NA 
                              Adventure                Action               History               Mystery 
                              NA                    NA                    NA                    NA 
                              Sci-Fi               Musical              Thriller             Film-Noir 
                              NA                    NA                    NA                    NA 
                              Talk-Show             Game-Show            Reality-TV                 Adult 
                              NA                    NA                    NA                    NA 
                              titleTypemovie        titleTypeshort    titleTypetvEpisode titleTypetvMiniSeries 
                              NA                    NA                    NA                    NA 
                              titleTypetvMovie     titleTypetvSeries      titleTypetvShort    titleTypetvSpecial 
                              NA                    NA                    NA                    NA 
                              titleTypevideo    titleTypevideoGame 
                              NA                    NA 
> install.packages("ranger")
  trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/ranger_0.17.0.tgz'
  Content type 'application/x-gzip' length 2555443 bytes (2.4 MB)
   ==================================================
  downloaded 2.4 MB

  The downloaded binary packages are in /var/folders/h1/56sr_wj10mv889dg61jz04ym0000gn/T//RtmpOK2r2r/downloaded_packages
> library(ranger)
  ranger 0.17.0 using 2 threads (default). Change with num.threads in ranger() and predict(), options(Ncpus = N), options(ranger.num.threads = N) or environment variable R_RANGER_NUM_THREADS.
                              
  Attaching package: ‘ranger’
                              
  The following object is masked from ‘package:randomForest’:
                                
  importance
                              
> rf_model <- ranger(
+     formula = cluster ~ ., 
+     data = data.frame(X_train, cluster = y_train), 
+     num.trees = 100
+ )
Growing trees.. Progress: 12%. Estimated remaining time: 3 minutes, 47 seconds.
Growing trees.. Progress: 25%. Estimated remaining time: 3 minutes, 21 seconds.
Growing trees.. Progress: 37%. Estimated remaining time: 2 minutes, 48 seconds.
Growing trees.. Progress: 50%. Estimated remaining time: 2 minutes, 12 seconds.
Growing trees.. Progress: 63%. Estimated remaining time: 1 minute, 35 seconds.
Growing trees.. Progress: 78%. Estimated remaining time: 54 seconds.
Growing trees.. Progress: 91%. Estimated remaining time: 22 seconds.
> print(rf_model)
  Ranger result
                              
  Call:
       ranger(formula = cluster ~ ., data = data.frame(X_train, cluster = y_train),      num.trees = 100) 
                              
       Type:                             Classification 
       Number of trees:                  100 
       Sample size:                      733101 
       Number of independent variables:  42 
       Mtry:                             6 
       Target node size:                 1 
       Variable importance mode:         none 
       Splitrule:                        gini 
       OOB prediction error:             0.05 % 
> train_data <- data.frame(X_train, cluster = y_train)
> rf_model <- ranger(
+     formula = cluster ~ ., 
+     data = train_data,  
+     num.trees = 100,    
+     mtry = sqrt(ncol(X_train)), 
+     importance = "impurity",    
+     probability = TRUE
+ )

> print(rf_model)
  Ranger result
                                                             
         Call:
                                           ranger(formula = cluster ~ ., data = train_data, num.trees = 100,      mtry = sqrt(ncol(X_train)), importance = "impurity", probability = TRUE) 
                                                             
         Type:                             Probability estimation 
         Number of trees:                  100 
         Sample size:                      733101 
         Number of independent variables:  42 
         Mtry:                             6 
         Target node size:                 10 
         Variable importance mode:         impurity 
         Splitrule:                        gini 
         OOB prediction error (Brier s.):  0.001260527 
> importance <- data.frame(
+     Feature = names(rf_model$variable.importance),
+     Importance = rf_model$variable.importance
+ )
> importance <- importance[order(importance$Importance, decreasing = TRUE), ]
> ggplot(importance, aes(x = reorder(Feature, Importance), y = Importance)) + geom_bar(stat = "identity", fill = "steelblue") + coord_flip() + labs(title = "Variable Importance", x = "Features", y = "Importance") + theme_minimal()
> test_data <- data.frame(X_test)
> predictions <- predict(rf_model, data = test_data)
> head(predictions$predictions)
  1            2            3            4           5
  [1,] 1.356760e-06 0.000000e+00 9.999986e-01 0.0000000000 0.000000000
  [2,] 9.750082e-01 2.021716e-03 0.000000e+00 0.0000155521 0.022954560
  [3,] 1.118475e-03 6.723543e-07 0.000000e+00 0.9922141856 0.006666667
  [4,] 9.935763e-01 2.219673e-05 0.000000e+00 0.0000000000 0.006401473
  [5,] 1.629363e-02 1.404324e-04 9.931773e-06 0.0000000000 0.983556004
  [6,] 9.976106e-01 2.109069e-05 0.000000e+00 0.0000000000 0.002368319
> library(caret)
> predicted_classes <- factor(ifelse(predictions$predictions[, 1] > 0.5, 1, 0), levels = levels(y_test))
> conf_matrix <- confusionMatrix(predicted_classes, y_test)
> print(conf_matrix)
  Confusion Matrix and Statistics
                                                             
  Reference
  Prediction     1     2     3     4     5
           1 47860     3     0     6    13
           2     0     0     0     0     0
           3     0     0     0     0     0
           4     0     0     0     0     0
           5     0     0     0     0     0
                                                             
Overall Statistics
                                                             
             Accuracy : 0.9995          
             95% CI : (0.9993, 0.9997)
             No Information Rate : 0.9995          
             P-Value [Acc > NIR] : 0.5564          
                                                             
             Kappa : 0               
                                                             
             Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: 1  Class: 2 Class: 3  Class: 4  Class: 5
Sensitivity            1.0000 0.000e+00       NA 0.0000000 0.0000000
Specificity            0.0000 1.000e+00        1 1.0000000 1.0000000
Pos Pred Value         0.9995       NaN       NA       NaN       NaN
Neg Pred Value            NaN 9.999e-01       NA 0.9998747 0.9997285
Prevalence             0.9995 6.265e-05        0 0.0001253 0.0002715
Detection Rate         0.9995 0.000e+00        0 0.0000000 0.0000000
Detection Prevalence   1.0000 0.000e+00        0 0.0000000 0.0000000
Balanced Accuracy      0.5000 5.000e-01       NA 0.5000000 0.5000000
> saveRDS(rf_model, file = "rf_model.rds")
> rf_model <- readRDS("rf_model.rds")


> library(glmnet)
> library(caret)
> data_ridge <- data_cleaned
> data_ridge$runtimeMinutes <- as.numeric(data_ridge$runtimeMinutes)
> X <- as.matrix(data_ridge[, !(names(data_ridge) %in% c("runtimeMinutes"))])
> y <- data_ridge$runtimeMinutes
> set.seed(123)
> train_indices <- createDataPartition(y, p = 0.8, list = FALSE)
> X_train <- X[train_indices, ]
> y_train <- y[train_indices]
> X_test <- X[-train_indices, ]
> y_test <- y[-train_indices]
> ridge_model <- glmnet(
+     x = X_train, 
+     y = y_train, 
+     alpha = 0,           
+     lambda = 10^seq(10, -2, length = 100) 
+  )
Warning message:
In storage.mode(xd) <- "double" : NAs introduced by coercion
> print(ridge_model)

Call:  glmnet(x = X_train, y = y_train, alpha = 0, lambda = 10^seq(10,      -2, length = 100)) 

    Df  %Dev    Lambda
1   40  0.00 1.000e+10
2   40  0.00 7.565e+09
3   40  0.00 5.722e+09
4   40  0.00 4.329e+09
5   40  0.00 3.275e+09
6   40  0.00 2.477e+09
7   40  0.00 1.874e+09
8   40  0.00 1.417e+09
9   40  0.00 1.072e+09
10  40  0.00 8.111e+08
11  40  0.00 6.136e+08
12  40  0.00 4.642e+08
13  40  0.00 3.511e+08
14  40  0.00 2.656e+08
15  40  0.00 2.009e+08
16  40  0.00 1.520e+08
17  40  0.00 1.150e+08
18  40  0.00 8.697e+07
19  40  0.00 6.579e+07
20  40  0.00 4.977e+07
21  40  0.00 3.765e+07
22  40  0.00 2.848e+07
23  40  0.00 2.154e+07
24  40  0.00 1.630e+07
25  40  0.00 1.233e+07
26  40  0.00 9.326e+06
27  40  0.00 7.055e+06
28  40  0.00 5.337e+06
29  40  0.00 4.037e+06
30  40  0.00 3.054e+06
31  40  0.00 2.310e+06
32  40  0.00 1.748e+06
33  40  0.00 1.322e+06
34  40  0.00 1.000e+06
35  40  0.00 7.565e+05
36  40  0.00 5.722e+05
37  40  0.00 4.329e+05
38  40  0.00 3.275e+05
39  40  0.00 2.477e+05
40  40  0.00 1.874e+05
41  40  0.00 1.417e+05
42  40  0.00 1.072e+05
43  40  0.00 8.111e+04
44  40  0.00 6.136e+04
45  40  0.00 4.642e+04
46  40  0.01 3.511e+04
47  40  0.01 2.656e+04
48  40  0.01 2.009e+04
49  40  0.01 1.520e+04
50  40  0.02 1.150e+04
51  40  0.02 8.697e+03
52  40  0.03 6.579e+03
53  40  0.04 4.977e+03
54  40  0.05 3.765e+03
55  40  0.07 2.848e+03
56  40  0.09 2.154e+03
57  40  0.12 1.630e+03
58  40  0.16 1.233e+03
59  40  0.21 9.330e+02
60  40  0.27 7.060e+02
61  40  0.36 5.340e+02
62  40  0.48 4.040e+02
63  40  0.63 3.050e+02
64  40  0.83 2.310e+02
65  40  1.09 1.750e+02
66  40  1.44 1.320e+02
67  40  1.88 1.000e+02
68  40  2.47 7.600e+01
69  40  3.22 5.700e+01
70  40  4.19 4.300e+01
71  40  5.42 3.300e+01
72  40  6.97 2.500e+01
73  40  8.89 1.900e+01
74  40 11.24 1.400e+01
75  40 14.03 1.100e+01
76  40 17.29 8.000e+00
77  40 20.95 6.000e+00
78  40 24.94 5.000e+00
79  40 29.13 4.000e+00
80  40 33.35 3.000e+00
81  40 37.43 2.000e+00
82  40 41.23 2.000e+00
83  40 44.62 1.000e+00
84  40 47.55 1.000e+00
85  40 49.99 1.000e+00
86  40 51.96 0.000e+00
87  40 53.49 0.000e+00
88  40 54.64 0.000e+00
89  40 55.48 0.000e+00
90  40 56.07 0.000e+00
91  40 56.47 0.000e+00
92  40 56.74 0.000e+00
93  40 56.91 0.000e+00
94  40 57.02 0.000e+00
95  40 57.09 0.000e+00
96  40 57.13 0.000e+00
97  40 57.15 0.000e+00
98  40 57.17 0.000e+00
99  40 57.18 0.000e+00
100 40 57.18 0.000e+00
> cv_ridge <- cv.glmnet(
+     x = X_train, 
+     y = y_train, 
+     alpha = 0, 
+     nfolds = 5 
+ )
There were 11 warnings (use warnings() to see them)
> best_lambda <- cv_ridge$lambda.min
> print(paste("Best Lambda:", best_lambda))
[1] "Best Lambda: 0.0689865398053808"
> plot(cv_ridge)
> final_ridge_model <- glmnet(
+     x = X_train, 
+     y = y_train, 
+     alpha = 0, 
+     lambda = best_lambda
+  )
Warning message:
In storage.mode(xd) <- "double" : NAs introduced by coercion
> y_pred <- predict(final_ridge_model, newx = X_test)
Warning message:
In cbind2(1, newx) %*% nbeta : NAs introduced by coercion
> mse <- mean((y_test - y_pred)^2)
> print(paste("Mean Squared Error:", mse))
[1] "Mean Squared Error: 0.385507872470302"
> ss_total <- sum((y_test - mean(y_test))^2)
> ss_residual <- sum((y_test - y_pred)^2)
> r_squared <- 1 - (ss_residual / ss_total)
> print(paste("R-squared:", r_squared))
[1] "R-squared: 0.608664821406951"
> coefficients <- as.matrix(coef(final_ridge_model))
> print(coefficients)
                                 s0
(Intercept)           -1.9921741896
primaryTitle           0.0000000000
isAdult                0.0000000000
startYear              0.0017488979
Documentary            0.0183095026
Short                 -0.3744569339
Animation             -0.1249485025
Comedy                -0.0057805044
Romance                0.0009451901
Sport                  0.0110928930
News                  -0.1404807512
Drama                  0.0694561604
Fantasy                0.0446492988
Horror                 0.0086123129
Biography              0.0781740644
Music                  0.0323036665
War                    0.1377208574
Crime                  0.0832160740
Western                0.2238570971
Family                -0.0318784700
Adventure              0.1316296403
Action                 0.1091717031
History                0.1090775392
Mystery                0.1539027264
Sci-Fi                 0.0394450792
Musical                0.1149209757
Thriller               0.0050315146
Film-Noir             -0.1820272975
Talk-Show              0.0516238739
Game-Show             -0.0440244477
Reality-TV            -0.0992243157
Adult                  0.0418093874
titleTypemovie         0.1003128887
titleTypeshort        -0.2944121224
titleTypetvEpisode    -0.1509241153
titleTypetvMiniSeries  1.1394935730
titleTypetvMovie       0.3024162120
titleTypetvSeries      0.4165298399
titleTypetvShort      -0.2523291415
titleTypetvSpecial     0.3329627969
titleTypevideo         0.1955470586
titleTypevideoGame    -0.1937630460
cluster               -0.4287717664
> coeff_df <- data.frame(
+     Feature = rownames(coefficients),
+     Coefficient = coefficients[, 1]
+ )
> ggplot(coeff_df, aes(x = reorder(Feature, Coefficient), y = Coefficient)) + geom_bar(stat = "identity") + coord_flip() + labs(title = "Feature Importance", x = "Features", y = "Coefficient")
> save(final_ridge_model, file = paste0(output_dir, "/ridge_model.RData"))
> prediction_results <- data.frame(
+     Actual = y_test,
+     Predicted = y_pred
+ )
> write.csv(prediction_results, file = paste0(output_dir, "/ridge_predictions.csv"), row.names = FALSE)
> 