# LEGO CASE STUDY: COMPLETE MODELING WORKFLOW
# Base R only; no additional packages required.
# In RStudio, run this script and select case-study2-data (1).csv.
# The embedded split/fold assignments reproduce the Python report's sampling.
# They assume the original CSV row order is unchanged.

# 1. LOAD AND CLEAN DATA --------------------------------------------------
input_file <- file.choose()
x <- read.csv(input_file, fileEncoding = 'windows-1252',
              stringsAsFactors = FALSE, na.strings = c('', 'NA'))
x$row_id <- seq_len(nrow(x))
x$Amazon_Price <- as.numeric(gsub('[$, ]', '', x$Amazon_Price))
age_text <- gsub('\u00bd', '.5', x$Ages)
matched <- grepl('Ages_[0-9]+', age_text)
x$age <- NA_real_
x$age[matched] <- as.numeric(sub('.*Ages_([0-9]+(?:\\.[0-9]+)?).*',
                                '\\1', age_text[matched], perl = TRUE))
d <- x[!is.na(x$Amazon_Price) & x$Amazon_Price > 0 &
       !is.na(x$Pieces) & x$Pieces > 0, ]
stopifnot(nrow(x) == 1304, nrow(d) == 826)
output_dir <- file.path(dirname(input_file), 'LEGO_modeling_results')
dir.create(output_dir, showWarnings = FALSE)

train_rows <- c(
  535, 345, 80, 463, 591, 657, 51, 167, 564, 8, 197, 286, 346, 151, 203, 595, 86, 390, 133, 729, 10, 712, 668, 464, 527,
  83, 65, 672, 247, 479, 499, 627, 120, 388, 785, 231, 523, 255, 344, 280, 599, 122, 695, 162, 332, 210, 168, 614, 432, 776,
  328, 152, 183, 567, 60, 276, 196, 41, 720, 711, 393, 218, 746, 456, 550, 678, 708, 241, 125, 814, 799, 649, 430, 349, 170,
  275, 370, 35, 457, 562, 268, 651, 164, 397, 160, 32, 184, 783, 317, 436, 477, 333, 470, 598, 532, 756, 339, 121, 267, 128,
  791, 637, 81, 691, 715, 304, 779, 193, 23, 182, 565, 669, 608, 123, 810, 337, 213, 67, 191, 85, 243, 156, 731, 697, 357,
  102, 454, 389, 347, 364, 815, 350, 509, 819, 407, 444, 246, 633, 534, 307, 784, 290, 143, 606, 618, 293, 46, 376, 330, 579,
  700, 422, 27, 269, 64, 737, 753, 803, 34, 403, 375, 119, 16, 398, 43, 416, 175, 780, 751, 801, 94, 622, 764, 518, 787,
  154, 117, 677, 817, 417, 702, 19, 24, 277, 31, 601, 1, 73, 447, 409, 373, 546, 116, 310, 572, 581, 561, 824, 39, 25,
  554, 327, 200, 206, 321, 453, 640, 382, 488, 767, 312, 650, 800, 265, 656, 385, 621, 309, 296, 524, 380, 777, 283, 725, 149,
  379, 522, 396, 106, 72, 115, 235, 671, 802, 438, 673, 50, 244, 426, 303, 279, 180, 202, 158, 458, 762, 232, 664, 658, 625,
  655, 734, 299, 676, 486, 298, 353, 33, 752, 387, 205, 126, 176, 607, 557, 236, 111, 96, 493, 21, 709, 683, 29, 278, 680,
  285, 451, 257, 185, 399, 594, 628, 212, 723, 90, 405, 377, 229, 192, 663, 760, 652, 552, 670, 571, 717, 604, 28, 703, 217,
  249, 190, 684, 392, 501, 71, 288, 693, 641, 537, 22, 806, 555, 772, 816, 369, 336, 585, 220, 234, 224, 515, 415, 289, 689,
  539, 569, 161, 582, 367, 475, 348, 797, 529, 739, 685, 483, 259, 590, 53, 812, 530, 95, 334, 512, 605, 105, 129, 643, 807,
  242, 7, 544, 404, 679, 548, 549, 62, 446, 63, 38, 690, 584, 166, 603, 735, 716, 401, 727, 13, 768, 578, 440, 287, 145,
  592, 414, 642, 602, 478, 26, 82, 362, 134, 2, 615, 20, 30, 335, 502, 795, 351, 84, 496, 331, 54, 826, 174, 437, 489,
  705, 758, 467, 778, 704, 424, 808, 150, 517, 219, 495, 465, 144, 127, 153, 239, 536, 728, 248, 574, 109, 384, 338, 804, 45,
  406, 659, 507, 325, 736, 169, 138, 423, 221, 226, 455, 520, 701, 580, 356, 811, 137, 617, 707, 624, 755, 722, 750, 294, 781,
  11, 794, 541, 252, 208, 394, 70, 195, 573, 630, 113, 326, 613, 313, 433, 503, 342, 612, 733, 508, 623, 442, 282, 408, 188,
  443, 635, 653, 460, 93, 411, 575, 809, 504, 476, 749, 558, 131, 754, 563, 610, 198, 216, 698, 820, 742, 482, 576, 472, 441,
  682, 258, 429, 789, 498, 636, 491, 666, 461, 77, 553, 726, 732, 66, 761, 207, 9, 78, 774, 59, 473, 425, 378, 481, 497,
  632, 796, 320, 365, 413, 142, 89, 648, 324, 322, 61, 487, 386, 420, 434, 302, 805, 297, 316, 771, 47, 87, 110, 361, 646,
  17, 494, 194, 788, 629, 402, 540, 439, 141, 199, 468, 15, 88, 266, 263, 223, 260, 4, 355, 490, 616, 474, 662, 140, 68,
  516, 343, 57, 305, 52, 514, 466, 531, 306, 435, 538, 696, 358, 187, 556, 130, 360, 245, 256, 391, 721, 155, 372, 825, 583,
  786, 159, 766, 782, 272, 485, 147, 718, 395, 419, 577, 818, 681, 589, 291, 412, 631, 214, 359, 181, 381, 100, 136, 177, 505,
  560, 597, 823, 40, 341, 3, 452, 69, 209, 609, 421, 254, 340, 410, 112, 225, 545, 74, 48, 639, 114, 97, 738, 215, 570,
  124, 107, 596, 18, 743, 99, 323, 383, 366, 511
)
test_rows <- c(
  264, 645, 638, 770, 763, 480, 686, 146, 354, 5, 171, 118, 274, 769, 790, 798, 314, 55, 492, 56, 233, 211, 626, 179, 173,
  92, 253, 543, 821, 559, 148, 748, 694, 692, 301, 445, 647, 79, 300, 270, 368, 204, 588, 740, 37, 49, 295, 506, 730, 566,
  747, 675, 427, 533, 674, 542, 525, 58, 238, 132, 513, 667, 91, 271, 178, 448, 526, 230, 6, 634, 315, 172, 428, 363, 462,
  76, 237, 36, 104, 284, 42, 744, 135, 189, 165, 459, 101, 519, 227, 450, 352, 311, 793, 75, 273, 759, 644, 600, 688, 611,
  240, 528, 665, 12, 587, 765, 201, 741, 228, 319, 620, 706, 329, 745, 471, 250, 186, 484, 139, 449, 157, 308, 14, 510, 262,
  586, 654, 374, 619, 281, 699, 773, 792, 98, 661, 44, 547, 400, 551, 719, 261, 713, 103, 568, 318, 222, 108, 163, 710, 431,
  813, 500, 593, 660, 521, 371, 251, 757, 418, 714, 687, 469, 292, 724, 775, 822
)
fold_id <- c(
  2, 3, 5, 5, 3, 1, 3, 4, 5, 1, 4, 1, 3, 1, 5, 2, 5, 5, 4, 3, 3, 3, 2, 4, 2,
  3, 2, 3, 4, 3, 2, 1, 3, 2, 2, 1, 2, 3, 3, 5, 1, 2, 2, 1, 4, 4, 5, 5, 1, 2,
  4, 5, 3, 3, 1, 2, 5, 1, 5, 1, 5, 4, 4, 2, 2, 4, 1, 5, 5, 4, 3, 3, 2, 5, 2,
  2, 4, 5, 1, 1, 4, 3, 1, 3, 3, 1, 5, 5, 5, 4, 3, 2, 4, 2, 3, 3, 5, 4, 5, 5,
  4, 2, 1, 3, 3, 2, 5, 1, 4, 5, 4, 5, 4, 5, 2, 4, 2, 4, 2, 1, 1, 2, 4, 5, 4,
  3, 3, 3, 3, 5, 4, 1, 1, 3, 1, 5, 4, 4, 1, 5, 5, 5, 2, 3, 3, 2, 5, 1, 2, 3,
  1, 1, 3, 4, 5, 1, 2, 2, 5, 2, 3, 4, 1, 3, 2, 3, 2, 4, 4, 1, 1, 2, 1, 3, 4,
  3, 5, 2, 2, 2, 5, 1, 1, 4, 3, 1, 5, 4, 1, 3, 2, 3, 2, 5, 4, 4, 2, 4, 5, 2,
  1, 2, 2, 2, 3, 2, 5, 4, 5, 1, 1, 3, 1, 5, 5, 4, 3, 1, 3, 4, 4, 1, 5, 3, 5,
  4, 1, 2, 4, 2, 2, 2, 4, 3, 2, 3, 1, 2, 4, 3, 4, 3, 2, 2, 5, 4, 1, 4, 3, 2,
  1, 4, 1, 5, 2, 5, 3, 4, 3, 5, 1, 2, 5, 2, 2, 5, 2, 1, 1, 2, 2, 5, 1, 2, 2,
  2, 2, 3, 3, 4, 2, 4, 2, 4, 3, 1, 3, 3, 3, 1, 5, 3, 2, 4, 1, 1, 5, 2, 1, 2,
  1, 5, 4, 1, 5, 5, 4, 2, 3, 2, 1, 1, 4, 2, 1, 5, 2, 2, 4, 5, 2, 5, 5, 5, 4,
  4, 1, 3, 1, 2, 3, 2, 1, 2, 3, 3, 1, 4, 2, 5, 5, 4, 5, 4, 1, 2, 3, 1, 2, 3,
  3, 2, 3, 1, 5, 4, 3, 5, 5, 5, 5, 3, 2, 2, 5, 5, 3, 3, 3, 3, 1, 5, 3, 3, 2,
  1, 1, 5, 1, 1, 5, 2, 5, 4, 3, 5, 1, 4, 4, 4, 5, 2, 1, 4, 5, 2, 3, 2, 1, 3,
  3, 5, 3, 3, 4, 4, 3, 4, 1, 5, 4, 5, 5, 3, 3, 1, 2, 1, 5, 5, 5, 1, 1, 2, 5,
  1, 3, 2, 4, 1, 4, 1, 4, 5, 5, 1, 1, 1, 5, 2, 4, 4, 4, 4, 1, 2, 3, 1, 3, 1,
  4, 5, 4, 3, 3, 2, 2, 2, 1, 4, 4, 4, 4, 2, 1, 5, 3, 5, 2, 4, 4, 4, 5, 5, 1,
  4, 4, 3, 3, 1, 5, 4, 3, 2, 5, 2, 5, 3, 1, 5, 4, 1, 1, 5, 1, 1, 5, 4, 1, 2,
  1, 4, 4, 2, 5, 3, 2, 1, 3, 2, 5, 4, 1, 5, 1, 5, 3, 4, 3, 2, 3, 1, 3, 1, 2,
  2, 3, 3, 3, 4, 5, 3, 3, 3, 3, 1, 1, 5, 2, 5, 1, 2, 1, 1, 5, 1, 4, 4, 2, 1,
  4, 1, 4, 3, 2, 5, 1, 2, 1, 5, 4, 3, 5, 1, 4, 4, 3, 4, 2, 5, 2, 4, 1, 2, 2,
  4, 5, 2, 3, 4, 4, 4, 5, 4, 3, 1, 2, 3, 5, 4, 3, 4, 2, 4, 4, 5, 5, 1, 3, 1,
  3, 5, 2, 2, 3, 4, 2, 3, 5, 5, 2, 4, 4, 1, 4, 3, 1, 4, 5, 2, 3, 5, 1, 5, 4,
  1, 2, 1, 3, 4, 3, 5, 3, 3, 1, 2, 3, 1, 5, 4, 5, 3, 3, 4, 4, 1, 2, 3, 1, 2,
  4, 2, 5, 4, 4, 1, 5, 3, 5, 4
)

train <- x[train_rows, ]
test <- x[test_rows, ]
model_names <- c('Raw price: pieces', 'Log price: log pieces',
                 'Log price: structure', 'Log price: theme',
                 'Log price: theme + construction')

# 2. PREPROCESS USING TRAINING DATA ONLY ----------------------------------
# For log models, imputation is on the transformed scale.
# Missing minifigures can indicate zero OR unknown; never replace all by zero.
# Indicators require >=5 missing training observations to avoid singleton fits.
# Pool themes with fewer than 15 training observations into Other.
make_design <- function(a, b, model) {
  A <- matrix(1, nrow(a), 1, dimnames = list(NULL, 'Intercept'))
  B <- matrix(1, nrow(b), 1, dimnames = list(NULL, 'Intercept'))
  columns <- 'Pieces'
  labels <- if (model == 1) 'pieces' else 'log_pieces'
  if (model >= 3) {
    columns <- c(columns, 'Minifigures', 'age')
    labels <- c(labels, 'log1p_minifigures', 'minimum_age')
  }
  if (model == 5) {
    columns <- c(columns, 'Pages', 'Unique_Pieces')
    labels <- c(labels, 'log_pages', 'log_unique_pieces')
  }
  for (j in seq_along(columns)) {
    col <- columns[j]; aa <- a[[col]]; bb <- b[[col]]
    if (model > 1 && col != 'age') {
      aa <- if (col == 'Minifigures') log1p(aa) else log(aa)
      bb <- if (col == 'Minifigures') log1p(bb) else log(bb)
    }
    missing_a <- is.na(aa); missing_b <- is.na(bb)
    med <- median(aa, na.rm = TRUE)
    aa[missing_a] <- med; bb[missing_b] <- med
    A <- cbind(A, aa); B <- cbind(B, bb)
    colnames(A)[ncol(A)] <- colnames(B)[ncol(B)] <- labels[j]
    if (sum(missing_a) >= 5) {
      A <- cbind(A, as.numeric(missing_a)); B <- cbind(B, as.numeric(missing_b))
      colnames(A)[ncol(A)] <- colnames(B)[ncol(B)] <- paste0(labels[j], '_missing')
    }
  }
  cats <- if (model >= 3) 'Year' else character()
  if (model == 3) cats <- c(cats, 'Size')
  if (model >= 4) cats <- c(cats, 'Theme')
  for (col in cats) {
    aa <- as.character(a[[col]]); bb <- as.character(b[[col]])
    aa[is.na(aa)] <- 'Unknown'; bb[is.na(bb)] <- 'Unknown'
    if (col == 'Theme') {
      counts <- table(aa); common <- names(counts[counts >= 15])
      aa[!aa %in% common] <- 'Other'; bb[!bb %in% common] <- 'Other'
    }
    levels <- sort(unique(aa), method = 'radix')
    if (length(levels) > 1) for (lev in levels[-1]) {
      A <- cbind(A, as.numeric(aa == lev)); B <- cbind(B, as.numeric(bb == lev))
      colnames(A)[ncol(A)] <- colnames(B)[ncol(B)] <- paste(col, lev, sep = '_')
    }
  }
  list(A = A, B = B)
}

# 3. FIT AND BACK-TRANSFORM -----------------------------------------------
fit_predict <- function(a, b, model) {
  design <- make_design(a, b, model)
  y <- if (model == 1) a$Amazon_Price else log(a$Amazon_Price)
  fit <- lm.fit(design$A, y)
  if (fit$rank < ncol(design$A)) stop('Rank-deficient design: inspect predictors.')
  smearing <- if (model == 1) 1 else mean(exp(fit$residuals))
  prediction <- drop(design$B %*% fit$coefficients)
  if (model > 1) prediction <- exp(prediction) * smearing
  list(prediction = prediction, fit = fit, A = design$A,
       B = design$B, y = y, smearing = smearing)
}
rmse <- function(y, p) sqrt(mean((y - p)^2))
mae <- function(y, p) mean(abs(y - p))

# 4. FIVE-FOLD CROSS-VALIDATION ON TRAINING DATA ---------------------------
comparison <- data.frame(Model = model_names, CV_RMSE = NA_real_, CV_MAE = NA_real_)
for (model in 1:5) {
  predictions <- numeric(nrow(train))
  for (fold in 1:5) {
    validation <- fold_id == fold
    predictions[validation] <- fit_predict(train[!validation, ],
                                            train[validation, ], model)$prediction
  }
  comparison$CV_RMSE[model] <- rmse(train$Amazon_Price, predictions)
  comparison$CV_MAE[model] <- mae(train$Amazon_Price, predictions)
}
print(comparison)
write.csv(comparison, file.path(output_dir, 'model_comparison.csv'), row.names = FALSE)
selected <- which.min(comparison$CV_RMSE)
cat('\nSelected model:', model_names[selected], '\n')

# 5. HELD-OUT EVALUATION --------------------------------------------------
result <- fit_predict(train, test, selected)
pred <- result$prediction
metrics <- data.frame(
  Measure = c('Test RMSE', 'Test MAE', 'Test R-squared', 'Mean baseline RMSE', 'Smearing factor'),
  Value = c(rmse(test$Amazon_Price, pred), mae(test$Amazon_Price, pred),
            1 - sum((test$Amazon_Price - pred)^2) / sum((test$Amazon_Price - mean(test$Amazon_Price))^2),
            rmse(test$Amazon_Price, rep(mean(train$Amazon_Price), nrow(test))), result$smearing))
print(metrics)
write.csv(metrics, file.path(output_dir, 'test_metrics.csv'), row.names = FALSE)
held_out <- data.frame(Item_Number = test$Item_Number, Set_Name = test$Set_Name,
                       Observed = test$Amazon_Price, Predicted = pred,
                       Error = pred - test$Amazon_Price)
write.csv(held_out, file.path(output_dir, 'held_out_predictions.csv'), row.names = FALSE)
print(head(held_out[order(abs(held_out$Error), decreasing = TRUE), ], 5))

# 6. HC3 ROBUST COEFFICIENT INTERVALS --------------------------------------
# Intervals are conditional on the selected model and imputed predictors.
# They do not account for model-selection uncertainty.
A <- result$A; e <- result$fit$residuals; fitted <- result$fit$fitted.values
n <- nrow(A); p <- ncol(A)
bread <- solve(crossprod(A))
h <- rowSums((A %*% bread) * A)
V_HC3 <- bread %*% crossprod(A, A * as.numeric((e / (1 - h))^2)) %*% bread
se <- sqrt(diag(V_HC3)); beta <- result$fit$coefficients
critical <- qt(.975, df = n - p)
coefficients <- data.frame(Term = colnames(A), Estimate = beta, HC3_SE = se,
                           CI_low = beta - critical * se, CI_high = beta + critical * se,
                           p_value = 2 * pt(abs(beta / se), df = n - p, lower.tail = FALSE))
print(coefficients, row.names = FALSE)
write.csv(coefficients, file.path(output_dir, 'coefficients.csv'), row.names = FALSE)

# 7. DIAGNOSTICS AND INFLUENCE --------------------------------------------
# Studentized Breusch-Pagan LM statistic (same default as statsmodels).
aux <- lm.fit(A, e^2)
aux_r2 <- 1 - sum(aux$residuals^2) / sum((e^2 - mean(e^2))^2)
bp_stat <- n * aux_r2
cat('\nBreusch-Pagan p-value:', pchisq(bp_stat, p - 1, lower.tail = FALSE), '\n')
mse <- sum(e^2) / (n - p)
cooks <- e^2 / (p * mse) * h / (1 - h)^2
largest <- which.max(cooks)
cat('Largest Cook distance:', max(cooks), '\nSet:', train$Set_Name[largest], '\n')
# Sensitivity only: do NOT remove the observation from the primary results.
sensitivity <- fit_predict(train[-largest, ], test, selected)
cat('Sensitivity RMSE excluding this training observation:',
    rmse(test$Amazon_Price, sensitivity$prediction), '\n')

# 8. SAVE COLORLESS GRAPHS ------------------------------------------------
png(file.path(output_dir, 'model_comparison.png'), width = 1800, height = 1000, res = 200)
par(mar = c(5, 15, 2, 1))
barplot(rev(comparison$CV_RMSE), names.arg = rev(model_names), horiz = TRUE,
        las = 1, col = 'white', border = 'black',
        xlab = 'Training cross-validation RMSE ($)')
dev.off()
png(file.path(output_dir, 'predictions.png'), width = 1400, height = 1200, res = 200)
limits <- range(c(0, test$Amazon_Price, pred))
plot(test$Amazon_Price, pred, pch = 1, xlim = limits, ylim = limits,
     xlab = 'Observed Amazon price ($)', ylab = 'Predicted Amazon price ($)',
     main = 'Held-out predictions')
abline(0, 1, lty = 2)
dev.off()
png(file.path(output_dir, 'diagnostics.png'), width = 2000, height = 900, res = 200)
par(mfrow = c(1, 2))
plot(fitted, e, pch = 1, xlab = 'Fitted log price', ylab = 'Residual',
     main = 'Training residuals')
abline(h = 0, lty = 2)
qqnorm(e, pch = 1, main = 'Normal Q-Q plot'); qqline(e, col = 'black')
dev.off()
png(file.path(output_dir, 'cooks_distance.png'), width = 1600, height = 1000, res = 200)
plot(cooks, type = 'h', xlab = 'Training observation', ylab = "Cook's distance",
     main = 'Influence diagnostics')
abline(h = 4/n, lty = 2)
dev.off()

# 9. RECORD REPRODUCIBILITY DETAILS ---------------------------------------
write.csv(data.frame(row_id = train_rows, fold = fold_id),
          file.path(output_dir, 'training_split.csv'), row.names = FALSE)
write.csv(data.frame(row_id = test_rows), file.path(output_dir, 'test_split.csv'), row.names = FALSE)
capture.output(sessionInfo(), file = file.path(output_dir, 'R_session_info.txt'))
cat('\nAll results saved in:', output_dir, '\n')
# Do not repeatedly tune models using these test results.
# The 2018-2020 data do not validate predictions for today's market.
