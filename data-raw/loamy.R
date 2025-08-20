set.seed(123)
x <- do.call('rbind', lapply(1:3, \(i) data.frame(id = paste0(LETTERS[1:10], i),
                                                  taxpartsize = c("fine-loamy","coarse-loamy","fine-loamy","fine-loamy", "coarse-loamy", "coarse-loamy", "coarse-loamy", "loamy-skeletal", "loamy-skeletal", "loamy-skeletal"),
                                                  depth = runif(10, 35, 150),
                                                  pscs_clay = c(runif(4, 18, 35), runif(6, 14, 18)),
                                                  pscs_frags = c(runif(3, 0, 15), runif(4, 10, 34), runif(3, 35, 60) + c(0, 40, 0)))))
x$taxpartsize <- interaction(cut(x$pscs_clay, c(0, 18, 35, 60, 100)),
                             cut(x$pscs_frags, c(0, 35, 90, 100)), drop = TRUE)
x$taxpartsize <- match(as.character(x$taxpartsize), c("(0,18].(0,35]", "(18,35].(0,35]", "(0,18].(35,90]", "(18,35].(35,90]"))
x$taxpartsize <- c("coarse-loamy", "fine-loamy", "loamy-skeletal", "loamy-skeletal")[x$taxpartsize]
x$taxpartsize[x$depth <= 50] <- "loamy"
x$taxpartsize[x$pscs_frags > 90] <- "fragmental"
loamy <- x
usethis::use_data(loamy, overwrite = TRUE)
