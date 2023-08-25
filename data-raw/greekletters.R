greekletters <- data.frame(greek_name = c("Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta",
                                          "Eta", "Theta", "Iota", "Kappa", "Lambda", "Mu", "Nu",
                                          "Xi", "Omicron", "Pi", "Rho", "Sigma", "Tau", "Upsilon",
                                          "Phi", "Chi", "Psi", "Omega"),
                           greek_upper = c("\\u0391", "\\u0392", "\\u0393", "\\u0394", "\\u0395", "\\u0396",
                                           "\\u0397", "\\u0398", "\\u0399", "\\u039a", "\\u039b", "\\u039c",
                                           "\\u039d", "\\u039e", "\\u039f", "\\u03a0", "\\u03a1", "\\u03a3",
                                           "\\u03a4", "\\u03a5", "\\u03a6", "\\u03a7", "\\u03a8", "\\u03a9"),
                           greek_lower = c("\\u03b1", "\\u03b2", "\\u03b3", "\\u03b4", "\\u03b5", "\\u03b6",
                                           "\\u03b7", "\\u03b8", "\\u03b9", "\\u03ba", "\\u03bb", "\\u03bc",
                                           "\\u03bd", "\\u03be", "\\u03bf", "\\u03c0", "\\u03c1", "\\u03c3",
                                           "\\u03c4", "\\u03c5", "\\u03c6", "\\u03c7", "\\u03c8", "\\u03c9"),
                           equivalent = c("A", "B", "G", "D", "E", "Z", "E", "Th", "I", "K", "L", "M", "N",
                                          "X", "O", "P", "Rh", "S", "T", "U", "Ph", "Kh", "Ps", "M"))
usethis::use_data(greekletters, overwrite = TRUE)

