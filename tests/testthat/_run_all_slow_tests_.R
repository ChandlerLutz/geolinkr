## tests/testthat/_run_all_slow_tests_.R

slow_test_files <- list.files(here::here("tests/slow-tests"), pattern = "^test-.*\\.R$",
                              full.names = TRUE)

lapply(slow_test_files, source)
