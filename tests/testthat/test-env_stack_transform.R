library(terra)

# creating a list of spatRaster stacks where each element is an environmental variable
# and each layer is a depth

env1_d1 <- rast(ncol = 50, nrow = 50)
values(env1_d1) <- sample(c(1:100), size = 2500, replace = T)
env2_d1 <- rast(ncol = 50, nrow = 50)
values(env2_d1) <- sample(c(1:100), size = 2500, replace = T)
env1_d2 <- rast(ncol = 50, nrow = 50)
values(env1_d2) <- sample(c(1:100), size = 2500, replace = T)
env2_d2 <- rast(ncol = 50, nrow = 50)
values(env2_d2) <- sample(c(1:100), size = 2500, replace = T)

env1 <- c(env1_d1, env1_d2)
env2 <- c(env2_d1, env2_d2)
envs <- list(env1, env2)
envnames <- c("env1", "env2")

# env_stack_transform() test
test_that("env_stack_transform behaves as expected", {
  expect_error(env_stack_transform())
  test_result <- env_stack_transform(envs_all = envs, envs_names = envnames)
  expect_equal(class(test_result), "list")
  expect_length(test_result, 2)
  expect_equal(dim(test_result[[1]])[3], 2)
  expect_equal(class(test_result[[1]])[1], "SpatRaster")
})
