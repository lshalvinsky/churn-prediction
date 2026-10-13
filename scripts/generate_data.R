source(file.path("R", "data.R"))

customers <- generate_customers()
path <- save_customers(customers)
message("Generated ", nrow(customers), " customers at ", normalizePath(path))
