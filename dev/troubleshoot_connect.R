

# Load required libraries
library(DBI)
library(RMariaDB)

# Define connection parameters manually for testing
host <- "1.100.1.7" # e.g., "127.0.0.1" or "db.example.com"
port <- 3306 # or your custom port
dbname <- "archive_IAV"
username <- "umysqlread"
password <- "pwdmysqlpois%"

# Try to connect
tryCatch({
      con <-dbConnect(
          drv = RMariaDB::MariaDB(),
          dbname = dbname,
          host = host,
          port = port,
          user = username,
          password = password,
          mysql =TRUE
      )# If connection is successful
      message("✅ Connection successful!")
      
# Optional: list tables
      print(dbListTables(con))
      
# Disconnect
      dbDisconnect(con)
    }, error = function(e) {
      message("❌ Connection failed: ", e$message)
    })

