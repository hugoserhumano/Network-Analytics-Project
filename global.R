#global.R
#global.R
#global.R

# Load data from CSV file
dt.player.transfers = fread("FootballPlayersTransfers_Final.csv")

#### For GLOBAL

# Metadata Page
variable_names <- colnames(dt.player.transfers)
metadata <- data.frame(
  Variable = variable_names,
  Description = c(
    "Club who bought the player",
    "Player's Name",
    "Player's Age",
    "Player's Nationality",
    "Player's Position",
    "Abbreviated Position",
    "Transfermarkt's estimated market value of the player in euros",
    "Club of Origin",
    "Country in which the Club of Origin plays",
    "Transfer fee in euros",
    "Summer or winter",
    "Buyer's League",
    "First year of the season of the transfer, e.g. 2020 for 2020/21",
    "True if the transfer was a loan; False if it wasn't a loan"
  )
)

# Fixing name of one of the clubs
dt.player.transfers$club_buyer <- ifelse(dt.player.transfers$club_buyer == "Stade Brestois 29", "Stade Brest 29", dt.player.transfers$club_buyer)
dt.player.transfers$club_seller <- ifelse(dt.player.transfers$club_seller == "Stade Brestois", "Stade Brest 29", dt.player.transfers$club_seller)

#Main table general stats
# Key the data.table by the "name" column
setkey(dt.player.transfers, name)


num.transf <- nrow(dt.player.transfers)
num.free.transf <- sum(dt.player.transfers$fee == 0, na.rm = TRUE)
num.transf.fee.nan <- nrow(dt.player.transfers[is.na(dt.player.transfers$fee),])
num.loan <- nrow(dt.player.transfers[dt.player.transfers$is_loan == "True",])
num.play <- dt.player.transfers[, .N, by = name][, .N]
num.clubs.buy <-  nrow(dt.player.transfers[unique(dt.player.transfers$club_buyer),])
num.clubs.sell <- nrow(dt.player.transfers[unique(dt.player.transfers$club_seller),])
num.coun.sell <- nrow(dt.player.transfers[unique(dt.player.transfers$dealing_country),])
num.league <- nrow(dt.player.transfers[unique(dt.player.transfers$league),])
num.age <- nrow(dt.player.transfers[unique(dt.player.transfers$age),])
num.nat <- nrow(dt.player.transfers[unique(dt.player.transfers$nationality),])
num.pos <- nrow(dt.player.transfers[unique(dt.player.transfers$position),])
num.seas <- nrow(dt.player.transfers[unique(dt.player.transfers$season),])

stats_df <- data.frame(
  Number_of_Transfers = num.transf, 
  Number_Free_Transfers = num.free.transf,
  Number_Transfers_Null_Fee = num.transf.fee.nan,
  Number_Loans = num.loan,
  Number_Players = num.play,
  Number_Clubs_Buyers = num.clubs.buy,
  Number_Clubs_Sellers = num.clubs.sell,
  Number_Countries_Sellers = num.coun.sell,
  Number_Leagues = num.league,
  Number_Age_Groups = num.age,
  Number_Nationalities = num.nat,
  Number_Positions = num.pos,
  Number_Seasons = num.seas
)

stats_df_t <- t(stats_df)
#stats_df
colnames(stats_df_t) <- c("Stats")
stats_df_t




#chord graph
players <- dt.player.transfers[, list(name = unique(name), type = TRUE)]
league <- dt.player.transfers[, list(name = unique(league), type = FALSE)]
all.vertices <- rbind(players, league)
g2 <- graph.data.frame(dt.player.transfers[, list(league, name)],
                       directed = FALSE,
                       vertices = all.vertices)
g2.league <- bipartite.projection(g2)$proj1
#league inputs
selected_leagues <- selected_league <- unique(dt.player.transfers$league)
# Filter the data by the selected leagues
# Extract the node attributes (i.e., the league of each club)
club_leagues <- V(g2.league)$name
# Filter the graph based on the selected leagues
g.filtered <- g2.league %>%
  delete.vertices(which(!club_leagues %in% selected_leagues))
# Convert the edge list of g.filtered into a data frame
edges_df <- get.data.frame(g.filtered, what = "edges")
# Create a data frame with all possible node combinations from the selected leagues
all_combinations <- tidyr::crossing(from = selected_leagues, to = selected_leagues)
# Left join the all_combinations data frame with the edges_df data frame, filling missing values with zeros
m <- all_combinations %>%
  left_join(edges_df, by = c("from", "to")) %>%
  replace_na(list(weight = 0)) %>%
  pivot_longer(cols = -c(from, to)) %>%
  pivot_wider(names_from = to, values_from = value, values_fill = 0) %>%
  column_to_rownames(var = "from")
# Reorder the matrix according to the selected_leagues
m_reordered <- m[match(selected_leagues, rownames(m)), match(selected_leagues, colnames(m))]

# Network Exploration Page 1

# NOT FILTERED
# Create vertices
all.clubs <- dt.player.transfers[, list(name = unique(name), type = TRUE)]
all.players <- dt.player.transfers[, list(name = unique(club_buyer), type=FALSE)]
all.vertices <- rbind(all.players, all.clubs)

# Creating bipartite graph
g <- graph.data.frame(dt.player.transfers[, list(club_buyer, name)],
                      directed = FALSE,
                      vertices = all.vertices)

# Bipartite projection (clubs)
g.clubs <- bipartite.projection(g)$proj1

# FILTERED
selected.leagues <- unique(dt.player.transfers$league)

#-------------------------------------------------------------------------------------------------------
#Advanced 

dt.player.transfers <- dt.player.transfers %>%
  mutate(fee = replace_na(fee, 0),
         market_value = replace_na(market_value, 0))

selected.clubs.advanced2 <- unique(dt.player.transfers$club_buyer)

dt.player.transfers$diff<- abs(dt.player.transfers$market_value - dt.player.transfers$fee)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Function to preprocess the data and create a feature matrix for each player
create_player_features <- function(dt) {
  player.features <- dt %>%
    select(name, nationality, club_buyer, position, age, market_value, fee, diff) %>%
    group_by(name, .drop = TRUE) %>%
    summarize(
      nationality = first(nationality),
      club = first(club_buyer),
      position = first(position),
      age = first(age),
      market_value = first(market_value),
      fee = first(fee),
      diff = first(diff)
    )
  return(player.features)
}


