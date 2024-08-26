
# SNAP Project: Descriptive Network Analysis
  
#| echo: false
#| output: false
#| message: false
#| label: load packages


# Start with a clear environment
rm(list=ls())

######################################################################################
# The first time you run this file, you will need to install several packages.
# To do that, run the code section below. It may take a couple minutes.
# You only need to install packages once, next time you should skip the install lines.

list.of.packages <- c("robustbase","igraph","statnet", "kableExtra", "poweRlaw")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
if (!require("ggraph")) install.packages("ggraph")
if (!require("igraph")) install.packages("igraph")

# You need to load packages every time you run the script or restart R.
library(dplyr)
library(ggplot2)
library(poweRlaw)
library(ggraph)
library(igraph)

# To check whether your R loads these packages, run the following code
sessionInfo() ## check other attached packages. If igraph is listed there, you're ready!

#getwd()
#setwd('~/Desktop/SNAP')
list.files()

#Start creating network
metadata <- read.csv('products.csv')

# Filter metadata to include only 'Book' category and exclude books with a sales rank of -1, then sort by 'salesrank' in ascending order
top_books_sales <- metadata %>%
  filter(group == "Book" & salesrank >= 1) %>%
  arrange(salesrank)

# Select the top N books (e.g., top 10)
top_10_books_sales <- head(top_books_sales, n = 10)

# Print the top N books to see the result
print(top_10_books_sales)

#########################################################################################################

# Load the filtered co-purchase data and metadata
copurchase <- read.csv("filtered_copurchase_books.csv")

# Start with the top 10 books (seeds)
seed_ids <- top_10_books_sales$id

# Initialize a list to store all products in the snowball sample
snowball_sample <- seed_ids

# Function to get all direct neighbors
get_neighbors <- function(node_ids, copurchase_df) {
  neighbors <- unique(c(copurchase_df$Target[copurchase_df$Source %in% node_ids],
                        copurchase_df$Source[copurchase_df$Target %in% node_ids]))
  return(neighbors)
}

# Iteratively expand the network
for (i in 1:2) {  # Adjust the number of iterations as needed
  new_nodes <- get_neighbors(snowball_sample, copurchase)
  snowball_sample <- unique(c(snowball_sample, new_nodes))
}

# Extract the subgraph for the sampled nodes
final_network <- copurchase %>% filter(Source %in% snowball_sample & Target %in% snowball_sample)

# Create the network graph using igraph
book_graph <- graph_from_data_frame(final_network, directed = TRUE)
edges_df <- igraph::as_data_frame(book_graph, what = "edges")

# Extract nodes (vertices) into a data frame
nodes_df <- igraph::as_data_frame(book_graph, what = "vertices")

# Save edges to a CSV file
write.csv(edges_df, "edges.csv", row.names = FALSE)

# Save nodes to a CSV file
write.csv(nodes_df, "nodes.csv", row.names = FALSE)


# Calculate centrality measures (e.g., degree)
centrality_degree <- degree(book_graph, mode = "all")

# Plot the graph with clearer visualization using ggraph
ggraph(book_graph, layout = "fr") +  # Use Fruchterman-Reingold layout for better spacing
  geom_edge_link(alpha = 0.8, color = "gray") +  # Gray edges with some transparency
  geom_node_point(aes(size = centrality_degree, color = centrality_degree)) +  # Nodes sized and colored by degree centrality
  geom_node_text(aes(label = ifelse(centrality_degree > quantile(centrality_degree, 0.9), name, "")), 
                 vjust = 1.5, hjust = 0.5) +  # Label only the most central nodes
  scale_color_viridis_c() +  # Use a color scale that is colorblind-friendly
  theme_void() +  # Remove axis and grid
  theme(legend.position = "right")  # Position the legend on the right

# Save the plot to a file
ggsave("book_graph_plot.png", width = 10, height = 8)

########################################################################################

#IF we want it to be undirected
## identifying communities of products that are closely related regardless of the direction of the relationship, 
##an undirected graph simplifies the analysis and highlights mutual relationships.

# Convert the directed graph to an undirected graph
undirected_book_graph <- as.undirected(book_graph, mode = "collapse")


# Now apply the Louvain method
louvain_communities <- cluster_louvain(undirected_book_graph)

# Assign community membership to nodes
V(undirected_book_graph)$community <- membership(louvain_communities)

# Opens a new graphics device for plotting, which can sometimes help if the plot isn't appearing.
dev.new()  # Open a new graphics device window

# Visualize the network with communities
ggraph(undirected_book_graph, layout = "fr") +
  geom_edge_link(alpha = 0.8, color = "gray") +
  geom_node_point(aes(size = centrality_degree, color = as.factor(V(undirected_book_graph)$community))) +
  geom_node_text(aes(label = ifelse(centrality_degree > quantile(centrality_degree, 0.9), name, "")),
                 vjust = 1.5, hjust = 0.5) +
  scale_color_viridis_d() +
  theme_void() +
  theme(legend.position = "right")

ggsave("undirected_book_graph_community_plot.png", width = 10, height = 8)

#########################################################################################################

##After detecting communities, analyze the clusters to understand which books are grouped together. 
##This can help in making recommendations or identifying hidden relationships.

# Get cluster memberships for the undirected graph
undirected_clusters <- membership(louvain_communities)

# Add cluster membership to the node attributes
V(undirected_book_graph)$cluster <- undirected_clusters

# Summarize the cluster distribution
undirected_cluster_summary <- table(undirected_clusters)
print(undirected_cluster_summary)

dev.new()  # Open a new graphics device window


# Visualize the clusters in the undirected graph
ggraph(undirected_book_graph, layout = "fr") +
  geom_edge_link(alpha = 0.8, color = "gray") +
  geom_node_point(aes(size = centrality_degree, color = as.factor(undirected_clusters))) +
  geom_node_text(aes(label = ifelse(centrality_degree > quantile(centrality_degree, 0.9), name, "")),
                 vjust = 1.5, hjust = 0.5) +
  scale_color_viridis_d() +
  theme_void() +
  theme(legend.position = "right")

ggsave("undirected_book_graph_cluster_plot.png", width = 10, height = 8)


# Number of clusters in the undirected graph
num_undirected_clusters <- length(unique(undirected_clusters))
print(paste("Number of clusters in the undirected graph:", num_undirected_clusters))

# Example: Analyze the first cluster
cluster_1_books <- V(undirected_book_graph)[undirected_clusters == 1]$name
print(cluster_1_books)

##The list you see under cluster_1_books is the first cluster detected in the network. 
##All these book IDs (e.g., "33", "35", "53", etc.) are considered to be part of the same community within the network.
##This cluster indicates that these books are more closely related to each other in terms of co-purchase behavior than they are to books outside this cluster. 
##People who buy one of these books are more likely to buy another book from this same list.

 #######################################################################################################################################
## If we want it to be directed: 
## Since the relationship between products is not symmetric 
##(i.e., customers buying product A often also buy product B, but not necessarily the other way around), a directed graph preserves this information.
# Detect communities using the Walktrap algorithm
walktrap_communities <- cluster_walktrap(book_graph)

# Assign community membership to nodes
V(book_graph)$community <- membership(walktrap_communities)

# Opens a new graphics device for plotting, which can sometimes help if the plot isn't appearing.
dev.new()  # Open a new graphics device window


# Visualize the network with communities
ggraph(book_graph, layout = "fr") +
  geom_edge_link(alpha = 0.8, color = "gray") +
  geom_node_point(aes(size = centrality_degree, color = as.factor(V(book_graph)$community))) +
  geom_node_text(aes(label = ifelse(centrality_degree > quantile(centrality_degree, 0.9), name, "")),
                 vjust = 1.5, hjust = 0.5) +
  scale_color_viridis_d() +
  theme_void() +
  theme(legend.position = "right")


ggsave("directed_book_graph_community_plot.png", width = 10, height = 8)

#####################################################################################################################
# Get cluster memberships for the directed graph
directed_clusters <- membership(walktrap_communities)

# Add cluster membership to the node attributes
V(book_graph)$cluster <- directed_clusters

# Summarize the cluster distribution
directed_cluster_summary <- table(directed_clusters)
print(directed_cluster_summary)

# Visualize the clusters in the directed graph
ggraph(book_graph, layout = "fr") +
  geom_edge_link(alpha = 0.8, color = "gray") +
  geom_node_point(aes(size = centrality_degree, color = as.factor(directed_clusters))) +
  geom_node_text(aes(label = ifelse(centrality_degree > quantile(centrality_degree, 0.9), name, "")),
                 vjust = 1.5, hjust = 0.5) +
  scale_color_viridis_d() +
  theme_void() +
  theme(legend.position = "right")

ggsave("directed_book_graph_cluster_plot.png", width = 10, height = 8)

# Analyze the first cluster in the directed graph
cluster_1_books_directed <- V(book_graph)[directed_clusters == 1]$name
print(cluster_1_books_directed)

# Create a data frame to compare cluster memberships
cluster_comparison <- data.frame(
  Book = V(book_graph)$name,
  Undirected_Cluster = undirected_clusters,
  Directed_Cluster = directed_clusters
)

# Display the comparison
print(cluster_comparison)

######By comparing the clusters, draw insights into how the directionality of edges influences the grouping of books. For example:
#Undirected Clusters: Tend to group books that are mutually related, ignoring the direction of the co-purchase relationship.
#Directed Clusters: May reveal asymmetries in relationships, where one book leads to another but not necessarily the reverse.

#################################################################################################################
#Calculating other network stats for both directed and undirected graphs

###Density###
undirected_density <- graph.density(undirected_book_graph)
directed_density <- graph.density(book_graph)
print(undirected_density)
print(directed_density)

###Betweenness###
#Retrieve top 5 nodes w/highest betweenness
betweenness_undirected <- betweenness(undirected_book_graph, directed = FALSE)
betweenness_directed <- betweenness(book_graph, directed = TRUE)

betweenness_undirected_df <- data.frame(
  node = as.numeric(names(betweenness_undirected)),
  betweenness = as.numeric(betweenness_undirected)
)

top_5_undirected <- betweenness_undirected_df %>%
  arrange(desc(betweenness)) %>%
  slice_max(order_by = betweenness, n = 5)

# Print the top 5 nodes
print(top_5_undirected)

betweenness_directed_df <- data.frame(
  node = as.numeric(names(betweenness_directed)),
  betweenness = as.numeric(betweenness_directed)
)

top_5_directed <- betweenness_directed_df %>%
  arrange(desc(betweenness)) %>%
  slice_max(order_by = betweenness, n = 5)

# Print the top 5 nodes
print(top_5_directed)

###Closeness##
#Retrieve top 5

closeness_directed <- closeness(book_graph, mode = "out")

# Calculate closeness centrality for the undirected graph
closeness_undirected <- closeness(undirected_book_graph, mode = "all")

closeness_directed_df <- data.frame(
  node = names(closeness_directed),
  closeness = closeness_directed
)

closeness_undirected_df <- data.frame(
  node = names(closeness_undirected),
  closeness = closeness_undirected
)

# Print the top 5 nodes by closeness centrality for directed graph
top_5_closeness_directed <- closeness_directed_df %>%
  arrange(desc(closeness)) %>%
  slice_max(order_by = closeness, n = 5)

print(top_5_closeness_directed)

# Print the top 5 nodes by closeness centrality for undirected graph
top_5_closeness_undirected <- closeness_undirected_df %>%
  arrange(desc(closeness)) %>%
  slice_max(order_by = closeness, n = 5)

print(top_5_closeness_undirected)

####Authority and Hub### (directed only)

hub_scores <- hub_score(book_graph, scale = TRUE)
authorities_scores <- authority_score(book_graph, scale = TRUE)

# Convert to data frames for easier handling
hub_scores_df <- data.frame(
  node = names(hub_scores$vector),
  hub_score = hub_scores$vector
)

authorities_scores_df <- data.frame(
  node = names(authorities_scores$vector),
  authority_score = authorities_scores$vector
)

# Print the top 5 hubs
top_5_hubs <- hub_scores_df %>%
  arrange(desc(hub_score)) %>%
  slice_max(order_by = hub_score, n = 5)

print("Top 5 Hub Scores (Directed Graph):")
print(top_5_hubs)

# Print the top 5 authorities
top_5_authorities <- authorities_scores_df %>%
  arrange(desc(authority_score)) %>%
  slice_max(order_by = authority_score, n = 5)

print("Top 5 Authority Scores (Directed Graph):")
print(top_5_authorities)

###Transtivity###
directed_transitivity <- transitivity(book_graph, type = "global")
print(paste("Transitivity (Directed Graph):", directed_transitivity))

undirected_transitivity <- transitivity(undirected_book_graph, type = "global")
print(paste("Transitivity (Undirected Graph):", undirected_transitivity))

################################################################################

### Run ERGM Analysis###

##Attributes could be salesrank, review count, downloads, ratings from products.csv###
###In this context, the relationship you are modeling is directional: it is about the likelihood of moving from purchasing a book to purchasing another item. The direction matters because it represents a sequential or causal relationship: "If a person purchases a book, they are likely to purchase a specific item next."###

if (!require("ergm")) install.packages("ergm")
if (!require("network")) install.packages("network")

# Load the libraries
library(ergm)
library(network)

directed_book_graph_edges <- read.csv("edges.csv")

# Create the network object, directed = TRUE for a directed graph
book_ergm2_network <- network(directed_book_graph_edges, directed = TRUE)

# Add node attributes from products.csv to the network object
set.vertex.attribute(book_ergm2_network, "salesrank",metadata$salesrank)
set.vertex.attribute(book_ergm2_network, "review_cnt", metadata$review_cnt)
set.vertex.attribute(book_ergm2_network, "downloads", metadata$downloads)
set.vertex.attribute(book_ergm2_network, "rating", metadata$rating)
set.vertex.attribute(book_ergm2_network, "group", metadata$group)

model <- ergm(book_ergm2_network ~ 
                edges + 
                mutual + 
                nodecov("salesrank") + 
                nodematch("review_cnt") +
                nodecov("downloads") +
                nodecov("rating")
            +nodematch("group"))


# Display the results
summary(model)


#Save Rdata
save.image(file = "final.RData")





