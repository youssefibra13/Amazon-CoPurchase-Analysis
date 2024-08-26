# Amazon Co-Purchase Analysis
Introduction
Amazon started as a small online bookstore in 1994 and has since grown into a global e-commerce giant, serving over 300 million active users with more than 12 million products across various categories. To maintain its competitive edge, Amazon continuously evolves its business strategies, particularly by personalizing customer experiences through tailored recommendations. This project aims to enhance Amazon's product recommendation system by analyzing co-purchasing patterns, specifically within the books category, to improve personalized marketing tactics.

# Key Research Questions
This project focuses on two primary questions that are critical for optimizing Amazon's recommendation system:

What main two categories of products should Amazon recommend to users if they purchase a book?
If a user purchases a specific book, what specific products in each category should Amazon further recommend?
By answering these questions, we aim to refine Amazon's recommendation algorithms, ensuring customers receive the most relevant and appealing suggestions based on their purchase behavior.

# Data Description & Preliminary Analysis
Our analysis is based on Amazon's co-purchase data, which includes information on products such as books, CDs, music, and videos. The dataset consists of 403,394 nodes and 3,387,388 edges, where each node represents a product, and edges represent co-purchasing relationships between products. We focused on the books category, applying both undirected and directed graph visualizations to explore the network of co-purchases and uncover patterns in consumer behavior.

# Visualization & Findings
To understand the co-purchasing patterns, we utilized undirected and directed graphs:

Undirected Graphs: These helped in identifying clusters of products frequently purchased together, regardless of the order. The analysis highlighted key products with high degree centrality, making them potential candidates for Amazon's recommendation system.

Directed Graphs: These provided insights into the sequential nature of purchases, helping us understand common purchasing paths and how certain products lead to subsequent purchases. This analysis was crucial for tailoring product recommendations to match consumer purchasing patterns.

Our visualizations revealed that certain books, such as "Life of Pi" and "The Young Undergrounds," frequently appear in co-purchase networks, indicating their potential as key products for recommendation.

# Centrality Measures & Further Analysis
We conducted several centrality measures to identify influential products in the network:

Density: The low density of both the undirected (0.015) and directed (0.010) graphs indicates that the networks are sparsely connected, reflecting unique co-purchasing patterns and specialized customer interests.

Betweenness Centrality: Nodes with high betweenness, like "Life of Pi," serve as key connectors between different products, making them crucial for introducing customers to new product categories.

Closeness Centrality: High closeness centrality in certain books suggests they are quick to reach from other nodes, highlighting their potential as central products in the recommendation system.

Authority & Hub Scores: In the directed network, certain books like "Burnt Toast" exhibited high authority, while others showed strong hub scores, indicating their importance in leading to other product purchases.

Transitivity: The transitivity measures (27% for undirected, 22% for directed) suggest that Amazon could leverage these relationships to market related products together, enhancing the overall recommendation strategy.

# Conclusion
This analysis provides Amazon with a comprehensive understanding of co-purchasing patterns within the books category, offering actionable insights to improve their recommendation system. By focusing on key products and leveraging their positions within the network, Amazon can deliver more personalized and effective recommendations, ultimately enhancing customer satisfaction and driving sales.

# Future Work
Future analyses could delve deeper into customer segmentation to further refine recommendations based on individual preferences. Additionally, expanding the analysis to other product categories could provide a broader understanding of Amazon's co-purchasing network.
