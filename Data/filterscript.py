import pandas as pd

# Load the co-purchase data
copurchase_df = pd.read_csv('copurchase.csv')  # Adjust path as necessary

# Load the metadata
metadata_df = pd.read_csv('products.csv')  # Adjust path as necessary

# Filter metadata to include only rows where 'group' is 'Book'
book_metadata_df = metadata_df[metadata_df['group'] == 'Book']

# Filter co-purchase data to include only rows where both source and target are in the filtered metadata
filtered_copurchase_df = copurchase_df[
    (copurchase_df['Source'].isin(book_metadata_df['id'])) &
    (copurchase_df['Target'].isin(book_metadata_df['id']))
]

# Save the filtered co-purchase data to a new CSV file
filtered_copurchase_df.to_csv('filtered_copurchase_books.csv', index=False)

# Let's see how many records were retained after filtering
print(len(filtered_copurchase_df), len(copurchase_df))  # This will return the number of rows in the filtered and original datasets