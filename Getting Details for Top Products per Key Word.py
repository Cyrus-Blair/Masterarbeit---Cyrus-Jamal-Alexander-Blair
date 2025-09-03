# Press Umschalt+F10 to execute it or replace it with your code.
# Press Alt+Umschalt+E to execute Selection in Console
# Press Double Shift to search everywhere for classes, files, tool windows, actions, and settings.

import requests
import csv
from datetime import datetime
from dateutil.relativedelta import relativedelta

### Global variables

directory_input = r"C:\Users\Cyrus Blair\Documents\Uni\Master\Masterarbeit\Der Einfluss des Digital Markets Act auf Self-Preferencing bei Amazon\Code\Getting Sistrix Data via API\Data\Top_Products_per_Key_Word.csv"
directory_output = r"C:\Users\Cyrus Blair\Documents\Uni\Master\Masterarbeit\Der Einfluss des Digital Markets Act auf Self-Preferencing bei Amazon\Code\Getting Sistrix Data via API\Data\Top_Products_per_Key_Word_with_Details.csv"
api_key = 'NV6RfCVrfVtNpUcs5K7SvZYgs6MhZQf8WF'


### Function

def api_call_details_for_top_products(country: str = 'de'):
    """
    Fetch product details from Sistrix for a list of ASINs and write the results to a CSV file.
    """
    header = [
        "keyword", "search_date", "closest_date", "organic_position", "asin",
        "country", "title", "listed_since",
        "price", "currency",
        "seller_name", "seller_id",
        "reviews", "avg_review",
        "keywords_count"
    ]
    rows = []

    base_url = 'https://api.sistrix.com/marketplace.product.overview'

    # Step 1: Read input CSV
    with open(directory_input, mode='r', encoding='utf-8') as infile:
        reader = csv.DictReader(infile, delimiter=';')
        for row in reader:
            asin = row['asin']
            keyword = row['keyword']
            search_date = row['search_date']
            closest_date = row['closest_date']
            organic_position = row.get('organic_position', '')

            payload = {
                'api_key': api_key,
                'asin': asin,
                'country': country,
                'format': 'json',
            }

            try:
                response = requests.post(base_url, data=payload)
                response.raise_for_status()
                data = response.json()

                product = data.get("answer", {}).get("product", {})
                price_data = product.get("price", {})
                seller_data = product.get("seller", {})
                reviews_data = product.get("reviews", {})
                keywords_data = product.get("keywords", {})

                new_row  = [
                    keyword, search_date, closest_date, organic_position, asin,
                    product.get("country", ""),
                    product.get("title", ""),
                    product.get("listed_since", ""),
                    price_data.get("price", ""),
                    price_data.get("currency", ""),
                    seller_data.get("seller_name", ""),
                    seller_data.get("seller_id", ""),
                    reviews_data.get("reviews", ""),
                    reviews_data.get("avg_review", ""),
                    keywords_data.get("count", "")
                ]

                rows.append(new_row)

            except Exception as e:
                print(f"[ERROR] Failed to process ASIN {asin}: {e}")

    # Write to CSV
    with open(directory_output, mode='w', newline='', encoding='utf-8') as file:
        writer = csv.writer(file, delimiter=';')
        writer.writerow(header)
        writer.writerows(rows)

    print(f"[INFO] Successfully wrote data for {len(rows)} ASIN(s) to {directory_output}")


### Function Calls

api_call_details_for_top_products()
