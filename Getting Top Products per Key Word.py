# Press Umschalt+F10 to execute it or replace it with your code.
# Press Alt+Umschalt+E to execute Selection in Console
# Press Double Shift to search everywhere for classes, files, tool windows, actions, and settings.

import requests
import csv
from datetime import datetime
from dateutil.relativedelta import relativedelta

### Global variables

directory = r"C:\Users\Cyrus Blair\Documents\Uni\Master\Masterarbeit\Der Einfluss des Digital Markets Act auf Self-Preferencing bei Amazon\Code\Getting Sistrix Data via API\Data\Top_Products_per_Key_Word.csv"
api_key = 'NV6RfCVrfVtNpUcs5K7SvZYgs6MhZQf8WF'

key_word_list_waldfogel = ['AA batteries', 'Desk', 'Office chair', 'Weighted blanket', 'baby wipes', 'backpack', 'bath towel', 'bed frame',
                 'bed sheets', 'bluetooth speaker', 'chocolate', 'coconut oil', 'coffee', 'coffee maker', 'computer desk',
                 'diapers', 'dishwasher pods', 'dog bed', 'dog food', 'dresses', 'dumbbells', 'dutch oven', 'earrings',
                 'extension cord', 'face mask', 'fan', 'file folders', 'fish oil', 'gaming chair', 'gift card', 'hand soap',
                 'hdmi cable', 'headphones', 'hoodie', 'ibuprofen', 'immersion blender', 'iphone 11 case', 'iphone charger',
                 'jeans', 'keyboard', 'kids clothes', 'knife', 'laptop bag', 'led light bulb', 'lingerie for women',
                 'long sleeve t shirt men', 'luggage', 'mattress', 'maxi dresses for women', 'melatonin', 'mens underwear',
                 'micro sd card', 'mirror', 'mouse pad', 'mouthwash', 'necklace', 'nuts', 'outdoor rug',
                 'paper towels', 'patio furniture', 'pillow', 'power bank', 'printer paper', 'protein powder',
                 'razors for men', 'rice', 'robe', 'salt', 'sandals for women', 'shelf', 'shoe rack', 'shoes',
                 'shower curtain', 'sleeping bag', 'socks', 'solar lights outdoor', 'storage bins', 'summer dresses for women',
                 'sunglasses for women', 'swimsuit', 'tablet', 'tank tops for women', 'tea', 'toaster', 'toilet paper',
                 'trash bags', 'trash can', 'tv stand', 'umbrella', 'usb c cable', 'vacuum cleaner', 'vitamin d',
                 'watch', 'water bottle', 'water filter', 'wine glasses', 'winter coats', 'wireless earbuds',
                 'wireless mouse', 'yoga mat', "ziploc bags"]

key_word_list = [keyword.lower() for keyword in key_word_list_waldfogel]  # Makes it lowercas so API recognizes it better

### Function

def api_call_top_products_by_keywords(
        keywords: list,
        search_date: str = '2022-11-01',
        end_date: str = '2023-07-01',
        max_retries: int = 24,
        output_csv_path: str = "output.csv"
):
    """
    Fetch SERP data from Sistrix for a list of keywords,
    retrying up to `max_retries` months forward if no data is returned.
    Both the 'from' and 'to' dates shift by one month each retry.
    """
    header = ["keyword", "search_date", "closest_date", "organic_position", "asin"]
    rows = []

    base_url = 'https://api.sistrix.com/marketplace.serp.history'

    # Convert input strings to datetime objects
    orig_from = datetime.strptime(search_date, "%Y-%m-%d")
    orig_to = datetime.strptime(end_date, "%Y-%m-%d")

    for kw in keywords:
        attempt = 0
        current_from = orig_from
        current_to = orig_to
        success = False

        while attempt < max_retries and not success:
            payload = {
                "api_key": api_key,
                "kw": kw,
                "from": current_from.strftime("%Y-%m-%d"),
                "to": current_to.strftime("%Y-%m-%d"),
                "num_dates": "1",
                "format": "json",
            }

            try:
                resp = requests.get(base_url, params=payload, timeout=10)
                data = resp.json()
            except Exception as e:
                print(f"[{kw}] Request error on {payload['from']}–{payload['to']}: {e}")
                break  # abort this keyword

            answer = data.get('answer')
            if not answer or not isinstance(answer, list):
                print(f"[{kw}] Unexpected response format, skipping.")
                break

            # Did we actually get any SERP entries?
            found = False
            for item in answer:
                if isinstance(item, dict) and item.get('SERP'):
                    for serp in item['SERP']:
                        rows.append([
                            kw,
                            payload['from'],
                            serp.get('date', ''),
                            serp.get('organic_position', ''),
                            serp.get('asin', '')
                        ])
                    found = True

            if found:
                print(f"[{kw}] Data found for {payload['from']}–{payload['to']}")
                success = True
            else:
                # advance both from/to by one month
                attempt += 1
                current_from += relativedelta(months=+1)
                current_to += relativedelta(months=+1)
                print(f"[{kw}] No data on {payload['from']}–{payload['to']}, "
                      f"retrying with {current_from.strftime('%Y-%m-%d')}–"
                      f"{current_to.strftime('%Y-%m-%d')} (attempt {attempt})")

        if not success:
            print(f"[{kw}] No data after {max_retries} retries, moving on.")

    with open(directory, mode='w', newline='', encoding='utf-8') as file:
        writer = csv.writer(file, delimiter=';')
        writer.writerow(header)
        writer.writerows(rows)


### Function Calls

api_call_top_products_by_keywords(key_word_list)
