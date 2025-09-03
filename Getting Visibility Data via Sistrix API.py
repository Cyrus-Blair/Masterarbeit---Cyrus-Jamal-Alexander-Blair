# Press Umschalt+F10 to execute it or replace it with your code.
# Press Alt+Umschalt+E to execute Selection in Console
# Press Double Shift to search everywhere for classes, files, tool windows, actions, and settings.

import os
import requests
import csv
import pandas as pd
import time
from datetime import datetime, timedelta


script_path = r"C:\Users\Cyrus Blair\Documents\Uni\Master\Masterarbeit\Der Einfluss des Digital Markets Act auf Self-Preferencing bei Amazon\Code\Getting Sistrix Data via API\Data"
os.chdir(script_path)

class SistrixAPIHandler:
    def __init__(
        self,
        api_key="NV6RfCVrfVtNpUcs5K7SvZYgs6MhZQf8WF",
        credit_limit=15000,
        remaining_credits=21375-15000,   # Last Reset:08.07.2025 - 15:30 Uhr
        domains=['de']
    ):
        self.api_key = api_key
        self.credit_limit = credit_limit
        self.remaining_credits = remaining_credits
        self.domains = domains
        self.collected_data = []

    def call_product_visibility(self, asin, date_str, domain, max_retries=3):
        """
            Sends a POST request to the Sistrix API for a given ASIN and date.

            Args:
                asin (str): The ASIN of the product.
                date (str): The date in 'YYYY-MM-DD' format.
        """
        baseUrl = 'https://api.sistrix.com/marketplace.visindex.product'
        post = {
            'api_key': self.api_key,
            'asin': asin,
            'country': domain,
            'history': 'true',
            'date': date_str,
            'format': 'json',
        }

        for attempt in range(max_retries + 1):
            try:
                response = requests.post(baseUrl, post, timeout=(5, 15))
                response.raise_for_status()
                return response.json()
            except (ReadTimeout, ConnectionError) as e:
                if attempt < max_retries:
                    wait = 2 ** (attempt + 1)  # exponentieller Backoff: 2s, 4s, 16s, …
                    print(
                        f"[WARNUNG] Versuch {attempt + 1} fehlgeschlagen (ASIN {asin}, Date {date_str}). Warte {wait} s und retry …")
                    time.sleep(wait)
                    continue
                else:
                    print(f"[ERROR] Alle {max_retries + 1} Versuche gescheitert für ASIN {asin}, Date {date_str}: {e}")
                    return {}
            except requests.HTTPError as e:
                # z. B. 4xx/5xx‐Fehler – hier ggf. nicht neu versuchen, sondern direkt abbrechen
                print(f"[ERROR] HTTPError {response.status_code} für ASIN {asin}, Date {date_str}: {e}")
                return {}
            except Exception as e:
                print(f"[ERROR] Unerwarteter Fehler für ASIN {asin}, Date {date_str}: {e}")
                return {}

    def process_response(self, asin, date_str, domain):
        response_json = self.call_product_visibility(asin, date_str, domain)

        # Check for known "no result" error
        if response_json.get("status") == "fail":
            error_list = response_json.get("error", [])
            if error_list and error_list[0].get("error_code") == "1000":
                # Skip printing for expected no result cases
                return
            else:
                print(f"[WARNING] Unexpected API error for ASIN {asin} on {date_str} (domain: {domain})")
                print(f"Response: {response_json}")
                return

        try:
            info = response_json["info"][0]
            answer = response_json["answer"][0]["sichtbarkeitsindex"][0]

            row = {
                "product-asin": answer.get("product-asin"),
                "product-title": answer.get("product-title"),
                "date": answer.get("date"),
                "sichtbarkeitsindex": answer.get("value"),
                "country": info.get("country"),
                "type": info.get("type"),
            }
            self.collected_data.append(row)
        except (KeyError, IndexError, TypeError) as e:
            print(f"[WARNING] Failed to parse response for ASIN {asin} on {date_str}: {e}")
            print(f"rsponse {response_json}")

    def handle(self, products, start_date, end_date):
        if self.remaining_credits <= 0:
            print("No credits left.")
            return

        start = datetime.strptime(start_date, '%Y-%m-%d')
        end = datetime.strptime(end_date, '%Y-%m-%d')
        total_days = (end - start).days

        num_weeks = (total_days + 6) // 7   # Anzahl Wochen (aufrunden, damit der letzte Teil‐Intervall nicht verloren geht)
        total_calls = num_weeks * len(products)
        call_count = 0
        first_time = True

        if total_calls > self.remaining_credits:
            print(f"Not enough credits. Only {self.remaining_credits} credits left.")
            # Limit the number of calls to remaining credits
            call_count = 0

            for domain in self.domains:
                print(f"Handling domain: {domain}")

                if not first_time:
                    # Sleep, um API nicht zu überfluten
                    time.sleep(60)  # 1 min vor jeder neuen Domain

                for week in range(num_weeks):
                    current_date = start + timedelta(days=week * 7)
                    date_str = current_date.strftime('%Y-%m-%d')
                    print(f"Handling week {week + 1}, Date: {date_str}")

                    # Sleep, um API nicht zu überfluten
                    time.sleep(15)  # 15 sek vor jeder Woche

                    if not first_time:
                        self.save_to_csv(f"{domain}_partial_week_{week}.csv")  # Zwischenspeichern

                        # Sleep, um API nicht zu überfluten
                        time.sleep(15)  # 15 sek vor jeder Woche

                    for asin in products:
                        if call_count >= self.remaining_credits:
                            print("Reached the credit limit. Stopping.")
                            self.save_to_csv()
                            return

                        # Kleiner Sleep, um API nicht zu überfluten
                        time.sleep(0.15)  # 150 ms Pause zwischen Calls
                        first_time = False

                        self.process_response(asin, date_str, domain)
                        call_count += 1
                # Proceed with all API calls if there are enough credits

        for domain in self.domains:
            print(f"Handling domain: {domain}")

            if not first_time:
                # Sleep, um API nicht zu überfluten
                time.sleep(60)  # 1 min vor jeder neuen Domain

            for week in range(num_weeks):
                current_date = start + timedelta(days=week * 7)
                date_str = current_date.strftime('%Y-%m-%d')
                print(f"Handling week {week+1}, Date: {date_str}")

                if not first_time:
                    self.save_to_csv(f"{domain}_partial_week_{week+33}.csv")  # Zwischenspeichern

                    # Sleep, um API nicht zu überfluten
                    time.sleep(15)  # 15 sek vor jeder Woche

                for asin in products:
                    self.process_response(asin, date_str, domain)

                    # Kleiner Sleep, um API nicht zu überfluten
                    time.sleep(0.15)  # 150 ms Pause zwischen Calls

                    first_time = False

        self.save_to_csv()

    def save_to_csv(self, filename="product_visibility.csv"):
        df = pd.DataFrame(self.collected_data)
        df.to_csv(filename, sep=';', index=False)
        print(f"Saved {len(df)} records to {filename}")



# Reades in asins of the sample of the Top ranked Products per Key word
data = pd.read_csv("Top_Products_per_Key_Word_with_Details_sampled_new.csv", delimiter=';')
all_asins = data["asin"].unique().tolist()
print(f"Number of ASINs: {len(all_asins)}")


"""
# Domain DE - Alle Wochen 400 Produkte
Visibility_Handler_DE = SistrixAPIHandler()
Visibility_Handler_DE.handle(all_asins, "2023-07-01", "2024-03-01")

# Domain DE - Fehlende Wochen 1. Versuch wegen Error 400 Produkte
Visibility_Handler_DE = SistrixAPIHandler()
Visibility_Handler_DE.handle(all_asins, "2023-10-14", "2024-03-01")

# Domain DE - Fehlende Wochen 2. Versuch wegen Error 400 Produkte
Visibility_Handler_DE = SistrixAPIHandler()
Visibility_Handler_DE.handle(all_asins, "2024-02-03", "2024-03-01")


# Domain FR - Alle Wochen 400 Produkte
Visibility_Handler_FR = SistrixAPIHandler(domains=['fr'])
Visibility_Handler_FR.handle(all_asins, "2023-07-01", "2024-03-01")

# Domain FR - Fehlende Wochen 1. Versuch wegen Error 400 Produkte
Visibility_Handler_FR = SistrixAPIHandler(domains=['fr'])
Visibility_Handler_FR.handle(all_asins, "2023-10-07", "2024-03-01")

# Domain FR - Fehlende Wochen 2. Versuch wegen Error 400 Produkte
Visibility_Handler_FR = SistrixAPIHandler(domains=['fr'])
Visibility_Handler_FR.handle(all_asins, "2023-12-02", "2024-03-01")


# Domain UK - Alle Wochen 400 Produkte
Visibility_Handler_UK = SistrixAPIHandler(domains=['uk'])
Visibility_Handler_UK.handle(all_asins, "2023-07-01", "2024-03-01")

# Domain UK - Fehlende Wochen 1. Versuch wegen Error 400 Produkte
Visibility_Handler_UK = SistrixAPIHandler(domains=['uk'])
Visibility_Handler_UK.handle(all_asins, "2023-12-30", "2024-03-01")


# Domain IT - Alle Wochen 400 Produkte
Visibility_Handler_IT = SistrixAPIHandler(domains=['it'])
Visibility_Handler_IT.handle(all_asins, "2023-07-01", "2024-03-01")

# Domain IT - Fehlende Wochen 1. Versuch wegen Error 400 Produkte
Visibility_Handler_IT = SistrixAPIHandler(domains=['it'])
Visibility_Handler_IT.handle(all_asins, "2024-01-06", "2024-03-01")


# Domain ES - Alle Wochen 400 Produkte
Visibility_Handler_ES = SistrixAPIHandler(domains=['es'])
Visibility_Handler_ES.handle(all_asins, "2023-07-01", "2024-03-01")

# Domain ES - Fehlende Wochen 1. Versuch wegen Error 400 Produkte
Visibility_Handler_ES = SistrixAPIHandler(domains=['es'])
Visibility_Handler_ES.handle(all_asins, "2023-12-30", "2024-03-01")

# Domain ES - Fehlende Wochen 2. Versuch wegen Error 400 Produkte
Visibility_Handler_ES = SistrixAPIHandler(domains=['es'])
Visibility_Handler_ES.handle(all_asins, "2024-02-17", "2024-03-01")
"""

