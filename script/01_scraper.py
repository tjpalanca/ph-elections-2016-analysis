import cfscrape
import json

scraper = cfscrape.create_scraper()

def scrape(url):
  data = scraper.get(url).json()
  return data
