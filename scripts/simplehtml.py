#!/usr/bin/env python3

import sys
import requests

# Requires `pip install readability-lxml`
from readability import Document

def get_page_contents(url):
    try:
        response = requests.get(url)
        doc = Document(response.content.decode('utf-8'))
        return str(doc.summary())
    except Exception as e:
        return str(e)

if __name__ == "__main__":
    if len(sys.argv) == 1:
        print(f"usage: {sys.argv[0]} <url>")
        sys.exit(2)
    print(get_page_contents(sys.argv[1]))
