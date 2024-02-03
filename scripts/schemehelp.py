#!/usr/bin/env python3

# Expects a Scheme keyword as the only argument.
# Uses w3m to display the results

import sys
import os
import requests
import subprocess
import urllib.parse

if len(sys.argv) == 1:
    sys.exit(f"usage: {sys.argv[0]} <scheme-keyword>")

BASE_URL = "https://man.scheme.org/"
keyword = sys.argv[1]

url = f"{BASE_URL}/{urllib.parse.quote(keyword)}.3scm"

try:
    response = requests.get(url)
    if response.status_code != 200:
        print(f"No results found for {keyword}")
        input("Press Enter to continue...")
        sys.exit(0)
    os.execvp("w3m", ["w3m", url])

except Exception as e:
    print(f"Error while looking up Scheme help for {keyword}")
    print(f"{e}")
    print("Make sure w3m is installed.")
    print(f"Try visiting {url}")
    input("Press Enter to continue...")
