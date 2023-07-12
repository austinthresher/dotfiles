#!/usr/bin/env python3

# Expects a CMake keyword as the only argument.
# Uses w3m to display the results

import sys
import os
import requests
import subprocess

if len(sys.argv) == 1:
    sys.exit(f"usage: {sys.argv[0]} <cmake-keyword>")

BASE_URL = "https://cmake.org/cmake/help/latest"
keyword = sys.argv[1]

var_url = f"{BASE_URL}/variable/{keyword}.html"
cmd_url = f"{BASE_URL}/command/{keyword}.html"
search_url = f"{BASE_URL}/search.html?q={keyword}"

try:
    url = var_url
    response = requests.get(url)
    if response.status_code != 200:
        url = cmd_url
        response = requests.get(url)
    if response.status_code != 200:
        print(f"No results found for {keyword}. Try searching at:")
        print(search_url)
        input("Press Enter to continue...")
        sys.exit(0)
    tag = keyword.lower().replace("_", "-")
    os.execvp("w3m", ["w3m", url + f"#{tag}"])

except Exception as e:
    print(f"Error while looking up CMake help for {keyword}")
    print(f"{e}")
    print("Make sure w3m is installed.")
    print("Try visiting one of these URLs:")
    print(var_url)
    print(cmd_url)
    print(search_url)
    input("Press Enter to continue...")
