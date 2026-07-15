#!/usr/bin/env python3
"""
Fetch Scopus abstracts + author keywords for Ron Johnston's DOI-linked publications.

Usage (from a terminal, while connected to the institutional VPN):

    export SCOPUS_API_KEY="your-key-here"
    python3 fetch_scopus_abstracts.py

The key is read from the environment, never hardcoded here, so it's safe to
keep this script anywhere (including a folder that might later be committed).

Requires no third-party packages (uses only the standard library).

Safe to interrupt with Ctrl+C and re-run: it checkpoints to
scopus_abstracts.json every 10 records and skips DOIs already fetched.
"""

import json
import os
import sys
import time
import urllib.request
import urllib.error

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.join(SCRIPT_DIR, "..")
PUBLICATIONS_JS = os.path.join(REPO_ROOT, "publications_data.js")
OUTPUT_JSON = os.path.join(SCRIPT_DIR, "scopus_abstracts.json")

API_KEY = os.environ.get("SCOPUS_API_KEY")
if not API_KEY:
    sys.exit('Set SCOPUS_API_KEY first, e.g.:\n  export SCOPUS_API_KEY="your-key-here"')

SLEEP_SECONDS = 0.5  # be polite to Elsevier's rate limit


def load_dois():
    with open(PUBLICATIONS_JS, encoding="utf-8") as f:
        text = f.read()
    text = text[len("const PUBLICATIONS = "):].rstrip(";\n")
    recs = json.loads(text)
    dois = []
    for r in recs:
        link = r.get("link") or ""
        if link.startswith("https://doi.org/"):
            dois.append((r["o"], link[len("https://doi.org/"):]))
    return dois


def extract_abstract(resp):
    core = resp.get("coredata", {})
    desc = core.get("dc:description")
    if desc:
        return desc.strip()
    try:
        return resp["item"]["bibrecord"]["head"]["abstracts"].strip()
    except (KeyError, TypeError, AttributeError):
        return None


def extract_keywords(resp):
    kw = resp.get("authkeywords")
    if not kw:
        return []
    if isinstance(kw, str):
        return [k.strip() for k in kw.split("|") if k.strip()]
    entries = kw.get("author-keyword") if isinstance(kw, dict) else kw
    if entries is None:
        return []
    if isinstance(entries, dict):
        entries = [entries]
    out = []
    for e in entries:
        if isinstance(e, dict):
            val = e.get("$")
            if val:
                out.append(val.strip())
        elif isinstance(e, str):
            out.append(e.strip())
    return out


def fetch_one(doi):
    url = f"https://api.elsevier.com/content/abstract/doi/{doi}?view=FULL"
    req = urllib.request.Request(url, headers={
        "X-ELS-APIKey": API_KEY,
        "Accept": "application/json",
    })
    try:
        with urllib.request.urlopen(req, timeout=20) as r:
            data = json.loads(r.read().decode("utf-8"))
    except urllib.error.HTTPError as e:
        body = e.read().decode("utf-8", errors="ignore")
        if e.code == 401:
            return {"status": "unauthorized", "detail": body[:200]}
        if e.code == 404:
            return {"status": "not_found"}
        return {"status": f"http_error_{e.code}", "detail": body[:200]}
    except Exception as e:
        return {"status": "error", "detail": str(e)}

    resp = data.get("abstracts-retrieval-response")
    if not resp:
        return {"status": "no_response"}
    return {
        "status": "ok",
        "abstract": extract_abstract(resp),
        "keywords": extract_keywords(resp),
    }


def main():
    dois = load_dois()
    print(f"Found {len(dois)} DOI-linked publications.")

    results = {}
    if os.path.exists(OUTPUT_JSON):
        with open(OUTPUT_JSON, encoding="utf-8") as f:
            results = json.load(f)
        print(f"Resuming: {len(results)} already fetched.")

    todo = [(o, doi) for o, doi in dois if doi not in results]
    print(f"{len(todo)} remaining to fetch.\n")

    # Fail fast if Scopus isn't granting full-text access from this network.
    preflight_failures = 0
    for i, (o, doi) in enumerate(todo, 1):
        result = fetch_one(doi)
        result["o"] = o
        results[doi] = result

        if result["status"] == "unauthorized":
            preflight_failures += 1
            if i <= 3 and preflight_failures == i:
                print(f"  [{i}] unauthorized: {doi}")
        else:
            preflight_failures = 0  # reset once we see a non-401

        if i == 3 and preflight_failures == 3:
            print("\nAll of the first 3 requests were unauthorized.")
            print("Scopus isn't granting full-text access from this network —")
            print("double check you're connected to the institutional VPN and try again.")
            sys.exit(1)

        if i % 10 == 0 or i == len(todo):
            with open(OUTPUT_JSON, "w", encoding="utf-8") as f:
                json.dump(results, f, ensure_ascii=False, indent=2)
            print(f"[{i}/{len(todo)}] checkpoint saved -> {OUTPUT_JSON}")

        time.sleep(SLEEP_SECONDS)

    with open(OUTPUT_JSON, "w", encoding="utf-8") as f:
        json.dump(results, f, ensure_ascii=False, indent=2)

    ok = sum(1 for v in results.values() if v.get("status") == "ok")
    with_abstract = sum(1 for v in results.values() if v.get("abstract"))
    with_keywords = sum(1 for v in results.values() if v.get("keywords"))
    unauthorized = sum(1 for v in results.values() if v.get("status") == "unauthorized")
    not_found = sum(1 for v in results.values() if v.get("status") == "not_found")

    print()
    print(f"Total processed:   {len(results)}")
    print(f"OK responses:      {ok}")
    print(f"With abstract:     {with_abstract}")
    print(f"With keywords:     {with_keywords}")
    print(f"Unauthorized:      {unauthorized}")
    print(f"Not found:         {not_found}")


if __name__ == "__main__":
    main()
