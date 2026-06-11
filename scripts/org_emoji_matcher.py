#!/usr/bin/env python3
"""
org_emoji_matcher.py

Usage (CLI):
  echo '["Clean up local space on iPhone","Optimize current savings returns"]' | python org_emoji_matcher.py
Returns JSON mapping { "Clean up local space on iPhone": ["📱"], ... }.
A task whose best match scores below EMOJI_THRESHOLD maps to [] (no emoji is
better than a wrong emoji).

Environment:
  EMOJI_MODEL      sentence-transformers model name
                   (default: BAAI/bge-small-en-v1.5; a lighter fallback is
                   sentence-transformers/all-MiniLM-L6-v2)
  EMOJI_THRESHOLD  minimum cosine similarity to emit an emoji (default: 0.30)

Requirements:
  pip install sentence-transformers emoji numpy
"""

import sys
import json
import os
from pathlib import Path
from typing import Dict, List, Tuple

try:
    from sentence_transformers import SentenceTransformer
except Exception as e:
    raise RuntimeError("Please install sentence-transformers: pip install sentence-transformers") from e

try:
    import emoji as emoji_pkg
except Exception as e:
    raise RuntimeError("Please install emoji: pip install emoji") from e

import numpy as np

MODEL_NAME = os.environ.get("EMOJI_MODEL", "BAAI/bge-small-en-v1.5")
THRESHOLD = float(os.environ.get("EMOJI_THRESHOLD", "0.50"))
CACHE_DIR = Path.home() / ".cache" / "emoji_matcher"
EMB_CACHE = CACHE_DIR / "emoji_embeddings.npz"
EMOJI_LIST_CACHE = CACHE_DIR / "emoji_list.json"
CACHE_DIR.mkdir(parents=True, exist_ok=True)

# bge-family models expect a short instruction in front of the *query* (the task
# text), but not in front of the indexed documents (the emoji descriptions).
# Other models don't need it; keep it empty for them.
QUERY_PREFIXES = {
    "bge": "Represent this sentence for searching relevant passages: ",
}


def query_prefix_for(model_name: str) -> str:
    name = model_name.lower()
    for key, prefix in QUERY_PREFIXES.items():
        if key in name:
            return prefix
    return ""


# Codepoints that are technically emoji but carry no useful task meaning and
# only add noise to nearest-neighbour matching.  Kept deliberately small (the
# pool stays broad); just the zodiac/astrology block here.
DENY_CODEPOINTS = set(range(0x2648, 0x2653 + 1)) | {0x26CE}  # Aries..Pisces, Ophiuchus


def _emoji_description(e: str) -> str:
    """Human-readable description of E: its demojized name plus any aliases.

    Enriching the text the emoji is embedded from (e.g. "page facing up,
    document") gives each emoji more semantic anchors than its bare name alone.
    Returns an empty string when nothing readable can be derived.
    """
    words: List[str] = []

    name = emoji_pkg.demojize(e, language="en")
    if name.startswith(":") and name.endswith(":"):
        words.append(name.strip(":").replace("_", " "))

    data = emoji_pkg.EMOJI_DATA.get(e, {})
    for alias in data.get("alias", []):
        if alias.startswith(":") and alias.endswith(":"):
            words.append(alias.strip(":").replace("_", " "))

    # de-duplicate while preserving order
    seen = set()
    uniq = []
    for w in words:
        if w and w not in seen:
            seen.add(w)
            uniq.append(w)
    return ", ".join(uniq)


def build_emoji_catalog() -> List[Tuple[str, str]]:
    """
    Return list of (emoji_char, description) pairs.

    Filters out multi-codepoint sequences (flags, skin tones), the zodiac
    block, and anything with no readable description.  The description is the
    emoji's name plus aliases (see `_emoji_description`).
    """
    try:
        all_keys = list(emoji_pkg.EMOJI_DATA.keys())
    except Exception:
        all_keys = list(emoji_pkg.distinct_emoji_list(
            "".join(chr(i) for i in range(0x1F300, 0x1FAFF))))

    emoji_items = []
    for e in all_keys:
        if len(e) == 0:
            continue
        # skip flag sequences (regional indicators) and skin-tone modifiers
        if any(ord(ch) in range(0x1F1E6, 0x1F1FF + 1) for ch in e):
            continue
        # skip the zodiac/astrology block
        if any(ord(ch) in DENY_CODEPOINTS for ch in e):
            continue
        desc = _emoji_description(e)
        if not desc:
            continue
        emoji_items.append((e, desc))

    # unique by description
    seen = set()
    uniq = []
    for e, n in emoji_items:
        if n in seen:
            continue
        seen.add(n)
        uniq.append((e, n))
    return uniq


def load_or_build_catalog():
    if EMOJI_LIST_CACHE.exists():
        try:
            with open(EMOJI_LIST_CACHE, "r", encoding="utf-8") as f:
                data = json.load(f)
                return data  # list of [emoji, description]
        except Exception:
            pass
    catalog = build_emoji_catalog()
    with open(EMOJI_LIST_CACHE, "w", encoding="utf-8") as f:
        json.dump(catalog, f, ensure_ascii=False)
    return catalog


def load_model():
    return SentenceTransformer(MODEL_NAME)


def build_or_load_embeddings(model, catalog):
    # catalog: list of [emoji, description]
    names = [name for (_, name) in catalog]
    if EMB_CACHE.exists():
        try:
            data = np.load(EMB_CACHE, allow_pickle=True)
            cached_names = list(data["names"])
            cached_model = str(data["model"]) if "model" in data else None
            # Names AND model must match: same names with a different model
            # would otherwise return embeddings from the wrong vector space.
            if cached_names == names and cached_model == MODEL_NAME:
                return data["embeddings"]
        except Exception:
            pass
    print("Computing emoji embeddings (this may take a few seconds)...", file=sys.stderr)
    embeddings = model.encode(names, convert_to_numpy=True, show_progress_bar=True)
    np.savez_compressed(EMB_CACHE, names=names, embeddings=embeddings, model=MODEL_NAME)
    return embeddings


def find_top_k(model, emoji_catalog, emoji_embeddings, texts: List[str],
               top_k=1, threshold=0.0, query_prefix=""):
    """
    For each input text, return up to top_k (emoji, name, score) picks whose
    cosine similarity is >= threshold.  Texts below threshold get an empty list.
    `query_prefix` is prepended to each text before encoding (not to emoji
    names); used for instruction-tuned models like bge.
    """
    names = [name for (_, name) in emoji_catalog]
    chars = [ch for (ch, _) in emoji_catalog]
    results = {}

    queries = [query_prefix + t for t in texts] if query_prefix else texts
    enc = model.encode(queries, convert_to_numpy=True, show_progress_bar=False)

    emoji_norm = emoji_embeddings / np.linalg.norm(emoji_embeddings, axis=1, keepdims=True)
    text_norm = enc / np.linalg.norm(enc, axis=1, keepdims=True)
    sims = np.dot(text_norm, emoji_norm.T)
    for i, text in enumerate(texts):
        sim_row = sims[i]
        idx = np.argsort(-sim_row)[:top_k]
        picks = [(chars[j], names[j], float(sim_row[j]))
                 for j in idx if float(sim_row[j]) >= threshold]
        results[text] = picks
    return results


def match(texts: List[str]) -> Dict[str, List[str]]:
    """Map each task string to its (possibly empty) list of emoji chars."""
    catalog = load_or_build_catalog()
    model = load_model()
    emoji_embeddings = build_or_load_embeddings(model, catalog)
    results = find_top_k(
        model, catalog, emoji_embeddings, texts,
        top_k=1, threshold=THRESHOLD, query_prefix=query_prefix_for(MODEL_NAME))
    return {text: [ch for (ch, _name, _score) in results[text]] for text in texts}


def main():
    # read JSON array from stdin or args
    if not sys.stdin.isatty():
        payload = sys.stdin.read()
    elif len(sys.argv) > 1:
        payload = sys.argv[1]
    else:
        print("Usage: echo '[\"task1\",\"task2\"]' | python org_emoji_matcher.py", file=sys.stderr)
        sys.exit(2)

    texts = json.loads(payload)
    if not isinstance(texts, list):
        raise RuntimeError("Input must be a JSON list of strings")

    out = match(texts)
    print(json.dumps(out, ensure_ascii=False))


if __name__ == "__main__":
    main()
