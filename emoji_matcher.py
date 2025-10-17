#!/usr/bin/env python3
"""
emoji_matcher.py

Usage (CLI):
  echo '["Clean up local space on iPhone","Optimize current savings returns"]' | python emoji_matcher.py
Returns JSON mapping { "Clean up local space on iPhone": ["📱", "💾", ...], ... }

Requirements:
  pip install sentence-transformers emoji numpy scikit-learn tqdm
"""

import sys
import json
import os
from pathlib import Path
from typing import List, Dict, Tuple

try:
    from sentence_transformers import SentenceTransformer, util
except Exception as e:
    raise RuntimeError("Please install sentence-transformers: pip install sentence-transformers") from e

try:
    import emoji as emoji_pkg
except Exception as e:
    raise RuntimeError("Please install emoji: pip install emoji") from e

import numpy as np
from tqdm import tqdm

MODEL_NAME = os.environ.get("EMOJI_MODEL", "sentence-transformers/all-MiniLM-L6-v2")
CACHE_DIR = Path.home() / ".cache" / "emoji_matcher"
EMB_CACHE = CACHE_DIR / "emoji_embeddings.npz"
EMOJI_LIST_CACHE = CACHE_DIR / "emoji_list.json"
CACHE_DIR.mkdir(parents=True, exist_ok=True)


def build_emoji_catalog() -> List[Tuple[str, str]]:
    """
    Return list of (emoji_char, short_name) pairs.
    Uses emoji.EMOJI_DATA keys and emoji.demojize to get human-readable names.
    Filters out multi-codepoint sequences (like flags) to keep things small.
    """
    emoji_items = []
    # EMOJI_DATA (dict) contains emoji entries in newer emoji package versions
    try:
        all_keys = list(emoji_pkg.EMOJI_DATA.keys())
    except Exception:
        # fallback: attempt to parse a large sample by iterating through unicode range
        # but typically EMOJI_DATA will exist.
        all_keys = list(emoji_pkg.distinct_emoji_list("".join(chr(i) for i in range(0x1F300, 0x1FAFF))))

    # We prefer single-character (single-rune) emoji candidates to avoid sequences
    for e in all_keys:
        # normalize: skip empty or complex sequences (approx)
        if len(e) == 0:
            continue
        # skip flag sequences or skin-tone modifiers heuristically:
        if any(ord(ch) in range(0x1F1E6, 0x1F1FF + 1) for ch in e):  # flags contain regional indicators
            continue
        # get short name via demojize and tidy it
        name = emoji_pkg.demojize(e, language='en')
        # demojize returns like ":mobile_phone:" — strip colons and underscores
        if name.startswith(":") and name.endswith(":"):
            name = name.strip(":").replace("_", " ")
        else:
            # fallback: use unicode codepoint
            name = "emoji " + "+".join(hex(ord(ch)) for ch in e)
        emoji_items.append((e, name))
    # unique by name
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
                return data  # list of [emoji, name]
        except Exception:
            pass
    catalog = build_emoji_catalog()
    with open(EMOJI_LIST_CACHE, "w", encoding="utf-8") as f:
        json.dump(catalog, f, ensure_ascii=False)
    return catalog


def load_model():
    model = SentenceTransformer(MODEL_NAME)
    return model


def build_or_load_embeddings(model, catalog):
    # catalog: list of [emoji, name]
    names = [name for (_, name) in catalog]
    if EMB_CACHE.exists():
        try:
            data = np.load(EMB_CACHE, allow_pickle=True)
            cached_names = list(data["names"])
            if cached_names == names:
                emb = data["embeddings"]
                return emb
        except Exception:
            pass
    # compute embeddings
    print("Computing emoji embeddings (this may take a few seconds)...", file=sys.stderr)
    embeddings = model.encode(names, convert_to_numpy=True, show_progress_bar=True)
    np.savez_compressed(EMB_CACHE, names=names, embeddings=embeddings)
    return embeddings


def find_top_k(model, emoji_catalog, emoji_embeddings, texts: List[str], top_k=3):
    """
    For each input text, compute embedding and return top_k emoji chars
    """
    names = [name for (_, name) in emoji_catalog]
    chars = [ch for (ch, _) in emoji_catalog]
    results = {}
    # encode input texts in batch
    enc = model.encode(texts, convert_to_numpy=True, show_progress_bar=False)
    # normalize for cosine speed
    emoji_norm = emoji_embeddings / np.linalg.norm(emoji_embeddings, axis=1, keepdims=True)
    text_norm = enc / np.linalg.norm(enc, axis=1, keepdims=True)
    # cosine similarity matrix: (n_texts, n_emojis)
    sims = np.dot(text_norm, emoji_norm.T)
    for i, text in enumerate(texts):
        sim_row = sims[i]
        idx = np.argsort(-sim_row)[:top_k]
        picks = [(chars[j], names[j], float(sim_row[j])) for j in idx]
        results[text] = picks
    return results


def main():
    # read JSON array from stdin or args
    if not sys.stdin.isatty():
        payload = sys.stdin.read()
    elif len(sys.argv) > 1:
        payload = sys.argv[1]
    else:
        print("Usage: echo '[\"task1\",\"task2\"]' | python emoji_matcher.py", file=sys.stderr)
        sys.exit(2)

    texts = json.loads(payload)
    if not isinstance(texts, list):
        raise RuntimeError("Input must be a JSON list of strings")

    catalog = load_or_build_catalog()
    model = load_model()
    emoji_embeddings = build_or_load_embeddings(model, catalog)
    results = find_top_k(model, catalog, emoji_embeddings, texts, top_k=1)

    # compact JSON: map to list of emoji chars only (first match)
    out = {text: [ch for (ch, name, score) in results[text]] for text in texts}
    print(json.dumps(out, ensure_ascii=False))


if __name__ == "__main__":
    main()
