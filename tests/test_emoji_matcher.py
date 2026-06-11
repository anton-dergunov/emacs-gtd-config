import json
import numpy as np
import pytest

import scripts.org_emoji_matcher as em


# -----------------------------
# Fake model for testing
# -----------------------------
class DummyModel:
    def encode(self, texts, convert_to_numpy=True, show_progress_bar=False):
        # deterministic fake embeddings
        if isinstance(texts, list):
            return np.array([[len(t), 1.0] for t in texts])
        return np.array([len(texts), 1.0])


# -----------------------------
# Test catalog creation
# -----------------------------
def test_build_emoji_catalog_non_empty():
    catalog = em.build_emoji_catalog()
    assert isinstance(catalog, list)
    assert len(catalog) > 0
    emoji, name = catalog[0]
    assert isinstance(emoji, str)
    assert isinstance(name, str)


def test_build_emoji_catalog_excludes_zodiac():
    """Zodiac/astrology emoji are denylisted, so e.g. ♐ never appears."""
    catalog = em.build_emoji_catalog()
    chars = {ch for (ch, _) in catalog}
    for sign in ("♈", "♐", "♓", "⛎"):
        assert sign not in chars


def test_description_includes_aliases():
    """Descriptions are enriched with aliases, not just the bare unicode name."""
    # 📄 demojizes to "page facing up"; its alias is :page_facing_up: too, but
    # an emoji like 😀 carries the :grinning: alias alongside "grinning face".
    desc = em._emoji_description("😀")
    assert "grinning" in desc


# -----------------------------
# Test similarity search
# -----------------------------
def test_find_top_k_basic():
    model = DummyModel()

    emoji_catalog = [
        ("📱", "mobile phone"),
        ("💾", "floppy disk"),
    ]

    emoji_embeddings = np.array([
        [10.0, 1.0],
        [5.0, 1.0],
    ])

    texts = ["phone task"]

    results = em.find_top_k(
        model,
        emoji_catalog,
        emoji_embeddings,
        texts,
        top_k=1,
    )

    assert "phone task" in results
    assert len(results["phone task"]) == 1


def test_find_top_k_threshold_filters_weak_match():
    """A best match below the threshold yields an empty pick list."""
    model = DummyModel()
    emoji_catalog = [("📱", "mobile phone")]
    # Single emoji embedding orthogonal-ish to make cosine modest, but easier:
    # force a high threshold so nothing qualifies.
    emoji_embeddings = np.array([[10.0, 1.0]])

    results = em.find_top_k(
        model, emoji_catalog, emoji_embeddings, ["phone task"],
        top_k=1, threshold=1.01)  # impossible cosine -> always filtered
    assert results["phone task"] == []


def test_query_prefix_only_for_bge():
    assert em.query_prefix_for("BAAI/bge-small-en-v1.5") != ""
    assert em.query_prefix_for("sentence-transformers/all-MiniLM-L6-v2") == ""


# -----------------------------
# Test JSON output formatting logic
# -----------------------------
def test_output_formatting():
    fake_results = {
        "Task": [("📱", "mobile phone", 0.9)]
    }

    out = {text: [ch for (ch, _, _) in fake_results[text]]
           for text in fake_results}

    assert out == {"Task": ["📱"]}
