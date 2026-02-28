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
