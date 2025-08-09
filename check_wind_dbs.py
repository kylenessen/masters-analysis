#!/usr/bin/env python3
"""
Quick checker to validate that all wind database files referenced in
data/deployments_combined.csv exist under data/wind.

It reads the `wind_meter_name` column, builds expected `<name>.s3db` files,
and compares with actual files present in `data/wind`.

Usage: python check_wind_dbs.py [csv_path] [wind_dir]
Defaults: csv_path=data/deployments_combined.csv, wind_dir=data/wind
"""

from __future__ import annotations

import csv
import sys
from pathlib import Path


def load_expected(csv_path: Path) -> set[str]:
    expected: set[str] = set()
    with csv_path.open("r", encoding="utf-8-sig", newline="") as f:
        reader = csv.DictReader(f)
        if "wind_meter_name" not in reader.fieldnames:  # type: ignore[arg-type]
            raise SystemExit(
                f"Column 'wind_meter_name' not found in {csv_path}. Found: {reader.fieldnames}"
            )
        for row in reader:
            name = (row.get("wind_meter_name") or "").strip()
            if not name or name.lower() in {"na", "n/a", "none", "null"}:
                continue
            expected.add(name)
    return expected


def main(argv: list[str]) -> int:
    csv_path = Path(argv[1]) if len(argv) > 1 else Path("data/deployments_combined.csv")
    wind_dir = Path(argv[2]) if len(argv) > 2 else Path("data/wind")

    if not csv_path.exists():
        print(f"CSV not found: {csv_path}")
        return 2
    if not wind_dir.exists():
        print(f"Wind directory not found: {wind_dir}")
        return 2

    expected_names = load_expected(csv_path)
    expected_files = {f"{name}.s3db" for name in expected_names}
    # Group expected by lowercase to catch case-variant duplicates in CSV
    expected_groups: dict[str, set[str]] = {}
    for e in expected_files:
        expected_groups.setdefault(e.lower(), set()).add(e)
    # Representative expected for each lowercase key (deterministic pick)
    expected_lower = {low: sorted(variants)[0] for low, variants in expected_groups.items()}

    actual_files = {p.name for p in wind_dir.glob("*.s3db")}
    actual_lower = {a.lower(): a for a in actual_files}

    # Compute missing (case-insensitive)
    missing: list[str] = []
    case_mismatch: list[tuple[str, str]] = []  # (expected, actual)
    for low, expected in expected_lower.items():
        if low not in actual_lower:
            missing.append(expected)
        else:
            actual = actual_lower[low]
            if actual != expected:
                case_mismatch.append((expected, actual))

    # Extras = actual not referenced in CSV
    extras: list[str] = []
    for low, actual in actual_lower.items():
        if low not in expected_lower:
            extras.append(actual)

    print("Wind DBs check")
    print("-" * 40)
    print(f"CSV: {csv_path}")
    print(f"Wind dir: {wind_dir}")
    print(f"Unique meters in CSV: {len(expected_names)}")
    if any(len(v) > 1 for v in expected_groups.values()):
        dup_count = sum(1 for v in expected_groups.values() if len(v) > 1)
        print(
            f"Note: {dup_count} meter name(s) differ only by case in CSV; using case-insensitive matching."
        )
        for low, variants in sorted(expected_groups.items()):
            if len(variants) > 1:
                pretty = ", ".join(sorted(variants))
                print(f"  - {low}: {pretty}")
    print(f".s3db files present: {len(actual_files)}")
    print()

    if missing:
        print(f"Missing ({len(missing)}):")
        for name in sorted(missing):
            print(f"  - {name}")
        print()
    else:
        print("Missing: none\n")

    if extras:
        print(f"Extra files not referenced in CSV ({len(extras)}):")
        for name in sorted(extras):
            print(f"  - {name}")
        print()
    else:
        print("Extras: none\n")

    if case_mismatch:
        print(f"Case mismatches ({len(case_mismatch)}):")
        for expected, actual in sorted(case_mismatch):
            print(f"  - expected '{expected}' but found '{actual}'")
        print()
    else:
        print("Case mismatches: none\n")

    # Non-zero exit if there are missing files
    return 1 if missing else 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
