#!/usr/bin/env python3
"""
Parameter sweep: build observation pairs at every possible time interval.

Within-day intervals (30 min – max daylight, step 30 min):
  - Column names match the 30-min analysis (monarch_gam_analysis.qmd).
  - Response: butterfly_difference_cbrt
  - Key predictors: total_butterflies_t_lag, max_gust, temperature_avg,
    butterflies_direct_sun_t_lag, time_within_day_t

Cross-day intervals (1 day – max deployment length, step 1 day):
  - Column names match the daily analysis (monarch_daily_gam_analysis.qmd).
  - Response: butterfly_diff_95th_sqrt
  - Key predictors: butterflies_95th_percentile_t_1, wind_max_gust_t_1,
    temp_max_t_1, temp_min_t_1, temp_at_max_count_t_1,
    sum_butterflies_direct_sun_t_1
  - Five anchor strategies:
      first_to_first, last_to_last, mid_to_mid, max_to_max, max_to_sunset

Outputs:
  parameter_sweep/within_day_pairs.csv
  parameter_sweep/cross_day_pairs.csv
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

import numpy as np
import pandas as pd
import sqlite3
from datetime import datetime, timedelta
from data_prep_lag import ButterflyCountProcessor


# ---------------------------------------------------------------------------
# Data loading
# ---------------------------------------------------------------------------

def load_observations() -> pd.DataFrame:
    """Process all deployment JSONs → master daytime observation table."""
    processor = ButterflyCountProcessor()
    obs = processor.process_deployments("data/deployments")
    obs = obs.sort_values(["deployment_id", "timestamp"]).reset_index(drop=True)
    obs["date"] = obs["timestamp"].dt.date
    obs["deployment_day"] = (
        obs["deployment_id"] + "_" + obs["timestamp"].dt.strftime("%Y%m%d")
    )
    return obs


def load_temperature_all() -> pd.DataFrame:
    """Load full temperature CSV with parsed timestamps (includes night)."""
    temp = pd.read_csv("data/temperature_data_2023.csv",
                       usecols=["filename", "deployment_id", "timestamp", "temperature"])
    temp["ts"] = pd.to_datetime(temp["timestamp"].astype(str), format="%Y%m%d%H%M%S")
    return temp


def load_wind_connections() -> dict:
    dep = pd.read_csv("data/deployments.csv")
    mapping = dep.set_index("deployment_id")["wind_meter_name"].to_dict()
    db_dir = Path("data/wind")
    db_files = {p.stem: p for p in db_dir.glob("*.s3db")}
    return {
        dep_id: db_files[meter]
        for dep_id, meter in mapping.items()
        if meter in db_files
    }


def preload_wind_data(wind_map: dict) -> dict:
    """Load all wind data into memory: {deployment_id: DataFrame}."""
    wind_cache = {}
    loaded_dbs = {}
    for dep_id, db_path in wind_map.items():
        db_key = str(db_path)
        if db_key not in loaded_dbs:
            with sqlite3.connect(str(db_path)) as conn:
                df = pd.read_sql_query(
                    "SELECT time, speed, gust FROM Wind ORDER BY time", conn
                )
            df["time"] = pd.to_datetime(df["time"])
            df["speed"] = pd.to_numeric(
                df["speed"].astype(str).str.strip(), errors="coerce"
            )
            df["gust"] = pd.to_numeric(
                df["gust"].astype(str).str.strip(), errors="coerce"
            )
            loaded_dbs[db_key] = df
        wind_cache[dep_id] = loaded_dbs[db_key]
    return wind_cache


# ---------------------------------------------------------------------------
# Weather helpers
# ---------------------------------------------------------------------------

def wind_metrics_for_interval(wind_df: pd.DataFrame,
                              start: datetime, end: datetime) -> dict:
    mask = (wind_df["time"] >= start) & (wind_df["time"] <= end)
    subset = wind_df.loc[mask]
    if subset.empty:
        return {"max_gust": np.nan, "avg_sustained": np.nan}
    gusts = subset["gust"].dropna()
    speeds = subset["speed"].dropna()
    return {
        "max_gust": gusts.max() if len(gusts) else np.nan,
        "avg_sustained": speeds.mean() if len(speeds) else np.nan,
    }


def temp_stats_for_interval(temp_all: pd.DataFrame, dep_id: str,
                            start: datetime, end: datetime) -> dict:
    """Get temp stats from the full (24h) temperature data for a deployment."""
    dep_temp = temp_all[temp_all["deployment_id"] == dep_id]
    mask = (dep_temp["ts"] >= start) & (dep_temp["ts"] <= end)
    subset = dep_temp.loc[mask, "temperature"].dropna()
    if len(subset) == 0:
        return {"temp_avg": np.nan, "temp_max": np.nan, "temp_min": np.nan}
    return {
        "temp_avg": subset.mean(),
        "temp_max": subset.max(),
        "temp_min": subset.min(),
    }


def sun_exposure_for_interval(obs: pd.DataFrame, dep_id: str,
                              start: datetime, end: datetime) -> float:
    dep_obs = obs[obs["deployment_id"] == dep_id]
    mask = (dep_obs["timestamp"] >= start) & (dep_obs["timestamp"] <= end)
    return dep_obs.loc[mask, "butterflies_direct_sun"].sum()


# ---------------------------------------------------------------------------
# Daily summaries (for cross-day pairing)
# ---------------------------------------------------------------------------

def build_daily_summaries(obs: pd.DataFrame, temp_all: pd.DataFrame) -> pd.DataFrame:
    """Per deployment-day: first/last/midday/max-count obs + daily stats."""
    records = []
    for dd, grp in obs.groupby("deployment_day"):
        g = grp.sort_values("timestamp")
        if len(g) < 2:
            continue

        first_row = g.iloc[0]
        last_row = g.iloc[-1]
        max_idx = g["total_butterflies"].idxmax()
        max_row = g.loc[max_idx]

        # Midday = closest to 12:30
        midday_target = g["timestamp"].iloc[0].replace(
            hour=12, minute=30, second=0
        )
        mid_idx = (g["timestamp"] - midday_target).abs().idxmin()
        mid_row = g.loc[mid_idx]

        # 95th percentile of butterfly counts
        pct_95 = np.percentile(g["total_butterflies"], 95)

        # Temperature at time of max count
        dep_id = first_row["deployment_id"]
        dep_temp = temp_all[temp_all["deployment_id"] == dep_id]
        max_ts = max_row["timestamp"]
        temp_diffs = (dep_temp["ts"] - max_ts).abs()
        if len(temp_diffs) > 0:
            closest_temp_idx = temp_diffs.idxmin()
            temp_at_max = dep_temp.loc[closest_temp_idx, "temperature"]
        else:
            temp_at_max = np.nan

        # Sum butterflies in direct sun for the day
        sun_sum = g["butterflies_direct_sun"].sum()

        records.append({
            "deployment_day": dd,
            "deployment_id": dep_id,
            "date": first_row["date"],
            # Anchor timestamps
            "first_ts": first_row["timestamp"],
            "first_count": first_row["total_butterflies"],
            "last_ts": last_row["timestamp"],
            "last_count": last_row["total_butterflies"],
            "mid_ts": mid_row["timestamp"],
            "mid_count": mid_row["total_butterflies"],
            "max_ts": max_row["timestamp"],
            "max_count": max_row["total_butterflies"],
            # Derived daily stats
            "pct_95": pct_95,
            "temp_at_max_count": temp_at_max,
            "sum_direct_sun": sun_sum,
            "day_length_hours": (
                (last_row["timestamp"] - first_row["timestamp"]).total_seconds()
                / 3600
            ),
            "n_obs": len(g),
        })
    df = pd.DataFrame(records)
    df = df.sort_values(["deployment_id", "date"]).reset_index(drop=True)

    # Add day_sequence within each deployment
    df["day_sequence"] = df.groupby("deployment_id").cumcount() + 1
    return df


# ---------------------------------------------------------------------------
# Within-day pair builder
# ---------------------------------------------------------------------------

def build_within_day_pairs(obs: pd.DataFrame, interval_min: int,
                           tolerance_min: int = 10) -> pd.DataFrame:
    """Build non-overlapping lag pairs within each deployment-day.

    Matches the column names expected by monarch_gam_analysis.qmd.
    """
    pairs = []
    interval_td = timedelta(minutes=interval_min)
    tolerance_td = timedelta(minutes=tolerance_min)

    for dd, grp in obs.groupby("deployment_day"):
        g = grp.sort_values("timestamp").reset_index(drop=True)
        n = len(g)
        if n < 2:
            continue

        # Calculate time-within-day for all obs
        first_time = g["timestamp"].iloc[0]
        g_time_in_day = (
            (g["timestamp"] - first_time).dt.total_seconds() / 60
        )

        used = set()
        order = 0

        for i in range(n):
            if i in used:
                continue
            target = g.loc[i, "timestamp"] + interval_td
            # Find closest unused obs near target
            candidates = g.loc[~g.index.isin(used) & (g.index > i)]
            if candidates.empty:
                continue
            diffs = (candidates["timestamp"] - target).abs()
            best_idx = diffs.idxmin()
            if diffs[best_idx] > tolerance_td:
                continue

            order += 1
            start = g.loc[i]
            end = g.loc[best_idx]
            actual_lag = (
                (end["timestamp"] - start["timestamp"]).total_seconds() / 60
            )
            diff = end["total_butterflies"] - start["total_butterflies"]

            pairs.append({
                "interval_minutes": interval_min,
                "deployment_id": start["deployment_id"],
                "deployment_day": dd,
                # Response
                "butterfly_difference_cbrt": (
                    np.sign(diff) * np.abs(diff) ** (1 / 3)
                ),
                # Predictors (names match 30-min analysis)
                "total_butterflies_t_lag": start["total_butterflies"],
                "butterflies_direct_sun_t_lag": start["butterflies_direct_sun"],
                "time_within_day_t": g_time_in_day[best_idx],
                "observation_order_within_day_t": order,
                # Placeholders for weather — filled later
                "timestamp_start": start["timestamp"],
                "timestamp_end": end["timestamp"],
                "actual_lag_minutes": actual_lag,
            })
            used.add(i)
            used.add(best_idx)

    return pd.DataFrame(pairs)


# ---------------------------------------------------------------------------
# Cross-day pair builder
# ---------------------------------------------------------------------------

ANCHOR_STRATEGIES = [
    "first_to_first",
    "last_to_last",
    "mid_to_mid",
    "max_to_max",
    "max_to_sunset",
]

ANCHOR_COLS = {
    "first_to_first": ("first_ts", "first_count", "first_ts", "first_count"),
    "last_to_last":   ("last_ts",  "last_count",  "last_ts",  "last_count"),
    "mid_to_mid":     ("mid_ts",   "mid_count",   "mid_ts",   "mid_count"),
    "max_to_max":     ("max_ts",   "max_count",   "max_ts",   "max_count"),
    "max_to_sunset":  ("max_ts",   "max_count",   "last_ts",  "last_count"),
}


def build_cross_day_pairs(daily: pd.DataFrame,
                          interval_days: int) -> pd.DataFrame:
    """Build cross-day pairs with all anchor strategies.

    Column names match monarch_daily_gam_analysis.qmd.
    """
    pairs = []

    for dep_id, dep_grp in daily.groupby("deployment_id"):
        dep_grp = dep_grp.sort_values("date").reset_index(drop=True)

        for i in range(len(dep_grp)):
            row_a = dep_grp.iloc[i]
            target_date = row_a["date"] + timedelta(days=interval_days)
            match = dep_grp[dep_grp["date"] == target_date]
            if match.empty:
                continue
            row_b = match.iloc[0]

            for strategy in ANCHOR_STRATEGIES:
                ts_a_col, ct_a_col, ts_b_col, ct_b_col = ANCHOR_COLS[strategy]
                ts_start = row_a[ts_a_col]
                ts_end = row_b[ts_b_col]
                count_start = row_a[ct_a_col]
                count_end = row_b[ct_b_col]

                # Response: difference in butterfly count at anchor points
                count_diff = count_end - count_start
                count_diff_sqrt = np.sign(count_diff) * np.sqrt(np.abs(count_diff))

                pairs.append({
                    "interval_days": interval_days,
                    "anchor_strategy": strategy,
                    "deployment_id": dep_id,
                    "day_sequence": row_a["day_sequence"],
                    # Response: butterfly diff at anchor points
                    "butterfly_diff_95th_sqrt": count_diff_sqrt,
                    # Baseline: butterfly count at start anchor point
                    "butterflies_95th_percentile_t_1": count_start,
                    # Day-level predictors from start day
                    "temp_at_max_count_t_1": row_a["temp_at_max_count"],
                    "sum_butterflies_direct_sun_t_1": row_a["sum_direct_sun"],
                    # Interval timestamps for weather aggregation
                    "timestamp_start": ts_start,
                    "timestamp_end": ts_end,
                })

    return pd.DataFrame(pairs)


# ---------------------------------------------------------------------------
# Weather enrichment
# ---------------------------------------------------------------------------

def add_weather_within_day(pairs: pd.DataFrame, obs: pd.DataFrame,
                           temp_all: pd.DataFrame,
                           wind_cache: dict) -> pd.DataFrame:
    """Add temperature_avg and max_gust for within-day pairs."""
    temp_avgs = []
    max_gusts = []

    total = len(pairs)
    for idx, row in pairs.iterrows():
        if idx % max(1, total // 10) == 0:
            print(f"  Within-day weather: {idx}/{total}")

        dep = row["deployment_id"]
        start, end = row["timestamp_start"], row["timestamp_end"]

        # Temperature: average from all observations in interval
        tstats = temp_stats_for_interval(temp_all, dep, start, end)
        temp_avgs.append(tstats["temp_avg"])

        # Wind: max gust over interval
        if dep in wind_cache:
            wm = wind_metrics_for_interval(wind_cache[dep], start, end)
            max_gusts.append(wm["max_gust"])
        else:
            max_gusts.append(np.nan)

    pairs = pairs.copy()
    pairs["temperature_avg"] = temp_avgs
    pairs["max_gust"] = max_gusts
    return pairs


def add_weather_cross_day(pairs: pd.DataFrame,
                          temp_all: pd.DataFrame,
                          wind_cache: dict) -> pd.DataFrame:
    """Add temp_max_t_1, temp_min_t_1, wind_max_gust_t_1 for cross-day."""
    temp_maxs = []
    temp_mins = []
    wind_maxs = []

    total = len(pairs)
    for idx, row in pairs.iterrows():
        if idx % max(1, total // 10) == 0:
            print(f"  Cross-day weather: {idx}/{total}")

        dep = row["deployment_id"]
        start, end = row["timestamp_start"], row["timestamp_end"]

        # Temperature: full 24h data over interval
        tstats = temp_stats_for_interval(temp_all, dep, start, end)
        temp_maxs.append(tstats["temp_max"])
        temp_mins.append(tstats["temp_min"])

        # Wind: max gust over interval
        if dep in wind_cache:
            wm = wind_metrics_for_interval(wind_cache[dep], start, end)
            wind_maxs.append(wm["max_gust"])
        else:
            wind_maxs.append(np.nan)

    pairs = pairs.copy()
    pairs["temp_max_t_1"] = temp_maxs
    pairs["temp_min_t_1"] = temp_mins
    pairs["wind_max_gust_t_1"] = wind_maxs
    return pairs


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    print("=" * 70)
    print("PARAMETER SWEEP: Building pairs at all intervals")
    print("=" * 70)

    # --- Load data ---
    print("\n[1/5] Processing deployment JSONs...")
    obs = load_observations()
    print(f"  {len(obs)} daytime observations, "
          f"{obs['deployment_id'].nunique()} deployments")

    print("\n[2/5] Loading temperature data (24h)...")
    temp_all = load_temperature_all()
    print(f"  {len(temp_all)} records")

    print("\n[3/5] Loading wind data...")
    wind_map = load_wind_connections()
    wind_cache = preload_wind_data(wind_map)
    print(f"  {len(wind_cache)} deployments with wind")

    print("\n[4/5] Building daily summaries...")
    daily = build_daily_summaries(obs, temp_all)
    print(f"  {len(daily)} deployment-days")

    # --- Determine interval ranges ---
    max_within_min = int(daily["day_length_hours"].quantile(0.75) * 60)
    max_within_min = (max_within_min // 30) * 30
    print(f"  Max within-day interval (75th pct): {max_within_min} min")

    dep_spans = daily.groupby("deployment_id")["date"].agg(["min", "max"])
    dep_spans["span"] = dep_spans.apply(
        lambda r: (r["max"] - r["min"]).days, axis=1
    )
    max_cross_days = dep_spans["span"].max()
    print(f"  Max cross-day interval: {max_cross_days} days")

    # --- Within-day pairs ---
    print("\n[5a/5] Building within-day pairs...")
    within_intervals = list(range(30, max_within_min + 1, 30))
    print(f"  Testing intervals: {within_intervals}")

    all_within = []
    for interval in within_intervals:
        tolerance = max(5, interval // 6)
        p = build_within_day_pairs(obs, interval, tolerance_min=tolerance)
        if len(p) >= 20:
            all_within.append(p)
            print(f"    {interval:4d} min: {len(p):5d} pairs")
        else:
            print(f"    {interval:4d} min: {len(p):5d} pairs — SKIPPED")

    within_df = pd.concat(all_within, ignore_index=True) if all_within else pd.DataFrame()

    if not within_df.empty:
        print(f"\n  Adding weather to {len(within_df)} within-day pairs...")
        within_df = add_weather_within_day(within_df, obs, temp_all, wind_cache)
        # Drop working columns
        within_df = within_df.drop(columns=["timestamp_start", "timestamp_end"])
        out = Path("parameter_sweep/within_day_pairs.csv")
        within_df.to_csv(out, index=False)
        print(f"  Saved {len(within_df)} rows → {out}")

    # --- Cross-day pairs ---
    print("\n[5b/5] Building cross-day pairs...")
    all_cross = []
    min_pairs_per_strategy = 4  # need at least 4 per anchor strategy
    for n_days in range(1, max_cross_days + 1):
        p = build_cross_day_pairs(daily, n_days)
        # Check if any single strategy has enough pairs
        if len(p) == 0:
            print(f"    {n_days:3d} day(s): 0 pairs — STOPPING")
            break
        per_strat = p.groupby("anchor_strategy").size()
        min_per = per_strat.min()
        if min_per >= min_pairs_per_strategy:
            all_cross.append(p)
            print(f"    {n_days:3d} day(s): {len(p):4d} pairs "
                  f"({min_per}-{per_strat.max()} per strategy)")
        else:
            print(f"    {n_days:3d} day(s): {len(p):4d} pairs "
                  f"({min_per} per strategy) — STOPPING")
            break

    cross_df = pd.concat(all_cross, ignore_index=True) if all_cross else pd.DataFrame()

    if not cross_df.empty:
        print(f"\n  Adding weather to {len(cross_df)} cross-day pairs...")
        cross_df = add_weather_cross_day(cross_df, temp_all, wind_cache)
        cross_df = cross_df.drop(columns=["timestamp_start", "timestamp_end"])
        out = Path("parameter_sweep/cross_day_pairs.csv")
        cross_df.to_csv(out, index=False)
        print(f"  Saved {len(cross_df)} rows → {out}")

    # --- Summary ---
    print("\n" + "=" * 70)
    print("SUMMARY")
    print("=" * 70)
    if not within_df.empty:
        n_int = within_df["interval_minutes"].nunique()
        print(f"  Within-day: {len(within_df)} pairs across {n_int} intervals")
    if not cross_df.empty:
        for strat in ANCHOR_STRATEGIES:
            s = cross_df[cross_df["anchor_strategy"] == strat]
            n_int = s["interval_days"].nunique()
            print(f"  Cross-day ({strat}): {len(s)} pairs across {n_int} intervals")


if __name__ == "__main__":
    main()
