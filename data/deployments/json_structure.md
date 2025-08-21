# JSON Structure Comparison

This document outlines the two different JSON structures found in the provided files. `SC1.json` and `SC2.json` use the first structure, while the rest of the files use the second.

---

## Structure 1: Flat Structure

This structure is a flat object where each top-level key is the image filename. The value for each key is an object containing the image's classification data.

### Example (`SC1.json`, `SC2.json`)

```json
{
  "SC2_20231117140001.JPG": {
    "confirmed": true,
    "cells": {
      "cell_0_0": {
        "count": "0",
        "directSun": false
      },
      "cell_0_1": {
        "count": "0",
        "directSun": false
      }
    },
    "index": 0,
    "notes": "Optional notes about the image can go here.",
    "user": "Username"
  }
}
````

-----

## Structure 2: Nested Structure

This structure contains top-level metadata (`rows` and `columns`). The image data is nested within a `classifications` object, which then contains the data for each image, keyed by filename.

### Example (All other files)

```json
{
  "rows": 20,
  "columns": 36,
  "classifications": {
    "SC6_20231215132001.JPG": {
      "confirmed": true,
      "cells": {
        "cell_7_14": {
          "count": "0",
          "directSun": false
        },
        "cell_4_12": {
          "count": "0",
          "directSun": false
        }
      },
      "index": 0,
      "user": "VVR",
      "isNight": false
    }
  }
}
```
