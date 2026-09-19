# Boundary check: Kashmir / northern outline

**Requirement:** the India map must depict the **complete India-claimed extent**
of Jammu & Kashmir and Ladakh (including the Gilgit-Baltistan / PoK area to the
north-west and Aksai Chin to the east), not the Line-of-Control / Line-of-Actual-
Control truncation used by Natural Earth and `d3po::subnational`.

**What was checked (2026-09-19):** the `india_map` polygons for "Jammu and Kashmir"
and "Ladakh" were rendered on their own and inspected. Extents:
- Jammu and Kashmir: lon 73.40-76.78, lat 32.28-35.16
- Ladakh: lon 72.53-80.33, lat 32.34-37.08

The northern boundary reaches ~37 deg N (covering the north-western claimed area)
and Ladakh extends east to ~80.3 deg E (covering the Aksai Chin region). This is
the **complete claimed outline**, consistent with SimpleMaps' stated coverage and
the requirement. It is **not** LoC/LAC-truncated.

**How the map is described (accurately) in the app and docs:** "Boundaries:
SimpleMaps, CC BY 4.0. A boundary depiction here is not an official Government of
India map." No political prose is added; this is a geometry-only choice recorded
in `data-raw/MAP_SOURCE.md`.

**Reproduce:** the crop used for this check is produced from `india_map`; the layer
itself is built by `data-raw/build_india_map.R` (SHA-256 pinned in `MAP_SOURCE.md`).
