# India boundary geometry: source, licence and reproducibility

`data-raw/india_states_map.rds` (packaged as `india_map`) is the India
states/union-territories boundary layer used by BharatViz's choropleth.

## Source
- **Provider:** SimpleMaps ([simplemaps.com/gis/country/in](https://simplemaps.com/gis/country/in))
- **Direct file:** state/admin-1 GeoJSON, free tier
  (`https://simplemaps.com/static/svg/country/in/admin1/in.json`)
- **Retrieved:** 2026-09-17
- **Licence:** Creative Commons Attribution 4.0 (CC BY 4.0) —
  <https://creativecommons.org/licenses/by/4.0/> (the SimpleMaps free tier is
  distributed under CC BY 4.0; attribution required, no endorsement implied).

## Attribution (as shown in the app)
> Boundaries: SimpleMaps, CC BY 4.0. A boundary depiction here is not an official
> Government of India map.

SimpleMaps does not endorse this project. All renaming, crosswalking, validation
and format conversion is the author's own work and is not warranted by SimpleMaps.

## Reproducibility
- **Build script (source of truth):** `data-raw/build_india_map.R`. It downloads
  the GeoJSON fresh (a browser User-Agent is required; the source 403s R's default
  agent), sets/validates CRS EPSG:4326, applies `st_make_valid()`, crosswalks the
  state names/codes used across this ecosystem, asserts exactly 36 valid non-empty
  units, and writes `data-raw/india_states_map.rds`. Re-run it from the package
  root, then re-run `data-raw/build_data.R`.
- **Prepared input (checksum-pinned):** because the build is network-dependent, the
  prepared RDS is vendored so the package build is deterministic:
  - file: `data-raw/india_states_map.rds`
  - SHA-256: `b8b6d0c5cd27e40912e4a24a789ce593ed4d06ea81e5f4f45b142d574d2d7324`
  - This RDS is byte-identical to the one produced/validated for the `asi`/`plfs`
    packages from the same source and script.

## Kashmir / northern boundary
Unlike the Natural Earth / `d3po::subnational` geometry this replaces (which
follows only the Line of Control / Line of Actual Control), the SimpleMaps
Jammu & Kashmir and Ladakh polygons extend to the **complete India-claimed
extent** (the source's stated coverage; verified visually against the rendered
map, see `docs/BOUNDARY_CHECK.md`). This is a geometry-only choice; no political
prose is added.

## Result
An `sf` object: `state_name`/`region`, `state_code`, `geometry`; 36 rows;
EPSG:4326; all geometries valid and non-empty.

## Licence note
The CC BY 4.0 attribution applies to the map geometry. It does not change the
package's own code licence (Apache-2.0) or the NFHS data terms
(see `NFHS_SOURCE.md`).
