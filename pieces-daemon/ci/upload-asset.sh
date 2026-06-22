#!/usr/bin/env bash
# Upload (clobber) a file as a release asset on the Forgejo instance.
# Self-contained: no `gh`, only curl + (jq or python3).
#   usage: TOKEN=.. SERVER=.. REPO=.. ci/upload-asset.sh <tag> <file>
set -euo pipefail
tag="${1:?tag}"; file="${2:?file}"; name="$(basename "$file")"
: "${TOKEN:?need TOKEN}"; : "${SERVER:?need SERVER}"; : "${REPO:?need REPO}"
api="${SERVER}/api/v1/repos/${REPO}"
hdr=(-H "Authorization: token ${TOKEN}")

get_id() {  # stdin: json object -> .id
  if command -v jq >/dev/null 2>&1; then jq -r '.id // empty'
  else python3 -c 'import sys,json;print(json.load(sys.stdin).get("id",""))'; fi
}
find_asset_id() {  # stdin: json array -> id of asset whose name == $AN
  if command -v jq >/dev/null 2>&1; then jq -r --arg n "$AN" 'map(select(.name==$n))[0].id // empty'
  else python3 -c 'import sys,json,os;a=json.load(sys.stdin);n=os.environ["AN"];print(next((str(x["id"]) for x in a if x.get("name")==n),""))'; fi
}

# ensure the release exists (idempotent; tolerates a concurrent creator)
rel="$(curl -fsS "${hdr[@]}" "${api}/releases/tags/${tag}" 2>/dev/null || true)"
if ! printf '%s' "$rel" | grep -q '"id"'; then
  curl -fsS -X POST "${hdr[@]}" -H 'Content-Type: application/json' \
    "${api}/releases" -d "{\"tag_name\":\"${tag}\",\"name\":\"${tag}\",\"target_commitish\":\"main\"}" \
    >/dev/null 2>&1 || true
  rel="$(curl -fsS "${hdr[@]}" "${api}/releases/tags/${tag}")"
fi
relid="$(printf '%s' "$rel" | get_id)"
[ -n "$relid" ] || { echo "ERROR: cannot resolve release id for ${tag}"; exit 1; }

# clobber an existing asset of the same name, then upload
export AN="$name"
aid="$(curl -fsS "${hdr[@]}" "${api}/releases/${relid}/assets" 2>/dev/null | find_asset_id || true)"
if [ -n "${aid:-}" ]; then
  curl -fsS -X DELETE "${hdr[@]}" "${api}/releases/${relid}/assets/${aid}" >/dev/null 2>&1 || true
fi
curl -fsS -X POST "${hdr[@]}" -F "attachment=@${file}" \
  "${api}/releases/${relid}/assets?name=${name}" >/dev/null
echo "uploaded ${name} -> release ${tag} (id ${relid})"
