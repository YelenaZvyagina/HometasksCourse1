#!/usr/bin/env bash

set -eu
set -o pipefail

echo "Restoring dotnet tools..."
dotnet tool restore
dotnet paket config add-token "https://nuget.pkg.github.com/Anastasia-Nikitina/index.json" "$NUGET_AUTH_TOKEN"
dotnet paket restore

PAKET_SKIP_RESTORE_TARGETS=true FAKE_DETAILED_ERRORS=true dotnet fake build -t "$@"
