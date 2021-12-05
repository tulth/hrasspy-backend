#!/bin/bash
set -e

stack build
stack install
sudo mv ../../.local/bin/hrasspy-backend-exe ../../../../opt/rhasspy/hrasspy-backend-exe
sudo systemctl restart hrasspy-backend.service
sudo systemctl status hrasspy-backend.service
