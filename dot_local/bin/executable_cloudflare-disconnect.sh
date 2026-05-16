#!/usr/bin/env bash

# Prompted password immediately after startup
sudo -k

warp-cli disconnect
sleep 2
sudo systemctl stop warp-svc.service
