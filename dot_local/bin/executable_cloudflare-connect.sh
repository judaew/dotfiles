#!/usr/bin/env bash

# Prompted password immediately after startup
sudo -k

sudo systemctl start warp-svc.service
sleep 2
warp-cli connect
