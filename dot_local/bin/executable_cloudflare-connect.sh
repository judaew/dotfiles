#!/usr/bin/env bash

# Prompted password immediately after startup
sudo echo > /dev/null 2>&1

sudo systemctl start warp-svc.service
sleep 2
warp-cli connect
