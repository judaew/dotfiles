#!/bin/bash

# Prompted password immediately after startup
sudo echo > /dev/null 2>&1

warp-cli disconnect
sleep 2
sudo systemctl stop warp-svc.service
