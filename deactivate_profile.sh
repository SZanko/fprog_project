#!/usr/bin/env sh

sudo sysctl -w kernel.perf_event_paranoid=2
sudo sysctl -w kernel.kptr_restrict=1
