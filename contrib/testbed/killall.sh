#!/bin/sh

DISTS='bahamut hybrid unreal ultimate28 ultimate30'
TESTBED='/home/lucas/testbed'

# kill all thales

ps x |grep thales |grep -v grep | awk '{print $1}' | xargs kill
