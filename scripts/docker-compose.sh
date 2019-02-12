#!/bin/sh -x

docker-compose -p $USER -f $(dirname $(readlink -f $0))/../docker-compose-$USER@$(hostname).yml "$@"
