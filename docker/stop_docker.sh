#!/bin/bash
echo "Run the docker environment"
export DOCKER_IMAGE=myominnoo/wpgnarcan:dev.0.0.0.9000
export MOUNT_DIR=/Users/myominnoo/Documents/GitHub/wpgnarcan
docker-compose down