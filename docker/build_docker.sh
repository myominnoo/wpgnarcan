#!/bin/bash
echo "Build the docker"
docker buildx build \
  --platform linux/amd64,linux/arm64 \
  -t myominnoo/wpgnarcan:dev.0.0.0.9000 \
  --push \
  .
  
  
if [[ $? = 0 ]] ; then
  echo "Docker build and push successful"
else
  echo "Docker build failed"
fi