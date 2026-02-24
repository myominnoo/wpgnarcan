# One-time setup: create a buildx builder
docker buildx create --name multibuilder --use
docker buildx inspect --bootstrap