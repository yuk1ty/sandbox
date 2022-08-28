# crabcan

## Run on docker

```
docker build -t crabcan . --platform linux/amd64
```

```
docker run --name crabcan --rm -it crabcan:latest cargo watch -x 'run -- --mount ./mount-dir/ --command bash --uid 0'
```
