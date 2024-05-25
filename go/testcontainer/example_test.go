package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"testing"
	"time"

	"cloud.google.com/go/storage"
	"github.com/testcontainers/testcontainers-go"
	"github.com/testcontainers/testcontainers-go/wait"
)

func TestWithGCS(t *testing.T) {
	ctx := context.Background()
	req := testcontainers.ContainerRequest{
		Image:        "fsouza/fake-gcs-server:1.47.4",
		ExposedPorts: []string{"4443/tcp"},
		WaitingFor:   wait.ForLog("Ready to accept connections").WithStartupTimeout(time.Minute * 5),
	}
	container, err := testcontainers.GenericContainer(ctx, testcontainers.GenericContainerRequest{
		ContainerRequest: req,
		Started:          true,
	})
	if err != nil {
		log.Fatalf("Could not start container: %s", err)
	}
	defer func() {
		if err := container.Terminate(ctx); err != nil {
			log.Fatalf("Could not stop container: %s", err)
		}
	}()

	err = os.Setenv("STORAGE_EMULATOR_HOST", "localhost:4443")
	if err != nil {
		log.Fatal(err)
	}
	client, err := storage.NewClient(ctx)
	if err != nil {
		log.Fatal(err)
	}
	bkt := client.Bucket("test-bucket")
	if err := bkt.Create(ctx, "test", nil); err != nil {
		log.Fatal(err)
	}
	attrs, err := bkt.Attrs(ctx)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("bucket %s, created at %s, is located in %s with storage class %s\n",
		attrs.Name, attrs.Created, attrs.Location, attrs.StorageClass)
}
