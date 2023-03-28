package main

import (
	"context"
	"net/http"

	"github.com/gin-gonic/gin"
	flagd "github.com/open-feature/go-sdk-contrib/providers/flagd/pkg"
	"github.com/open-feature/go-sdk/pkg/openfeature"
)

const defaultMessage = "Hello!"
const newWelcomeMessage = "Hello, welcome to this OpenFeature-enabled website!"

func main() {
	// Use flagd as the OpenFeature provider
	openfeature.SetProvider(flagd.NewProvider())

	// Initialize OpenFeature client
	client := openfeature.NewClient("GoStartApp")

	// Initialize Go Gin
	engine := gin.Default()

	// Setup a simple endpoint
	engine.GET("/hello", func(c *gin.Context) {
		welcomeMessage, _ := client.BooleanValue(context.Background(), "welcome-message", false, openfeature.EvaluationContext{})

		if welcomeMessage {
			c.JSON(http.StatusOK, newWelcomeMessage)
			return
		} else {
			c.JSON(http.StatusOK, defaultMessage)
			return
		}
	})

	engine.Run()
}
