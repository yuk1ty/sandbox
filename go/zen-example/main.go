package main

import (
	"context"
	"log"
	"log/slog"
	"net/http"

	"github.com/unkeyed/unkey/go/pkg/fault"
	"github.com/unkeyed/unkey/go/pkg/otel/logging"
	"github.com/unkeyed/unkey/go/pkg/zen"
	"github.com/unkeyed/unkey/go/pkg/zen/validation"
)

type CreateUserRequest struct {
	Name     string `json:"name"`
	Email    string `json:"email"`
	Password string `json:"password"`
}

type CreateUserResponse struct {
	ID    string `json:"id"`
	Name  string `json:"name"`
	Email string `json:"email"`
}

func main() {
	logger := logging.New()

	server, err := zen.New(zen.Config{
		NodeID: "quickstart-server",
		Logger: logger,
	})
	if err != nil {
		log.Fatalf("failed to create server: %v", err)
	}

	validator, err := validation.New()
	if err != nil {
		log.Fatalf("failed to create validator: %v", err)
	}

	helloRoute := zen.NewRoute("GET", "/hello", func(ctx context.Context, s *zen.Session) error {
		return s.JSON(http.StatusOK, map[string]string{
			"message": "Hello, world!",
		})
	})

	createUserRoute := zen.NewRoute("POST", "/users", func(ctx context.Context, s *zen.Session) error {
		var req CreateUserRequest
		if err := s.BindBody(&req); err != nil {
			return err // This will be handled by error middleware
		}

		if len(req.Password) < 8 {
			return fault.New("password too short",
				fault.WithTag(fault.BAD_REQUEST),
				fault.WithDesc(
					"password must be at least 8 characters",      // Internal description
					"Password must be at least 8 characters long", // User-facing message
				),
			)
		}

		userID := "user_pretendthisisrandom"

		return s.JSON(http.StatusCreated, CreateUserResponse{
			ID:    userID,
			Name:  req.Name,
			Email: req.Email,
		})
	})

	server.RegisterRoute(
		[]zen.Middleware{
			zen.WithLogging(logger),
			zen.WithErrorHandling(logger),
		},
		helloRoute,
	)

	server.RegisterRoute(
		[]zen.Middleware{
			zen.WithTracing(),
			zen.WithLogging(logger),
			zen.WithErrorHandling(logger),
			zen.WithValidation(validator),
		},
		createUserRoute,
	)

	logger.Info("starting server",
		"address", ":8080",
	)
	err = server.Listen(context.Background(), ":8080")
	if err != nil {
		logger.Error("server error", slog.String("error", err.Error()))
	}
}
