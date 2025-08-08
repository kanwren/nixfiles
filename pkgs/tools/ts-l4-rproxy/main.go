package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"log/slog"
	"net"
	"os"
	"os/signal"
	"strings"
	"sync"
	"time"

	"golang.org/x/sync/errgroup"
	"tailscale.com/tsnet"
)

type proxyConfig struct {
	protocol   string
	listenAddr string
	targetAddr string
}

func parseProxySpec(spec string) (proxyConfig, error) {
	parts := strings.SplitN(spec, ",", 3)
	if len(parts) != 3 {
		return proxyConfig{}, errors.New("proxy must be in the format 'protocol,listen_addr,target_addr'")
	}
	protocol := strings.TrimSpace(parts[0])
	listenAddr := strings.TrimSpace(parts[1])
	targetAddr := strings.TrimSpace(parts[2])

	if protocol != "tcp" {
		return proxyConfig{}, errors.New("only 'tcp' protocol is supported")
	}

	return proxyConfig{
		protocol:   protocol,
		listenAddr: listenAddr,
		targetAddr: targetAddr,
	}, nil
}

func listen(ctx context.Context, logger *slog.Logger, srv *tsnet.Server, config proxyConfig) error {
	protocol := config.protocol
	listenAddr := config.listenAddr
	targetAddr := config.targetAddr

	logger = logger.With(
		slog.String("protocol", protocol),
		slog.String("listen_addr", listenAddr),
		slog.String("target_addr", targetAddr),
	)

	ln, err := srv.Listen(protocol, listenAddr)
	if err != nil {
		return err
	}
	defer ln.Close()

	var wg sync.WaitGroup
	var tempDelay time.Duration
	const maxDelay = 1 * time.Second
	defer wg.Wait()

	for {
		uconn, err := ln.Accept()
		if err != nil {
			if errors.Is(err, net.ErrClosed) {
				return nil
			}
			if ctx.Err() != nil {
				return ctx.Err()
			}

			if tempDelay == 0 {
				tempDelay = 5 * time.Millisecond
			} else {
				tempDelay *= 2
			}
			if tempDelay > maxDelay {
				tempDelay = maxDelay
			}
			time.Sleep(tempDelay)
			continue
		}

		tempDelay = 0

		wg.Add(1)
		go func() {
			defer wg.Done()
			defer uconn.Close()
			dconn, err := net.Dial("tcp", targetAddr)
			if err != nil {
				logger.Error("failed to dial", slog.String("error", err.Error()))
				return
			}
			defer dconn.Close()
			wg.Add(1)
			go func() {
				io.Copy(dconn, uconn)
				wg.Done()
			}()
			io.Copy(uconn, dconn)
		}()
	}
}

func run(ctx context.Context, logger *slog.Logger, args []string) error {
	flags := flag.NewFlagSet("tsnet-tcp-rproxy", flag.ExitOnError)
	var (
		name     = flags.String("name", "", "Name of the TCP reverse proxy")
		stateDir = flags.String("state-dir", "", "Directory to store state files")
	)
	flags.Parse(args)

	if name == nil || *name == "" {
		return errors.New("name is required")
	}
	if stateDir == nil || *stateDir == "" {
		return errors.New("state-dir is required")
	}

	var proxyConfigs []proxyConfig
	for _, proxySpec := range flags.Args() {
		config, err := parseProxySpec(proxySpec)
		if err != nil {
			return fmt.Errorf("invalid proxy spec %q: %w", proxySpec, err)
		}
		proxyConfigs = append(proxyConfigs, config)
	}

	authkey, ok := os.LookupEnv("TS_AUTHKEY")
	if !ok {
		return errors.New("TS_AUTHKEY environment variable is required")
	}

	tsLogger := logger.WithGroup("tsnet")
	srv := tsnet.Server{
		Dir:      *stateDir,
		Hostname: *name,
		AuthKey:  authkey,
		Logf: func(format string, args ...interface{}) {
			tsLogger.Debug(fmt.Sprintf(format, args...))
		},
	}

	eg, egCtx := errgroup.WithContext(ctx)
	for _, config := range proxyConfigs {
		eg.Go(func() error {
			return listen(egCtx, logger, &srv, config)
		})
	}

	<-egCtx.Done()
	if err := srv.Close(); err != nil {
		return err
	}
	return eg.Wait()
}

func main() {
	ctx, cancel := signal.NotifyContext(context.Background(), os.Interrupt)
	defer cancel()

	logger := slog.New(slog.NewTextHandler(os.Stdout, &slog.HandlerOptions{
		Level: slog.LevelInfo,
	}))

	if err := run(ctx, logger, os.Args[1:]); err != nil {
		logger.Error(fmt.Sprintf("error: %v", err))
		os.Exit(1)
	}
}
