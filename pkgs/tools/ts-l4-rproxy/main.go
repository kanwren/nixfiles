package main

import (
	"context"
	"errors"
	"flag"
	"io"
	"log"
	"net"
	"os"
	"os/signal"
	"strings"
	"sync"
	"time"

	"golang.org/x/sync/errgroup"
	"tailscale.com/tsnet"
)

func run(ctx context.Context, args []string) error {
	flags := flag.NewFlagSet("tsnet-tcp-rproxy", flag.ExitOnError)
	var (
		name     = flags.String("name", "", "Name of the TCP reverse proxy")
		stateDir = flags.String("state-dir", "", "Directory to store state files")
		proxy    = flags.String("proxy", "", "Protocol to proxy, address to listen on, and target address (comma-separated)")
	)
	flags.Parse(args)

	if name == nil || *name == "" {
		return errors.New("name is required")
	}
	if stateDir == nil || *stateDir == "" {
		return errors.New("state-dir is required")
	}
	if proxy == nil || *proxy == "" {
		return errors.New("proxy is required")
	}

	proxyParts := strings.SplitN(*proxy, ",", 3)
	if len(proxyParts) != 3 {
		return errors.New("proxy must be in the format 'protocol,listen_addr,target_addr'")
	}
	protocol, listenAddr, targetAddr := proxyParts[0], proxyParts[1], proxyParts[2]

	if protocol != "tcp" {
		return errors.New("only 'tcp' protocol is supported")
	}

	authkey, ok := os.LookupEnv("TS_AUTHKEY")
	if !ok {
		return errors.New("TS_AUTHKEY environment variable is required")
	}

	s := tsnet.Server{
		Dir:      *stateDir,
		Hostname: *name,
		AuthKey:  authkey,
		Logf:     log.Printf,
	}

	eg, egCtx := errgroup.WithContext(ctx)
	eg.Go(func() error {
		ln, err := s.Listen("tcp", listenAddr)
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
				if egCtx.Err() != nil {
					return egCtx.Err()
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
					uconn.Close()
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
	})

	<-egCtx.Done()
	if err := s.Close(); err != nil {
		return err
	}
	return eg.Wait()
}

func main() {
	ctx, cancel := signal.NotifyContext(context.Background(), os.Interrupt)
	defer cancel()

	if err := run(ctx, os.Args[1:]); err != nil {
		log.Fatal(err)
	}
}
