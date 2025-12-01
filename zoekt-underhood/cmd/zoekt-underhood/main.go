package main

import (
	//"context"
	"flag"
	"fmt"
	//"html/template"
	//"io/ioutil"
	"log"
	"net/http"
	"net/http/pprof"
	"os"
	"path/filepath"
	//"strings"
	"time"

	"github.com/TreeTide/zoekt-underhood/web"
	//"github.com/google/zoekt"
	//"github.com/google/zoekt/build"
	"github.com/google/zoekt/shards"
	"github.com/prometheus/client_golang/prometheus/promhttp"
	"go.uber.org/automaxprocs/maxprocs"
	"golang.org/x/net/trace"
)

const logFormat = "2006-01-02T15-04-05.999999999Z07"

func divertLogs(dir string, interval time.Duration) {
	t := time.NewTicker(interval)
	var last *os.File
	for {
		nm := filepath.Join(dir, fmt.Sprintf("zoekt-underhood.%s.%d.log", time.Now().Format(logFormat), os.Getpid()))
		fmt.Fprintf(os.Stderr, "writing logs to %s\n", nm)

		f, err := os.Create(nm)
		if err != nil {
			// There is not much we can do now.
			fmt.Fprintf(os.Stderr, "can't create output file %s: %v\n", nm, err)
			os.Exit(2)
		}

		log.SetOutput(f)
		last.Close()

		last = f

		<-t.C
	}
}

func main() {
	logDir := flag.String("log_dir", "", "log to this directory rather than stderr.")
	logRefresh := flag.Duration("log_refresh", 24*time.Hour, "if using --log_dir, start writing a new file this often.")

	listen := flag.String("listen", ":6080", "listen on this address.")
	index := flag.String("index", "", "set index directory to use")
	enablePprof := flag.Bool("pprof", false, "set to enable remote profiling.")
	sslCert := flag.String("ssl_cert", "", "set path to SSL .pem holding certificate.")
	sslKey := flag.String("ssl_key", "", "set path to SSL .pem holding key.")
	flag.Parse()

	if *logDir != "" {
		if fi, err := os.Lstat(*logDir); err != nil || !fi.IsDir() {
			log.Fatalf("%s is not a directory", *logDir)
		}
		// We could do fdup acrobatics to also redirect
		// stderr, but it is simpler and more portable for the
		// caller to divert stderr output if necessary.
		go divertLogs(*logDir, *logRefresh)
	}

	// Tune GOMAXPROCS to match Linux container CPU quota.
	maxprocs.Set()

	if *index == "" {
		log.Fatalf("Please specify index directory with -index")
	}
	if fi, err := os.Lstat(*index); err != nil || !fi.IsDir() {
		log.Fatalf("%s is not a directory (for index)", *index)
	}

	searcher, err := shards.NewDirectorySearcher(*index)
	if err != nil {
		log.Fatal(err)
	}

	s := &web.Server{
		Searcher: searcher,
	}

	handler, err := web.NewMux(s)
	if err != nil {
		log.Fatal(err)
	}

	handler.Handle("/metrics", promhttp.Handler())

	if *enablePprof {
		handler.HandleFunc("/debug/pprof/", pprof.Index)
		handler.HandleFunc("/debug/pprof/cmdline", pprof.Cmdline)
		handler.HandleFunc("/debug/pprof/profile", pprof.Profile)
		handler.HandleFunc("/debug/pprof/symbol", pprof.Symbol)
		handler.HandleFunc("/debug/pprof/trace", pprof.Trace)
		handler.HandleFunc("/debug/requests/", trace.Traces)
		handler.HandleFunc("/debug/events/", trace.Events)
	}

	if *sslCert != "" || *sslKey != "" {
		log.Printf("serving HTTPS on %s", *listen)
		err = http.ListenAndServeTLS(*listen, *sslCert, *sslKey, handler)
	} else {
		log.Printf("serving HTTP on %s", *listen)
		err = http.ListenAndServe(*listen, handler)
	}
	log.Printf("ListenAndServe: %v", err)
}
