module github.com/TreeTide/zoekt-underhood

go 1.13

require (
	github.com/google/zoekt v0.0.0-20211108135652-f8e8ada171c7
	github.com/prometheus/client_golang v1.5.1
	go.uber.org/automaxprocs v1.3.0
	golang.org/x/net v0.0.0-20210726213435-c6fcb2dbf985
)

replace github.com/google/zoekt => github.com/sourcegraph/zoekt v0.0.0-20220309143736-eba22ccc3c61
