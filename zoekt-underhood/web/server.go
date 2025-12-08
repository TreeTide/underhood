package web

import (
	"crypto/sha1"
	"encoding/json"
	"fmt"
	//"html"
	"log"
	"net/http"
	"slices"
	"sort"
	"strings"
	"time"

	"golang.org/x/net/context"

	"github.com/sourcegraph/zoekt"
	"github.com/sourcegraph/zoekt/query"
)

// Notes:
//
// When doing Zoekt queries, hit numbers are not estimated. This could lead to
// missing some results (though the default limits are pretty high).
//
// Some remarks about UTF-8 support in the code.

type Server struct {
	Searcher zoekt.Searcher

	// Version string for this server.
	Version string

	startTime time.Time
}

func NewMux(s *Server) (*http.ServeMux, error) {
	s.startTime = time.Now()

	mux := http.NewServeMux()
	mux.HandleFunc("/api/filetree", s.serveFileTree)
	mux.HandleFunc("/api/source", s.serveSource)
	mux.HandleFunc("/api/decor", s.serveDecors)
	mux.HandleFunc("/api/search-xref", s.serveSearchXref)

	return mux, nil
}

type FileTree struct {
	// For now we use repo:path format. Name for backwards compatibility.
	// Should be unique.
	KytheUri string `json:"kytheUri"`

	// The name displayed in the tree - either a repository, or a path component.
	Display string `json:"display"`

	// Usually generated files are not indexed in Zoekt, only source.
	OnlyGenerated bool `json:"onlyGenerated"`

	// True if file, false if directory.
	IsFile bool `json:"isFile"`

	// nil means unknown, client should make a further request to discover.
	// only meaningful for directories.
	Children *[]FileTree `json:"children"`
}

func (s *Server) serveFileTree(w http.ResponseWriter, r *http.Request) {
	if err := s.serveFileTreeErr(w, r); err != nil {
		http.Error(w, err.Error(), http.StatusTeapot)
	}
}

func (s *Server) serveFileTreeErr(w http.ResponseWriter, r *http.Request) error {
	// Assumption: all paths (in request, in Zoekt response) are normalized.
	log.Printf("request: %v", r.URL)
	top := ""
	if tops, ok := r.URL.Query()["top"]; ok {
		top = tops[0]
	}
	ticket, err := parseTicket(top)
	if err != nil {
		return err
	}
	topRepo := ticket.repo
	topPath := ticket.path
	topBranch := ticket.branch

	sOpts := zoekt.SearchOptions{
		MaxWallTime: 10 * time.Second,
	}
	sOpts.SetDefaults()

	ctx := r.Context()

	rq := "r:"
	if topRepo != "" {
		// TODO: [repo filter] in Zoekt was substring-match now, and pinning with
		//     regexp is already supported. Before it was not, and we must have
		//     filtered for the exact repo when iterating the results later.
		//
		//     But now it could be reworked!
		//
		rq += "^" + topRepo + "$"

		if topBranch != "" {
			// NOTE(repo-filter,branch): regexp not supporten on zoekt branch: yet.
			//   So we need to post-filter.
			rq += " b:" + topBranch
		}

		if topPath == "" {
			// Well, zoekt obviously doesn't return dir matches. So something like
			//
			//     rq += " f:^[^/]*$"
			//
			// wouldn't work. So fetch all files from repo now, and post-process
			// to filter the relevant ones only.
			//
			// Note: we rely on getting back all files, so we can harvest the
			// top-level dirs.
			//
			// TODO(filetree): Need to check the num estimates to be sure we didn't
			// miss any dirs (due to skipped files within). Even better, could patch
			// upstream to support one-match-per-file, to surely (ok, even more
			// likely) fit within limits.
			//
			rq += " f:^.*$"
		} else {
			rq += " f:^" + topPath + "/.*$"
		}
	}
	log.Printf("query: %v", rq)

	q, err := query.Parse(rq)
	if err != nil {
		return err
	}

	subtrees := []FileTree{}
	if topRepo == "" {
		opts := zoekt.ListOptions{
			Field: 0,
		}
		result, err := s.Searcher.List(ctx, q, &opts)
		if err != nil {
			return err
		}

		for _, re := range result.Repos {
			r := re.Repository
			if len(r.Branches) == 0 {
				// A non-git-like repo. For example plain dir.
				t := FileTree{
					KytheUri:      r.Name,
					Display:       r.Name,
					OnlyGenerated: false,
					IsFile:        false,
					Children:      nil,
				}
				subtrees = append(subtrees, t)

			} else {
				for _, b := range r.Branches {
					ticketId := r.Name + "@" + b.Name
					t := FileTree{
						KytheUri:      ticketId,
						Display:       ticketId,
						OnlyGenerated: false,
						IsFile:        false,
						Children:      nil,
					}
					subtrees = append(subtrees, t)
				}
			}
		}
	} else {
		result, err := s.Searcher.Search(ctx, q, &sOpts)
		if err != nil {
			return err
		}

		seen := map[string]bool{}
		for _, f := range result.Files {
			if f.Repository != topRepo {
				// See [repo filter]
				continue
			}
			if topBranch != "" {
				found := false
				for _, b := range f.Branches {
					if b == topBranch {
						found = true
						break
					}
				}
				if !found {
					continue
				}
			}
			prefix := ""
			if topPath != "" {
				prefix = topPath + "/"
			}
			relative := strings.TrimPrefix(f.FileName, prefix)
			relParts := strings.Split(relative, "/")
			currentPart := relParts[0]
			// Note: Zoekt won't return a sole directory as a match, only some files
			// within a directory. This also implies that any directory we encounter
			// will be non-empty.
			isFile := len(relParts) == 1
			if _, exists := seen[currentPart]; !exists {
				seen[currentPart] = true
				t := FileTree{
					KytheUri:      f.Repository + ":" + prefix + currentPart,
					Display:       currentPart,
					OnlyGenerated: false,
					IsFile:        isFile,
					// Note: as we query all files below 'top' now, we could as well
					// eagerly build the full subtree. That might be a future option.
					Children: nil,
				}
				subtrees = append(subtrees, t)
			}
		}
	}
	sort.Slice(subtrees, func(i, j int) bool {
		if subtrees[i].IsFile != subtrees[j].IsFile {
			return subtrees[j].IsFile
		}
		return subtrees[i].Display < subtrees[j].Display
	})

	w.Header().Set("Content-Type", "application/json; charset=UTF-8")
	w.WriteHeader(http.StatusOK)
	if err = json.NewEncoder(w).Encode(FileTree{
		KytheUri:      "toplevel",
		Display:       "wontshow",
		OnlyGenerated: false,
		IsFile:        false,
		Children:      &subtrees,
	}); err != nil {
		return err
	}
	//fmt.Fprintf(w, "{}", html.EscapeString(r.URL.Path))
	return nil
}

func (s *Server) serveSource(w http.ResponseWriter, r *http.Request) {
	if err := s.serveSourceErr(w, r); err != nil {
		http.Error(w, err.Error(), http.StatusTeapot)
	}
}

func (s *Server) serveSourceErr(w http.ResponseWriter, r *http.Request) error {
	log.Printf("request: %v", r.URL)
	tickets, ok := r.URL.Query()["ticket"]
	if !ok || len(tickets) > 1 {
		return fmt.Errorf("expected ticket parameter")
	}
	ticket := tickets[0]
	tick, err := parseTicket(ticket)
	if err != nil {
		return err
	}
	if !tick.complete() {
		return fmt.Errorf("Expected ticket in repo:path format")
	}
	repo := tick.repo
	branch := tick.branch
	path := tick.path

	sOpts := zoekt.SearchOptions{
		MaxWallTime: 10 * time.Second,
	}
	sOpts.SetDefaults()
	// TODO estimate matches and set max counts to enable result to be included.
	//   Normally there would be exactly 1 hit, but see [repo filter] comment.
	sOpts.Whole = true

	ctx := r.Context()

	// Note the [repo filter].
	rq := "r:" + repo + " f:^" + path + "$"
	if branch != "" {
		// NOTE(repo-filter,branch): the zoekt branch filter is substring-based
		//   and doesn't support regexp. So we filter later as well.
		rq += " b:" + branch
	}
	log.Printf("query: %v", rq)

	q, err := query.Parse(rq)
	if err != nil {
		return err
	}

	result, err := s.Searcher.Search(ctx, q, &sOpts)
	if err != nil {
		return err
	}

	for _, f := range result.Files {
		// See NOTE(repo-filter,branch) above.
		if f.Repository != repo {
			continue
		}
		if branch != "" && !slices.Contains(f.Branches, branch) {
			continue
		}
		w.Header().Set("Content-Type", "application/json; charset=UTF-8")
		if err = json.NewEncoder(w).Encode(SourceResponse{
			Content:  string(f.Content),
			Language: f.Language,
		}); err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			return err
		}
		return nil
	}
	return fmt.Errorf("Requested file not in response. Query: %v", rq)
}

// Serving decors is not supported, would need pre-calculated references.
func (s *Server) serveDecors(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json; charset=UTF-8")
	w.WriteHeader(http.StatusOK)
	// Just return an empty list of decors. string type arbitrarily chosen,
	// doesn't matter.
	if err := json.NewEncoder(w).Encode(struct {
		Decors []string `json:"decors"`
	}{
		Decors: []string{},
	}); err != nil {
		http.Error(w, err.Error(), http.StatusTeapot)
	}
}

type SourceResponse struct {
	Content  string `json:"content"`
	Language string `json:"language"`
}

// Mirrors Underhood's XRefReply (though the two converged away from original
// Kythe-only).
type UhXRefReply struct {
	Refs      []UhSiteGroup `json:"refs"`
	RefCounts UhRefCounts   `json:"refCounts"`
	// Below unused by zoekt-underhood, populated to default values.
	Calls        []string `json:"calls"`
	CallCount    int      `json:"callCount"`
	Definitions  []string `json:"definitions"`
	Declarations []string `json:"declarations"`
}

type UhRefCounts struct {
	Lines int `json:"rcLines"`
	Files int `json:"rcFiles"`
	// Exact file content match.
	DupFiles int `json:"rcDupFiles"`
	// The lines found in a given file are the same (other lines can differ).
	// Greater than or equal to DupFiles.
	DupMatches int `json:"rcDupMatches"`
}

type UhSiteGroup struct {
	Files []UhFileSites `json:"sFileSites"`
}

// fileSites is the internal version of UhFileSites, before some postprocessing
// steps could have happened.
type fileSites struct {
	containingFile UhDisplayedFile
	snippets       []UhSnippet
	// For deduping on file content.
	fileChecksum []byte
	// Hash of line content of snippets, for grouping.
	snippetsHash []byte
}

type UhFileSites struct {
	ContainingFile UhDisplayedFile  `json:"sContainingFile"`
	IsDupOf        *UhDisplayedFile `json:"sDupOfFile"`
	// NOTE(snippet-sort): the Snippets are sorted by their GroupSortId, and ties
	// are broken by line number (that is, appearance order).
	//
	// Warning: the GroupSortId-s need not be continuous.
	Snippets []UhSnippet `json:"sSnippets"`
}

type UhDisplayedFile struct {
	FileTicket  string   `json:"dfFileTicket"`
	DisplayName string   `json:"dfDisplayName"`
	Branches    []string `json:"dfBranches"`
	Language    string   `json:"dfLanguage"`
}

type UhSnippet struct {
	Text           string   `json:"snippetText"`
	FullSpan       CmRange  `json:"snippetFullSpan"`
	OccurrenceSpan CmRange  `json:"snippetOccurrenceSpan"`
	LinesBefore    []string `json:"linesBefore"`
	LinesAfter     []string `json:"linesAfter"`
	// NOTE(snippet-sort): When sorting the snippets of a file in different ways,
	// snippets in the same GroupSortId should be kept together.
	GroupSortId int `json:"groupSortId"`
}

type CmRange struct {
	From CmPoint `json:"from"`
	To   CmPoint `json:"to"`
}

type CmPoint struct {
	Line int `json:"line"`
	Ch   int `json:"ch"`
}

func (s *Server) serveSearchXref(w http.ResponseWriter, r *http.Request) {
	if err := s.serveSearchXrefErr(w, r); err != nil {
		http.Error(w, err.Error(), http.StatusTeapot)
	}
}

func (s *Server) serveSearchXrefErr(w http.ResponseWriter, r *http.Request) error {
	// Notes: Sources are assumed to be UTF-8 (that's what the UI expects).
	// If that wouldn't stand, either repos would need to be converted to UTF-8
	// before indexing, or we could attempt on-the-fly conversion here based on
	// heuristics.
	//
	// That said, since Zoekt API returns positions in bytes, but Underhood (and
	// CodeMirror that it uses) expects them in characters (codepoints?),
	// conversion between the two would be needed. Thankfully we would only need
	// to convert within the line, as line numbers are not affected. That could
	// be done, but in the mean time, correct line fragment spans are only
	// returned for plain-text code.
	log.Printf("request: %v", r.URL)
	selections, ok := r.URL.Query()["selection"]
	if !ok || len(selections) > 1 {
		return fmt.Errorf("expected selection parameter")
	}
	selection := selections[0]

	casings, ok := r.URL.Query()["casing"]
	casing := "auto"
	if ok {
		c := casings[0]
		if c == "yes" || c == "no" || c == "auto" {
			casing = c
		}
	}

	modes, ok := r.URL.Query()["mode"]
	mode := "Lax"
	if ok {
		m := modes[0]
		if m == "Lax" || m == "Boundary" || m == "Raw" {
			mode = m
		}
	}

	tickets, ok := r.URL.Query()["ticket"]
	if !ok {
		// Make up a dummy ticket, in case one was not supplied.
		tickets = []string{"nosuchrepo:nosuchfile"}
	}
	if len(tickets) > 1 {
		return fmt.Errorf("expected single ticket parameter")
	}
	ticket := tickets[0]
	// TODO(branch): codepath doesn't take ticket.branch into account.
	// Might do once the repo-filter was reworked.
	queryTicket, err := parseTicket(ticket)
	if err != nil {
		return err
	}

	ctx := r.Context()

	fileSites := []fileSites{}

	var rq string
	if mode == "Raw" {
		rq = selection
	} else {
		// See https://github.com/sourcegraph/zoekt/issues/139 for not wrapping in quotes
		moddedSelection := escapeLiteralQuery(selection)
		if mode == "Boundary" {
			moddedSelection = "\\b" + moddedSelection + "\\b"
		}
		rq = "case:" + casing + " " + moddedSelection
	}

	if err := s.appendSearches(rq, ctx, &fileSites); err != nil {
		return err
	}
	// Note: if the [repo filter] was more precise, we could shoot multiple
	// well-crafted queries and just concat them. But for now resort to sorting.
	//
	// TODO(repo-filter): actually repo-filter with recent zoekt accepts regex,
	// so this can be reworked.
	//
	sort.SliceStable(fileSites, func(i, j int) bool {
		ti, err := parseTicket(fileSites[i].containingFile.FileTicket)
		if err != nil {
			return false
		}
		tj, err := parseTicket(fileSites[j].containingFile.FileTicket)
		if err != nil {
			return false
		}
		if ti.repo != tj.repo {
			if ti.repo == queryTicket.repo {
				return true
			}
			if tj.repo == queryTicket.repo {
				return false
			}
		}
		// Same repo from now on.
		if ti.repo == queryTicket.repo && ti.path != tj.path {
			if ti.path == queryTicket.path {
				return true
			}
			if tj.path == queryTicket.path {
				return false
			}
		}
		return false // Keep original order
	})

	// keyed by file content hash (fileChecksum)
	seenTickets := map[string]UhDisplayedFile{}

	// keyed by match content hash (snippetsHash)
	contentGroups := map[string][]UhFileSites{}
	contentGroupOrder := []string{}

	snipCnt := 0
	fileCnt := 0
	fileDupCnt := 0
	matchDupCnt := 0
	for _, fs := range fileSites {
		// Dedup
		var dupTick *UhDisplayedFile = nil
		if seenTick, ok := seenTickets[string(fs.fileChecksum)]; ok {
			dupTick = &seenTick
			fileDupCnt += 1
		} else {
			seenTickets[string(fs.fileChecksum)] = fs.containingFile
		}
		// To content group
		h := string(fs.snippetsHash)
		s := UhFileSites{
			ContainingFile: fs.containingFile,
			IsDupOf:        dupTick,
			Snippets:       fs.snippets,
		}
		if _, ok := contentGroups[h]; ok {
			contentGroups[h] = append(contentGroups[h], s)
			matchDupCnt += 1
		} else {
			contentGroups[h] = []UhFileSites{s}
			contentGroupOrder = append(contentGroupOrder, h)
		}
		fileCnt += 1
		snipCnt += len(fs.snippets)
	}

	gs := []UhSiteGroup{}
	for _, h := range contentGroupOrder {
		gs = append(gs, UhSiteGroup{
			Files: contentGroups[h],
		})
	}

	if err := json.NewEncoder(w).Encode(UhXRefReply{
		Refs: gs,
		RefCounts: UhRefCounts{
			Lines:      snipCnt,
			Files:      fileCnt,
			DupFiles:   fileDupCnt,
			DupMatches: matchDupCnt,
		},
		Calls:        []string{},
		CallCount:    0,
		Definitions:  []string{},
		Declarations: []string{},
	}); err != nil {
		return err
	}
	return nil
}

func (s *Server) appendSearches(rq string, ctx context.Context, manyFileSites *[]fileSites) error {
	log.Printf("query: %v", rq)
	q, err := query.Parse(rq)
	if err != nil {
		return err
	}
	numContextLines := 1 // TODO(configure,plumb)

	sOpts := zoekt.SearchOptions{
		MaxWallTime:     10 * time.Second,
		NumContextLines: numContextLines,
	}
	sOpts.SetDefaults()

	// Number of files to return - fixed for now. TODO(configure): expose as param
	num := 500

	// TODO(limits): revise in light of zoekt upstream changes. See for example ShardRepoMaxMatchCount.
	// BEGIN cargo-cult limiting from zoekt:internal/json/json.go
	if result, err := s.Searcher.Search(ctx, q, &zoekt.SearchOptions{EstimateDocCount: true}); err != nil {
		return err
	} else if numdocs := result.ShardFilesConsidered; numdocs > 10000 {
		// If the search touches many shards and many files, we
		// have to limit the number of matches.  This setting
		// is based on the number of documents eligible after
		// considering reponames, so large repos (both
		// android, chromium are about 500k files) aren't
		// covered fairly.

		// 10k docs, 50 num -> max match = (250 + 250 / 10)
		sOpts.ShardMaxMatchCount = num*5 + (5*num)/(numdocs/1000)
	} else {
		// Virtually no limits for a small corpus; important
		// matches are just as expensive as normal matches.
		n := numdocs + num*100
		sOpts.ShardMaxMatchCount = n
		sOpts.TotalMaxMatchCount = n
	}
	sOpts.MaxDocDisplayCount = num

	result, err := s.Searcher.Search(ctx, q, &sOpts)
	if err != nil {
		return err
	}

	for _, f := range result.Files {
		// NOTE(branches): the ticket we return here is a branch-less
		// ticket, and we indicate the possible branches separately. The client
		// will need to compose the specific branch they are interested in upon
		// request.
		ticket := f.Repository
		ticket += ":" + f.FileName
		inFile := UhDisplayedFile{
			FileTicket:  ticket,
			DisplayName: ticket,
			Branches:    f.Branches,
			Language:    f.Language,
		}
		snippets := []UhSnippet{}
		snippetsHash := sha1.New()
		nextGroudSortId := 0
		for _, l := range f.LineMatches {
			// For now we only return first fragment match in line for bolding.
			firstFrag := l.LineFragments[0]
			lineNum := l.LineNumber - 1
			snippetsHash.Write(l.Line)
			// TODO(text): handle if non-UTF8 etc?
			clippedLine := string(l.Line)
			if len(clippedLine) > 250 {
				// TODO(text): adjust returned line/ch values? or otherwise indicate clip?
				clippedLine = clippedLine[:30] + " [[[...line too long, clipped...]]] " + clippedLine[len(clippedLine)-30:]
			}
			snippet := UhSnippet{
				Text: clippedLine,
				// Inventing one based on approximation.
				FullSpan: CmRange{
					From: CmPoint{
						Line: lineNum,
						Ch:   0,
					},
					To: CmPoint{
						Line: lineNum,
						// TODO: Zoekt supplies range in bytes, while we need chars.
						//       Would need to convert based on observing line content.
						// Note: the end is not really used now.
						Ch: l.LineEnd - l.LineStart,
					},
				},
				OccurrenceSpan: CmRange{
					From: CmPoint{
						Line: lineNum,
						Ch:   hackyConv(clippedLine, firstFrag.LineOffset), // TODO convert better
					},
					To: CmPoint{
						Line: lineNum,
						Ch:   hackyConv(clippedLine, firstFrag.LineOffset+firstFrag.MatchLength), // TODO convert better
					},
				},
				LinesBefore: strings.Split(string(l.Before), "\n"),
				LinesAfter:  strings.Split(string(l.After), "\n"),
				GroupSortId: nextGroudSortId,
			}
			snippets = append(snippets, snippet)
			nextGroudSortId += 1
		}
		// First sort by line num to remove dup context lines and reassign group.
		// Then restore group sort.
		sort.Slice(snippets, func(i, j int) bool {
			return snippets[i].lineNum() < snippets[j].lineNum()
		})
		maxLine := -999999
		for i := range snippets {
			s := &snippets[i]
			lineNum := s.lineNum()
			if i < len(snippets)-1 {
				// Remove any LinesAfter that matches or runs over the next snippet.
				aftersToRemove := lineNum + len(s.LinesAfter) - snippets[i+1].lineNum() + 1
				if aftersToRemove > 0 {
					k := len(s.LinesAfter) - aftersToRemove
					s.LinesAfter = s.LinesAfter[:k]
				}
			}
			if i > 0 {
				// Remove any LinesBefore that was already covered.
				beforesToRemove := maxLine - (lineNum - len(s.LinesBefore)) + 1
				if beforesToRemove > 0 {
					s.LinesBefore = s.LinesBefore[beforesToRemove:]
					s.GroupSortId = snippets[i-1].GroupSortId
				}
			}
			maxLine = max(maxLine, lineNum+len(s.LinesAfter))
		}
		sort.SliceStable(snippets, func(i, j int) bool {
			return snippets[i].GroupSortId < snippets[j].GroupSortId
		})
		*manyFileSites = append(*manyFileSites, fileSites{
			containingFile: inFile,
			snippets:       snippets,
			fileChecksum:   f.Checksum,
			snippetsHash:   snippetsHash.Sum(nil),
		})
	}
	return nil
}

func (s *UhSnippet) lineNum() int {
	return s.OccurrenceSpan.From.Line
}

// hackyConv returns the codepoint(aka rune)-offset for the given byte offset
// in the given (UTF-8) string. It is hacky for two reasons. The minor one is
// performance, we reiterate etc. The other one is that these are the UTF-8
// codepoint offsets on a potentially unnormalized string. While the frontend
// in Javascript uses UTF-16, so there can be problems with non-BMP codepoints?
// Not exactly sure, would need to test. For now this covers the trivial
// examples.
func hackyConv(s string, offs int) int {
	i := 0
	for totalBytes, _ := range s {
		if totalBytes >= offs {
			return i
		}
		i += 1
	}
	return i
}

// ticket breaks down a unique artifact identifier to its parts.
//
// Any param is empty if not present in ticket.
type ticket struct {
	// The repo name. Might contain slashes (just saying).
	repo string
	// The branch, tag, or other version-name associated with the indexed
	// artifact. Dependent on the indexer. Can be empty.
	branch string
	// Empty for non-file artifact.
	path string
}

// parseTicket parses a stringy ticket of form "some/repo-name[@branch][:some/path]".
func parseTicket(t string) (ticket, error) {
	// NOTE(filename-colon,ticket-escaping): escaping would be needed, in case filename can contain colon.
	//   But, it seems Zoekt doesn't escape either internally (see ResultID), so
	//   maybe we can live with assuming colon won't be part of filenames.
	//   Or, we can survive colons, if we only care about the initial single colon
	//   that delimits the repo@version part.
	parts := strings.SplitN(t, ":", 2)
	res := ticket{}
	if len(parts) > 0 {
		repoParts := strings.SplitN(parts[0], "@", 2)
		res.repo = repoParts[0]
		if len(repoParts) > 1 {
			res.branch = repoParts[1]
		}
	}
	if len(parts) > 1 {
		res.path = parts[1]
	}
	return res, nil
}

// It is fine for version to be empty.
func (t *ticket) complete() bool {
	return t.repo != "" && t.path != ""
}

func escapeLiteralQuery(s string) string {
	toEscape := ":()[]\\.*?^$+{}, "
	var r strings.Builder
	for _, c := range s {
		if strings.ContainsAny(string(c), toEscape) {
			r.WriteRune('\\')
		}
		r.WriteRune(c)
	}
	return r.String()
}
