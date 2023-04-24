namespace MastodonPlayground

module LuceneExperiment =
    open Lucene.Net.Util
    open Lucene.Net.Store
    open Lucene.Net.Analysis.Standard
    open Lucene.Net.Index
    open Lucene.Net.Documents
    open Lucene.Net.Search
    open Lucene.Net.Facet.Taxonomy
    open Lucene.Net.Facet.Taxonomy.Directory
    open Lucene.Net.Facet

    let private luceneVersion = LuceneVersion.LUCENE_48;

    let cleanUsername (username:string) =
        //username.Replace("@", "_").Replace(".", "_")
        username
        
    let combine path (username:string) =
        System.IO.Path.Combine(
            path,
            (username |> cleanUsername)
        )

    let private luceneIndexLocation =
        FileAccess.getTempDirectoryPath "luceneIndex" |> combine

    let private luceneTaxonomyLocation = 
        FileAccess.getTempDirectoryPath "luceneTaxonomy" |> combine

    let private FIELD_AUTHOR = "author"
    let private FIELD_CONTENT = "content"
    let private FIELD_URL = "url"
    let private FIELD_CREATED_AT = "created_at"
    let private FIELD_TAG = "tag"

    let private facetConfig =
        let config = new FacetsConfig()
        config.SetMultiValued(FIELD_TAG, true)
        config.SetHierarchical(FIELD_CREATED_AT, true)
        config

    let private toDocumentWithCategories (post:Mastonet.Entities.Status) =
        let doc = new Document();
        
        let getValue func (post:Mastonet.Entities.Status) =
            match post.Reblog with
            | null -> func post
            | _ -> func post.Reblog

        let author = getValue (fun p -> p.Account.AccountName) post
        let createdAt = getValue (fun p -> p.CreatedAt) post
        let tags = getValue (fun p -> p.Tags) post

        doc.Add(new TextField(FIELD_AUTHOR, author, Field.Store.YES));
        doc.Add(new FacetField(FIELD_AUTHOR, author))
        doc.Add(new TextField(FIELD_CONTENT, (getValue (fun p -> p.Content) post), Field.Store.YES));
        doc.Add(new TextField(FIELD_URL, (getValue (fun p -> p.Url) post), Field.Store.YES));
        doc.Add(new TextField(FIELD_CREATED_AT, createdAt.ToString("o"), Field.Store.YES));
        doc.Add(new FacetField(FIELD_CREATED_AT, createdAt.Year.ToString(), createdAt.Month.ToString()))
        
        // add tags as separate fields
        tags |> Seq.iter (fun t -> doc.Add(new TextField(FIELD_TAG, t.Name, Field.Store.YES)));
        tags |> Seq.iter (fun t -> doc.Add(new FacetField(FIELD_TAG, t.Name)))

        doc

    let private indexPostsInternal username posts =
        
        let indexLocation = username |> luceneIndexLocation
        printfn "Lucene index location: %s" indexLocation

        let facetLocation = username |> luceneTaxonomyLocation
        printfn "Lucene taxonomy location: %s" facetLocation

        // facet bits
        use taxonomyDir = FSDirectory.Open(facetLocation);
        use taxonomyWriter = new DirectoryTaxonomyWriter(taxonomyDir, OpenMode.CREATE)
        
        // index writing bits
        use indexDir = FSDirectory.Open(indexLocation);
        let standardAnalyzer = new StandardAnalyzer(luceneVersion);
        let indexConfig = new IndexWriterConfig(luceneVersion, standardAnalyzer);
        indexConfig.OpenMode <- OpenMode.CREATE
        use writer = new IndexWriter(indexDir, indexConfig);
        
        let addToIndex doc =
            let builtDocument = facetConfig.Build(taxonomyWriter, doc)
            writer.AddDocument(builtDocument)

        //add posts to index
        posts
        |> List.map toDocumentWithCategories
        |> List.iter addToIndex

        //Flush and commit the index data to the directory
        writer.Commit();
        taxonomyWriter.Commit()

    let indexPosts accessToken username =

        printfn "Indexing posts for user %s" username
        
        // get posts from either local cache or API
        let posts = username |> MastodonClientAdapter.getPostsFromFileOrWeb accessToken

        match posts with
        | [] -> 
            printfn "No posts to index for user %s" username
        | _ ->
            posts |> indexPostsInternal username
            printfn "Done indexing %i posts for user %s" (posts.Length) username

    let searchPosts username searchQuery =

        printfn "Searching posts for query %s" searchQuery

        // search
        use indexDir = FSDirectory.Open(username |> luceneIndexLocation)
        let searcher = new IndexSearcher(DirectoryReader.Open(indexDir))
        let queryBuilder = new QueryBuilder(new StandardAnalyzer(luceneVersion))
        let query = queryBuilder.CreatePhraseQuery(FIELD_CONTENT, searchQuery, 1)
        
        // now facets
        use taxonomyDir = FSDirectory.Open(username |> luceneTaxonomyLocation)
        let taxoReader = new DirectoryTaxonomyReader(taxonomyDir)
        let collector = new FacetsCollector()
        let topDocs = FacetsCollector.Search(searcher, query, 10, collector)

        printfn "Found %i posts" topDocs.TotalHits

        let facets = new FastTaxonomyFacetCounts(FacetsConfig.DEFAULT_INDEX_FIELD_NAME, taxoReader, facetConfig, collector);
        let tagFacets = facets.GetTopChildren(10, FIELD_TAG)
        if (tagFacets = null |> not) then
            tagFacets.LabelValues |> Seq.iter (fun (labelValue) -> printfn "%s: %f" (labelValue.Label) (labelValue.Value))
        let authorFacets = facets.GetTopChildren(10, FIELD_AUTHOR)
        authorFacets.LabelValues |> Seq.iter (fun (labelValue) -> printfn "%s: %f" (labelValue.Label) (labelValue.Value))
        let createdFacets = facets.GetTopChildren(10, FIELD_CREATED_AT)
        createdFacets.LabelValues |> Seq.iter (fun (labelValue) -> printfn "%s: %f" (labelValue.Label) (labelValue.Value))
        let createdMonthFacets = facets.GetTopChildren(10, FIELD_CREATED_AT, "2023")
        createdMonthFacets.LabelValues |> Seq.iter (fun (labelValue) -> printfn "%s: %f" (labelValue.Label) (labelValue.Value))

        System.Console.WriteLine("Press any key to continue...")
        System.Console.ReadLine() |> ignore

        topDocs.ScoreDocs
        |> Array.map (fun scoreDoc -> (scoreDoc, searcher.Doc(scoreDoc.Doc)))
        |> Array.iter (fun (scoreDoc, doc) ->
            printfn "%s" (doc.Get(FIELD_CONTENT))
            printfn "URL: %s" (doc.Get(FIELD_URL))
            printfn "Created: %s" (doc.Get(FIELD_CREATED_AT))
            doc.GetValues(FIELD_TAG) |> Array.iter (fun tag -> printfn "Tag: %s" tag)
            printfn "Author: %s" (doc.Get(FIELD_AUTHOR))
            printfn "Score: %f" (scoreDoc.Score)
            printfn ""
        )
