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
    let private luceneIndexLocation = FileAccess.getTempDirectoryPath "luceneIndex";
    let private luceneTaxonomyLocation = FileAccess.getTempDirectoryPath "luceneTaxonomy";
    let private facetConfig =
        let config = new FacetsConfig()
        config.SetMultiValued("tag", true)
        config.SetHierarchical("created", true)
        config

    let getPostsForIndexing accessToken username =
        match Configuration.doUseCache with
        | true ->
            printfn "Getting posts from cache"
            let fromFilesystem = FileAccess.getUserPostsLastFetchTime username

            match fromFilesystem with
            | Some content ->
                content
                |> Serialization.deserialize<List<Mastonet.Entities.Status>>
            | None -> 
                []

        | false ->
            printfn "Getting posts from Mastodon"
            let fromApi = MastodonClientAdapter.getPosts accessToken username
            
            match fromApi with
            | None -> 
                []
            | Some unwrapped -> unwrapped

    let private toDocumentWithCategories (post:Mastonet.Entities.Status) =
        let doc = new Document();
        
        doc.Add(new TextField("author", post.Account.AccountName, Field.Store.YES));
        doc.Add(new FacetField("author", post.Account.AccountName))
        doc.Add(new TextField("content", post.Content, Field.Store.YES));
        doc.Add(new TextField("url", post.Url, Field.Store.YES));
        doc.Add(new TextField("created_at", post.CreatedAt.ToString("o"), Field.Store.YES));
        doc.Add(new FacetField("created", post.CreatedAt.Year.ToString(), post.CreatedAt.Month.ToString()))
        
        // add tags as separate fields
        post.Tags |> Seq.iter (fun t -> doc.Add(new TextField("tag", t.Name, Field.Store.YES)));
        post.Tags |> Seq.iter (fun t -> doc.Add(new FacetField("tag", t.Name)))

        doc

    let private indexPostsInternal posts =
        
        printfn "Lucene index location: %s" luceneIndexLocation

        // facet bits
        use taxonomyDir = FSDirectory.Open(luceneTaxonomyLocation);
        use taxonomyWriter = new DirectoryTaxonomyWriter(taxonomyDir, OpenMode.CREATE)
        
        // index writing bits
        use indexDir = FSDirectory.Open(luceneIndexLocation);
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
        let posts = getPostsForIndexing accessToken username

        indexPostsInternal posts

        // facetPostsInternal posts

        printfn "Done indexing %i posts for user %s" (posts.Length) username

    let searchPosts searchQuery =

        printfn "Searching posts for query %s" searchQuery

        // search
        use indexDir = FSDirectory.Open(luceneIndexLocation)
        let searcher = new IndexSearcher(DirectoryReader.Open(indexDir))
        let queryBuilder = new QueryBuilder(new StandardAnalyzer(luceneVersion))
        let query = queryBuilder.CreatePhraseQuery("content", searchQuery, 1)
        
        // now facets
        use taxonomyDir = FSDirectory.Open(luceneTaxonomyLocation)
        let taxoReader = new DirectoryTaxonomyReader(taxonomyDir)
        let collector = new FacetsCollector()
        let topDocs = FacetsCollector.Search(searcher, query, 10, collector)

        printfn "Found %i posts" topDocs.TotalHits

        let facets = new FastTaxonomyFacetCounts(FacetsConfig.DEFAULT_INDEX_FIELD_NAME, taxoReader, facetConfig, collector);
        let tagFacets = facets.GetTopChildren(10, "tag")
        tagFacets.LabelValues |> Seq.iter (fun (labelValue) -> printfn "%s: %f" (labelValue.Label) (labelValue.Value))
        let authorFacets = facets.GetTopChildren(10, "author")
        authorFacets.LabelValues |> Seq.iter (fun (labelValue) -> printfn "%s: %f" (labelValue.Label) (labelValue.Value))
        let createdFacets = facets.GetTopChildren(10, "created")
        createdFacets.LabelValues |> Seq.iter (fun (labelValue) -> printfn "%s: %f" (labelValue.Label) (labelValue.Value))
        let createdMonthFacets = facets.GetTopChildren(10, "created", "2023")
        createdMonthFacets.LabelValues |> Seq.iter (fun (labelValue) -> printfn "%s: %f" (labelValue.Label) (labelValue.Value))

        System.Console.WriteLine("Press any key to continue...")
        System.Console.ReadLine() |> ignore

        topDocs.ScoreDocs
        |> Array.map (fun scoreDoc -> (scoreDoc, searcher.Doc(scoreDoc.Doc)))
        |> Array.iter (fun (scoreDoc, doc) ->
            printfn "%s" (doc.Get("content"))
            printfn "URL: %s" (doc.Get("url"))
            printfn "Created: %s" (doc.Get("created_at"))
            doc.GetValues("tag") |> Array.iter (fun tag -> printfn "Tag: %s" tag)
            printfn "Author: %s" (doc.Get("author"))
            printfn "Score: %f" (scoreDoc.Score)
            printfn ""
        )
