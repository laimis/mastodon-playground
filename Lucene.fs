namespace MastodonPlayground

module LuceneExperiment =
    open Lucene.Net.Util
    open Lucene.Net.Store
    open Lucene.Net.Analysis.Standard
    open Lucene.Net.Index
    open Lucene.Net.Documents
    open Lucene.Net.Search

    let private luceneVersion = LuceneVersion.LUCENE_48;
    let private luceneIndexLocation = FileAccess.getTempDirectoryPath "luceneIndex";

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
    let private toDocument (post:Mastonet.Entities.Status) =
        let doc = new Document();
        
        doc.Add(new TextField("author", post.Account.AccountName, Field.Store.YES));
        doc.Add(new TextField("content", post.Content, Field.Store.YES));
        doc.Add(new TextField("url", post.Url, Field.Store.YES));
        doc.Add(new TextField("tags", post.Tags |> Seq.map (fun t -> t.Name) |> String.concat " ", Field.Store.YES));
        doc.Add(new TextField("created_at", post.CreatedAt.ToString("o"), Field.Store.YES));
        
        doc

    let private indexPostsInternal posts =
        
        printfn "Lucene index location: %s" luceneIndexLocation

        use indexDir = FSDirectory.Open(luceneIndexLocation);

        //Create an analyzer to process the text 
        let standardAnalyzer = new StandardAnalyzer(luceneVersion);

        //Create an index writer
        let indexConfig = new IndexWriterConfig(luceneVersion, standardAnalyzer);
        indexConfig.OpenMode <- OpenMode.CREATE
        use writer = new IndexWriter(indexDir, indexConfig);

        let addToIndex doc =
            writer.AddDocument(doc)

        //add posts to index
        posts |> List.map toDocument |> List.iter addToIndex

        //Flush and commit the index data to the directory
        writer.Commit();

    let indexPosts accessToken username =

        printfn "Indexing posts for user %s" username
        
        // get posts from either local cache or API
        let posts = getPostsForIndexing accessToken username

        indexPostsInternal posts

        printfn "Done indexing %i posts for user %s" (posts.Length) username

    let searchPosts searchQuery =

        printfn "Searching posts for query %s" searchQuery

        use indexDir = FSDirectory.Open(luceneIndexLocation);

        let searcher = new IndexSearcher(DirectoryReader.Open(indexDir));

        let query = new TermQuery(new Term("content", "searchQuery"));

        let topDocs = searcher.Search(query, 10);

        printfn "Found %i posts" topDocs.TotalHits

        topDocs.ScoreDocs
        |> Array.map (fun x -> searcher.Doc(x.Doc))
        |> Array.iter (fun x ->
            printfn "%s" (x.Get("content"))
            printfn "%s" (x.Get("url"))
        )
