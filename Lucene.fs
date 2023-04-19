namespace MastodonPlayground

module LuceneExperiment =
    open Lucene.Net.Util
    open Lucene.Net.Store
    open Lucene.Net.Analysis.Standard
    open Lucene.Net.Index
    open Lucene.Net.Documents

    let private toDocument (post:Mastonet.Entities.Status) =
        let doc = new Document();
        
        doc.Add(new TextField("author", post.Account.AccountName, Field.Store.YES));
        doc.Add(new TextField("content", post.Content, Field.Store.YES));
        doc.Add(new TextField("url", post.Url, Field.Store.YES));
        doc.Add(new TextField("tags", post.Tags |> Seq.map (fun t -> t.Name) |> String.concat " ", Field.Store.YES));
        doc.Add(new TextField("created_at", post.CreatedAt.ToString("o"), Field.Store.YES));
        
        doc

    let indexPosts accessToken username =

        printfn "Indexing posts for user %s" username
        
        // get posts from either local cache or API
        let posts =
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


        let luceneVersion = LuceneVersion.LUCENE_48;

        let luceneIndexLocation = FileAccess.getTempDirectoryPath "luceneIndex";

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

        printfn "Done indexing %i posts for user %s" (posts.Length) username