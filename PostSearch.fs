namespace MastodonPlayground

module PostSearch =

    let private toFilename username = 
        System.IO.Path.Combine(
            System.IO.Path.GetTempPath(),
            $"posts-{username}.json"
        )

    let private getPostsFromFileOrWeb accessToken username =
        let postsFile = toFilename username

        let exists = System.IO.File.Exists(postsFile)
        match exists with
        | true ->
            printfn "Loading posts from file %s" postsFile
            let json = System.IO.File.ReadAllText(postsFile)
            Newtonsoft.Json.JsonConvert.DeserializeObject<List<Mastonet.Entities.Status>>(json)
        | false ->
            printfn "Getting posts from API"
            let postsOption = MastodonClientAdapter.getPosts accessToken username

            match postsOption with
            | None -> 
                printfn "No posts found"
                []
            | Some posts -> 
                let json = Newtonsoft.Json.JsonConvert.SerializeObject(posts)
                System.IO.File.WriteAllText(postsFile, json)
                posts

    let run accessToken username (term:string) =
        
        // see if we have posts saved
        let posts = getPostsFromFileOrWeb accessToken username
        
        let postsWithTerm =
            posts
            |> List.filter (fun post -> post.Content.Contains(term, System.StringComparison.OrdinalIgnoreCase))
        
        printfn "Found %i posts with term %s" postsWithTerm.Length term

        postsWithTerm
        |> List.iter (fun post -> printfn "%s\n\n" post.Content)