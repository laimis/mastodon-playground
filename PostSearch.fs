namespace MastodonPlayground

module PostSearch =
    open MastodonClientAdapter

    let private toFilename username = 
        // filename should include the timestamp so that we can refresh the data
        let timestamp = System.DateTime.Now.ToString("yyyyMMddHHmm")
        System.IO.Path.Combine(
            System.IO.Path.GetTempPath(),
            $"posts-{username}-{timestamp}.json"
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
            let postsOption = getPosts accessToken username All

            match postsOption with
            | None -> 
                printfn "No posts found"
                []
            | Some posts -> 
                let json = Newtonsoft.Json.JsonConvert.SerializeObject(posts)
                System.IO.File.WriteAllText(postsFile, json)
                posts

    let run accessToken username (term:string) =
        
        let getContentForStatus (status:Mastonet.Entities.Status) =
            match status.Reblog with
            | null -> status.Content
            | _ -> status.Reblog.Content

        let postsWithTerm =
            getPostsFromFileOrWeb accessToken username
            |> List.map (fun s -> (s, s |> getContentForStatus))
            |> List.filter (fun (_, content) -> content.Contains(term, System.StringComparison.OrdinalIgnoreCase))
            
        printfn "Found %i posts with term %s" postsWithTerm.Length term

        postsWithTerm
        |> List.iter (fun (post, content) ->
            let link = post.Url
            printfn "%s" content
            printfn "%s" link
            printfn "\n"
            printfn "Press any key to continue"
            System.Console.ReadKey() |> ignore
            System.Console.Clear() |> ignore
        )