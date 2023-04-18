namespace MastodonPlayground

module PostSearch =
    open MastodonClientAdapter


    let private getPostsFromFileOrWeb accessToken username =
        
        let localCache =
            match Configuration.doUseCache with
            | true ->
                printfn "Getting posts from cache"
                FileAccess.getUserPosts username
            | false -> None

        match localCache with
        | Some content ->
            content |> Serialization.deserialize<List<Mastonet.Entities.Status>>
        | None ->
            printfn "Getting posts from API"
            let postsOption = getPosts accessToken username

            match postsOption with
            | None -> 
                printfn "No posts found"
                []
            | Some posts -> 
                posts 
                    |> Serialization.serialize
                    |> FileAccess.saveUserPosts username
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