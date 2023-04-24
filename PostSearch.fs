namespace MastodonPlayground

module PostSearch =
    open MastodonClientAdapter


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